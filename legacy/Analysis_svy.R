# Analysis_svy.R
# Survey-design-aware refactor of Analysis_double_disagg_2.R.
# Workflow unchanged: caller supplies an analysis plan + dataset, gets a
# long-format result table back. Estimates now use srvyr (survey package under
# the hood) so weights/strata/clusters are respected and SE + 95% CI are
# included in the output.

suppressPackageStartupMessages({
  library(dplyr)
  library(srvyr)
  library(purrr)
  library(tibble)
  library(rlang)
  library(cli)
})

# ---- design constructor ------------------------------------------------------

make_design <- function(df,
                        weights = NULL,
                        strata  = NULL,
                        ids     = NULL,
                        fpc     = NULL,
                        nest    = FALSE) {
  to_formula <- function(x) if (is.null(x)) NULL else stats::as.formula(paste0("~", x))

  # Build a survey::svydesign (formula-based, no tidyselect quirks) and wrap.
  sd <- survey::svydesign(
    ids     = if (is.null(ids)) ~1 else to_formula(ids),
    weights = to_formula(weights),
    strata  = to_formula(strata),
    fpc     = to_formula(fpc),
    data    = df,
    nest    = nest
  )
  srvyr::as_survey(sd)
}

# ---- multi-select expansion --------------------------------------------------

# Kobo-style multi-select columns are often already expanded as sibling columns
# named `var/opt`, `var.opt`, or `var___opt`. We detect those before splitting
# the string column so we don't clash with existing names.

.MS_SEPS <- c("___", "/", ".")

detect_ms_options <- function(df, vars) {
  out <- setNames(vector("list", length(vars)), vars)
  for (v in vars) {
    prefixes <- paste0(v, .MS_SEPS)
    matches <- names(df)[vapply(names(df), function(n) {
      any(vapply(prefixes, function(p) startsWith(n, p), logical(1)))
    }, logical(1))]
    out[[v]] <- matches
  }
  out
}

expand_multiselect <- function(df, vars, sep = "; ") {
  ms_options <- list()
  for (v in vars) {
    existing <- detect_ms_options(df, v)[[v]]
    if (length(existing) > 0) {
      ms_options[[v]] <- existing
      next
    }
    if (!v %in% names(df)) {
      warning("expand_multiselect: variable '", v, "' not found in df; skipping")
      next
    }

    tokens_per_row <- strsplit(as.character(df[[v]]), sep, fixed = TRUE)
    all_tokens <- unique(unlist(tokens_per_row))
    all_tokens <- all_tokens[!is.na(all_tokens) & nzchar(all_tokens)]

    new_cols <- character(length(all_tokens))
    for (i in seq_along(all_tokens)) {
      tok <- all_tokens[i]
      candidate <- paste0(v, "___", tok)
      final <- candidate
      k <- 1
      while (final %in% names(df)) {
        final <- paste0(candidate, "_x", k)
        k <- k + 1
        warning("expand_multiselect: name collision for '", candidate,
                "'; using '", final, "'")
      }
      flags <- as.integer(vapply(tokens_per_row,
                                 function(toks) tok %in% toks, logical(1)))
      flags[is.na(df[[v]])] <- NA_integer_
      df[[final]] <- flags
      new_cols[i] <- final
    }
    ms_options[[v]] <- new_cols
  }
  attr(df, "ms_options") <- ms_options
  df
}

# Recover the option label from an expanded column name.
.option_label <- function(ques, opt) {
  for (sep in .MS_SEPS) {
    pref <- paste0(ques, sep)
    if (startsWith(opt, pref)) {
      return(substr(opt, nchar(pref) + 1L, nchar(opt)))
    }
  }
  opt
}

# ---- aggregators (uniform return shape) --------------------------------------

# Every aggregator returns a tibble with columns:
#   Var1, Freq, SE, CI_low, CI_high, aggregation_method, variable,
#   count, valid, disaggregation, disagg_level

.empty_row <- function(ques, method, disag, level) {
  tibble::tibble(
    Var1 = NA_character_,
    Freq = NA_real_, SE = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
    aggregation_method = method,
    variable = ques,
    count = 0L, valid = 0L,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

.svy_data <- function(design) design$variables

single_select_svy <- function(design, ques, disag, level, ms_options = NULL) {
  vals <- .svy_data(design)[[ques]]
  valid_n <- sum(!is.na(vals))
  if (valid_n == 0) return(.empty_row(ques, "perc", disag, level))

  d <- design %>% srvyr::filter(!is.na(.data[[ques]]))
  res <- d %>%
    srvyr::group_by(!!rlang::sym(ques)) %>%
    srvyr::summarise(
      prop = srvyr::survey_mean(vartype = c("se", "ci"), na.rm = TRUE),
      cnt  = srvyr::unweighted(dplyr::n())
    )

  tibble::tibble(
    Var1   = as.character(res[[ques]]),
    Freq   = res$prop     * 100,
    SE     = res$prop_se  * 100,
    CI_low = res$prop_low * 100,
    CI_high= res$prop_upp * 100,
    aggregation_method = "perc",
    variable = ques,
    count = res$cnt,
    valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

multi_select_svy <- function(design, ques, disag, level, ms_options = NULL) {
  if (is.null(ms_options) || is.null(ms_options[[ques]])) {
    ms_options <- list()
    ms_options[[ques]] <- detect_ms_options(.svy_data(design), ques)[[ques]]
  }
  opts <- ms_options[[ques]]
  if (length(opts) == 0) {
    warning("multi_select_svy: no expanded binary columns found for '", ques,
            "'. Run expand_multiselect() first.")
    return(.empty_row(ques, "perc", disag, level))
  }

  rows <- purrr::map_dfr(opts, function(opt) {
    vals <- .svy_data(design)[[opt]]
    valid_n <- sum(!is.na(vals))
    if (valid_n == 0) {
      return(tibble::tibble(
        Var1 = .option_label(ques, opt),
        Freq = NA_real_, SE = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
        count = 0L, valid = 0L
      ))
    }
    d <- design %>% srvyr::filter(!is.na(.data[[opt]]))
    r <- d %>% srvyr::summarise(
      prop = srvyr::survey_mean(as.numeric(.data[[opt]]),
                                vartype = c("se", "ci"), na.rm = TRUE),
      cnt  = srvyr::unweighted(sum(.data[[opt]] == 1, na.rm = TRUE))
    )
    tibble::tibble(
      Var1   = .option_label(ques, opt),
      Freq   = r$prop     * 100,
      SE     = r$prop_se  * 100,
      CI_low = r$prop_low * 100,
      CI_high= r$prop_upp * 100,
      count  = r$cnt,
      valid  = valid_n
    )
  })

  rows$aggregation_method <- "perc"
  rows$variable           <- ques
  rows$disaggregation     <- as.character(disag)
  rows$disagg_level       <- as.character(level)
  rows
}

.summary_stat <- function(design, ques, disag, level, method,
                          summariser) {
  vals <- .svy_data(design)[[ques]]
  valid_n <- sum(!is.na(suppressWarnings(as.numeric(vals))))
  if (valid_n == 0) return(.empty_row(ques, method, disag, level))

  d <- design %>% srvyr::filter(!is.na(.data[[ques]]))
  res <- summariser(d, ques)

  tibble::tibble(
    Var1 = NA_character_,
    Freq    = as.numeric(res$val),
    SE      = as.numeric(res$val_se),
    CI_low  = as.numeric(res$val_low),
    CI_high = as.numeric(res$val_upp),
    aggregation_method = method,
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

stat_mean_svy <- function(design, ques, disag, level, ms_options = NULL) {
  .summary_stat(design, ques, disag, level, "mean", function(d, q) {
    d %>% srvyr::summarise(
      val = srvyr::survey_mean(as.numeric(.data[[q]]),
                               vartype = c("se", "ci"), na.rm = TRUE)
    )
  })
}

stat_sum_svy <- function(design, ques, disag, level, ms_options = NULL) {
  .summary_stat(design, ques, disag, level, "sum", function(d, q) {
    d %>% srvyr::summarise(
      val = srvyr::survey_total(as.numeric(.data[[q]]),
                                vartype = c("se", "ci"), na.rm = TRUE)
    )
  })
}

stat_quantile_svy <- function(design, ques, disag, level, q, method,
                              ms_options = NULL) {
  vals <- .svy_data(design)[[ques]]
  valid_n <- sum(!is.na(suppressWarnings(as.numeric(vals))))
  if (valid_n == 0) return(.empty_row(ques, method, disag, level))

  d <- design %>% srvyr::filter(!is.na(.data[[ques]]))
  res <- d %>% srvyr::summarise(
    val = srvyr::survey_quantile(as.numeric(.data[[ques]]),
                                 quantiles = q,
                                 vartype   = c("se", "ci"),
                                 na.rm     = TRUE)
  )
  # survey_quantile returns columns named val_q<NN>, val_q<NN>_se, _low, _upp
  qstem <- grep("^val_q\\d+$", names(res), value = TRUE)[1]

  tibble::tibble(
    Var1 = NA_character_,
    Freq    = as.numeric(res[[qstem]]),
    SE      = as.numeric(res[[paste0(qstem, "_se")]]),
    CI_low  = as.numeric(res[[paste0(qstem, "_low")]]),
    CI_high = as.numeric(res[[paste0(qstem, "_upp")]]),
    aggregation_method = method,
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

stat_min_unweighted <- function(design, ques, disag, level, ms_options = NULL) {
  vals <- suppressWarnings(as.numeric(.svy_data(design)[[ques]]))
  valid_n <- sum(!is.na(vals))
  if (valid_n == 0) return(.empty_row(ques, "min_unweighted", disag, level))
  tibble::tibble(
    Var1 = NA_character_,
    Freq = min(vals, na.rm = TRUE),
    SE = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
    aggregation_method = "min_unweighted",
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

stat_max_unweighted <- function(design, ques, disag, level, ms_options = NULL) {
  vals <- suppressWarnings(as.numeric(.svy_data(design)[[ques]]))
  valid_n <- sum(!is.na(vals))
  if (valid_n == 0) return(.empty_row(ques, "max_unweighted", disag, level))
  tibble::tibble(
    Var1 = NA_character_,
    Freq = max(vals, na.rm = TRUE),
    SE = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
    aggregation_method = "max_unweighted",
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

# ---- dispatch table ----------------------------------------------------------

.INT_METHODS <- c("mean", "median", "sum", "firstq", "thirdq", "min", "max")
.KOBO_TYPES  <- c("select_one", "select_multiple", "integer")

pick_aggregator <- function(kobo_type, aggregation_method) {
  if (kobo_type == "select_one")      return(single_select_svy)
  if (kobo_type == "select_multiple") return(multi_select_svy)
  if (kobo_type == "integer") {
    return(switch(
      aggregation_method,
      mean   = stat_mean_svy,
      sum    = stat_sum_svy,
      median = function(d, q, dg, lv, ms) stat_quantile_svy(d, q, dg, lv, 0.50, "median",  ms),
      firstq = function(d, q, dg, lv, ms) stat_quantile_svy(d, q, dg, lv, 0.25, "1st_Qu",  ms),
      thirdq = function(d, q, dg, lv, ms) stat_quantile_svy(d, q, dg, lv, 0.75, "3rd_Qu",  ms),
      min    = stat_min_unweighted,
      max    = stat_max_unweighted,
      stop("unknown aggregation_method '", aggregation_method,
           "' for kobo_type 'integer'")
    ))
  }
  stop("unknown kobo_type '", kobo_type, "'")
}

# ---- validation --------------------------------------------------------------

validate_plan <- function(ap, df) {
  required <- c("variable", "kobo_type", "aggregation_method", "disaggregation")
  missing_cols <- setdiff(required, names(ap))
  if (length(missing_cols) > 0) {
    stop("analysis plan is missing columns: ",
         paste(missing_cols, collapse = ", "))
  }

  bad_kt <- setdiff(unique(ap$kobo_type), .KOBO_TYPES)
  if (length(bad_kt) > 0) {
    stop("unknown kobo_type values: ", paste(bad_kt, collapse = ", "))
  }

  # select_one and integer variables must exist as columns on the data
  needs_col <- ap$variable[ap$kobo_type %in% c("select_one", "integer")]
  bad_vars <- setdiff(unique(needs_col), names(df))
  if (length(bad_vars) > 0) {
    stop("variables in analysis plan not present in data: ",
         paste(bad_vars, collapse = ", "))
  }

  # select_multiple: either the source column or sibling binary columns
  ms_vars <- unique(ap$variable[ap$kobo_type == "select_multiple"])
  for (v in ms_vars) {
    if (v %in% names(df)) next
    siblings <- detect_ms_options(df, v)[[v]]
    if (length(siblings) == 0) {
      stop("select_multiple variable '", v,
           "' not found and no expanded sibling columns (",
           paste0(v, .MS_SEPS, "*", collapse = ", "), ") detected")
    }
  }

  bad_disag <- setdiff(
    unique(ap$disaggregation[
      !is.na(ap$disaggregation) & ap$disaggregation != "all"
    ]),
    names(df)
  )
  if (length(bad_disag) > 0) {
    stop("disaggregation columns not in data: ",
         paste(bad_disag, collapse = ", "))
  }

  int_rows <- ap[ap$kobo_type == "integer", , drop = FALSE]
  bad_int <- setdiff(unique(int_rows$aggregation_method), .INT_METHODS)
  if (length(bad_int) > 0) {
    stop("invalid aggregation_method for integer kobo_type: ",
         paste(bad_int, collapse = ", "))
  }

  if ("repeat_for" %in% names(ap)) {
    rf <- unique(ap$repeat_for[!is.na(ap$repeat_for)])
    bad_rf <- setdiff(rf, names(df))
    if (length(bad_rf) > 0) {
      stop("repeat_for columns not in data: ",
           paste(bad_rf, collapse = ", "))
    }
  }

  invisible(TRUE)
}

# ---- inner runner ------------------------------------------------------------

.run_plan <- function(design, plan, ms_options) {
  n <- nrow(plan)
  if (n == 0) return(NULL)

  results <- vector("list", n)
  cli::cli_progress_bar("Analyzing", total = n, clear = FALSE)

  for (i in seq_len(n)) {
    row <- plan[i, , drop = FALSE]
    fn    <- pick_aggregator(row$kobo_type, row$aggregation_method)
    disag <- row$disaggregation
    ques  <- row$variable

    if (is.na(disag) || disag == "all") {
      lab <- if (is.na(disag)) NA_character_ else "all"
      results[[i]] <- fn(design, ques, lab, lab, ms_options)
    } else {
      lvls <- unique(.svy_data(design)[[disag]])
      results[[i]] <- purrr::map_dfr(lvls, function(lvl) {
        d_sub <- if (is.na(lvl)) {
          design %>% srvyr::filter(is.na(.data[[disag]]))
        } else {
          design %>% srvyr::filter(.data[[disag]] == lvl)
        }
        fn(d_sub, ques, disag, lvl, ms_options)
      })
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  dplyr::bind_rows(results)
}

# ---- public entry point ------------------------------------------------------

analyze_survey <- function(design,
                           analysis_plan,
                           multi_response_sep = "; ") {
  df <- .svy_data(design)
  validate_plan(analysis_plan, df)

  ms_options <- attr(df, "ms_options")
  if (is.null(ms_options)) {
    ms_vars <- unique(analysis_plan$variable[
      analysis_plan$kobo_type == "select_multiple"
    ])
    ms_options <- detect_ms_options(df, ms_vars)
  }

  has_rf <- "repeat_for" %in% names(analysis_plan) &&
            any(!is.na(analysis_plan$repeat_for))

  if (has_rf) {
    ap_rf    <- dplyr::filter(analysis_plan, !is.na(repeat_for))
    ap_no_rf <- dplyr::filter(analysis_plan,  is.na(repeat_for))
  } else {
    ap_rf    <- NULL
    ap_no_rf <- analysis_plan
  }

  result_no_rf <- NULL
  if (!is.null(ap_no_rf) && nrow(ap_no_rf) > 0) {
    result_no_rf <- .run_plan(design, ap_no_rf, ms_options)
    if (!is.null(result_no_rf)) result_no_rf$repeat_for <- NA_character_
  }

  result_rf <- NULL
  if (!is.null(ap_rf) && nrow(ap_rf) > 0) {
    rf_groups <- split(ap_rf, ap_rf$repeat_for)
    result_rf <- purrr::imap_dfr(rf_groups, function(grp, rf_col) {
      lvls <- unique(df[[rf_col]])
      purrr::map_dfr(lvls, function(lvl) {
        d_sub <- if (is.na(lvl)) {
          design %>% srvyr::filter(is.na(.data[[rf_col]]))
        } else {
          design %>% srvyr::filter(.data[[rf_col]] == lvl)
        }
        res <- .run_plan(d_sub, grp, ms_options)
        if (!is.null(res)) res$repeat_for <- as.character(lvl)
        res
      })
    })
  }

  out <- dplyr::bind_rows(result_no_rf, result_rf)
  if (nrow(out) == 0) return(out)

  dplyr::select(
    out,
    Disaggregation       = disaggregation,
    Disaggregation_level = disagg_level,
    Question             = variable,
    Response             = Var1,
    Aggregation_method   = aggregation_method,
    Result               = Freq,
    SE                   = SE,
    CI_low               = CI_low,
    CI_high              = CI_high,
    Count                = count,
    Denominator          = valid,
    repeat_for           = repeat_for
  )
}
