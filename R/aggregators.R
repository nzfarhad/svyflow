#' Per-type aggregators
#'
#' These functions compute one row (or a few rows for multi-response /
#' grouped proportions) of analysis output. They all share the same
#' signature `(design, ques, disag, level, ms_options, result_format,
#' digits)` and return a tibble with a uniform 11-column shape (the
#' internal `.AGG_COLS` vector).
#'
#' End users normally do not call these directly - they are invoked by
#' [analyze_survey()] via the internal `pick_aggregator()`. They are
#' documented as a group because their contract is what new aggregators
#' must match.
#'
#' @param design A [srvyr::tbl_svy] survey design.
#' @param ques Character. The variable name to aggregate.
#' @param disag Character. The disaggregation column name, or `"all"` /
#'   `NA` when none.
#' @param level The level of the disaggregation column being filtered to,
#'   or `"all"` / `NA` when none.
#' @param ms_options Named list of multi-select option columns, as built by
#'   [expand_multiselect()] / [detect_ms_options()]. Only consumed by
#'   [multi_select_svy()]; ignored by the others (kept on the signature
#'   for dispatcher uniformity).
#' @param result_format One of `"proportion"` (default; 0-1 numeric),
#'   `"percent"` (0-100 numeric), or `"percent_fmt"` (character `"53.3%"`).
#'   Only consumed by the proportion-producing aggregators
#'   ([single_select_svy()], [multi_select_svy()]); the others accept it
#'   for signature uniformity and ignore it for the `Freq` value (but they
#'   do coerce `Freq` to character in `"percent_fmt"` so the final
#'   `analyze_survey()` output keeps a stable column type).
#' @param digits Non-negative numeric scalar, or `NULL` for no rounding.
#'   Applied to `Freq` / `SE` / `CI_*` in the two percent modes; ignored
#'   in `"proportion"` mode to keep raw precision. Default `1`.
#' @param ci A [ci_opts()] bundle controlling the confidence-interval
#'   method (level, df, proportion method, quantile interval). Defaults
#'   to `ci_opts()`, which reproduces the historical behaviour. Only the
#'   knobs relevant to a given aggregator are read (`prop_method` by the
#'   proportion aggregators, `interval_type` / `qrule` by the quantile
#'   one); the rest accept it for signature uniformity.
#'
#' @return A tibble with columns `Var1, Freq, SE, CI_low, CI_high,
#'   aggregation_method, variable, count, valid, disaggregation,
#'   disagg_level`.
#'
#' @name aggregators
#' @keywords internal
NULL

#' @rdname aggregators
#' @keywords internal
single_select_svy <- function(design, ques, disag, level, ms_options = NULL,
                              result_format = "proportion", digits = 1,
                              ci = ci_opts()) {
  .validate_format_args(result_format, digits)
  ci <- .as_ci_opts(ci)
  vals <- .svy_data(design)[[ques]]
  valid_n <- sum(!is.na(vals))
  if (valid_n == 0) return(.empty_row(ques, "perc", disag, level, result_format))

  d <- srvyr::filter(design, !is.na(.data[[ques]]))
  res <- dplyr::summarise(
    srvyr::group_by(d, !!rlang::sym(ques)),
    prop = if (is.null(ci$prop_method))
             srvyr::survey_mean(vartype = c("se", "ci"),
                                level = ci$ci_level, df = ci$df)
           else
             srvyr::survey_mean(vartype = c("se", "ci"),
                                level = ci$ci_level, df = ci$df,
                                proportion = TRUE, prop_method = ci$prop_method),
    cnt  = srvyr::unweighted(dplyr::n())
  )

  tibble::tibble(
    Var1    = as.character(res[[ques]]),
    Freq    = .format_prop(res$prop,     result_format, digits),
    SE      = .format_prop(res$prop_se,  result_format, digits),
    CI_low  = .format_prop(res$prop_low, result_format, digits),
    CI_high = .format_prop(res$prop_upp, result_format, digits),
    aggregation_method = "perc",
    variable = ques,
    count = res$cnt,
    valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

#' @rdname aggregators
#' @keywords internal
multi_select_svy <- function(design, ques, disag, level, ms_options = NULL,
                             result_format = "proportion", digits = 1,
                             ci = ci_opts()) {
  .validate_format_args(result_format, digits)
  ci <- .as_ci_opts(ci)
  if (is.null(ms_options) || is.null(ms_options[[ques]])) {
    ms_options <- list()
    ms_options[[ques]] <- detect_ms_options(.svy_data(design), ques)[[ques]]
  }
  opts <- ms_options[[ques]]
  if (length(opts) == 0) {
    warning("multi_select_svy: no expanded binary columns found for '", ques,
            "'. Run expand_multiselect() first.")
    return(.empty_row(ques, "perc", disag, level, result_format))
  }

  na_val <- if (result_format == "percent_fmt") NA_character_ else NA_real_

  rows <- purrr::map_dfr(opts, function(opt) {
    vals <- .svy_data(design)[[opt]]
    valid_n <- sum(!is.na(vals))
    if (valid_n == 0) {
      return(tibble::tibble(
        Var1 = .option_label(ques, opt),
        Freq = na_val,
        SE = na_val, CI_low = na_val, CI_high = na_val,
        count = 0L, valid = 0L
      ))
    }
    d <- srvyr::filter(design, !is.na(.data[[opt]]))
    r <- dplyr::summarise(
      d,
      prop = if (is.null(ci$prop_method))
               srvyr::survey_mean(as.numeric(.data[[opt]]),
                                  vartype = c("se", "ci"), na.rm = TRUE,
                                  level = ci$ci_level, df = ci$df)
             else
               srvyr::survey_mean(as.numeric(.data[[opt]]),
                                  vartype = c("se", "ci"), na.rm = TRUE,
                                  level = ci$ci_level, df = ci$df,
                                  proportion = TRUE,
                                  prop_method = ci$prop_method),
      cnt  = srvyr::unweighted(sum(.data[[opt]] == 1, na.rm = TRUE))
    )
    tibble::tibble(
      Var1    = .option_label(ques, opt),
      Freq    = .format_prop(r$prop,     result_format, digits),
      SE      = .format_prop(r$prop_se,  result_format, digits),
      CI_low  = .format_prop(r$prop_low, result_format, digits),
      CI_high = .format_prop(r$prop_upp, result_format, digits),
      count   = r$cnt,
      valid   = valid_n
    )
  })

  rows$aggregation_method <- "perc"
  rows$variable           <- ques
  rows$disaggregation     <- as.character(disag)
  rows$disagg_level       <- as.character(level)
  rows[, .AGG_COLS, drop = FALSE]
}

# Shared body for survey_mean / survey_total. The caller supplies the actual
# srvyr summariser; this helper handles validity-checking and tibble assembly.
.summary_stat <- function(design, ques, disag, level, method, summariser,
                          result_format, digits) {
  vals <- .svy_data(design)[[ques]]
  valid_n <- sum(!is.na(suppressWarnings(as.numeric(vals))))
  if (valid_n == 0) return(.empty_row(ques, method, disag, level, result_format))

  d <- srvyr::filter(design, !is.na(.data[[ques]]))
  res <- summariser(d, ques)

  tibble::tibble(
    Var1 = NA_character_,
    Freq    = .coerce_freq_if_fmt(as.numeric(res$val),     result_format, digits),
    SE      = .coerce_freq_if_fmt(as.numeric(res$val_se),  result_format, digits),
    CI_low  = .coerce_freq_if_fmt(as.numeric(res$val_low), result_format, digits),
    CI_high = .coerce_freq_if_fmt(as.numeric(res$val_upp), result_format, digits),
    aggregation_method = method,
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

#' @rdname aggregators
#' @keywords internal
stat_mean_svy <- function(design, ques, disag, level, ms_options = NULL,
                          result_format = "proportion", digits = 1,
                          ci = ci_opts()) {
  .validate_format_args(result_format, digits)
  ci <- .as_ci_opts(ci)
  .summary_stat(design, ques, disag, level, "mean", function(d, q) {
    dplyr::summarise(d,
      val = srvyr::survey_mean(as.numeric(.data[[q]]),
                               vartype = c("se", "ci"), na.rm = TRUE,
                               level = ci$ci_level, df = ci$df)
    )
  }, result_format, digits)
}

#' @rdname aggregators
#' @keywords internal
stat_sum_svy <- function(design, ques, disag, level, ms_options = NULL,
                         result_format = "proportion", digits = 1,
                         ci = ci_opts()) {
  .validate_format_args(result_format, digits)
  ci <- .as_ci_opts(ci)
  .summary_stat(design, ques, disag, level, "sum", function(d, q) {
    dplyr::summarise(d,
      val = srvyr::survey_total(as.numeric(.data[[q]]),
                                vartype = c("se", "ci"), na.rm = TRUE,
                                level = ci$ci_level, df = ci$df)
    )
  }, result_format, digits)
}

#' @rdname aggregators
#' @param q Numeric, single quantile in `[0, 1]`.
#' @param method Aggregation label written into `aggregation_method`
#'   (e.g. `"median"`, `"1st_Qu"`).
#' @keywords internal
stat_quantile_svy <- function(design, ques, disag, level, q, method,
                              ms_options = NULL,
                              result_format = "proportion", digits = 1,
                              ci = ci_opts()) {
  .validate_format_args(result_format, digits)
  ci <- .as_ci_opts(ci)
  vals <- .svy_data(design)[[ques]]
  valid_n <- sum(!is.na(suppressWarnings(as.numeric(vals))))
  if (valid_n == 0) return(.empty_row(ques, method, disag, level, result_format))

  d <- srvyr::filter(design, !is.na(.data[[ques]]))
  res <- dplyr::summarise(
    d,
    val = srvyr::survey_quantile(as.numeric(.data[[ques]]),
                                 quantiles     = q,
                                 vartype       = c("se", "ci"),
                                 na.rm         = TRUE,
                                 level         = ci$ci_level,
                                 df            = ci$df,
                                 interval_type = ci$interval_type,
                                 qrule         = ci$qrule)
  )
  qstem <- grep("^val_q\\d+$", names(res), value = TRUE)[1]

  tibble::tibble(
    Var1 = NA_character_,
    Freq    = .coerce_freq_if_fmt(as.numeric(res[[qstem]]),
                                  result_format, digits),
    SE      = .coerce_freq_if_fmt(as.numeric(res[[paste0(qstem, "_se")]]),
                                  result_format, digits),
    CI_low  = .coerce_freq_if_fmt(as.numeric(res[[paste0(qstem, "_low")]]),
                                  result_format, digits),
    CI_high = .coerce_freq_if_fmt(as.numeric(res[[paste0(qstem, "_upp")]]),
                                  result_format, digits),
    aggregation_method = method,
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

#' @rdname aggregators
#' @keywords internal
stat_min_unweighted <- function(design, ques, disag, level, ms_options = NULL,
                                result_format = "proportion", digits = 1,
                                ci = ci_opts()) {
  .validate_format_args(result_format, digits)
  vals <- suppressWarnings(as.numeric(.svy_data(design)[[ques]]))
  valid_n <- sum(!is.na(vals))
  if (valid_n == 0) return(.empty_row(ques, "min_unweighted", disag, level,
                                      result_format))
  tibble::tibble(
    Var1 = NA_character_,
    Freq = .coerce_freq_if_fmt(min(vals, na.rm = TRUE), result_format, digits),
    SE = if (result_format == "percent_fmt") NA_character_ else NA_real_,
    CI_low = if (result_format == "percent_fmt") NA_character_ else NA_real_,
    CI_high = if (result_format == "percent_fmt") NA_character_ else NA_real_,
    aggregation_method = "min_unweighted",
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}

#' @rdname aggregators
#' @keywords internal
stat_max_unweighted <- function(design, ques, disag, level, ms_options = NULL,
                                result_format = "proportion", digits = 1,
                                ci = ci_opts()) {
  .validate_format_args(result_format, digits)
  vals <- suppressWarnings(as.numeric(.svy_data(design)[[ques]]))
  valid_n <- sum(!is.na(vals))
  if (valid_n == 0) return(.empty_row(ques, "max_unweighted", disag, level,
                                      result_format))
  tibble::tibble(
    Var1 = NA_character_,
    Freq = .coerce_freq_if_fmt(max(vals, na.rm = TRUE), result_format, digits),
    SE = if (result_format == "percent_fmt") NA_character_ else NA_real_,
    CI_low = if (result_format == "percent_fmt") NA_character_ else NA_real_,
    CI_high = if (result_format == "percent_fmt") NA_character_ else NA_real_,
    aggregation_method = "max_unweighted",
    variable = ques,
    count = valid_n, valid = valid_n,
    disaggregation = as.character(disag),
    disagg_level   = as.character(level)
  )
}
