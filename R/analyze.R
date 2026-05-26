#' Run an analysis plan against a survey design
#'
#' The public entry point. Validates the plan, resolves multi-select option
#' columns (using the `"ms_options"` attribute on the design's data if
#' present, otherwise detecting siblings), and walks every plan row
#' producing one or more result rows. Single disaggregation iterates over
#' `unique(df[[disag]])`. Double disaggregation (the optional `repeat_for`
#' column on the plan) iterates over `unique(df[[repeat_for]])` on the
#' outside before the per-row disaggregation.
#'
#' @param design A [srvyr::tbl_svy] survey design (typically from
#'   [make_design()]).
#' @param analysis_plan Data frame describing the analyses to run. Columns:
#'   - `variable`: column to aggregate.
#'   - `kobo_type`: one of `"select_one"`, `"select_multiple"`, `"integer"`.
#'   - `aggregation_method`: only consulted for `kobo_type == "integer"`;
#'      one of `"mean"`, `"median"`, `"sum"`, `"firstq"`, `"thirdq"`,
#'      `"min"`, `"max"`.
#'   - `disaggregation`: a column name, `"all"`, or `NA`.
#'   - `repeat_for` (optional): a column name to add a second
#'      disaggregation axis, or `NA`.
#' @param multi_response_sep Separator used by [expand_multiselect()] if it
#'   needs to be invoked on the design's data. Defaults to `"; "`.
#'
#' @return A [`svyflow_results`] tibble with columns:
#'   `Disaggregation`, `Disaggregation_level`, `Question`, `Response`,
#'   `Aggregation_method`, `Result`, `SE`, `CI_low`, `CI_high`, `Count`,
#'   `Denominator`, `repeat_for`.
#'
#' @examples
#' df <- data.frame(
#'   gender = sample(c("m","f"), 100, TRUE),
#'   age    = round(rnorm(100, 35, 8))
#' )
#' ap <- data.frame(
#'   variable = c("gender", "age"),
#'   kobo_type = c("select_one", "integer"),
#'   aggregation_method = c(NA, "mean"),
#'   disaggregation = c("all", "gender")
#' )
#' analyze_survey(make_design(df), ap)
#'
#' @seealso [make_design()], [expand_multiselect()], [validate_plan()]
#' @export
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
    ap_rf    <- analysis_plan[!is.na(analysis_plan$repeat_for), , drop = FALSE]
    ap_no_rf <- analysis_plan[ is.na(analysis_plan$repeat_for), , drop = FALSE]
  } else {
    ap_rf    <- NULL
    ap_no_rf <- analysis_plan
  }

  result_no_rf <- NULL
  if (!is.null(ap_no_rf) && nrow(ap_no_rf) > 0) {
    result_no_rf <- run_plan_internal(design, ap_no_rf, ms_options)
    if (!is.null(result_no_rf)) result_no_rf$repeat_for <- NA_character_
  }

  result_rf <- NULL
  if (!is.null(ap_rf) && nrow(ap_rf) > 0) {
    rf_groups <- split(ap_rf, ap_rf$repeat_for)
    result_rf <- purrr::imap_dfr(rf_groups, function(grp, rf_col) {
      lvls <- unique(df[[rf_col]])
      purrr::map_dfr(lvls, function(lvl) {
        d_sub <- if (is.na(lvl)) {
          srvyr::filter(design, is.na(.data[[rf_col]]))
        } else {
          srvyr::filter(design, .data[[rf_col]] == lvl)
        }
        res <- run_plan_internal(d_sub, grp, ms_options)
        if (!is.null(res)) res$repeat_for <- as.character(lvl)
        res
      })
    })
  }

  out <- dplyr::bind_rows(result_no_rf, result_rf)
  if (nrow(out) == 0) {
    return(new_svyflow_results(out))
  }

  # Reorder and rename to the public schema. No NSE here so R CMD check is
  # happy; .OUT_RENAME maps internal -> public column names.
  out <- out[, names(.OUT_RENAME), drop = FALSE]
  names(out) <- unname(.OUT_RENAME)

  new_svyflow_results(out)
}

# Walk an analysis plan against a single design. Internal helper; the public
# entry point analyze_survey() handles repeat_for above this layer.
run_plan_internal <- function(design, plan, ms_options) {
  n <- nrow(plan)
  if (n == 0) return(NULL)

  results <- vector("list", n)
  cli::cli_progress_bar("Analyzing", total = n, clear = TRUE)

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
          srvyr::filter(design, is.na(.data[[disag]]))
        } else {
          srvyr::filter(design, .data[[disag]] == lvl)
        }
        fn(d_sub, ques, disag, lvl, ms_options)
      })
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  dplyr::bind_rows(results)
}
