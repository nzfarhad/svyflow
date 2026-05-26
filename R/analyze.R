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
#'   - `variable_label` (optional): display label substituted into the
#'      `Question` column when `use_labels = TRUE`. `NA` falls back to
#'      `variable`.
#'   - `disaggregation_label` (optional): display label substituted into
#'      the `Disaggregation` column when `use_labels = TRUE`. `NA` falls
#'      back to `disaggregation` (including the literal `"all"`).
#'   - `repeat_for` (optional): a column name to add a second
#'      disaggregation axis, or `NA`.
#' @param multi_response_sep Separator used by [expand_multiselect()] if it
#'   needs to be invoked on the design's data. Defaults to `"; "`.
#' @param result_format How proportion-producing rows (`select_one`,
#'   `select_multiple`) report `Result`/`SE`/`CI_*`. One of `"proportion"`
#'   (default; 0-1 numeric), `"percent"` (0-100 numeric), or
#'   `"percent_fmt"` (character `"53.3%"`; coerces the `Result` column
#'   to character for non-proportion rows too, so the column type stays
#'   stable). Non-proportion rows (mean, sum, median, etc.) report raw
#'   values regardless of this setting.
#' @param digits Non-negative numeric scalar, or `NULL` for no rounding.
#'   Applied to numeric outputs in the two percent modes; ignored in
#'   `"proportion"` mode so the default does not crush precision. Default
#'   `1`.
#' @param use_labels If `TRUE` (the default), substitute `variable_label`
#'   into the output's `Question` column and `disaggregation_label` into
#'   `Disaggregation` whenever those columns exist on the plan and the
#'   per-row value is not `NA`. If `FALSE`, the raw column names are used.
#'   Plans without the label columns are unaffected (backward compatible).
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
                           multi_response_sep = "; ",
                           result_format = "proportion",
                           digits = 1,
                           use_labels = TRUE) {
  .validate_format_args(result_format, digits)
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
    result_no_rf <- run_plan_internal(design, ap_no_rf, ms_options,
                                      result_format, digits, use_labels)
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
        res <- run_plan_internal(d_sub, grp, ms_options,
                                 result_format, digits, use_labels)
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

  new_svyflow_results(out, result_format = result_format, digits = digits)
}

# Walk an analysis plan against a single design. Internal helper; the public
# entry point analyze_survey() handles repeat_for above this layer.
run_plan_internal <- function(design, plan, ms_options,
                              result_format = "proportion", digits = 1,
                              use_labels = TRUE) {
  n <- nrow(plan)
  if (n == 0) return(NULL)

  has_var_lbl   <- use_labels && "variable_label"        %in% names(plan)
  has_disag_lbl <- use_labels && "disaggregation_label"  %in% names(plan)

  results <- vector("list", n)
  cli::cli_progress_bar("Analyzing", total = n, clear = TRUE)

  for (i in seq_len(n)) {
    row <- plan[i, , drop = FALSE]
    fn    <- pick_aggregator(row$kobo_type, row$aggregation_method)
    disag <- row$disaggregation
    ques  <- row$variable

    if (is.na(disag) || disag == "all") {
      lab <- if (is.na(disag)) NA_character_ else "all"
      rows_i <- fn(design, ques, lab, lab, ms_options,
                   result_format, digits)
    } else {
      lvls <- unique(.svy_data(design)[[disag]])
      rows_i <- purrr::map_dfr(lvls, function(lvl) {
        d_sub <- if (is.na(lvl)) {
          srvyr::filter(design, is.na(.data[[disag]]))
        } else {
          srvyr::filter(design, .data[[disag]] == lvl)
        }
        fn(d_sub, ques, disag, lvl, ms_options, result_format, digits)
      })
    }

    # Substitute labels in the internal column names that map to the
    # public Question / Disaggregation columns. NA labels fall back to
    # the raw column name already written by the aggregator.
    if (!is.null(rows_i) && nrow(rows_i) > 0) {
      if (has_var_lbl) {
        lbl <- row$variable_label
        if (!is.na(lbl)) rows_i$variable <- as.character(lbl)
      }
      if (has_disag_lbl) {
        lbl <- row$disaggregation_label
        if (!is.na(lbl)) rows_i$disaggregation <- as.character(lbl)
      }
    }
    results[[i]] <- rows_i

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  dplyr::bind_rows(results)
}
