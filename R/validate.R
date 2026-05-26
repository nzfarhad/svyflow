#' Validate an analysis plan against a data frame
#'
#' Run before any aggregation to surface common mistakes - missing columns,
#' typos in `kobo_type`, `aggregation_method` values that don't apply to the
#' given `kobo_type`, disaggregation columns that don't exist. Called
#' automatically by [analyze_survey()]; exported so users can lint a plan
#' independently.
#'
#' For `select_multiple` variables, the variable name need **not** exist as a
#' string column in the data, so long as sibling binary columns are present
#' (see [detect_ms_options()]).
#'
#' @param ap Analysis plan data frame. Required columns: `variable`,
#'   `kobo_type`, `aggregation_method`, `disaggregation`. Optional column:
#'   `repeat_for`.
#' @param df Data frame the plan will be run against.
#'
#' @return `TRUE` invisibly on success. Throws an error otherwise.
#'
#' @examples
#' df <- data.frame(gender = c("m","f"), age = c(30, 40))
#' ap <- data.frame(
#'   variable = c("gender", "age"),
#'   kobo_type = c("select_one", "integer"),
#'   aggregation_method = c(NA, "mean"),
#'   disaggregation = c("all", "all")
#' )
#' validate_plan(ap, df)
#'
#' @seealso [analyze_survey()]
#' @export
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
