#' The `svyflow_results` class
#'
#' [analyze_survey()] returns a tibble of class `svyflow_results`. The class
#' is a hook for future output formatters (Excel, HTML, publication-style
#' wide tables) which will dispatch on it via S3. There is no functional
#' difference from a regular tibble today besides the [`print()`] header.
#'
#' @name svyflow_results
#' @keywords internal
NULL

# Constructor. Internal; use the return value of analyze_survey().
new_svyflow_results <- function(x) {
  if (!inherits(x, "tbl_df")) x <- tibble::as_tibble(x)
  class(x) <- c("svyflow_results", class(x))
  x
}

#' @export
print.svyflow_results <- function(x, ...) {
  n_q   <- length(unique(x$Question))
  n_dis <- length(unique(paste(x$Disaggregation, x$Disaggregation_level)))
  has_rf <- "repeat_for" %in% names(x) && any(!is.na(x$repeat_for))
  cli::cli_text(
    "{.cls svyflow_results}: {.val {nrow(x)}} rows, ",
    "{.val {n_q}} question{?s}, ",
    "{.val {n_dis}} disaggregation level{?s}",
    if (has_rf) ", repeat_for present" else ""
  )
  NextMethod()
}
