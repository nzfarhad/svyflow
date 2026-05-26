#' Build a survey design
#'
#' Thin convenience wrapper around [survey::svydesign()] that accepts column
#' names as strings and wraps the result as a [srvyr::tbl_svy] so it can be
#' used downstream with the tidy verbs.
#'
#' If `weights`, `strata`, `ids`, and `fpc` are all `NULL`, the returned design
#' is a simple random sample - useful for non-survey data or for running the
#' same analysis plan unweighted as a baseline.
#'
#' @param df Data frame containing the survey responses.
#' @param weights Name of the column with sampling weights, or `NULL`.
#' @param strata Name of the column identifying strata, or `NULL`.
#' @param ids Name of the column identifying primary sampling units, or
#'   `NULL` (defaults to `~1`, i.e. independent observations).
#' @param fpc Name of the finite-population correction column, or `NULL`.
#' @param nest If `TRUE`, relabel cluster ids to be nested within strata.
#'   Passed through to [survey::svydesign()].
#'
#' @return A [srvyr::tbl_svy] object.
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(10), s = rep(c("a","b"), each = 5), w = runif(10, 0.5, 1.5)
#' )
#' make_design(df)                                    # SRS
#' make_design(df, weights = "w")                     # weighted
#' make_design(df, weights = "w", strata = "s")       # stratified
#'
#' @seealso [analyze_survey()], [expand_multiselect()]
#' @export
make_design <- function(df,
                        weights = NULL,
                        strata  = NULL,
                        ids     = NULL,
                        fpc     = NULL,
                        nest    = FALSE) {
  to_formula <- function(x) if (is.null(x)) NULL else stats::as.formula(paste0("~", x))

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
