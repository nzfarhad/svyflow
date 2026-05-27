#' Confidence-interval options for survey estimates
#'
#' Bundles the knobs that control how standard errors and confidence
#' intervals are produced by the underlying \pkg{srvyr} / \pkg{survey}
#' engine. Pass the result to [analyze_survey()] or to any of the
#' `summarize_*()` wrappers via their `ci` argument. The defaults reproduce
#' the package's historical behaviour exactly, so existing code is
#' unaffected.
#'
#' @param ci_level Confidence level, a single number strictly between 0 and
#'   1. Default `0.95` (95% CI). Sets the multiplier on the SE via the
#'   `(1 + ci_level) / 2` quantile.
#' @param df Degrees of freedom for the interval. `NULL` (default) uses the
#'   survey design's degrees of freedom (`survey::degf`), giving a
#'   t-distribution interval. `Inf` uses the normal approximation
#'   (multiplier `1.96` at 95%). A finite number forces a specific t df.
#' @param prop_method Confidence-interval method for **proportions**
#'   (`select_one` / `select_multiple` only). `NULL` (default) keeps the
#'   plain Wald interval on the 0/1 mean (`survey::svymean`), which can fall
#'   outside `[0, 1]` for rare outcomes. Set to one of `"logit"`,
#'   `"likelihood"`, `"asin"`, `"beta"`, `"mean"`, `"xlogit"` to use
#'   `survey::svyciprop`-style bounded intervals (`"logit"` is the usual
#'   recommendation for rare indicators). Ignored by numeric aggregators.
#' @param interval_type,qrule Passed to [srvyr::survey_quantile()] for
#'   **quantile** statistics (`median`, quartiles, [summarize_quantile()]).
#'   Defaults `"mean"` / `"math"` match srvyr's own defaults. Ignored by
#'   non-quantile aggregators.
#'
#' @return A validated list of class `svyflow_ci_opts`.
#'
#' @examples
#' # Defaults (== current behaviour: 95%, design-df t interval, Wald props)
#' ci_opts()
#'
#' # Normal-approximation 90% intervals
#' ci_opts(ci_level = 0.90, df = Inf)
#'
#' # Logit proportions (better for rare outcomes near 0% / 100%)
#' ci_opts(prop_method = "logit")
#'
#' @seealso [analyze_survey()], [summarize_select_one()]
#' @export
ci_opts <- function(ci_level = 0.95,
                    df = NULL,
                    prop_method = NULL,
                    interval_type = "mean",
                    qrule = "math") {
  if (!is.numeric(ci_level) || length(ci_level) != 1 ||
      is.na(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be a single number strictly between 0 and 1.")
  }
  if (!is.null(df)) {
    if (!is.numeric(df) || length(df) != 1 || is.na(df) || df <= 0) {
      stop("`df` must be a positive number (Inf for the normal ",
           "approximation), or NULL for the design degrees of freedom.")
    }
  }
  if (!is.null(prop_method)) {
    prop_method <- match.arg(
      prop_method,
      c("logit", "likelihood", "asin", "beta", "mean", "xlogit")
    )
  }
  interval_type <- match.arg(
    interval_type,
    c("mean", "beta", "xlogit", "asin", "score", "quantile")
  )
  qrule <- match.arg(
    qrule,
    c("math", "school", "shahvaish", paste0("hf", 1:9))
  )

  structure(
    list(
      ci_level      = ci_level,
      df            = df,
      prop_method   = prop_method,
      interval_type = interval_type,
      qrule         = qrule
    ),
    class = "svyflow_ci_opts"
  )
}

# Coerce/validate a user-supplied `ci` argument into a svyflow_ci_opts list.
# Accepts an existing svyflow_ci_opts object, a plain named list of ci_opts
# arguments, or NULL (-> defaults).
.as_ci_opts <- function(ci) {
  if (is.null(ci)) return(ci_opts())
  if (inherits(ci, "svyflow_ci_opts")) return(ci)
  if (is.list(ci)) return(do.call(ci_opts, ci))
  stop("`ci` must be created with ci_opts() (or a list of its arguments).")
}
