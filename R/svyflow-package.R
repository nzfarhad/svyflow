#' svyflow: Survey-Design-Aware Descriptive Analysis
#'
#' Produces design-correct descriptive statistics for survey data driven by an
#' analysis-plan data frame. The typical workflow is:
#'
#' 1. Build a survey design once with [make_design()].
#' 2. (Optional) pre-expand Kobo / SurveyCTO multi-select columns into binary
#'    indicators with [expand_multiselect()] - skipped automatically if the
#'    export already contains the sibling binary columns.
#' 3. Run [analyze_survey()] with a data frame analysis plan.
#'
#' The result is a long-format tibble of class `svyflow_results`, carrying
#' point estimates, standard errors and 95% confidence intervals. Output
#' formatters (Excel, HTML, etc.) are planned and will dispatch on that class.
#'
#' Beyond descriptives, [compare_groups()] runs survey-design-aware
#' significance tests (t-test / ANOVA / Wilcoxon / Kruskal-Wallis for numeric
#' indicators, Rao-Scott chi-square / Fisher's exact for categorical ones),
#' choosing the right test automatically and reporting an effect size
#' alongside each p-value.
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

# Suppress R CMD check NOTES for symbols used inside dplyr / srvyr NSE.
utils::globalVariables(c(
  "repeat_for",
  # compare_groups(): symbols referenced inside survey / srvyr formulas.
  "diff", "abs_diff", "pos"
))
