# ----------------------------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------------------------

# Build a one-row analysis_plan for a wrapper call.
.one_row_plan <- function(variable, kobo_type, method = NA,
                          disaggregation = NULL,
                          variable_label = NULL,
                          disaggregation_label = NULL) {
  disag <- if (is.null(disaggregation)) "all" else disaggregation
  tibble::tibble(
    variable             = variable,
    kobo_type            = kobo_type,
    aggregation_method   = if (is.na(method)) NA_character_ else method,
    disaggregation       = disag,
    variable_label       = if (is.null(variable_label)) NA_character_
                           else as.character(variable_label),
    disaggregation_label = if (is.null(disaggregation_label)) NA_character_
                           else as.character(disaggregation_label)
  )
}

# Map (aggregation_method, q) -> human-readable label.
.method_label <- function(method, q = NULL) {
  switch(
    method,
    mean   = "Mean",
    sum    = "Sum",
    median = "Median",
    firstq = "Q25",
    thirdq = "Q75",
    min    = "Min",
    max    = "Max",
    quantile = sprintf("Q%02d", round(as.numeric(q) * 100)),
    method
  )
}

# Header for the Result column in categorical wrappers.
.result_column_name <- function(result_format) {
  if (identical(result_format, "proportion")) "Proportion" else "Percentage"
}

# Rename / drop columns for the categorical long-output schema.
.rename_categorical <- function(long, variable, variable_label,
                                disaggregation, disaggregation_label,
                                result_format) {
  has_disag <- !is.null(disaggregation) && !identical(disaggregation, "all")
  var_col   <- if (is.null(variable_label)) variable else variable_label
  result_col <- .result_column_name(result_format)

  out <- long
  # Drop columns that the wrapper considers redundant for single-indicator use.
  out$Question           <- NULL
  out$Aggregation_method <- NULL
  out$repeat_for         <- NULL
  out$Disaggregation     <- NULL
  if (!has_disag) out$Disaggregation_level <- NULL

  # Rename Response -> <variable_label>.
  names(out)[names(out) == "Response"] <- var_col
  # Rename Result -> Proportion / Percentage.
  names(out)[names(out) == "Result"] <- result_col

  if (has_disag) {
    disag_col <- if (is.null(disaggregation_label)) disaggregation
                 else disaggregation_label
    names(out)[names(out) == "Disaggregation_level"] <- disag_col
    # Reorder: indicator col, disagg col, then stat cols.
    stat_cols <- setdiff(names(out), c(var_col, disag_col))
    out <- out[, c(var_col, disag_col, stat_cols), drop = FALSE]
  } else {
    stat_cols <- setdiff(names(out), var_col)
    out <- out[, c(var_col, stat_cols), drop = FALSE]
  }
  out
}

# Format a single "estimate (low-high)" cell. Handles both numeric input
# (proportion/percent) and character input (percent_fmt, which already
# carries "%"). digits controls decimal places for numeric input.
.format_ci_cell <- function(result, ci_low, ci_high, digits) {
  fmt_num <- function(x) {
    d <- if (is.null(digits)) 1 else digits
    formatC(x, format = "f", digits = d)
  }
  to_chr <- function(x) {
    if (is.character(x)) x else fmt_num(as.numeric(x))
  }
  r  <- to_chr(result)
  lo <- to_chr(ci_low)
  hi <- to_chr(ci_high)
  ifelse(is.na(result) | is.na(ci_low) | is.na(ci_high),
         NA_character_,
         paste0(r, " (", lo, "\u2013", hi, ")"))
}

# Pivot a long-form categorical table to wide. `long` has columns
# var_col, disag_col, Result, SE, CI_low, CI_high, Count, Denominator
# (Result column may already be renamed to Proportion/Percentage; we accept
# either).
.crosstab_pivot <- function(long, var_col, disag_col, with_ci, digits) {
  # Pull the value column name (Proportion or Percentage).
  value_col <- intersect(c("Proportion", "Percentage", "Result"), names(long))[1]
  if (is.na(value_col)) stop("crosstab_pivot: no value column found in input")

  if (with_ci) {
    long$.cell <- .format_ci_cell(long[[value_col]],
                                  long$CI_low, long$CI_high, digits)
    keep <- c(var_col, disag_col, ".cell")
    wide <- tidyr::pivot_wider(long[, keep, drop = FALSE],
                               names_from  = !!rlang::sym(disag_col),
                               values_from = ".cell")
  } else {
    keep <- c(var_col, disag_col, value_col)
    wide <- tidyr::pivot_wider(long[, keep, drop = FALSE],
                               names_from  = !!rlang::sym(disag_col),
                               values_from = !!rlang::sym(value_col))
  }
  wide
}

# Rename / drop columns for the numeric long-output schema.
.rename_numeric <- function(long, variable, variable_label,
                            disaggregation, disaggregation_label,
                            method_label, result_only) {
  has_disag <- !is.null(disaggregation) && !identical(disaggregation, "all")
  var_label <- if (is.null(variable_label)) variable else variable_label

  out <- long
  # Indicator column: replace Question values with the label.
  out$Question <- as.character(var_label)
  names(out)[names(out) == "Question"] <- "Indicator"

  # Rename Result -> method (Mean / Sum / etc.).
  names(out)[names(out) == "Result"] <- method_label

  # Drop columns not used in numeric output.
  out$Response           <- NULL
  out$Aggregation_method <- NULL
  out$repeat_for         <- NULL
  out$Denominator        <- NULL  # equals Count for raw quantities
  out$Disaggregation     <- NULL

  if (has_disag) {
    disag_col <- if (is.null(disaggregation_label)) disaggregation
                 else disaggregation_label
    names(out)[names(out) == "Disaggregation_level"] <- disag_col
  } else {
    out$Disaggregation_level <- NULL
  }

  if (result_only) {
    keep <- c("Indicator",
              if (has_disag)
                if (is.null(disaggregation_label)) disaggregation
                else disaggregation_label,
              method_label)
    out <- out[, keep, drop = FALSE]
  } else {
    # Reorder: Indicator, disagg (if any), method, SE, CI_low, CI_high, Count.
    front <- c("Indicator",
               if (has_disag)
                 if (is.null(disaggregation_label)) disaggregation
                 else disaggregation_label,
               method_label)
    rest <- setdiff(names(out), front)
    out <- out[, c(front, rest), drop = FALSE]
  }
  out
}

# Final class wrap; mirrors new_svyflow_results() but with a different class.
# The input frequently carries an inherited "svyflow_results" class (it's the
# return type of analyze_survey()), but the rename helpers have dropped the
# columns that print.svyflow_results expects. Strip that class so NextMethod
# dispatches straight to print.tbl_df.
new_svyflow_summary <- function(x) {
  if (!inherits(x, "tbl_df")) x <- tibble::as_tibble(x)
  class(x) <- setdiff(class(x), "svyflow_results")
  class(x) <- c("svyflow_summary", class(x))
  x
}

# Shared dispatcher used by all categorical wrappers.
.summarize_categorical <- function(design, variable, kobo_type,
                                   disaggregation, variable_label,
                                   disaggregation_label,
                                   result_format, digits,
                                   crosstab, with_ci, ci) {
  plan <- .one_row_plan(variable, kobo_type,
                        disaggregation = disaggregation,
                        variable_label = variable_label,
                        disaggregation_label = disaggregation_label)
  long <- analyze_survey(design, plan,
                         result_format = result_format,
                         digits        = digits,
                         use_labels    = TRUE,
                         ci            = ci)

  renamed <- .rename_categorical(long, variable, variable_label,
                                 disaggregation, disaggregation_label,
                                 result_format)

  if (isTRUE(crosstab) && !is.null(disaggregation) &&
      !identical(disaggregation, "all")) {
    var_col   <- if (is.null(variable_label)) variable else variable_label
    disag_col <- if (is.null(disaggregation_label)) disaggregation
                 else disaggregation_label
    wide <- .crosstab_pivot(renamed, var_col, disag_col, with_ci, digits)
    return(new_svyflow_summary(wide))
  }

  new_svyflow_summary(renamed)
}

# Shared dispatcher used by all numeric wrappers.
.summarize_numeric <- function(design, variable, method, q,
                               disaggregation, variable_label,
                               disaggregation_label,
                               digits, result_only, ci) {
  plan <- .one_row_plan(variable, "integer", method = method,
                        disaggregation = disaggregation,
                        variable_label = variable_label,
                        disaggregation_label = disaggregation_label)
  long <- analyze_survey(design, plan,
                         result_format = "proportion",
                         digits        = digits,
                         use_labels    = TRUE,
                         ci            = ci)
  method_label <- .method_label(if (is.null(q)) method else "quantile", q = q)
  renamed <- .rename_numeric(long, variable, variable_label,
                             disaggregation, disaggregation_label,
                             method_label, result_only)
  new_svyflow_summary(renamed)
}


# ----------------------------------------------------------------------------
# Public categorical wrappers
# ----------------------------------------------------------------------------

#' Summarise a single-choice (select_one) categorical indicator
#'
#' Builds a one-row analysis plan and delegates to [analyze_survey()], then
#' reshapes the long output into a publication-ready table: one row per
#' response value, with `variable_label` as the row-header column name and
#' `Result` renamed to `Proportion` or `Percentage` depending on
#' `result_format`. Optionally pivots a disaggregated result into a
#' crosstab.
#'
#' @param design A [srvyr::tbl_svy] survey design (typically from
#'   [make_design()]).
#' @param variable Character. The column to summarise.
#' @param disaggregation Character or `NULL`. A grouping column name; pass
#'   `NULL` (or `"all"`) for no disaggregation.
#' @param variable_label Display label for `variable`. Used as the column
#'   header for response values. Falls back to `variable` when `NULL`.
#' @param disaggregation_label Display label for `disaggregation`. Used as
#'   the column header for the disaggregation column in long output. Falls
#'   back to `disaggregation` when `NULL`.
#' @param result_format,digits Passed through to [analyze_survey()].
#'   `result_format` is `"proportion"` (default; 0-1 numeric, value column
#'   named `Proportion`), `"percent"` (0-100 numeric, `Percentage`) or
#'   `"percent_fmt"` (character `"53.3%"`, `Percentage`). `digits` is the
#'   rounding applied to the percent modes; default `1`.
#' @param crosstab If `TRUE` (and `disaggregation` is set), pivot the long
#'   output to a wide table: rows = response values, columns =
#'   disaggregation levels. `SE` / `CI_*` / `Count` / `Denominator` are
#'   dropped from the wide view.
#' @param with_ci If `TRUE` (only meaningful when `crosstab = TRUE`),
#'   format wide-table cells as `"estimate (CI_low-CI_high)"`.
#' @param ci A [ci_opts()] bundle controlling the confidence-interval
#'   method. For proportions the relevant knob is `prop_method` (e.g.
#'   `ci_opts(prop_method = "logit")` for bounded intervals on rare
#'   outcomes), plus the universal `ci_level` / `df`. Defaults to
#'   `ci_opts()` (95% t-interval, plain Wald proportions).
#'
#' @return A tibble of class `svyflow_summary`. Schema depends on
#'   `disaggregation` and `crosstab`:
#'
#'   - No disaggregation:
#'     `<variable_label>` | `Proportion`/`Percentage` | `SE` | `CI_low` |
#'     `CI_high` | `Count` | `Denominator`
#'   - Disaggregation, long (`crosstab = FALSE`):
#'     `<variable_label>` | `<disaggregation_label>` | `Proportion` | ...
#'   - Disaggregation, `crosstab = TRUE`:
#'     `<variable_label>` | <one column per disaggregation level>
#'
#' @examples
#' df  <- data.frame(
#'   edu_lvl = sample(c("none","primary","secondary"), 200, TRUE),
#'   gender  = sample(c("m","f"), 200, TRUE),
#'   weight  = runif(200, 0.5, 2.0)
#' )
#' des <- make_design(df, weights = "weight")
#'
#' summarize_select_one(des, "edu_lvl", variable_label = "Education")
#' summarize_select_one(des, "edu_lvl",
#'                      disaggregation = "gender",
#'                      variable_label = "Education",
#'                      disaggregation_label = "Sex",
#'                      crosstab = TRUE, with_ci = TRUE)
#'
#' @seealso [summarize_select_multiple()], [analyze_survey()],
#'   [format_results()]
#' @family summarize
#' @export
summarize_select_one <- function(design, variable,
                                 disaggregation = NULL,
                                 variable_label = NULL,
                                 disaggregation_label = NULL,
                                 result_format = "proportion",
                                 digits = 1,
                                 crosstab = FALSE,
                                 with_ci  = FALSE,
                                 ci = ci_opts()) {
  .summarize_categorical(design, variable, "select_one",
                         disaggregation, variable_label,
                         disaggregation_label,
                         result_format, digits, crosstab, with_ci, ci)
}

#' Summarise a multi-select (select_multiple) categorical indicator
#'
#' Same contract as [summarize_select_one()] but for multi-response
#' variables (one binary column per option, Kobo / SurveyCTO style).
#' Returns one row per option. Detection of option columns happens
#' through [detect_ms_options()] (siblings of the form `var/opt` or
#' `var___opt`); if none are present, call [expand_multiselect()] on the
#' data frame first.
#'
#' @inheritParams summarize_select_one
#'
#' @return A `svyflow_summary` tibble. See [summarize_select_one()] for
#'   the schema (one row per option instead of per response).
#'
#' @examples
#' df <- data.frame(
#'   hh_needs = c("cash; food", "shelter", "cash; shelter"),
#'   gender   = c("m","f","f")
#' )
#' df <- expand_multiselect(df, vars = "hh_needs", sep = "; ")
#' summarize_select_multiple(make_design(df), "hh_needs",
#'                           variable_label = "Household needs")
#'
#' @seealso [summarize_select_one()], [expand_multiselect()],
#'   [detect_ms_options()]
#' @family summarize
#' @export
summarize_select_multiple <- function(design, variable,
                                      disaggregation = NULL,
                                      variable_label = NULL,
                                      disaggregation_label = NULL,
                                      result_format = "proportion",
                                      digits = 1,
                                      crosstab = FALSE,
                                      with_ci  = FALSE,
                                      ci = ci_opts()) {
  .summarize_categorical(design, variable, "select_multiple",
                         disaggregation, variable_label,
                         disaggregation_label,
                         result_format, digits, crosstab, with_ci, ci)
}


# ----------------------------------------------------------------------------
# Public numeric wrappers
# ----------------------------------------------------------------------------

#' Survey-design-aware mean of a numeric indicator
#'
#' Computes the (design-correct) mean of `variable`, optionally
#' disaggregated. Returns a publication-ready tibble with the mean in a
#' column literally named `Mean`, plus `SE`, `CI_low`, `CI_high`, `Count`.
#' Use `result_only = TRUE` to drop the auxiliary columns for compact
#' output.
#'
#' @param design A [srvyr::tbl_svy] survey design.
#' @param variable Character. The numeric column to summarise.
#' @param disaggregation Character or `NULL`. A grouping column name;
#'   `NULL` (or `"all"`) means no disaggregation.
#' @param variable_label Display label for `variable`. Becomes the value
#'   of the `Indicator` column. Falls back to `variable` when `NULL`.
#' @param disaggregation_label Display label for `disaggregation`. Used as
#'   the column header for the disaggregation column. Falls back to
#'   `disaggregation` when `NULL`.
#' @param digits Non-negative numeric scalar, or `NULL` for no rounding.
#'   Passed through to [analyze_survey()]; numeric statistics are not
#'   rescaled, so rounding only takes effect in the `percent_fmt` form
#'   (not generally useful here). Default `NULL`.
#' @param result_only If `TRUE`, return only `Indicator`, the disaggregation
#'   column (if any), and the `Mean` column. `SE` / `CI_*` / `Count` are
#'   dropped.
#' @param ci A [ci_opts()] bundle controlling the confidence-interval
#'   method. For numeric statistics the relevant knobs are the universal
#'   `ci_level` / `df` (e.g. `ci_opts(ci_level = 0.90, df = Inf)` for a
#'   90% normal-approximation interval); for quantiles also
#'   `interval_type` / `qrule`. Defaults to `ci_opts()`.
#'
#' @return A tibble of class `svyflow_summary` with columns
#'   `Indicator`, optionally the disaggregation column, then `Mean`,
#'   `SE`, `CI_low`, `CI_high`, `Count` (unless `result_only = TRUE`).
#'
#' @examples
#' df  <- data.frame(
#'   hh_size = stats::rpois(200, 5) + 1,
#'   gender  = sample(c("m","f"), 200, TRUE),
#'   weight  = runif(200, 0.5, 2.0)
#' )
#' des <- make_design(df, weights = "weight")
#'
#' summarize_mean(des, "hh_size", variable_label = "Household size")
#' summarize_mean(des, "hh_size",
#'                disaggregation = "gender",
#'                variable_label = "Household size",
#'                disaggregation_label = "Sex")
#'
#' @seealso [summarize_sum()], [summarize_median()], [summarize_quantile()]
#' @family summarize
#' @export
summarize_mean <- function(design, variable,
                           disaggregation = NULL,
                           variable_label = NULL,
                           disaggregation_label = NULL,
                           digits = NULL,
                           result_only = FALSE,
                           ci = ci_opts()) {
  .summarize_numeric(design, variable, "mean", q = NULL,
                     disaggregation, variable_label,
                     disaggregation_label, digits, result_only, ci)
}

#' Survey-design-aware total (sum) of a numeric indicator
#'
#' Like [summarize_mean()] but reports the design-correct total. Value
#' column is named `Sum`.
#'
#' @inheritParams summarize_mean
#'
#' @return A `svyflow_summary` tibble with `Indicator`, optionally the
#'   disaggregation column, then `Sum`, `SE`, `CI_low`, `CI_high`, `Count`.
#'
#' @examples
#' df  <- data.frame(income = round(stats::rgamma(100, 2, scale = 200)),
#'                   weight = runif(100, 0.5, 2))
#' des <- make_design(df, weights = "weight")
#' summarize_sum(des, "income", variable_label = "Total income")
#'
#' @family summarize
#' @export
summarize_sum <- function(design, variable,
                          disaggregation = NULL,
                          variable_label = NULL,
                          disaggregation_label = NULL,
                          digits = NULL,
                          result_only = FALSE,
                          ci = ci_opts()) {
  .summarize_numeric(design, variable, "sum", q = NULL,
                     disaggregation, variable_label,
                     disaggregation_label, digits, result_only, ci)
}

#' Survey-design-aware median of a numeric indicator
#'
#' Shortcut for `summarize_quantile(..., q = 0.5)`. Value column is named
#' `Median`.
#'
#' @inheritParams summarize_mean
#'
#' @return A `svyflow_summary` tibble with `Indicator`, optionally the
#'   disaggregation column, then `Median`, `SE`, `CI_low`, `CI_high`,
#'   `Count`.
#'
#' @examples
#' df  <- data.frame(hh_size = stats::rpois(200, 5) + 1)
#' des <- make_design(df)
#' summarize_median(des, "hh_size", variable_label = "Household size")
#'
#' @seealso [summarize_quantile()]
#' @family summarize
#' @export
summarize_median <- function(design, variable,
                             disaggregation = NULL,
                             variable_label = NULL,
                             disaggregation_label = NULL,
                             digits = NULL,
                             result_only = FALSE,
                             ci = ci_opts()) {
  .summarize_numeric(design, variable, "median", q = NULL,
                     disaggregation, variable_label,
                     disaggregation_label, digits, result_only, ci)
}

#' Survey-design-aware quantile of a numeric indicator
#'
#' Computes the design-correct quantile at probability `q`. The value
#' column is named after the quantile (e.g. `Q25` for `q = 0.25`, `Q90`
#' for `q = 0.9`). For the three quartile shortcuts (0.25 / 0.5 / 0.75)
#' the call is dispatched through [analyze_survey()]; for arbitrary `q`
#' the underlying aggregator is called directly with the requested
#' probability.
#'
#' @inheritParams summarize_mean
#' @param q Numeric, single value in `[0, 1]`. The quantile probability.
#'
#' @return A `svyflow_summary` tibble with `Indicator`, optionally the
#'   disaggregation column, then `Q<NN>`, `SE`, `CI_low`, `CI_high`,
#'   `Count`.
#'
#' @examples
#' df  <- data.frame(income = round(stats::rgamma(200, 2, scale = 200)),
#'                   weight = runif(200, 0.5, 2))
#' des <- make_design(df, weights = "weight")
#' summarize_quantile(des, "income", q = 0.25, variable_label = "Income")
#' summarize_quantile(des, "income", q = 0.90, variable_label = "Income")
#'
#' @seealso [summarize_median()]
#' @family summarize
#' @export
summarize_quantile <- function(design, variable, q,
                               disaggregation = NULL,
                               variable_label = NULL,
                               disaggregation_label = NULL,
                               digits = NULL,
                               result_only = FALSE,
                               ci = ci_opts()) {
  if (!is.numeric(q) || length(q) != 1 || is.na(q) || q < 0 || q > 1) {
    stop("`q` must be a single numeric in [0, 1].")
  }
  # Dispatch to a canonical method when q matches one of the built-ins;
  # otherwise use the firstq/thirdq path is not appropriate. We always go
  # through the median branch by writing a one-off plan row with
  # method = "median" then patch the label -- but the dispatcher does not
  # support arbitrary q. Use firstq for q=0.25 / thirdq for q=0.75 /
  # median for q=0.5; otherwise temporarily handle via the median plan row
  # AFTER calling the underlying aggregator directly.
  if (isTRUE(all.equal(q, 0.25))) {
    method <- "firstq"
  } else if (isTRUE(all.equal(q, 0.5))) {
    method <- "median"
  } else if (isTRUE(all.equal(q, 0.75))) {
    method <- "thirdq"
  } else {
    return(.summarize_quantile_arbitrary(design, variable, q,
                                         disaggregation, variable_label,
                                         disaggregation_label,
                                         digits, result_only, ci))
  }
  res <- .summarize_numeric(design, variable, method, q = q,
                            disaggregation, variable_label,
                            disaggregation_label, digits, result_only, ci)
  res
}

# Arbitrary quantile path: bypass the analyze_survey/plan dispatcher (which
# only knows the named quartile methods) and call stat_quantile_svy directly
# with the requested q. Reuses the rename/reshape helpers.
.summarize_quantile_arbitrary <- function(design, variable, q,
                                          disaggregation, variable_label,
                                          disaggregation_label,
                                          digits, result_only, ci) {
  method_label <- .method_label("quantile", q = q)
  has_disag <- !is.null(disaggregation) && !identical(disaggregation, "all")

  build_row <- function(d_sub, disag, lvl) {
    stat_quantile_svy(d_sub, variable, disag, lvl, q, method_label,
                      ms_options = NULL,
                      result_format = "proportion", digits = digits,
                      ci = ci)
  }

  rows <- if (has_disag) {
    df <- .svy_data(design)
    lvls <- unique(df[[disaggregation]])
    purrr::map_dfr(lvls, function(lvl) {
      d_sub <- if (is.na(lvl)) {
        srvyr::filter(design, is.na(.data[[disaggregation]]))
      } else {
        srvyr::filter(design, .data[[disaggregation]] == lvl)
      }
      build_row(d_sub, disaggregation, lvl)
    })
  } else {
    build_row(design, NA_character_, NA_character_)
  }

  # Reshape into the analyze_survey long schema, then reuse the numeric
  # rename helper.
  long <- tibble::tibble(
    Disaggregation       = rows$disaggregation,
    Disaggregation_level = rows$disagg_level,
    Question             = if (is.null(variable_label)) variable
                           else as.character(variable_label),
    Response             = rows$Var1,
    Aggregation_method   = rows$aggregation_method,
    Result               = rows$Freq,
    SE                   = rows$SE,
    CI_low               = rows$CI_low,
    CI_high              = rows$CI_high,
    Count                = rows$count,
    Denominator          = rows$valid,
    repeat_for           = NA_character_
  )
  renamed <- .rename_numeric(long, variable, variable_label,
                             disaggregation, disaggregation_label,
                             method_label, result_only)
  new_svyflow_summary(renamed)
}

#' Unweighted minimum of a numeric indicator
#'
#' Reports the raw minimum, unweighted by design. Extrema are not survey-
#' weighted statistics, so weights are intentionally ignored even on a
#' weighted design. `SE` / `CI_*` are `NA`. Value column is named `Min`.
#'
#' @inheritParams summarize_mean
#'
#' @return A `svyflow_summary` tibble with `Indicator`, optionally the
#'   disaggregation column, then `Min` (and `NA` `SE` / `CI_*` / `Count`
#'   unless `result_only = TRUE`).
#'
#' @examples
#' df  <- data.frame(income = c(150, 320, 0, 980))
#' des <- make_design(df)
#' summarize_min(des, "income", variable_label = "Income")
#'
#' @seealso [summarize_max()]
#' @family summarize
#' @export
summarize_min <- function(design, variable,
                          disaggregation = NULL,
                          variable_label = NULL,
                          disaggregation_label = NULL,
                          digits = NULL,
                          result_only = FALSE,
                          ci = ci_opts()) {
  .summarize_numeric(design, variable, "min", q = NULL,
                     disaggregation, variable_label,
                     disaggregation_label, digits, result_only, ci)
}

#' Unweighted maximum of a numeric indicator
#'
#' Reports the raw maximum, unweighted by design. See [summarize_min()]
#' for the rationale. Value column is named `Max`.
#'
#' @inheritParams summarize_mean
#'
#' @return A `svyflow_summary` tibble with `Indicator`, optionally the
#'   disaggregation column, then `Max` (and `NA` `SE` / `CI_*` / `Count`
#'   unless `result_only = TRUE`).
#'
#' @examples
#' df  <- data.frame(income = c(150, 320, 0, 980))
#' des <- make_design(df)
#' summarize_max(des, "income", variable_label = "Income")
#'
#' @seealso [summarize_min()]
#' @family summarize
#' @export
summarize_max <- function(design, variable,
                          disaggregation = NULL,
                          variable_label = NULL,
                          disaggregation_label = NULL,
                          digits = NULL,
                          result_only = FALSE,
                          ci = ci_opts()) {
  .summarize_numeric(design, variable, "max", q = NULL,
                     disaggregation, variable_label,
                     disaggregation_label, digits, result_only, ci)
}


#' @export
print.svyflow_summary <- function(x, ...) {
  cli::cli_text("{.cls svyflow_summary}: {.val {nrow(x)}} row{?s}, ",
                "{.val {ncol(x)}} column{?s}")
  NextMethod()
}
