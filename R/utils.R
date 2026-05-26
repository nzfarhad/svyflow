# Internal constants and helpers shared across the package.

# Separator strings used by Kobo / SurveyCTO when exporting multi-select
# variables as one binary column per option. Order matters: "___" must be
# tried before "." so we don't wrongly strip a literal dot from inside an
# option label.
.MS_SEPS <- c("___", "/", ".")

# Valid kobo_type / aggregation_method values accepted by the analysis plan.
.KOBO_TYPES  <- c("select_one", "select_multiple", "integer")
.INT_METHODS <- c("mean", "median", "sum", "firstq", "thirdq", "min", "max")

# Column order on every aggregator's return tibble. Kept here so adding a new
# aggregator can be checked against a single source of truth.
.AGG_COLS <- c(
  "Var1", "Freq", "SE", "CI_low", "CI_high",
  "aggregation_method", "variable",
  "count", "valid",
  "disaggregation", "disagg_level"
)

# Final output column order produced by analyze_survey().
.OUT_COLS <- c(
  "Disaggregation", "Disaggregation_level",
  "Question", "Response", "Aggregation_method",
  "Result", "SE", "CI_low", "CI_high",
  "Count", "Denominator", "repeat_for"
)

# Internal column name -> public column name mapping used by analyze_survey().
.OUT_RENAME <- c(
  disaggregation     = "Disaggregation",
  disagg_level       = "Disaggregation_level",
  variable           = "Question",
  Var1               = "Response",
  aggregation_method = "Aggregation_method",
  Freq               = "Result",
  SE                 = "SE",
  CI_low             = "CI_low",
  CI_high            = "CI_high",
  count              = "Count",
  valid              = "Denominator",
  repeat_for         = "repeat_for"
)

# An "empty" aggregator row used when a variable is entirely NA. Same shape
# as a real aggregator return so it can be rbind-stacked safely.
.empty_row <- function(ques, method, disag, level) {
  tibble::tibble(
    Var1   = NA_character_,
    Freq   = NA_real_,
    SE     = NA_real_,
    CI_low = NA_real_,
    CI_high= NA_real_,
    aggregation_method = method,
    variable           = ques,
    count              = 0L,
    valid              = 0L,
    disaggregation     = as.character(disag),
    disagg_level       = as.character(level)
  )
}

# Pull the underlying data frame out of a srvyr / survey design.
.svy_data <- function(design) design$variables

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
