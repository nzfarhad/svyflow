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

# Accepted values of `result_format` across the public API. "proportion"
# returns 0-1 numeric, "percent" returns 0-100 numeric, "percent_fmt"
# returns a "53.3%" character string (Freq column only).
.RESULT_FORMATS <- c("proportion", "percent", "percent_fmt")

# Validate the (result_format, digits) pair. Called once at the top of
# analyze_survey(). Aggregators called directly via svyflow::: also run it
# so a bad call gets a clear error rather than a downstream type mismatch.
.validate_format_args <- function(result_format, digits) {
  if (!is.character(result_format) || length(result_format) != 1 ||
      !(result_format %in% .RESULT_FORMATS)) {
    stop("`result_format` must be one of: ",
         paste(shQuote(.RESULT_FORMATS), collapse = ", "))
  }
  if (!is.null(digits)) {
    if (!is.numeric(digits) || length(digits) != 1 ||
        is.na(digits) || digits < 0) {
      stop("`digits` must be a non-negative numeric scalar (or NULL).")
    }
  }
  invisible(TRUE)
}

# Apply scaling (proportion <-> percent) and optional rounding to a numeric
# proportion vector. Used for SE / CI columns where we never append "%".
# digits is ignored in "proportion" mode so the default digits=1 does not
# crush proportion precision; pass digits=NULL to disable rounding entirely.
.scale_prop <- function(x, result_format, digits) {
  val <- if (result_format == "proportion") x else x * 100
  if (result_format != "proportion" && !is.null(digits)) {
    val <- round(val, digits)
  }
  val
}

# Same as .scale_prop but for the Freq column: in "percent_fmt" mode the
# output is a character vector with a trailing "%".
.format_prop <- function(x, result_format, digits) {
  val <- .scale_prop(x, result_format, digits)
  if (result_format != "percent_fmt") return(val)
  d <- if (is.null(digits)) 1 else digits
  out <- ifelse(is.na(val), NA_character_,
                paste0(formatC(val, format = "f", digits = d), "%"))
  out
}

# Cast a raw numeric (mean, sum, median, etc.) into the same type used by
# Freq in percent_fmt mode, so bind_rows() does not have to widen the
# column. Non-proportion aggregators have no "%" — plain formatted number.
.coerce_freq_if_fmt <- function(x, result_format, digits) {
  if (result_format != "percent_fmt") return(x)
  d <- if (is.null(digits)) 1 else digits
  ifelse(is.na(x), NA_character_,
         formatC(x, format = "f", digits = d))
}

# An "empty" aggregator row used when a variable is entirely NA. Same shape
# as a real aggregator return so it can be rbind-stacked safely.
.empty_row <- function(ques, method, disag, level, result_format = "proportion") {
  freq_na <- if (result_format == "percent_fmt") NA_character_ else NA_real_
  tibble::tibble(
    Var1   = NA_character_,
    Freq   = freq_na,
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
