#' Reformat an existing svyflow_results tibble
#'
#' Converts the `Result`, `SE`, `CI_low`, `CI_high` columns between the three
#' supported representations (`"proportion"`, `"percent"`, `"percent_fmt"`)
#' *after* the analysis has run. The motivation is performance: re-running
#' [analyze_survey()] just to change presentation is wasteful when the plan
#' is large, and `format_results()` is a cheap post-processing pass.
#'
#' Aggregation-method aware: only rows where `Aggregation_method == "perc"`
#' (i.e. produced by `select_one` / `select_multiple` aggregators) are
#' rescaled and `%`-suffixed. Rows for `mean`, `sum`, `median`, `1st_Qu`,
#' `3rd_Qu`, `min_unweighted`, `max_unweighted` carry raw quantities; their
#' values are only type-coerced (character <-> numeric) when the target
#' format demands it, with no scaling and no `%`.
#'
#' Round-trip caveat: `"percent_fmt"` is a rounded character form, so
#' `percent_fmt -> proportion` recovers the rounded value, not the original
#' full-precision proportion.
#'
#' @param x A [`svyflow_results`] tibble, or any data frame carrying the
#'   public columns `Result`, `SE`, `CI_low`, `CI_high`, `Aggregation_method`.
#' @param to Target format: `"proportion"` (0-1 numeric), `"percent"` (0-100
#'   numeric), or `"percent_fmt"` (character `"53.3%"`).
#' @param from Source format. If `NULL` (the default), read from
#'   `attr(x, "result_format")` when present; otherwise inferred from the
#'   type / range of the `Result` column on proportion rows.
#' @param digits Non-negative numeric scalar, or `NULL` for no rounding.
#'   Applied when `to` is `"percent"` or `"percent_fmt"`; ignored for
#'   `"proportion"` so a default of `1` does not crush precision.
#'
#' @return A `svyflow_results` tibble with the four numeric/character
#'   columns reformatted. Attributes `result_format` and `digits` are
#'   refreshed to reflect the new state.
#'
#' @examples
#' df <- data.frame(
#'   gender = sample(c("m","f"), 200, TRUE),
#'   age    = round(rnorm(200, 35, 8))
#' )
#' ap <- data.frame(
#'   variable = c("gender", "age"),
#'   kobo_type = c("select_one", "integer"),
#'   aggregation_method = c(NA, "mean"),
#'   disaggregation = c("all", "gender")
#' )
#' res <- analyze_survey(make_design(df), ap)           # proportion (default)
#' format_results(res, to = "percent")                  # 0-100 numeric
#' format_results(res, to = "percent_fmt", digits = 2)  # "53.27%"
#'
#' @seealso [analyze_survey()]
#' @export
format_results <- function(x, to, from = NULL, digits = 1) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame / svyflow_results tibble.")
  }
  required <- c("Result", "SE", "CI_low", "CI_high", "Aggregation_method")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("`x` is missing required column(s): ",
         paste(shQuote(missing_cols), collapse = ", "))
  }
  .validate_format_args(to, digits)

  if (is.null(from)) {
    from <- attr(x, "result_format")
    if (is.null(from)) from <- .infer_result_format(x)
  } else {
    if (!is.character(from) || length(from) != 1 ||
        !(from %in% .RESULT_FORMATS)) {
      stop("`from` must be one of: ",
           paste(shQuote(.RESULT_FORMATS), collapse = ", "), " (or NULL)")
    }
  }

  if (identical(from, to)) {
    attr(x, "digits") <- digits
    return(if (inherits(x, "svyflow_results")) x else new_svyflow_results(x, to, digits))
  }

  is_perc <- !is.na(x$Aggregation_method) & x$Aggregation_method == "perc"
  cols <- c("Result", "SE", "CI_low", "CI_high")

  # Step 1: lift all four columns to numeric in the proportion (0-1) scale.
  num <- lapply(cols, function(cc) .to_proportion(x[[cc]], is_perc, from))
  names(num) <- cols

  # Step 2: write back in the requested target format.
  for (cc in cols) {
    x[[cc]] <- .from_proportion(num[[cc]], is_perc, to, digits)
  }

  attr(x, "result_format") <- to
  attr(x, "digits")        <- digits
  if (!inherits(x, "svyflow_results")) {
    x <- new_svyflow_results(x, result_format = to, digits = digits)
  }
  x
}

# Parse a column out of whichever source format it came from into a numeric
# vector on the proportion (0-1) scale. Non-perc rows are NOT rescaled — they
# stay in their native units; we only handle string -> numeric coercion.
.to_proportion <- function(col, is_perc, from) {
  if (is.character(col)) {
    # Strip a trailing "%" if present; survives both percent_fmt cells and
    # accidental "%" in numeric source modes.
    stripped <- sub("%$", "", col)
    num <- suppressWarnings(as.numeric(stripped))
  } else {
    num <- as.numeric(col)
  }
  if (from == "proportion") return(num)
  # from is "percent" or "percent_fmt": divide perc rows by 100 to get back
  # to proportion. Non-perc rows were never scaled, leave them.
  out <- num
  out[is_perc] <- num[is_perc] / 100
  out
}

# Write a proportion-scale numeric vector back out in the target format.
# is_perc marks the rows that should be scaled (and "%"-suffixed in
# percent_fmt). Non-perc rows keep their raw value; rounding by `digits`
# is applied ONLY to proportion (`is_perc`) rows, matching the behaviour
# of analyze_survey(result_format = "percent" / "percent_fmt"). In
# percent_fmt mode non-perc rows still get character-coerced (no %, but
# with `digits` decimals via formatC) so the column type stays stable.
.from_proportion <- function(num, is_perc, to, digits) {
  if (to == "proportion") return(num)

  scaled <- num
  scaled[is_perc] <- num[is_perc] * 100
  if (!is.null(digits)) scaled[is_perc] <- round(scaled[is_perc], digits)

  if (to == "percent") return(scaled)

  # to == "percent_fmt": all rows become character.
  d <- if (is.null(digits)) 1 else digits
  out <- ifelse(is.na(scaled), NA_character_,
                formatC(scaled, format = "f", digits = d))
  out[is_perc & !is.na(out)] <- paste0(out[is_perc & !is.na(out)], "%")
  out
}

# Fallback used when from = NULL and attr(x, "result_format") is missing.
# Looks at the type of the Result column and, if numeric, the range of
# proportion rows.
.infer_result_format <- function(x) {
  if (is.character(x$Result)) return("percent_fmt")
  is_perc <- !is.na(x$Aggregation_method) & x$Aggregation_method == "perc"
  vals <- suppressWarnings(as.numeric(x$Result[is_perc]))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return("proportion")
  if (max(vals, na.rm = TRUE) > 1) "percent" else "proportion"
}
