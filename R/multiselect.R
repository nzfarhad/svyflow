#' Detect already-expanded multi-select option columns
#'
#' Kobo and SurveyCTO exports commonly include one binary (0/1) column per
#' option for every `select_multiple` question, named like `var/opt`,
#' `var.opt`, or `var___opt`. This helper scans a data frame for those
#' sibling columns so callers know whether [expand_multiselect()] needs to
#' run.
#'
#' @param df Data frame.
#' @param vars Character vector of `select_multiple` variable names to look
#'   up.
#'
#' @return A named list. Each element is a character vector of detected
#'   option column names for the corresponding variable (empty if none
#'   detected).
#'
#' @examples
#' df <- data.frame(
#'   `needs/cash` = c(1L, 0L), `needs/food` = c(0L, 1L),
#'   check.names = FALSE
#' )
#' detect_ms_options(df, "needs")
#'
#' @seealso [expand_multiselect()]
#' @export
detect_ms_options <- function(df, vars) {
  out <- stats::setNames(vector("list", length(vars)), vars)
  for (v in vars) {
    prefixes <- paste0(v, .MS_SEPS)
    matches <- names(df)[vapply(names(df), function(n) {
      any(vapply(prefixes, function(p) startsWith(n, p), logical(1)))
    }, logical(1))]
    out[[v]] <- matches
  }
  out
}

#' Expand multi-select string columns into binary option columns
#'
#' Splits each named "; "-separated string column into one 0/1 column per
#' unique option (Kobo convention `var___opt`), but only for variables that
#' do **not** already have sibling binary columns (`var/opt`, `var.opt`,
#' `var___opt`). The detected or newly-created option columns are recorded
#' on the result as `attr(df, "ms_options")` so [analyze_survey()] can find
#' them without rescanning.
#'
#' If a generated column name would collide with an existing column, a
#' numeric suffix (`_x1`, `_x2`, ...) is appended and a warning is emitted.
#'
#' @param df Data frame.
#' @param vars Character vector of multi-select variable names.
#' @param sep Separator between options in the source string column.
#'   Defaults to `"; "`.
#'
#' @return The input data frame, possibly with new binary columns appended,
#'   and an `ms_options` attribute mapping each variable to its option
#'   column names.
#'
#' @examples
#' df <- data.frame(needs = c("cash; food", "food", NA, "shelter; cash"))
#' out <- expand_multiselect(df, vars = "needs")
#' attr(out, "ms_options")
#'
#' @seealso [detect_ms_options()], [analyze_survey()]
#' @export
expand_multiselect <- function(df, vars, sep = "; ") {
  ms_options <- list()
  for (v in vars) {
    existing <- detect_ms_options(df, v)[[v]]
    if (length(existing) > 0) {
      ms_options[[v]] <- existing
      next
    }
    if (!v %in% names(df)) {
      warning("expand_multiselect: variable '", v, "' not found in df; skipping")
      next
    }

    tokens_per_row <- strsplit(as.character(df[[v]]), sep, fixed = TRUE)
    all_tokens <- unique(unlist(tokens_per_row))
    all_tokens <- all_tokens[!is.na(all_tokens) & nzchar(all_tokens)]

    new_cols <- character(length(all_tokens))
    for (i in seq_along(all_tokens)) {
      tok <- all_tokens[i]
      candidate <- paste0(v, "___", tok)
      final <- candidate
      k <- 1
      while (final %in% names(df)) {
        final <- paste0(candidate, "_x", k)
        k <- k + 1
        warning("expand_multiselect: name collision for '", candidate,
                "'; using '", final, "'")
      }
      flags <- as.integer(vapply(tokens_per_row,
                                 function(toks) tok %in% toks, logical(1)))
      flags[is.na(df[[v]])] <- NA_integer_
      df[[final]] <- flags
      new_cols[i] <- final
    }
    ms_options[[v]] <- new_cols
  }
  attr(df, "ms_options") <- ms_options
  df
}
