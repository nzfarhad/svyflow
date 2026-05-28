#' Export svyflow results to a styled Excel workbook
#'
#' Writes the long output of [analyze_survey()] to an `.xlsx` file as
#' publication-ready crosstabs: **one sheet per disaggregation variable**,
#' plus an **Overall** sheet for the un-disaggregated (`"all"`) results.
#' Within each sheet, one crosstab block per question is stacked in plan
#' order, with **disaggregation levels as rows and an `Overall` summary row
#' at the bottom** of each block. If the analysis plan supplied a `group`
#' column, its values become **section-separator headers** between blocks.
#'
#' Values are written **exactly as they appear in `x`** — no number
#' formatting is imposed. Pass the default proportion output for 0-1 values,
#' or run [format_results()] first for percentages / formatted percentages.
#'
#' Styling follows a clean publication theme by default; override via
#' [xlsx_theme()].
#'
#' Double-disaggregation (`repeat_for`) rows are not included in this export.
#'
#' @param x A [`svyflow_results`] tibble from [analyze_survey()] (or any data
#'   frame with the public columns, optionally a `Group` column).
#' @param file Output path ending in `.xlsx`.
#' @param theme An [xlsx_theme()] object controlling fonts, colours and
#'   fills. Defaults to the standard publication theme.
#' @param overall_sheet If `TRUE` (default), add a dedicated sheet holding
#'   the un-disaggregated results for every question.
#' @param overall_label Row/sheet label for the un-disaggregated results.
#'   Default `"Overall"`.
#' @param with_ci If `TRUE`, compose each value cell as
#'   `"<estimate> (<CI_low> - <CI_high>)"` using the values exactly as they
#'   appear in `x` (no re-scaling or re-rounding). Set the precision /
#'   scale upstream with [format_results()]. Rows without confidence
#'   intervals (`min` / `max`) fall back to a plain estimate. Default
#'   `FALSE`.
#' @param with_counts How to display unweighted counts. `"none"` (default)
#'   omits them. `"row_label"` appends ` (n=<N>)` to every row label
#'   (disaggregation levels and the Overall row), where `N` is the level's
#'   `Denominator`. Requires a `Denominator` column on `x`; missing or `NA`
#'   denominators are silently skipped.
#' @param col_width Fixed width (Excel character-width units, ~9.3 px per
#'   unit at Calibri 11 pt) applied to every value column. Default `21`
#'   (~196 px) — comfortable for `"<est> (<lo> - <hi>)"` with one decimal.
#'   The row-label (first) column always sizes itself to its content.
#'   Long column **headers wrap** within the fixed width automatically.
#' @param ... Unused; for S3 compatibility.
#'
#' @return The output `file` path, invisibly.
#'
#' @examples
#' \donttest{
#' df <- data.frame(
#'   gender = sample(c("m", "f"), 200, TRUE),
#'   edu    = sample(c("none", "primary", "secondary"), 200, TRUE),
#'   weight = runif(200, 0.5, 2)
#' )
#' ap <- tibble::tribble(
#'   ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation, ~group,
#'   "edu",     "select_one", NA,                  "all",           "Education",
#'   "edu",     "select_one", NA,                  "gender",        "Education"
#' )
#' res <- analyze_survey(make_design(df, weights = "weight"), ap)
#' write_xlsx(res, tempfile(fileext = ".xlsx"))
#' }
#'
#' @seealso [analyze_survey()], [format_results()], [xlsx_theme()]
#' @export
write_xlsx <- function(x, file, ...) {
  UseMethod("write_xlsx")
}

#' @rdname write_xlsx
#' @export
write_xlsx.svyflow_results <- function(x, file,
                                       theme = xlsx_theme(),
                                       overall_sheet = TRUE,
                                       overall_label = "Overall",
                                       with_ci       = FALSE,
                                       with_counts   = c("none", "row_label"),
                                       col_width     = 21,
                                       ...) {
  theme       <- .as_xlsx_theme(theme)
  with_counts <- match.arg(with_counts)
  if (!is.numeric(col_width) || length(col_width) != 1 ||
      is.na(col_width) || col_width <= 0) {
    stop("`col_width` must be a single positive number (Excel width units).")
  }

  x <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("Disaggregation", "Disaggregation_level", "Question",
                "Response", "Aggregation_method", "Result")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("`x` is missing required column(s): ",
         paste(shQuote(missing_cols), collapse = ", "))
  }

  # v1: exclude double-disaggregation rows.
  if ("repeat_for" %in% names(x)) {
    x <- x[is.na(x$repeat_for), , drop = FALSE]
  }
  if (nrow(x) == 0) stop("`x` has no rows to export.")

  has_group <- "Group" %in% names(x)
  q_order   <- unique(x$Question)
  q_group   <- .question_groups(x, q_order, has_group)

  disag_vars <- unique(x$Disaggregation[!is.na(x$Disaggregation) &
                                        x$Disaggregation != "all"])

  wb     <- openxlsx::createWorkbook()
  styles <- .xlsx_styles(theme)

  for (g in disag_vars) {
    .write_xtab_sheet(wb, x, sheet = .safe_sheet(g), disag = g,
                      q_order = q_order, q_group = q_group,
                      has_group = has_group, overall_label = overall_label,
                      styles = styles,
                      with_ci = with_ci, with_counts = with_counts,
                      col_width = col_width)
  }
  if (isTRUE(overall_sheet)) {
    .write_xtab_sheet(wb, x, sheet = .safe_sheet(overall_label), disag = NULL,
                      q_order = q_order, q_group = q_group,
                      has_group = has_group, overall_label = overall_label,
                      styles = styles,
                      with_ci = with_ci, with_counts = with_counts,
                      col_width = col_width)
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}


# ---- internal helpers ------------------------------------------------------

# Excel sheet names: <= 31 chars, no : \ / ? * [ ]
.safe_sheet <- function(name) {
  s <- gsub("[:\\\\/?*\\[\\]]", "_", as.character(name))
  substr(s, 1, 31)
}

# Map each question to its (first non-NA) group label, preserving order.
.question_groups <- function(x, q_order, has_group) {
  if (!has_group) {
    g <- rep(NA_character_, length(q_order))
    names(g) <- q_order
    return(g)
  }
  g <- vapply(q_order, function(q) {
    gv <- x$Group[x$Question == q]
    gv <- gv[!is.na(gv)]
    if (length(gv)) as.character(gv[1]) else NA_character_
  }, character(1))
  names(g) <- q_order
  g
}

# Human-readable label for a numeric aggregation method.
.nice_method <- function(m) {
  switch(as.character(m),
    mean           = "Mean",
    sum            = "Sum",
    median         = "Median",
    `1st_Qu`       = "Q25",
    `3rd_Qu`       = "Q75",
    min_unweighted = "Min",
    max_unweighted = "Max",
    perc           = "Percentage",
    as.character(m)
  )
}

# Build one question's crosstab block as a data.frame. First column (named
# with the question label) holds the row labels (disaggregation levels +
# the overall label); remaining columns hold the values, written as-is.
#
# `with_ci`:        if TRUE, each cell becomes "<est> (<lo> - <hi>)" composed
#                   from Result / CI_low / CI_high in the source rows.
# `with_counts`:    "row_label" appends " (n=N)" to each row label using
#                   the level's Denominator; "none" leaves labels alone.
.question_block <- function(disagg_rows, all_rows, qlabel, overall_label,
                            with_ci = FALSE,
                            with_counts = c("none", "row_label")) {
  with_counts <- match.arg(with_counts)
  rows_all  <- rbind(disagg_rows, all_rows)
  na_val    <- if (is.character(rows_all$Result)) NA_character_ else NA_real_
  numeric_q <- all(is.na(rows_all$Response))

  lvls        <- unique(disagg_rows$Disaggregation_level)
  lvls        <- lvls[!is.na(lvls)]
  has_overall <- nrow(all_rows) > 0
  base_labels <- c(lvls, if (has_overall) overall_label)

  rows_for <- function(lbl) {
    if (has_overall && identical(lbl, overall_label)) {
      all_rows
    } else {
      disagg_rows[!is.na(disagg_rows$Disaggregation_level) &
                  disagg_rows$Disaggregation_level == lbl, , drop = FALSE]
    }
  }

  # Optional "(n=N)" suffix on the row labels.
  display_labels <- base_labels
  if (with_counts == "row_label" && "Denominator" %in% names(rows_all)) {
    display_labels <- vapply(base_labels, function(lbl) {
      rr <- rows_for(lbl)
      if (!nrow(rr)) return(lbl)
      d <- rr$Denominator[!is.na(rr$Denominator)]
      if (length(d) && is.finite(d[1])) {
        sprintf("%s (n=%d)", lbl, as.integer(d[1]))
      } else lbl
    }, character(1))
  }

  # Compose one cell, respecting with_ci. `est`, `lo`, `hi` are taken
  # verbatim from the source frame (no re-scaling / re-rounding).
  compose <- function(est, lo, hi) {
    if (!with_ci) return(est)
    if (length(est) == 0 || is.na(est)) return(NA_character_)
    if (length(lo) == 0 || is.na(lo) ||
        length(hi) == 0 || is.na(hi)) return(as.character(est))
    paste0(as.character(est), " (",
           as.character(lo), " - ",
           as.character(hi), ")")
  }
  cell_fun_val <- if (with_ci) NA_character_ else na_val

  # Value lookup for a (level, optional response) pair.
  build_cell <- function(rr, response = NULL) {
    if (!is.null(response)) {
      rr <- rr[!is.na(rr$Response) & rr$Response == response, , drop = FALSE]
    }
    if (!nrow(rr)) return(if (with_ci) NA_character_ else na_val)
    compose(
      rr$Result[1],
      if ("CI_low"  %in% names(rr)) rr$CI_low[1]  else NA,
      if ("CI_high" %in% names(rr)) rr$CI_high[1] else NA
    )
  }

  df <- data.frame(.row = display_labels, check.names = FALSE,
                   stringsAsFactors = FALSE)

  if (numeric_q) {
    vcol <- .nice_method(stats::na.omit(rows_all$Aggregation_method)[1])
    df[[vcol]] <- vapply(base_labels, function(lbl) build_cell(rows_for(lbl)),
                         FUN.VALUE = cell_fun_val)
  } else {
    responses <- unique(rows_all$Response)
    responses <- responses[!is.na(responses)]
    for (resp in responses) {
      df[[resp]] <- vapply(base_labels,
                           function(lbl) build_cell(rows_for(lbl), resp),
                           FUN.VALUE = cell_fun_val)
    }
  }

  names(df)[1] <- qlabel
  df
}

# openxlsx style objects derived from a theme.
.xlsx_styles <- function(theme) {
  list(
    header = openxlsx::createStyle(
      fontName       = theme$font_name,
      fontSize       = theme$header_font_size,
      textDecoration = if (theme$header_bold) "bold" else NULL,
      fontColour     = theme$header_font_color,
      fgFill         = theme$header_fill,
      halign         = "center",
      valign         = "center",
      wrapText       = TRUE,
      border         = "TopBottomLeftRight",
      borderColour   = theme$border_color
    ),
    body = openxlsx::createStyle(
      fontName     = theme$font_name,
      fontSize     = theme$body_font_size,
      fontColour   = theme$body_font_color,
      border       = "TopBottomLeftRight",
      borderColour = theme$border_color
    ),
    label = openxlsx::createStyle(
      fontName       = theme$font_name,
      fontSize       = theme$body_font_size,
      fontColour     = theme$body_font_color,
      textDecoration = if (theme$label_bold) "bold" else NULL,
      border         = "TopBottomLeftRight",
      borderColour   = theme$border_color
    ),
    section = openxlsx::createStyle(
      fontName       = theme$font_name,
      fontSize       = theme$section_font_size,
      textDecoration = if (theme$section_bold) "bold" else NULL,
      fontColour     = theme$section_font_color,
      fgFill         = theme$section_fill
    )
  )
}

# Write all question blocks for one sheet. disag = NULL means the Overall
# sheet (only the un-disaggregated rows).
.write_xtab_sheet <- function(wb, x, sheet, disag, q_order, q_group,
                              has_group, overall_label, styles,
                              with_ci = FALSE,
                              with_counts = "none",
                              col_width = 21) {
  openxlsx::addWorksheet(wb, sheet)
  rptr       <- 1L
  max_cols   <- 1L
  prev_group <- ""            # sentinel that never equals a real group
  any_block  <- FALSE
  col_chars  <- integer(0)    # per-column max content length, for autosizing

  upd <- function(values, col) {
    if (length(col_chars) < col) {
      col_chars <<- c(col_chars, integer(col - length(col_chars)))
    }
    vals <- as.character(values)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals)) {
      col_chars[col] <<- max(col_chars[col], max(nchar(vals)))
    }
  }

  for (q in q_order) {
    if (is.null(disag)) {
      all_rows    <- x[x$Question == q & x$Disaggregation == "all", , drop = FALSE]
      if (nrow(all_rows) == 0) next
      disagg_rows <- all_rows[0, , drop = FALSE]
    } else {
      disagg_rows <- x[x$Question == q & x$Disaggregation == disag, , drop = FALSE]
      if (nrow(disagg_rows) == 0) next
      all_rows    <- x[x$Question == q & x$Disaggregation == "all", , drop = FALSE]
    }

    if (has_group) {
      g <- q_group[[q]]
      if (!is.na(g) && !identical(g, prev_group)) {
        openxlsx::writeData(wb, sheet, g, startRow = rptr, startCol = 1)
        openxlsx::addStyle(wb, sheet, styles$section, rows = rptr, cols = 1,
                           gridExpand = TRUE, stack = TRUE)
        upd(g, 1)
        rptr <- rptr + 1L
      }
      prev_group <- if (is.na(g)) prev_group else g
    }

    df       <- .question_block(disagg_rows, all_rows, q, overall_label,
                                 with_ci = with_ci, with_counts = with_counts)
    ncol_df  <- ncol(df)
    nrow_df  <- nrow(df)
    max_cols <- max(max_cols, ncol_df)

    openxlsx::writeData(wb, sheet, df, startRow = rptr, startCol = 1,
                        colNames = TRUE)
    openxlsx::addStyle(wb, sheet, styles$header, rows = rptr,
                       cols = seq_len(ncol_df), gridExpand = TRUE, stack = TRUE)
    body_rows <- seq.int(rptr + 1L, rptr + nrow_df)
    openxlsx::addStyle(wb, sheet, styles$body, rows = body_rows,
                       cols = seq_len(ncol_df), gridExpand = TRUE, stack = TRUE)
    # Row-label column gets the (optionally bold) label style.
    openxlsx::addStyle(wb, sheet, styles$label, rows = body_rows, cols = 1,
                       gridExpand = TRUE, stack = TRUE)

    # Track per-column max content length across header + body for autosize.
    for (j in seq_len(ncol_df)) {
      upd(c(names(df)[j], df[[j]]), j)
    }

    rptr      <- rptr + nrow_df + 2L   # header + body + one blank spacer
    any_block <- TRUE
  }

  if (any_block) {
    # Row-label column sizes to its content (with sensible bounds).
    label_width <- if (length(col_chars) >= 1L) {
      min(max(col_chars[1] + 2L, 10L), 40L)
    } else 12L
    openxlsx::setColWidths(wb, sheet, cols = 1, widths = label_width)
    if (max_cols > 1L) {
      openxlsx::setColWidths(wb, sheet, cols = 2:max_cols,
                             widths = col_width)
    }
  }
}
