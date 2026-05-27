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
                                       ...) {
  theme <- .as_xlsx_theme(theme)

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
                      styles = styles)
  }
  if (isTRUE(overall_sheet)) {
    .write_xtab_sheet(wb, x, sheet = .safe_sheet(overall_label), disag = NULL,
                      q_order = q_order, q_group = q_group,
                      has_group = has_group, overall_label = overall_label,
                      styles = styles)
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
.question_block <- function(disagg_rows, all_rows, qlabel, overall_label) {
  rows_all <- rbind(disagg_rows, all_rows)
  na_val   <- if (is.character(rows_all$Result)) NA_character_ else NA_real_
  numeric_q <- all(is.na(rows_all$Response))

  lvls <- unique(disagg_rows$Disaggregation_level)
  lvls <- lvls[!is.na(lvls)]
  has_overall <- nrow(all_rows) > 0
  row_labels  <- c(lvls, if (has_overall) overall_label)

  rows_for <- function(lbl) {
    if (has_overall && identical(lbl, overall_label)) {
      all_rows
    } else {
      disagg_rows[!is.na(disagg_rows$Disaggregation_level) &
                  disagg_rows$Disaggregation_level == lbl, , drop = FALSE]
    }
  }

  df <- data.frame(.row = row_labels, check.names = FALSE,
                   stringsAsFactors = FALSE)

  if (numeric_q) {
    vcol <- .nice_method(stats::na.omit(rows_all$Aggregation_method)[1])
    df[[vcol]] <- vapply(row_labels, function(lbl) {
      rr <- rows_for(lbl)
      if (nrow(rr)) rr$Result[1] else na_val
    }, FUN.VALUE = na_val)
  } else {
    responses <- unique(rows_all$Response)
    responses <- responses[!is.na(responses)]
    for (resp in responses) {
      df[[resp]] <- vapply(row_labels, function(lbl) {
        rr <- rows_for(lbl)
        v  <- rr$Result[!is.na(rr$Response) & rr$Response == resp]
        if (length(v)) v[1] else na_val
      }, FUN.VALUE = na_val)
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
                              has_group, overall_label, styles) {
  openxlsx::addWorksheet(wb, sheet)
  rptr       <- 1L
  max_cols   <- 1L
  prev_group <- ""            # sentinel that never equals a real group
  any_block  <- FALSE

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
        rptr <- rptr + 1L
      }
      prev_group <- if (is.na(g)) prev_group else g
    }

    df       <- .question_block(disagg_rows, all_rows, q, overall_label)
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

    rptr      <- rptr + nrow_df + 2L   # header + body + one blank spacer
    any_block <- TRUE
  }

  if (any_block) {
    openxlsx::setColWidths(wb, sheet, cols = 1, widths = 24)
    if (max_cols > 1) {
      openxlsx::setColWidths(wb, sheet, cols = 2:max_cols, widths = 14)
    }
  }
}
