#' Export a standalone methods / summary workbook
#'
#' Writes a single-sheet `.xlsx` workbook documenting how an analysis was
#' produced: session info, the survey design specification, sample-size
#' counts, weights summary, CI options, result format, and (optionally) a
#' plan summary and a DEFF roll-up. Intended as a companion artefact to
#' [write_xlsx()] so a recipient can interpret and reproduce the results
#' workbook.
#'
#' The function is deliberately standalone — it does not touch
#' [analyze_survey()] or [write_xlsx()] and produces its own workbook.
#'
#' If `results` is supplied, its stamped `result_format` / `digits`
#' attributes are read in preference to the explicit arguments, reducing
#' the risk that the methods sheet drifts away from what was actually
#' computed. If the result carries a `DEFF` column, a roll-up section is
#' added.
#'
#' @param file Output path ending in `.xlsx`.
#' @param design The [srvyr::tbl_svy] used for the analysis (the value
#'   passed to [analyze_survey()]).
#' @param ci A [ci_opts()] bundle (or a list of its arguments). Documents
#'   the CI method used.
#' @param result_format,digits As passed to [analyze_survey()]. Overridden
#'   when `results` carries them as attributes.
#' @param plan Optional analysis-plan data frame. When supplied, the sheet
#'   gains a "Plan" section with indicator counts by `kobo_type` and
#'   `aggregation_method`.
#' @param results Optional [`svyflow_results`] tibble. When supplied with
#'   a `DEFF` column, a "Design effect (DEFF)" section summarises the
#'   distribution and lists the five highest DEFFs.
#' @param cover_notes Optional character vector of free-text lines —
#'   project name, funder, contact, footnote disclaimers, etc. When
#'   supplied, rendered as a "Project" section near the top of the
#'   sheet. Each element becomes one line. Named entries render as
#'   `"<name>: <value>"`; unnamed entries render verbatim.
#' @param theme An [xlsx_theme()] object controlling fonts, colours, and
#'   section styling.
#'
#' @return The output `file` path, invisibly.
#'
#' @examples
#' \donttest{
#' df <- data.frame(
#'   gender = sample(c("m", "f"), 200, TRUE),
#'   weight = runif(200, 0.5, 2)
#' )
#' des <- make_design(df, weights = "weight")
#' ap  <- tibble::tribble(
#'   ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation,
#'   "gender",  "select_one", NA,                  "all"
#' )
#' res <- analyze_survey(des, ap, deff = TRUE)
#' write_methods_xlsx(tempfile(fileext = ".xlsx"),
#'                    design = des, plan = ap, results = res)
#' }
#'
#' @seealso [write_xlsx()], [analyze_survey()], [ci_opts()]
#' @export
write_methods_xlsx <- function(file,
                               design,
                               ci            = ci_opts(),
                               result_format = "proportion",
                               digits        = 1,
                               plan          = NULL,
                               results       = NULL,
                               cover_notes   = NULL,
                               theme         = xlsx_theme()) {
  ci    <- .as_ci_opts(ci)
  theme <- .as_xlsx_theme(theme)
  .validate_format_args(result_format, digits)
  cover_notes <- .as_cover_notes(cover_notes)

  # Prefer attributes stamped by analyze_survey() so the sheet documents
  # what was actually computed, not whatever defaults the user passed in.
  if (!is.null(results)) {
    rf <- attr(results, "result_format")
    if (!is.null(rf)) result_format <- rf
    dg <- attr(results, "digits")
    if (!is.null(dg)) digits <- dg
  }

  dmeta <- .design_meta(design)
  smeta <- .session_meta()
  pmeta <- if (!is.null(plan))    .plan_meta(plan)       else NULL
  rmeta <- if (!is.null(results)) .deff_meta(results)    else NULL

  wb     <- openxlsx::createWorkbook()
  sheet  <- "Methods"
  openxlsx::addWorksheet(wb, sheet)
  styles <- .xlsx_styles(theme)
  # Sub-table heading: bold body text, no border (intentionally lighter
  # than the bordered `label` style used elsewhere on key/value rows).
  styles$subhead <- openxlsx::createStyle(
    fontName       = theme$font_name,
    fontSize       = theme$body_font_size,
    fontColour     = theme$body_font_color,
    textDecoration = "bold"
  )
  row    <- 1L

  row <- .mx_section(wb, sheet, row,
                     "svyflow analysis - methods summary", styles)
  row <- row + 1L  # one-row spacer after the title

  if (length(cover_notes) > 0) {
    row <- .mx_section(wb, sheet, row, "Project", styles)
    row <- .mx_text(wb, sheet, row, cover_notes, styles)
  }

  row <- .mx_section(wb, sheet, row, "Session", styles)
  row <- .mx_kv(wb, sheet, row, c(
    "Generated"  = smeta$timestamp,
    "User"       = smeta$user,
    "R version"  = smeta$r_version,
    "svyflow"    = smeta$svyflow_version,
    "survey"     = smeta$survey_version,
    "srvyr"      = smeta$srvyr_version,
    "openxlsx"   = smeta$openxlsx_version
  ), styles)

  row <- .mx_section(wb, sheet, row, "Data", styles)
  row <- .mx_kv(wb, sheet, row, c(
    "Rows"    = .fmt_int(dmeta$n),
    "Columns" = .fmt_int(dmeta$ncol)
  ), styles)

  row <- .mx_section(wb, sheet, row, "Survey design", styles)
  row <- .mx_kv(wb, sheet, row, c(
    "Weighting"      = if (dmeta$unweighted) "unweighted"
                       else "weighted (see Weights summary)",
    "Strata column"  = .na_dash(dmeta$strata_col),
    "Cluster / PSU"  = .na_dash(dmeta$ids_col),
    "FPC"            = if (isTRUE(!is.na(dmeta$fpc_col))) "set" else "not set"
  ), styles)

  row <- .mx_section(wb, sheet, row, "Sample sizes", styles)
  row <- .mx_kv(wb, sheet, row, c(
    "n (rows)"        = .fmt_int(dmeta$n),
    "Strata"          = .na_dash(dmeta$n_strata),
    "Clusters / PSUs" = .na_dash(dmeta$n_psu),
    "Design df"       = .na_dash(dmeta$design_df)
  ), styles)

  row <- .mx_section(wb, sheet, row, "Weights summary", styles)
  w <- dmeta$weights
  if (length(w) > 0 && any(!is.na(w))) {
    w <- w[!is.na(w)]
    cv <- if (mean(w) != 0) stats::sd(w) / mean(w) else NA_real_
    row <- .mx_kv(wb, sheet, row, c(
      "n"      = .fmt_int(length(w)),
      "Sum"    = .fmt_num(sum(w),            digits = 2),
      "Min"    = .fmt_num(min(w),            digits = 3),
      "Median" = .fmt_num(stats::median(w),  digits = 3),
      "Mean"   = .fmt_num(mean(w),           digits = 3),
      "Max"    = .fmt_num(max(w),            digits = 3),
      "CV"     = .fmt_num(cv,                digits = 3)
    ), styles)
  } else {
    row <- .mx_kv(wb, sheet, row,
                  c("Weights" = "(none / unweighted)"), styles)
  }

  row <- .mx_section(wb, sheet, row, "Confidence intervals", styles)
  row <- .mx_kv(wb, sheet, row, c(
    "Level"             = sprintf("%g%%", ci$ci_level * 100),
    "Degrees of freedom" = if (is.null(ci$df)) "design df"
                           else if (is.infinite(ci$df)) "Inf (normal)"
                           else as.character(ci$df),
    "Proportion method"  = if (is.null(ci$prop_method)) "Wald (default)"
                           else ci$prop_method,
    "Quantile interval"  = ci$interval_type,
    "Quantile rule"      = ci$qrule
  ), styles)

  row <- .mx_section(wb, sheet, row, "Result format", styles)
  row <- .mx_kv(wb, sheet, row, c(
    "Format" = result_format,
    "Digits" = .na_dash(digits)
  ), styles)

  if (!is.null(pmeta)) {
    row <- .mx_section(wb, sheet, row, "Plan", styles)
    row <- .mx_kv(wb, sheet, row, c(
      "Indicators"          = pmeta$n_indicators,
      "Disaggregation vars" = pmeta$n_disagg,
      "repeat_for present"  = if (pmeta$has_repeat_for) "yes" else "no",
      "Group column"        = if (pmeta$has_group)      "yes" else "no"
    ), styles)
    if (!is.null(pmeta$by_kobo_type) && nrow(pmeta$by_kobo_type) > 0) {
      row <- .mx_subhead(wb, sheet, row,
                         "Indicators by kobo_type", styles)
      row <- .mx_table(wb, sheet, row, pmeta$by_kobo_type, styles)
    }
    if (!is.null(pmeta$by_agg_method) && nrow(pmeta$by_agg_method) > 0) {
      row <- .mx_subhead(wb, sheet, row,
                         "Indicators by aggregation_method", styles)
      row <- .mx_table(wb, sheet, row, pmeta$by_agg_method, styles)
    }
  }

  if (!is.null(rmeta)) {
    row <- .mx_section(wb, sheet, row, "Design effect (DEFF)", styles)
    row <- .mx_kv(wb, sheet, row, c(
      "Rows with DEFF" = .fmt_int(rmeta$n_with_deff),
      "Mean DEFF"      = .fmt_num(rmeta$mean_deff,   digits = 2),
      "Median DEFF"    = .fmt_num(rmeta$median_deff, digits = 2),
      "Max DEFF"       = .fmt_num(rmeta$max_deff,    digits = 2),
      "Mean n_eff"     = .fmt_num(rmeta$mean_n_eff,  digits = 1)
    ), styles)
    row <- .mx_subhead(wb, sheet, row,
                       "Highest DEFF (top 5)", styles)
    row <- .mx_table(wb, sheet, row, rmeta$top_deff, styles)
  }

  row <- .mx_section(wb, sheet, row, "Notes", styles)
  row <- .mx_text(wb, sheet, row, c(
    "Denominator = count of non-missing (valid) responses for each indicator.",
    "Skip-logic NAs are dropped; the resulting base is the subgroup of respondents who reached the question.",
    "Quantile, min and max rows do not produce a design-correct DEFF.",
    "Excel cells are written verbatim from the analysis output - no number formatting is imposed."
  ), styles)

  openxlsx::setColWidths(wb, sheet, cols = 1, widths = 30)
  openxlsx::setColWidths(wb, sheet, cols = 2, widths = 36)
  openxlsx::setColWidths(wb, sheet, cols = 3:4, widths = 14)

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}


# ---- internal helpers -------------------------------------------------------

# Pull as much design metadata as we can from a srvyr / survey design.
# Reads the survey-design data slots rather than $call (srvyr wrappers
# overwrite $call with a placeholder string, so it can't be parsed).
.design_meta <- function(design) {
  if (is.null(design)) {
    stop("`design` is required.")
  }

  data <- design$variables
  n    <- if (!is.null(data)) nrow(data) else NA_integer_

  # Strata: survey::svydesign() inserts a single-column "V1" with one value
  # when no strata are given. Treat that as "not stratified".
  strata_df  <- design$strata
  strata_col <- if (!is.null(strata_df) && NCOL(strata_df) >= 1) {
    nm <- names(strata_df)
    if (NROW(strata_df) == 0 || length(unique(strata_df[[1]])) <= 1)
      NA_character_
    else paste(nm, collapse = ", ")
  } else NA_character_
  n_strata <- if (!is.null(strata_df) && NROW(strata_df) > 0 &&
                  !is.na(strata_col)) {
    length(unique(strata_df[[1]]))
  } else NA_integer_

  # Cluster / PSU: svydesign inserts a default "id" column of 1..n when
  # ids = ~1. Treat "every row is its own cluster" as no clustering.
  cluster_df <- design$cluster
  cluster_col <- if (!is.null(cluster_df) && NCOL(cluster_df) >= 1 &&
                     NROW(cluster_df) > 0) {
    nm <- names(cluster_df)
    if (length(unique(cluster_df[[1]])) >= NROW(cluster_df))
      NA_character_
    else paste(nm, collapse = ", ")
  } else NA_character_
  n_psu <- if (!is.null(cluster_df) && NROW(cluster_df) > 0 &&
               !is.na(cluster_col)) {
    length(unique(cluster_df[[1]]))
  } else NA_integer_

  # FPC: present when design$fpc$popsize is non-NULL.
  fpc_set <- !is.null(design$fpc) &&
             !is.null(design$fpc$popsize) &&
             length(design$fpc$popsize) > 0

  design_df <- tryCatch(as.integer(survey::degf(design)),
                        error = function(e) NA_integer_)

  w <- tryCatch(as.numeric(stats::weights(design, "sampling")),
                error = function(e) {
                  if (!is.null(design$pweights))
                    as.numeric(design$pweights)
                  else if (!is.null(design$prob))
                    1 / as.numeric(design$prob)
                  else NA_real_
                })
  # Detect "weights all == 1" (unweighted) so we can label it explicitly.
  unweighted <- length(w) > 0 && all(!is.na(w)) &&
                isTRUE(all.equal(stats::sd(w), 0)) &&
                isTRUE(all.equal(mean(w), 1))

  list(
    weights_col = if (unweighted) "(unweighted)" else NA_character_,
    strata_col  = strata_col,
    ids_col     = cluster_col,
    fpc_col     = if (fpc_set) "set" else NA_character_,
    n           = n,
    ncol        = if (!is.null(data)) ncol(data) else NA_integer_,
    n_strata    = n_strata,
    n_psu       = n_psu,
    design_df   = design_df,
    weights     = w,
    unweighted  = unweighted
  )
}

.session_meta <- function() {
  list(
    r_version        = paste(R.version$major, R.version$minor, sep = "."),
    svyflow_version  = as.character(utils::packageVersion("svyflow")),
    survey_version   = as.character(utils::packageVersion("survey")),
    srvyr_version    = as.character(utils::packageVersion("srvyr")),
    openxlsx_version = as.character(utils::packageVersion("openxlsx")),
    timestamp        = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    user             = tryCatch(Sys.info()[["user"]],
                                error = function(e) NA_character_)
  )
}

.plan_meta <- function(plan) {
  if (!is.data.frame(plan)) {
    stop("`plan` must be a data frame.")
  }
  if (!"kobo_type" %in% names(plan)) {
    stop("`plan` must have a `kobo_type` column.")
  }
  by_kobo <- .count_tbl(plan$kobo_type, "kobo_type")
  by_agg  <- if ("aggregation_method" %in% names(plan)) {
    am <- plan$aggregation_method
    am[is.na(am)] <- "(perc)"
    .count_tbl(am, "aggregation_method")
  } else NULL
  disag <- if ("disaggregation" %in% names(plan)) plan$disaggregation else character(0)
  n_disagg <- length(unique(disag[!is.na(disag) & disag != "all"]))

  list(
    n_indicators   = nrow(plan),
    by_kobo_type   = by_kobo,
    by_agg_method  = by_agg,
    n_disagg       = n_disagg,
    has_repeat_for = "repeat_for" %in% names(plan) &&
                       any(!is.na(plan$repeat_for)),
    has_group      = "group" %in% names(plan)
  )
}

.deff_meta <- function(results) {
  if (!is.data.frame(results)) return(NULL)
  if (!"DEFF" %in% names(results)) return(NULL)
  pop_methods <- c("perc", "mean", "sum")
  rows <- results[!is.na(results$Aggregation_method) &
                  results$Aggregation_method %in% pop_methods &
                  is.finite(results$DEFF), , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)

  ord <- order(-rows$DEFF)
  top_cols <- intersect(c("Question", "Response",
                          "Disaggregation", "Disaggregation_level",
                          "DEFF", "n_eff"),
                        names(rows))
  top <- rows[ord, top_cols, drop = FALSE]
  top <- utils::head(top, 5)
  # Tidy precision for display; matches the summary stats above.
  if ("DEFF"  %in% names(top)) top$DEFF  <- round(as.numeric(top$DEFF),  2)
  if ("n_eff" %in% names(top)) top$n_eff <- round(as.numeric(top$n_eff), 1)

  list(
    n_with_deff = nrow(rows),
    mean_deff   = mean(rows$DEFF,           na.rm = TRUE),
    median_deff = stats::median(rows$DEFF,  na.rm = TRUE),
    max_deff    = max(rows$DEFF,            na.rm = TRUE),
    mean_n_eff  = mean(rows$n_eff,          na.rm = TRUE),
    top_deff    = top
  )
}

.count_tbl <- function(x, name) {
  tab <- as.data.frame(table(x), stringsAsFactors = FALSE,
                       responseName = "n")
  names(tab)[1] <- name
  tab
}

# Coerce / validate the user-supplied free-text cover notes. Accepts a
# character vector (named or unnamed), a single string, or NULL. Returns a
# character vector with "name: value" formatting applied to any named
# entries, ready to be written one line per element.
.as_cover_notes <- function(x) {
  if (is.null(x)) return(character(0))
  if (!is.character(x) && !is.list(x)) {
    stop("`cover_notes` must be a character vector (named or unnamed), ",
         "or NULL.")
  }
  if (is.list(x)) x <- unlist(x, use.names = TRUE)
  nm <- names(x)
  vals <- as.character(x)     # NOTE: drops names; we kept them in `nm` above
  if (is.null(nm)) return(vals[!is.na(vals) & nzchar(vals)])
  out <- vapply(seq_along(vals), function(i) {
    v <- vals[[i]]
    if (is.na(v)) return(NA_character_)
    if (nzchar(nm[i])) sprintf("%s: %s", nm[i], v) else v
  }, character(1))
  out[!is.na(out) & nzchar(out)]
}

.formula_var <- function(f) {
  if (is.null(f)) return(NA_character_)
  v <- tryCatch(all.vars(stats::as.formula(f)),
                error = function(e) character(0))
  if (length(v) == 0) return(NA_character_)
  paste(v, collapse = ", ")
}

.na_dash <- function(x) {
  if (is.null(x) || length(x) == 0) return("-")
  if (is.na(x)) "-" else as.character(x)
}

.fmt_int <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  formatC(as.integer(x), format = "d", big.mark = ",")
}

.fmt_num <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) return("-")
  formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")
}

# Sheet-writing primitives. Each returns the next free row (with a one-row
# spacer already appended below the block).

.mx_section <- function(wb, sheet, row, text, styles) {
  openxlsx::writeData(wb, sheet, text, startRow = row, startCol = 1)
  openxlsx::mergeCells(wb, sheet, rows = row, cols = 1:2)
  openxlsx::addStyle(wb, sheet, styles$section, rows = row, cols = 1:2,
                     gridExpand = TRUE, stack = TRUE)
  row + 1L
}

.mx_subhead <- function(wb, sheet, row, text, styles) {
  openxlsx::writeData(wb, sheet, text, startRow = row, startCol = 1)
  openxlsx::addStyle(wb, sheet, styles$subhead, rows = row, cols = 1,
                     gridExpand = TRUE, stack = TRUE)
  row + 1L
}

.mx_kv <- function(wb, sheet, row, kv, styles) {
  n <- length(kv)
  if (n == 0) return(row)
  df <- data.frame(field = names(kv),
                   value = unname(as.character(kv)),
                   stringsAsFactors = FALSE)
  openxlsx::writeData(wb, sheet, df, startRow = row, startCol = 1,
                      colNames = FALSE)
  openxlsx::addStyle(wb, sheet, styles$label,
                     rows = row:(row + n - 1L), cols = 1,
                     gridExpand = TRUE, stack = TRUE)
  openxlsx::addStyle(wb, sheet, styles$body,
                     rows = row:(row + n - 1L), cols = 2,
                     gridExpand = TRUE, stack = TRUE)
  row + n + 1L
}

.mx_table <- function(wb, sheet, row, df, styles) {
  n  <- nrow(df)
  nc <- ncol(df)
  openxlsx::writeData(wb, sheet, df, startRow = row, startCol = 1,
                      colNames = TRUE)
  openxlsx::addStyle(wb, sheet, styles$header,
                     rows = row, cols = seq_len(nc),
                     gridExpand = TRUE, stack = TRUE)
  if (n > 0) {
    openxlsx::addStyle(wb, sheet, styles$body,
                       rows = (row + 1L):(row + n), cols = seq_len(nc),
                       gridExpand = TRUE, stack = TRUE)
  }
  row + n + 2L
}

.mx_text <- function(wb, sheet, row, lines, styles) {
  for (line in lines) {
    openxlsx::writeData(wb, sheet, line, startRow = row, startCol = 1)
    openxlsx::mergeCells(wb, sheet, rows = row, cols = 1:2)
    openxlsx::addStyle(wb, sheet, styles$body, rows = row, cols = 1:2,
                       gridExpand = TRUE, stack = TRUE)
    row <- row + 1L
  }
  row + 1L
}
