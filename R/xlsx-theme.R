#' Excel styling theme for write_xlsx()
#'
#' Bundles the visual options used by [write_xlsx()] when styling the
#' workbook. The defaults are a clean publication palette: a maroon header
#' fill with white bold Calibri text, plain white body rows (no zebra
#' striping), thin slate borders, and a mist-filled maroon section header.
#' Override any field to retheme.
#'
#' @param font_name Font family for all cells. Default `"Calibri"`.
#' @param header_fill Hex fill for table header rows. Default maroon
#'   `"#7D0E00"`.
#' @param header_font_color Hex font colour for header text. Default white
#'   `"#FFFFFF"`.
#' @param header_font_size Header font size (pt). Default `11`.
#' @param header_bold Bold header text? Default `TRUE`.
#' @param body_font_color Hex font colour for body cells. Default charcoal
#'   `"#333333"`.
#' @param body_font_size Body font size (pt). Default `10`.
#' @param border_color Hex colour for thin cell borders. Default slate
#'   `"#666666"`.
#' @param section_fill Hex fill for group/section separator rows. Default
#'   mist `"#EEECE1"`.
#' @param section_font_color Hex font colour for section text. Default
#'   maroon `"#7D0E00"`.
#' @param section_font_size Section font size (pt). Default `12`.
#' @param section_bold Bold section text? Default `TRUE`.
#' @param label_bold Bold the row-label column (levels / "Overall")?
#'   Default `FALSE`.
#'
#' @return A list of class `svyflow_xlsx_theme`.
#'
#' @examples
#' # Default theme
#' xlsx_theme()
#'
#' # Retheme: navy headers, Arial
#' xlsx_theme(font_name = "Arial", header_fill = "#1E3A5F")
#'
#' @seealso [write_xlsx()]
#' @export
xlsx_theme <- function(font_name          = "Calibri",
                       header_fill        = "#7D0E00",
                       header_font_color  = "#FFFFFF",
                       header_font_size   = 11,
                       header_bold        = TRUE,
                       body_font_color    = "#333333",
                       body_font_size     = 10,
                       border_color       = "#666666",
                       section_fill       = "#EEECE1",
                       section_font_color = "#7D0E00",
                       section_font_size  = 12,
                       section_bold       = TRUE,
                       label_bold         = FALSE) {
  chk_hex <- function(x, nm) {
    if (!is.character(x) || length(x) != 1 || !grepl("^#?[0-9A-Fa-f]{6}$", x)) {
      stop("`", nm, "` must be a single 6-digit hex colour (e.g. \"#7D0E00\").")
    }
  }
  chk_num <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0) {
      stop("`", nm, "` must be a positive numeric scalar.")
    }
  }
  if (!is.character(font_name) || length(font_name) != 1) {
    stop("`font_name` must be a single string.")
  }
  for (nm in c("header_fill", "header_font_color", "body_font_color",
               "border_color", "section_fill", "section_font_color")) {
    chk_hex(get(nm), nm)
  }
  for (nm in c("header_font_size", "body_font_size", "section_font_size")) {
    chk_num(get(nm), nm)
  }

  structure(
    list(
      font_name          = font_name,
      header_fill        = header_fill,
      header_font_color  = header_font_color,
      header_font_size   = header_font_size,
      header_bold        = isTRUE(header_bold),
      body_font_color    = body_font_color,
      body_font_size     = body_font_size,
      border_color       = border_color,
      section_fill       = section_fill,
      section_font_color = section_font_color,
      section_font_size  = section_font_size,
      section_bold       = isTRUE(section_bold),
      label_bold         = isTRUE(label_bold)
    ),
    class = "svyflow_xlsx_theme"
  )
}

# Coerce/validate a user-supplied theme into a svyflow_xlsx_theme.
.as_xlsx_theme <- function(theme) {
  if (is.null(theme)) return(xlsx_theme())
  if (inherits(theme, "svyflow_xlsx_theme")) return(theme)
  if (is.list(theme)) return(do.call(xlsx_theme, theme))
  stop("`theme` must be created with xlsx_theme() (or a list of its arguments).")
}
