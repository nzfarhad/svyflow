# svyflow (development version)

## New features

* `write_xlsx()` exports `analyze_survey()` results to a styled Excel
  workbook: one sheet per disaggregation variable plus an `Overall` sheet,
  with per-question crosstab blocks (disaggregation levels as rows, an
  `Overall` summary row last), in plan order. Values are written exactly as
  they appear in the input object (no number formatting is imposed) — pass
  the default proportion output or run `format_results()` first.
* `xlsx_theme()` supplies the workbook styling (a clean publication palette
  by default) and is fully overridable (font, header fill, header/body font
  colours and sizes, borders, section styling).
* The analysis plan gains an optional `group` column. When present it is
  carried into the result as a `Group` column and used by `write_xlsx()` as
  a section separator between question blocks. Plans without it are
  unaffected (the default output schema is unchanged).

# svyflow 0.1.0

* First release: `make_design()`, `analyze_survey()`, `validate_plan()`,
  `expand_multiselect()` / `detect_ms_options()`, `format_results()`,
  the `summarize_*()` single-indicator wrappers, and `ci_opts()`.
