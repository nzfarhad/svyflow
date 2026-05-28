# svyflow (development version)

## New features

* `write_xlsx()` exports `analyze_survey()` results to a styled Excel
  workbook: one sheet per disaggregation variable plus an `Overall` sheet,
  with per-question crosstab blocks (disaggregation levels as rows, an
  `Overall` summary row last), in plan order. Values are written exactly as
  they appear in the input object (no number formatting is imposed) — pass
  the default proportion output or run `format_results()` first.
* `write_xlsx()` gains two display options:
  - `with_ci = TRUE` composes each value cell as
    `"<estimate> (<CI_low> - <CI_high>)"`, respecting whatever scale /
    rounding is already in the input (set it upstream with
    `format_results()`).
  - `with_counts` controls how unweighted counts appear, with four modes:
    `"none"` (default), `"row_label"` (append ` (n=<Denominator>)` to each
    row label), `"inline"` (row-label suffix plus ` (n=<Count>)` on every
    value cell; with `with_ci` the count nests inside the CI parens as
    `"<est> (<lo> - <hi>; n=N)"`), and `"parallel"` (row-label suffix plus a
    sibling `"(n)"` column after every value column carrying the per-cell
    Count). The two cell-level modes also carry the level totals on the
    row labels automatically.
* `write_xlsx()` gains a `col_width` parameter (default `21`,
  ~196 px in Excel) controlling the fixed width of every value column;
  long headers wrap within that width. The row-label column sizes itself
  to its content, capped at 40.
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
