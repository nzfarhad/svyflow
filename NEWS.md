# svyflow 0.4.0

## New features

* `write_methods_xlsx()` — a standalone companion to `write_xlsx()` that
  writes a single-sheet `.xlsx` documenting how an analysis was produced.
  Useful for methodology annexes and for shipping a reproducibility
  trail alongside the results workbook. Does not touch `analyze_survey()`
  or `write_xlsx()`.
* Always-on sections cover **session info** (timestamp, user, R + key
  package versions), **data** dimensions, **survey design** (weighting
  flag, strata / cluster / FPC), **sample sizes** (n, strata count,
  PSU count, design df from `survey::degf()`), **weights summary**
  (sum, min/median/mean/max, CV), **CI options** (level, df,
  `prop_method`, quantile knobs), **result format**, and a static
  **notes** block with denominator / skip-logic caveats.
* Conditional sections:
  - **Plan** — included only when `plan =` is supplied. Counts
    indicators, disaggregation vars, presence of `repeat_for` / `group`,
    and breakdown tables by `kobo_type` and `aggregation_method`.
  - **Design effect (DEFF)** — included only when `results =` carries a
    `DEFF` column. Summary stats and the top five highest DEFFs.
  - **Project** — included only when `cover_notes =` is supplied; a
    free-text character vector for project name, funder, contact,
    footnote disclaimers, etc. Named entries render as
    `"<name>: <value>"`, unnamed render verbatim.
* When `results` is supplied, its stamped `result_format` / `digits`
  attributes are read in preference to the explicit args, so the
  Methods sheet documents what was actually computed rather than what
  the user typed.

# svyflow 0.3.0

## New features

* `analyze_survey()` and the proportion / mean / sum aggregators gain an
  optional `deff = FALSE` argument. When `TRUE`, the result carries two
  extra columns: `DEFF` (the design effect) and `n_eff` (the effective
  sample size, `Denominator / DEFF`). Quantile, min and max rows return
  `NA` for both because `survey::svyquantile()` does not produce a
  design effect and the extrema bypass the design entirely.
* DEFF is computed via `survey::svymean()` / `svytotal()`'s
  `deff = "replace"` variant, which compares variance to an SRS of size
  `n`. This matches the textbook DEFF (~1 for SRS) and is far more
  interpretable than the default `deff = TRUE` variant, which uses
  `sum(weights)` as the SRS reference and produces large values for any
  non-trivial weighting.
* The default output schema is unchanged when `deff = FALSE`; the two
  new columns are only present when explicitly requested.

# svyflow 0.2.0

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
* `write_xlsx()` workbook layout: the `Overall` sheet is now placed
  **first**, followed by the per-disaggregation sheets, followed by a new
  `Long` sheet at the end carrying the full long-form input as a single
  flat table. Toggle the long sheet via `long_sheet = FALSE` and rename
  via `long_label = "..."`.
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
