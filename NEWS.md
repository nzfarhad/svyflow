# svyflow 0.5.0

## New features

* `compare_groups()` — a single public function for survey-design-aware
  significance testing. Selects and runs the right test for each indicator
  comparing it across the levels of a grouping variable, and returns a tidy
  one-row-per-indicator `svyflow_summary` table. With `test = "auto"` the
  right survey-design-aware test is selected from the indicator type, the
  number of groups, and the `paired` / `parametric` flags:
  - **Numeric, 2 groups** — independent t-test (`ttest`) or Mann-Whitney U
    (`wilcoxon`); paired t-test (`paired_ttest`) or Wilcoxon signed-rank
    when `paired = TRUE` (needs `pair_by`).
  - **Numeric, 3+ groups** — design-based ANOVA (`anova`) or Kruskal-Wallis
    (`kruskal`).
  - **Categorical** — Rao-Scott F chi-square (`chisq`), automatically
    switching to Fisher's exact (`fisher`) when any expected cell count is
    below 5 (`small_sample`).
* Any branch can be forced via `test =`; a two-proportion z-test (`prop_z`)
  is available as an explicit opt-in for binary x binary comparisons.
* Output carries the test statistic, degrees of freedom, point estimate and
  CI where defined, an effect size (Cohen's d, rank-biserial, eta-squared,
  epsilon-squared, Cramer's V, odds ratio, or Cohen's h) with its type, the
  p-value, significance stars, and the sample size used.
* All tests except Fisher's exact respect the survey design (weights,
  strata, clusters). Fisher's exact has no survey-aware form, so it runs on
  the unweighted table and warns when selected.
* Effect sizes are design-consistent where well-defined: Cramer's V is
  computed from the weighted contingency proportions, and the Wilcoxon
  rank-biserial uses weighted quantities. Cohen's d (design-based variances,
  pooled by unweighted df) and the ANOVA eta-squared are documented
  approximations -- see the "Effect sizes" section of `?compare_groups`.
* Paired tests collapse the design to weighted independent pair differences;
  a warning fires when the original design carried strata or clusters, since
  that structure cannot be preserved per pair.

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
