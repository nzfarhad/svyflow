# svyflow

<!-- badges: start -->
<!-- badges: end -->

`svyflow` produces **design-correct descriptive statistics** (proportions,
means, totals, quantiles, min/max) for survey data, driven either by an
**analysis-plan data frame** (batch mode) or by **single-indicator wrapper
functions** (ad hoc mode). Estimates use the
[`srvyr`](https://cran.r-project.org/package=srvyr) /
[`survey`](https://cran.r-project.org/package=survey) engine, so weights,
strata, clusters and finite-population corrections are respected; standard
errors and confidence intervals are reported alongside every point estimate.

It is built for **monitoring & evaluation / humanitarian survey** workflows:
Kobo / SurveyCTO multi-select handling, single and double disaggregation,
human-readable labels, and publication-ready output.

## Installation

```r
# install.packages("remotes")
remotes::install_github("nzfarhad/svyflow")
```

## The two ways to use it

| Mode | Function(s) | When |
|------|-------------|------|
| **Batch** | `analyze_survey()` + an analysis plan | Many indicators at once, reproducible reporting |
| **Ad hoc** | `summarize_select_one()`, `summarize_mean()`, … | One indicator, publication-ready table |

Both share the same engine and the same `ci_opts()` / `result_format`
controls.

## Quick start — batch mode

```r
library(svyflow)

# 1. Build a survey design (weights = NULL gives simple random sampling)
des <- make_design(my_data, weights = "weight", strata = "province")

# 2. Describe what to compute, one row per (indicator x disaggregation)
plan <- tibble::tribble(
  ~variable,  ~kobo_type,        ~aggregation_method, ~disaggregation,
  "gender",   "select_one",      NA,                  "all",
  "edu_lvl",  "select_one",      NA,                  "gender",
  "hh_needs", "select_multiple", NA,                  "all",
  "hh_size",  "integer",         "mean",              "gender",
  "income",   "integer",         "median",            "all"
)

# 3. Run it
results <- analyze_survey(des, plan)
results
```

`analyze_survey()` returns a long tibble (class `svyflow_results`) with a
stable schema: `Disaggregation`, `Disaggregation_level`, `Question`,
`Response`, `Aggregation_method`, `Result`, `SE`, `CI_low`, `CI_high`,
`Count`, `Denominator`, `repeat_for`.

## Quick start — single indicators

```r
# A select_one, disaggregated, as a publication-ready crosstab
summarize_select_one(des, "edu_lvl",
                     disaggregation = "gender",
                     variable_label = "Education level",
                     disaggregation_label = "Sex",
                     crosstab = TRUE)
#> | Education level | male | female |
#> | primary         | 0.25 | 0.27   |
#> | secondary       | 0.30 | 0.32   |

# A weighted mean with the value column named after the statistic
summarize_mean(des, "hh_size", variable_label = "Household size")
#> | Indicator      | Mean | SE  | CI_low | CI_high | Count |
#> | Household size | 6.1  | 0.1 | 5.9    | 6.3     | 485   |
```

There is one wrapper per aggregator: `summarize_select_one()`,
`summarize_select_multiple()`, `summarize_mean()`, `summarize_sum()`,
`summarize_median()`, `summarize_quantile()`, `summarize_min()`,
`summarize_max()`.

## Output format: proportion, percent, or formatted

Categorical results default to **proportions** (0–1). Switch with
`result_format`, and control rounding with `digits`:

```r
summarize_select_one(des, "gender", result_format = "percent")      # 53.3
summarize_select_one(des, "gender", result_format = "percent_fmt")  # "53.3%"
```

Because `analyze_survey()` can be slow for large plans, you can reformat an
**existing** result without re-running the analysis:

```r
res <- analyze_survey(des, plan)            # proportions (default)
format_results(res, to = "percent")          # 0–100 numeric
format_results(res, to = "percent_fmt", digits = 2)  # "53.27%"
```

`format_results()` is aggregation-method aware: only proportion rows are
rescaled and `%`-suffixed; means, sums and quantiles keep their raw values.

## Confidence-interval methods

Defaults reproduce a 95% t-interval on the design degrees of freedom with
plain Wald proportions. Override with `ci_opts()`:

```r
# 90% normal-approximation interval
analyze_survey(des, plan, ci = ci_opts(ci_level = 0.90, df = Inf))

# Logit proportion intervals (better for rare outcomes near 0% / 100%)
summarize_select_one(des, "rare_indicator", ci = ci_opts(prop_method = "logit"))
```

`ci_opts()` knobs: `ci_level`, `df` (`NULL` = design df, `Inf` = normal),
`prop_method` (proportions only), `interval_type` / `qrule` (quantiles only).

## Multi-select (Kobo / SurveyCTO)

If your data has sibling binary columns (`var/opt` or `var___opt`), they are
detected automatically. If you only have a delimited string column, expand it
first:

```r
df <- expand_multiselect(df, vars = "hh_needs", sep = "; ")
summarize_select_multiple(make_design(df), "hh_needs",
                          variable_label = "Household needs")
```

## Labels

Add optional `variable_label` / `disaggregation_label` columns to a plan (or
pass them to the wrappers) and the output substitutes friendly names. In batch
mode this is controlled by `use_labels = TRUE` (the default).

## Learn more

See the vignette for a full worked example:

```r
vignette("svyflow")
```

## License

GPL (>= 3)