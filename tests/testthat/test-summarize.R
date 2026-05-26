# Tests for the single-indicator summarize_* wrappers.

# ---- categorical: select_one -----------------------------------------------

test_that("summarize_select_one: no disaggregation, default (proportion)", {
  df <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")

  out <- suppressWarnings(summarize_select_one(des, "edu_lvl",
                                               variable_label = "Education level"))
  expect_s3_class(out, "svyflow_summary")
  expect_true("Education level" %in% names(out))
  expect_true("Proportion"      %in% names(out))
  expect_true(all(c("SE", "CI_low", "CI_high", "Count", "Denominator")
                  %in% names(out)))
  # Var1-style column gone.
  expect_false(any(c("Var1", "Response", "Question") %in% names(out)))
  # Proportions in [0, 1] and sum to ~1.
  expect_true(all(out$Proportion >= 0 & out$Proportion <= 1, na.rm = TRUE))
  expect_equal(sum(out$Proportion, na.rm = TRUE), 1, tolerance = 0.005)
})

test_that("summarize_select_one: result_format = 'percent' renames Result to Percentage", {
  df <- make_test_data(n = 200)
  des <- make_design(df)
  out <- suppressWarnings(summarize_select_one(des, "gender",
                                               result_format = "percent"))
  expect_true("Percentage" %in% names(out))
  expect_false("Proportion" %in% names(out))
  expect_equal(sum(out$Percentage, na.rm = TRUE), 100, tolerance = 0.5)
})

test_that("summarize_select_one: variable_label falls back to variable", {
  df <- make_test_data(n = 100)
  des <- make_design(df)
  out <- suppressWarnings(summarize_select_one(des, "gender"))
  expect_true("gender" %in% names(out))
})

test_that("summarize_select_one: disaggregation produces long output with labelled column", {
  df <- make_test_data(n = 300)
  des <- make_design(df)
  out <- suppressWarnings(summarize_select_one(des, "edu_lvl",
                                               disaggregation = "gender",
                                               variable_label = "Education",
                                               disaggregation_label = "Sex"))
  expect_true("Education" %in% names(out))
  expect_true("Sex"       %in% names(out))
  # One row per (response, sex) combo.
  expect_equal(nrow(out),
               length(unique(df$edu_lvl[!is.na(df$edu_lvl)])) *
                 length(unique(df$gender)))
})

test_that("summarize_select_one: crosstab pivots to wide; SE/CI/Count dropped", {
  df <- make_test_data(n = 300)
  des <- make_design(df)
  out <- suppressWarnings(summarize_select_one(des, "edu_lvl",
                                               disaggregation = "gender",
                                               variable_label = "Education",
                                               crosstab = TRUE))
  expect_s3_class(out, "svyflow_summary")
  expect_true("Education" %in% names(out))
  expect_true(all(unique(df$gender) %in% names(out)))
  expect_false(any(c("SE", "CI_low", "CI_high", "Count", "Denominator")
                   %in% names(out)))
  # One row per unique edu_lvl response.
  expect_equal(nrow(out), length(unique(df$edu_lvl[!is.na(df$edu_lvl)])))
})

test_that("summarize_select_one: crosstab + with_ci produces 'est (low-high)' strings", {
  df <- make_test_data(n = 300)
  des <- make_design(df)
  out <- suppressWarnings(summarize_select_one(des, "edu_lvl",
                                               disaggregation = "gender",
                                               crosstab = TRUE,
                                               with_ci  = TRUE,
                                               digits = 2))
  # Find cells in any of the level columns.
  lvl_cols <- setdiff(names(out), "edu_lvl")
  for (cc in lvl_cols) {
    vals <- out[[cc]][!is.na(out[[cc]])]
    expect_true(all(grepl("\\(.*–.*\\)$", vals)),
                info = paste("column", cc, "missing CI parens"))
  }
})

# ---- categorical: select_multiple ------------------------------------------

test_that("summarize_select_multiple: returns one row per option with labelled header", {
  df <- make_test_data(n = 200)
  des <- make_design(df)
  out <- suppressWarnings(summarize_select_multiple(des, "hh_needs",
                                                    variable_label = "Household needs"))
  expect_true("Household needs" %in% names(out))
  expect_true("Proportion"      %in% names(out))
  expect_equal(nrow(out), 5L)  # cash / food / shelter / nfis / health
})

# ---- numeric ----------------------------------------------------------------

test_that("summarize_mean: default full table with Mean column", {
  df <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")
  out <- suppressWarnings(summarize_mean(des, "hh_size",
                                         variable_label = "Household size"))
  expect_s3_class(out, "svyflow_summary")
  expect_named(out, c("Indicator", "Mean", "SE", "CI_low", "CI_high", "Count"))
  expect_equal(out$Indicator, "Household size")
  expect_true(is.finite(out$Mean))
  expect_true(!is.na(out$SE))
})

test_that("summarize_mean: result_only = TRUE drops SE/CI/Count", {
  df <- make_test_data(n = 200)
  des <- make_design(df)
  out <- suppressWarnings(summarize_mean(des, "hh_size",
                                         variable_label = "Household size",
                                         result_only = TRUE))
  expect_named(out, c("Indicator", "Mean"))
})

test_that("summarize_mean: disaggregation adds the labelled column", {
  df <- make_test_data(n = 200)
  des <- make_design(df)
  out <- suppressWarnings(summarize_mean(des, "hh_size",
                                         disaggregation = "gender",
                                         variable_label = "Household size",
                                         disaggregation_label = "Sex"))
  expect_true("Sex"  %in% names(out))
  expect_true("Mean" %in% names(out))
  expect_equal(nrow(out), length(unique(df$gender)))
})

test_that("summarize_sum / median / min / max all return their named columns", {
  df <- make_test_data(n = 200)
  des <- make_design(df)
  expect_true("Sum"    %in% names(suppressWarnings(summarize_sum   (des, "income"))))
  expect_true("Median" %in% names(suppressWarnings(summarize_median(des, "hh_size"))))
  expect_true("Min"    %in% names(suppressWarnings(summarize_min   (des, "income"))))
  expect_true("Max"    %in% names(suppressWarnings(summarize_max   (des, "income"))))
})

test_that("summarize_quantile: q = 0.25 -> Q25, q = 0.9 -> Q90 (arbitrary)", {
  df <- make_test_data(n = 200)
  des <- make_design(df)
  q25 <- suppressWarnings(summarize_quantile(des, "income", q = 0.25))
  q90 <- suppressWarnings(summarize_quantile(des, "income", q = 0.9))
  expect_true("Q25" %in% names(q25))
  expect_true("Q90" %in% names(q90))
  expect_true(is.finite(q25$Q25))
  expect_true(is.finite(q90$Q90))
  # Q90 of income should be >= Q25 of income.
  expect_gte(q90$Q90, q25$Q25)
})

test_that("summarize_quantile: rejects q outside [0, 1]", {
  df <- make_test_data(n = 50)
  des <- make_design(df)
  expect_error(summarize_quantile(des, "income", q = 1.5), "must be a single numeric")
  expect_error(summarize_quantile(des, "income", q = -0.1), "must be a single numeric")
})

# ---- parity with analyze_survey --------------------------------------------

test_that("summarize_mean parity with analyze_survey for the same indicator", {
  df <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")
  ap  <- tibble::tibble(
    variable = "hh_size", kobo_type = "integer",
    aggregation_method = "mean", disaggregation = "all"
  )
  long <- suppressWarnings(analyze_survey(des, ap))
  wrap <- suppressWarnings(summarize_mean(des, "hh_size",
                                          variable_label = "Household size"))
  expect_equal(wrap$Mean, long$Result[long$Aggregation_method == "mean"])
})

test_that("summarize_select_one parity with analyze_survey for the same indicator", {
  df <- make_test_data(n = 300)
  des <- make_design(df)
  ap  <- tibble::tibble(
    variable = "edu_lvl", kobo_type = "select_one",
    aggregation_method = NA, disaggregation = "all"
  )
  long <- suppressWarnings(analyze_survey(des, ap))
  wrap <- suppressWarnings(summarize_select_one(des, "edu_lvl"))
  # Rows ordered the same way; compare numerics.
  expect_equal(sort(wrap$Proportion), sort(long$Result))
})
