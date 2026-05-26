# Aggregator-level tests. We exercise the internal aggregators via :::
# because they are not exported, but their contract (uniform return shape) is
# part of what we want to lock down.

test_that("aggregator return shape is uniform across types", {
  df <- make_test_data(n = 200)
  design <- make_design(df)

  expected_cols <- c(
    "Var1", "Freq", "SE", "CI_low", "CI_high",
    "aggregation_method", "variable", "count", "valid",
    "disaggregation", "disagg_level"
  )

  out_so <- svyflow:::single_select_svy(design, "gender", "all", "all")
  expect_named(out_so, expected_cols)

  ms_opts <- svyflow:::detect_ms_options(df, "hh_needs")
  out_ms <- svyflow:::multi_select_svy(design, "hh_needs", "all", "all", ms_opts)
  expect_named(out_ms, expected_cols)

  out_mean <- svyflow:::stat_mean_svy(design, "hh_size", "all", "all")
  expect_named(out_mean, expected_cols)

  out_sum <- svyflow:::stat_sum_svy(design, "income", "all", "all")
  expect_named(out_sum, expected_cols)

  out_med <- svyflow:::stat_quantile_svy(design, "hh_size", "all", "all",
                                         0.5, "median")
  expect_named(out_med, expected_cols)

  out_min <- svyflow:::stat_min_unweighted(design, "income", "all", "all")
  expect_named(out_min, expected_cols)
  expect_equal(out_min$aggregation_method, "min_unweighted")
  expect_true(is.na(out_min$SE))
})

test_that("unweighted survey_mean matches base mean()", {
  df <- make_test_data(n = 300)
  design <- make_design(df)
  out <- svyflow:::stat_mean_svy(design, "hh_size", "all", "all")
  expect_equal(out$Freq, mean(df$hh_size, na.rm = TRUE))
})

test_that("unweighted survey_total matches base sum()", {
  df <- make_test_data(n = 300)
  design <- make_design(df)
  out <- svyflow:::stat_sum_svy(design, "income", "all", "all")
  expect_equal(out$Freq, sum(df$income, na.rm = TRUE))
})

test_that("min_unweighted / max_unweighted match raw extrema", {
  df <- make_test_data(n = 300)
  design <- make_design(df)
  out_min <- svyflow:::stat_min_unweighted(design, "income", "all", "all")
  out_max <- svyflow:::stat_max_unweighted(design, "income", "all", "all")
  expect_equal(out_min$Freq, min(df$income, na.rm = TRUE))
  expect_equal(out_max$Freq, max(df$income, na.rm = TRUE))
  expect_true(is.na(out_min$SE))
  expect_true(is.na(out_max$SE))
})

test_that("all-NA short-circuit returns empty row with NA stats", {
  df <- data.frame(x = rep(NA_real_, 10))
  design <- make_design(df)
  out <- svyflow:::stat_mean_svy(design, "x", "all", "all")
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$Freq))
  expect_equal(out$valid, 0L)
})

test_that("multi_select_svy returns one row per option with shared denominator", {
  df <- make_test_data(n = 300)
  design <- make_design(df)
  ms_opts <- svyflow:::detect_ms_options(df, "hh_needs")
  out <- svyflow:::multi_select_svy(design, "hh_needs", "all", "all", ms_opts)
  expect_equal(nrow(out), 5L)
  expect_length(unique(out$valid), 1L)
  expect_equal(out$valid[1], sum(!is.na(df$hh_needs)))
})
