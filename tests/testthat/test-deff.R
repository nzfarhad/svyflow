# Tests for the optional design-effect (DEFF) + effective sample size
# (n_eff) output, opt-in via deff = TRUE.

test_that("deff = FALSE preserves the default schema", {
  df <- make_test_data(n = 200)
  res <- analyze_survey(make_design(df, weights = "weight"), make_test_plan())
  expect_false("DEFF"  %in% names(res))
  expect_false("n_eff" %in% names(res))
})

test_that("deff = TRUE adds DEFF and n_eff columns", {
  df  <- make_test_data(n = 300)
  res <- analyze_survey(make_design(df, weights = "weight"),
                        make_test_plan(), deff = TRUE)
  expect_true("DEFF"  %in% names(res))
  expect_true("n_eff" %in% names(res))
  expect_type(res$DEFF,  "double")
  expect_type(res$n_eff, "double")
})

test_that("DEFF is populated for select_one / select_multiple / mean / sum", {
  df  <- make_test_data(n = 400)
  res <- analyze_survey(make_design(df, weights = "weight"),
                        make_test_plan(), deff = TRUE)

  pop_methods <- c("perc", "mean", "sum")
  pop_rows    <- res[res$Aggregation_method %in% pop_methods &
                     is.na(res$repeat_for), , drop = FALSE]
  # At least some DEFFs are finite and positive.
  expect_true(any(is.finite(pop_rows$DEFF) & pop_rows$DEFF > 0))
})

test_that("quantile / min / max rows get DEFF = NA", {
  df  <- make_test_data(n = 400)
  res <- analyze_survey(make_design(df, weights = "weight"),
                        make_test_plan(), deff = TRUE)

  na_methods <- c("median", "1st_Qu", "3rd_Qu",
                  "min_unweighted", "max_unweighted")
  na_rows    <- res[res$Aggregation_method %in% na_methods, , drop = FALSE]
  expect_true(nrow(na_rows) > 0)
  expect_true(all(is.na(na_rows$DEFF)))
  expect_true(all(is.na(na_rows$n_eff)))
})

test_that("clustered design yields DEFF > 1 for proportions and means", {
  # Cluster-by-province induces intra-cluster correlation, so DEFF should
  # exceed 1 for at least some indicators on a weighted clustered design.
  df  <- make_test_data(n = 500)
  des <- make_design(df, weights = "weight", ids = "province")
  res <- analyze_survey(des, make_test_plan(), deff = TRUE)

  pop <- res[res$Aggregation_method %in% c("perc", "mean") &
             is.finite(res$DEFF), , drop = FALSE]
  expect_true(nrow(pop) > 0)
  expect_true(all(pop$DEFF > 0))
  expect_true(any(pop$DEFF > 1))
})

test_that("SRS design gives DEFF approximately 1 (replace variant)", {
  # svyflow passes deff = "replace" to survey, which uses n (not
  # sum(weights)) as the SRS reference. For an unweighted SRS this gives
  # values very close to 1 (small n/(n-1) finite-sample adjustment).
  df  <- make_test_data(n = 500)
  res <- analyze_survey(make_design(df), make_test_plan(), deff = TRUE)

  srs <- res[res$Aggregation_method %in% c("perc", "mean") &
             is.finite(res$DEFF), , drop = FALSE]
  expect_true(nrow(srs) > 0)
  expect_true(all(srs$DEFF > 0.9 & srs$DEFF < 1.1))
})

test_that("n_eff == Denominator / DEFF where DEFF is finite", {
  df  <- make_test_data(n = 300)
  res <- analyze_survey(make_design(df, weights = "weight"),
                        make_test_plan(), deff = TRUE)

  ok <- is.finite(res$DEFF) & res$DEFF > 0 &
        is.finite(res$Denominator) & res$Denominator > 0
  expect_true(any(ok))
  expect_equal(res$n_eff[ok], res$Denominator[ok] / res$DEFF[ok])
})

test_that("deff propagates through repeat_for", {
  df  <- make_test_data(n = 400)
  res <- analyze_survey(make_design(df, weights = "weight"),
                        make_test_plan(), deff = TRUE)
  rf_rows <- res[!is.na(res$repeat_for), , drop = FALSE]
  expect_true(nrow(rf_rows) > 0)
  expect_true("DEFF"  %in% names(rf_rows))
  expect_true("n_eff" %in% names(rf_rows))
})

test_that("aggregators expose DEFF column when called directly", {
  df     <- make_test_data(n = 300)
  design <- make_design(df, weights = "weight")

  out_so <- svyflow:::single_select_svy(design, "gender", "all", "all",
                                        deff = TRUE)
  expect_true("DEFF" %in% names(out_so))

  ms_opts <- svyflow:::detect_ms_options(df, "hh_needs")
  out_ms  <- svyflow:::multi_select_svy(design, "hh_needs", "all", "all",
                                        ms_opts, deff = TRUE)
  expect_true("DEFF" %in% names(out_ms))

  out_mn <- svyflow:::stat_mean_svy(design, "hh_size", "all", "all",
                                    deff = TRUE)
  expect_true("DEFF" %in% names(out_mn))

  out_sm <- svyflow:::stat_sum_svy(design, "income", "all", "all",
                                   deff = TRUE)
  expect_true("DEFF" %in% names(out_sm))

  out_min <- svyflow:::stat_min_unweighted(design, "income", "all", "all",
                                           deff = TRUE)
  expect_true("DEFF" %in% names(out_min))
  expect_true(is.na(out_min$DEFF))
})

test_that("deff rejects non-logical input", {
  df <- make_test_data(n = 50)
  expect_error(
    analyze_survey(make_design(df), make_test_plan(), deff = "yes"),
    "single TRUE or FALSE"
  )
  expect_error(
    analyze_survey(make_design(df), make_test_plan(), deff = NA),
    "single TRUE or FALSE"
  )
})
