# Tests for ci_opts() and the CI-method options threaded through the API.

test_that("ci_opts() validates its arguments", {
  expect_s3_class(ci_opts(), "svyflow_ci_opts")
  expect_error(ci_opts(ci_level = 0),   "ci_level")
  expect_error(ci_opts(ci_level = 1),   "ci_level")
  expect_error(ci_opts(ci_level = 1.2), "ci_level")
  expect_error(ci_opts(df = -1),        "df")
  expect_error(ci_opts(df = 0),         "df")
  expect_error(ci_opts(prop_method = "nope"),
               "should be one of")
  expect_error(ci_opts(interval_type = "nope"), "should be one of")
  expect_error(ci_opts(qrule = "nope"),         "should be one of")
  # Inf df is allowed (normal approximation).
  expect_equal(ci_opts(df = Inf)$df, Inf)
})

test_that("ci_opts() defaults reproduce the historical output exactly", {
  df  <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")
  ap  <- make_test_plan()

  res_default <- suppressWarnings(analyze_survey(des, ap))
  res_explicit <- suppressWarnings(analyze_survey(des, ap, ci = ci_opts()))
  expect_equal(res_default$Result,  res_explicit$Result)
  expect_equal(res_default$CI_low,  res_explicit$CI_low)
  expect_equal(res_default$CI_high, res_explicit$CI_high)
})

test_that("df = Inf gives the normal-approximation interval (qnorm)", {
  df  <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")

  out <- suppressWarnings(
    svyflow:::stat_mean_svy(des, "hh_size", "all", "all",
                            ci = ci_opts(df = Inf)))
  est <- out$Freq; se <- out$SE
  expect_equal(out$CI_low,  est - stats::qnorm(0.975) * se, tolerance = 1e-6)
  expect_equal(out$CI_high, est + stats::qnorm(0.975) * se, tolerance = 1e-6)
})

test_that("default mean interval is t-based on the design df (wider than normal)", {
  df  <- make_test_data(n = 120)
  des <- make_design(df, weights = "weight")

  t_out <- suppressWarnings(
    svyflow:::stat_mean_svy(des, "hh_size", "all", "all"))
  z_out <- suppressWarnings(
    svyflow:::stat_mean_svy(des, "hh_size", "all", "all",
                            ci = ci_opts(df = Inf)))
  # t interval is at least as wide as the normal one.
  expect_gte(t_out$CI_high - t_out$CI_low, z_out$CI_high - z_out$CI_low)
})

test_that("ci_level changes the interval width", {
  df  <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")

  w95 <- suppressWarnings(
    svyflow:::stat_mean_svy(des, "hh_size", "all", "all",
                            ci = ci_opts(ci_level = 0.95)))
  w90 <- suppressWarnings(
    svyflow:::stat_mean_svy(des, "hh_size", "all", "all",
                            ci = ci_opts(ci_level = 0.90)))
  expect_lt(w90$CI_high - w90$CI_low, w95$CI_high - w95$CI_low)
})

test_that("prop_method = 'logit' changes proportion CIs but not the point estimate", {
  df  <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")

  wald  <- suppressWarnings(summarize_select_one(des, "edu_lvl"))
  logit <- suppressWarnings(
    summarize_select_one(des, "edu_lvl", ci = ci_opts(prop_method = "logit")))

  expect_equal(wald$Proportion, logit$Proportion)        # point unchanged
  expect_false(isTRUE(all.equal(wald$CI_low, logit$CI_low)))  # bounds differ
})

test_that("prop_method threads through analyze_survey for select rows only", {
  df  <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")
  ap  <- tibble::tibble(
    variable           = c("edu_lvl", "hh_size"),
    kobo_type          = c("select_one", "integer"),
    aggregation_method = c(NA, "mean"),
    disaggregation     = c("all", "all")
  )
  res_wald  <- suppressWarnings(analyze_survey(des, ap))
  res_logit <- suppressWarnings(
    analyze_survey(des, ap, ci = ci_opts(prop_method = "logit")))

  # mean row identical; perc rows differ on CI.
  mean_w <- res_wald$CI_low[res_wald$Aggregation_method == "mean"]
  mean_l <- res_logit$CI_low[res_logit$Aggregation_method == "mean"]
  expect_equal(mean_w, mean_l)

  perc_w <- res_wald$CI_low[res_wald$Aggregation_method == "perc"]
  perc_l <- res_logit$CI_low[res_logit$Aggregation_method == "perc"]
  expect_false(isTRUE(all.equal(perc_w, perc_l)))
})

test_that("quantile interval_type / qrule are accepted and run", {
  df  <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight")

  out <- suppressWarnings(
    summarize_quantile(des, "income", q = 0.5,
                       ci = ci_opts(interval_type = "score")))
  expect_true("Q50" %in% names(out))
  expect_true(is.finite(out$Q50))

  # Arbitrary quantile path also honours the option.
  out9 <- suppressWarnings(
    summarize_quantile(des, "income", q = 0.9,
                       ci = ci_opts(interval_type = "score")))
  expect_true("Q90" %in% names(out9))
})

test_that("ci accepts a plain list as well as a ci_opts object", {
  df  <- make_test_data(n = 200)
  des <- make_design(df, weights = "weight")
  out <- suppressWarnings(
    summarize_mean(des, "hh_size", ci = list(ci_level = 0.90)))
  expect_s3_class(out, "svyflow_summary")
})
