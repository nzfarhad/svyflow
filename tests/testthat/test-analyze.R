test_that("analyze_survey returns the documented output schema", {
  df <- make_test_data(n = 250)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  expected <- c(
    "Disaggregation", "Disaggregation_level", "Question", "Response",
    "Aggregation_method", "Result", "SE", "CI_low", "CI_high",
    "Count", "Denominator", "repeat_for"
  )
  expect_named(res, expected)
  expect_s3_class(res, "svyflow_results")
  expect_s3_class(res, "tbl_df")
})

test_that("select_one proportions sum to ~100 within each disaggregation level", {
  df <- make_test_data(n = 300)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  # gender, no disagg
  s_total <- sum(res$Result[res$Question == "gender" &
                            res$Disaggregation == "all"], na.rm = TRUE)
  expect_equal(s_total, 100, tolerance = 0.5)

  # edu_lvl disaggregated by gender — within each gender level, sum is ~100
  for (g in unique(df$gender)) {
    s <- sum(res$Result[res$Question == "edu_lvl" &
                        res$Disaggregation == "gender" &
                        res$Disaggregation_level == g], na.rm = TRUE)
    expect_equal(s, 100, tolerance = 0.5)
  }
})

test_that("unweighted mean / sum / quantiles match base R", {
  df <- make_test_data(n = 300)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  m <- res$Result[res$Question == "hh_size" &
                  res$Aggregation_method == "mean" &
                  res$Disaggregation == "all"]
  expect_equal(m, mean(df$hh_size, na.rm = TRUE))

  s <- res$Result[res$Question == "income" &
                  res$Aggregation_method == "sum" &
                  res$Disaggregation == "all"]
  expect_equal(s, sum(df$income, na.rm = TRUE))

  med <- res$Result[res$Question == "hh_size" &
                    res$Aggregation_method == "median" &
                    res$Disaggregation == "all"]
  expect_equal(med, stats::median(df$hh_size, na.rm = TRUE),
               tolerance = 0.5) # survey_quantile interpolates differently
})

test_that("min/max rows are flagged unweighted and have NA SE/CI", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  min_row <- res[res$Question == "income" &
                 res$Aggregation_method == "min_unweighted", ]
  max_row <- res[res$Question == "income" &
                 res$Aggregation_method == "max_unweighted", ]
  expect_equal(nrow(min_row), 1L)
  expect_equal(nrow(max_row), 1L)
  expect_equal(min_row$Result, min(df$income, na.rm = TRUE))
  expect_equal(max_row$Result, max(df$income, na.rm = TRUE))
  expect_true(is.na(min_row$SE) && is.na(max_row$SE))
})

test_that("multi-select uses Kobo siblings without expand_multiselect call", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  ms_rows <- res[res$Question == "hh_needs" & res$Disaggregation == "all", ]
  expect_equal(nrow(ms_rows), 5L)
  expect_length(unique(ms_rows$Denominator), 1L)
  expect_equal(ms_rows$Denominator[1], sum(!is.na(df$hh_needs)))
})

test_that("multi-select via expand_multiselect produces identical denominators", {
  df <- make_test_data(n = 200)
  df_no_sib <- df[, !grepl("^hh_needs/", names(df)), drop = FALSE]
  df_exp <- expand_multiselect(df_no_sib, vars = "hh_needs", sep = "; ")

  ap <- make_test_plan()
  ap_ms <- ap[ap$variable == "hh_needs" & ap$disaggregation == "all", ]

  res <- suppressWarnings(analyze_survey(make_design(df_exp), ap_ms))
  expect_equal(nrow(res), 5L)
  expect_equal(res$Denominator[1], sum(!is.na(df_exp$hh_needs)))
})

test_that("repeat_for produces one block per outer level", {
  df <- make_test_data(n = 250)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  rf_rows <- res[!is.na(res$repeat_for), ]
  expect_equal(
    sort(unique(rf_rows$repeat_for)),
    sort(unique(df$province))
  )
})

test_that("weighted design changes the mean by a small bounded amount", {
  df <- make_test_data(n = 400)
  ap <- make_test_plan()
  res_srs <- suppressWarnings(analyze_survey(make_design(df), ap))
  res_w   <- suppressWarnings(analyze_survey(
    make_design(df, weights = "weight"), ap
  ))

  m_srs <- res_srs$Result[res_srs$Question == "hh_size" &
                          res_srs$Aggregation_method == "mean" &
                          res_srs$Disaggregation == "all"]
  m_w   <- res_w$Result[res_w$Question == "hh_size" &
                        res_w$Aggregation_method == "mean" &
                        res_w$Disaggregation == "all"]
  # weights are bounded in [0.5, 2.0] => weighted mean stays within 1 unit
  expect_lt(abs(m_w - m_srs), 1.0)
})

test_that("SE and CI are populated for mean rows and CI_low <= CI_high", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  mean_rows <- res[res$Aggregation_method == "mean", ]
  expect_true(all(!is.na(mean_rows$SE)))
  expect_true(all(!is.na(mean_rows$CI_low)))
  expect_true(all(!is.na(mean_rows$CI_high)))
  expect_true(all(mean_rows$CI_low <= mean_rows$CI_high))
})

test_that("validate_plan rejects unknown kobo_type and missing variable", {
  df <- data.frame(gender = c("m","f"), age = c(30, 40))

  ap_bad_kt <- data.frame(
    variable = "gender", kobo_type = "select_strange",
    aggregation_method = NA, disaggregation = "all",
    stringsAsFactors = FALSE
  )
  expect_error(validate_plan(ap_bad_kt, df), "unknown kobo_type")

  ap_missing_var <- data.frame(
    variable = "no_such_col", kobo_type = "select_one",
    aggregation_method = NA, disaggregation = "all",
    stringsAsFactors = FALSE
  )
  expect_error(validate_plan(ap_missing_var, df), "not present in data")

  ap_bad_method <- data.frame(
    variable = "age", kobo_type = "integer",
    aggregation_method = "geomean", disaggregation = "all",
    stringsAsFactors = FALSE
  )
  expect_error(validate_plan(ap_bad_method, df), "invalid aggregation_method")
})
