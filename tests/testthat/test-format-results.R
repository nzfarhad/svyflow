# Tests for the exported format_results() post-processor.

test_that("default round-trip: proportion -> percent -> percent_fmt -> proportion", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res_prop <- suppressWarnings(analyze_survey(make_design(df), ap))

  expect_equal(attr(res_prop, "result_format"), "proportion")
  expect_equal(attr(res_prop, "digits"), 1)

  res_perc <- format_results(res_prop, to = "percent")
  expect_type(res_perc$Result, "double")
  expect_equal(attr(res_perc, "result_format"), "percent")
  perc_rows <- res_perc$Result[res_perc$Aggregation_method == "perc"]
  expect_true(all(perc_rows >= 0 & perc_rows <= 100, na.rm = TRUE))

  res_fmt <- format_results(res_perc, to = "percent_fmt", digits = 2)
  expect_type(res_fmt$Result, "character")
  expect_true(all(grepl("%$",
    res_fmt$Result[res_fmt$Aggregation_method == "perc" & !is.na(res_fmt$Result)])))

  # Back to proportion. Because percent_fmt is rounded, parity is approximate.
  res_back <- format_results(res_fmt, to = "proportion")
  expect_type(res_back$Result, "double")
  back_perc <- res_back$Result[res_back$Aggregation_method == "perc"]
  orig_perc <- res_prop$Result[res_prop$Aggregation_method == "perc"]
  # 2 decimal places on percent => 1e-4 tolerance on proportion.
  expect_true(all(abs(back_perc - orig_perc) < 5e-4, na.rm = TRUE))
})

test_that("non-perc rows keep their raw values across all conversions", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res_prop <- suppressWarnings(analyze_survey(make_design(df), ap))

  mean_orig <- res_prop$Result[res_prop$Aggregation_method == "mean"]
  sum_orig  <- res_prop$Result[res_prop$Aggregation_method == "sum"]

  res_perc <- format_results(res_prop, to = "percent")
  expect_equal(res_perc$Result[res_perc$Aggregation_method == "mean"], mean_orig)
  expect_equal(res_perc$Result[res_perc$Aggregation_method == "sum"],  sum_orig)

  # In percent_fmt the column becomes character, but non-perc rows have no %.
  res_fmt <- format_results(res_prop, to = "percent_fmt", digits = 2)
  mean_chars <- res_fmt$Result[res_fmt$Aggregation_method == "mean"]
  expect_false(any(grepl("%$", mean_chars)))
  # And parse back identically.
  expect_equal(as.numeric(mean_chars), round(mean_orig, 2))
})

test_that("SE / CI columns follow the Result column", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res_prop <- suppressWarnings(analyze_survey(make_design(df), ap))

  res_fmt <- format_results(res_prop, to = "percent_fmt", digits = 1)
  perc_rows <- res_fmt[res_fmt$Aggregation_method == "perc", ]
  for (col in c("Result", "SE", "CI_low", "CI_high")) {
    vals <- perc_rows[[col]]
    expect_true(all(grepl("%$", vals[!is.na(vals)])),
                info = paste("missing % suffix on", col))
  }
})

test_that("identity conversion is a no-op besides refreshed attrs", {
  df <- make_test_data(n = 100)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  same <- format_results(res, to = "proportion")
  expect_equal(same$Result, res$Result)
  expect_equal(attr(same, "result_format"), "proportion")
})

test_that("from = NULL falls back to inference when the attribute is missing", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap,
                                         result_format = "percent"))
  # Strip the attribute to force inference.
  attr(res, "result_format") <- NULL

  res_back <- format_results(res, to = "proportion")
  perc_rows <- res_back$Result[res_back$Aggregation_method == "perc"]
  expect_true(all(perc_rows >= 0 & perc_rows <= 1, na.rm = TRUE))
})

test_that("explicit `from` overrides the attribute", {
  df <- make_test_data(n = 200)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))  # proportion

  # Lie about the source: claim it's already percent. Values should NOT be
  # multiplied by 100 a second time.
  res2 <- format_results(res, to = "percent", from = "percent")
  expect_equal(res2$Result, res$Result)
})

test_that("bad arguments give clear errors", {
  df <- make_test_data(n = 50)
  ap <- make_test_plan()
  res <- suppressWarnings(analyze_survey(make_design(df), ap))

  expect_error(format_results(res, to = "ratio"), "result_format")
  expect_error(format_results(res, to = "percent", from = "ratio"), "from")
  expect_error(format_results(res, to = "percent", digits = -1), "digits")
  expect_error(format_results(data.frame(x = 1), to = "percent"),
               "required column")
})
