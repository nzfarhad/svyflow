test_that("make_design returns an srvyr tbl_svy for SRS, weighted and stratified", {
  df <- make_test_data(n = 200)

  d_srs <- make_design(df)
  expect_s3_class(d_srs, "tbl_svy")
  expect_equal(nrow(d_srs$variables), nrow(df))

  d_w <- make_design(df, weights = "weight")
  expect_s3_class(d_w, "tbl_svy")
  # weights from the design should match the column (drop names from svy attr)
  expect_equal(unname(stats::weights(d_w)), df$weight)

  d_strat <- make_design(df, weights = "weight", strata = "strata")
  expect_s3_class(d_strat, "tbl_svy")
})

test_that("make_design supports ids and fpc arguments", {
  df <- make_test_data(n = 100)
  df$cluster <- rep(1:10, each = 10)
  df$fpc_col <- 1000

  d_clust <- make_design(df, ids = "cluster")
  expect_s3_class(d_clust, "tbl_svy")

  d_fpc <- make_design(df, ids = "cluster", fpc = "fpc_col")
  expect_s3_class(d_fpc, "tbl_svy")
})
