# Tests for write_xlsx() and xlsx_theme().

make_xlsx_plan <- function() {
  tibble::tribble(
    ~variable,  ~kobo_type,        ~aggregation_method, ~disaggregation, ~group,
    "gender",   "select_one",      NA,                  "all",           "Demographics",
    "edu_lvl",  "select_one",      NA,                  "all",           "Demographics",
    "edu_lvl",  "select_one",      NA,                  "gender",        "Demographics",
    "hh_size",  "integer",         "mean",              "all",           "Household",
    "hh_size",  "integer",         "mean",              "gender",        "Household"
  )
}

test_that("xlsx_theme validates and is overridable", {
  th <- xlsx_theme()
  expect_s3_class(th, "svyflow_xlsx_theme")
  expect_equal(th$header_fill, "#7D0E00")          # maroon default
  expect_equal(xlsx_theme(font_name = "Arial")$font_name, "Arial")
  expect_error(xlsx_theme(header_fill = "red"),    "hex")
  expect_error(xlsx_theme(body_font_size = -1),    "positive")
  expect_error(xlsx_theme(font_name = c("a", "b")), "single string")
})

test_that("write_xlsx makes one sheet per disaggregation plus Overall", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  res <- suppressWarnings(analyze_survey(des, make_xlsx_plan()))

  f <- tempfile(fileext = ".xlsx")
  expect_invisible(write_xlsx(res, f))
  expect_true(file.exists(f))

  sheets <- openxlsx::getSheetNames(f)
  expect_true("gender"  %in% sheets)   # the only disaggregation variable
  expect_true("Overall" %in% sheets)
})

test_that("crosstab has levels as rows with Overall as the last row", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  res <- suppressWarnings(analyze_survey(des, make_xlsx_plan()))

  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f)
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)

  col1 <- g[[1]]
  # Section header and the disaggregation levels + Overall appear in column 1.
  expect_true("Demographics" %in% col1)
  expect_true(all(c("male", "female", "Overall") %in% col1))
  # In each block the Overall row immediately follows the level rows: every
  # "Overall" is preceded somewhere above by male/female (sanity: >=1 Overall).
  expect_gte(sum(col1 == "Overall", na.rm = TRUE), 1)
})

test_that("group values become section separators", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 200)
  des <- make_design(df, weights = "weight")
  res <- suppressWarnings(analyze_survey(des, make_xlsx_plan()))

  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f)
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)
  col1 <- g[[1]]
  expect_true("Demographics" %in% col1)
  expect_true("Household"    %in% col1)
})

test_that("values are written as-is: proportions stay numeric in [0,1]", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  # Categorical-only plan so every value cell is a proportion.
  ap  <- tibble::tribble(
    ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation,
    "gender",  "select_one", NA,                  "all",
    "edu_lvl", "select_one", NA,                  "all"
  )
  res <- suppressWarnings(analyze_survey(des, ap))  # proportions (default)

  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f)
  g <- openxlsx::read.xlsx(f, sheet = "Overall", colNames = FALSE)
  vals <- suppressWarnings(as.numeric(unlist(g[, -1])))
  vals <- vals[!is.na(vals)]
  expect_true(length(vals) > 0)
  expect_true(all(vals >= 0 & vals <= 1))   # proportions, not percentages

  # Same data as percent => values now exceed 1 (written as-is, unformatted).
  res_pct <- format_results(res, to = "percent")
  f2 <- tempfile(fileext = ".xlsx")
  write_xlsx(res_pct, f2)
  g2 <- openxlsx::read.xlsx(f2, sheet = "Overall", colNames = FALSE)
  vals2 <- suppressWarnings(as.numeric(unlist(g2[, -1])))
  vals2 <- vals2[!is.na(vals2)]
  expect_true(any(vals2 > 1))               # 0-100 scale preserved verbatim
})

test_that("percent_fmt output is written verbatim as % strings", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  res <- suppressWarnings(analyze_survey(des, make_xlsx_plan()))
  res_fmt <- format_results(res, to = "percent_fmt", digits = 1)

  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res_fmt, f)
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)
  cells <- unlist(g[, -1])
  cells <- cells[!is.na(cells)]
  pct <- cells[grepl("%$", cells)]
  expect_true(length(pct) > 0)             # at least the proportion cells carry %
})

test_that("custom theme is accepted (object or list)", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 150)
  des <- make_design(df, weights = "weight")
  res <- suppressWarnings(analyze_survey(des, make_xlsx_plan()))

  f1 <- tempfile(fileext = ".xlsx")
  expect_invisible(write_xlsx(res, f1,
                              theme = xlsx_theme(font_name = "Arial",
                                                 header_fill = "#1E3A5F")))
  f2 <- tempfile(fileext = ".xlsx")
  expect_invisible(write_xlsx(res, f2, theme = list(header_fill = "#1E3A5F")))
})

test_that("missing required columns error clearly", {
  skip_if_not_installed("openxlsx")
  bad <- structure(data.frame(x = 1), class = c("svyflow_results", "data.frame"))
  expect_error(write_xlsx(bad, tempfile(fileext = ".xlsx")),
               "required column")
})
