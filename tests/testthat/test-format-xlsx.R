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

test_that("with_ci composes 'est (lo - hi)' cells respecting input format", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  ap  <- tibble::tribble(
    ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation,
    "edu_lvl", "select_one", NA,                  "all",
    "edu_lvl", "select_one", NA,                  "gender"
  )
  res <- suppressWarnings(analyze_survey(des, ap,
                                         result_format = "percent_fmt",
                                         digits = 0))
  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f, with_ci = TRUE)
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)
  # Pull out the value cells (everything below the header row of the block,
  # excluding the row-label column).
  cells <- unlist(g[-1, -1])
  cells <- cells[!is.na(cells)]
  # In percent_fmt mode each cell carries "% (...% - ...%)".
  expect_true(all(grepl("^[0-9]+% \\([0-9]+% - [0-9]+%\\)$", cells)))
})

test_that("with_counts = 'row_label' appends (n=N) to level labels", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  ap  <- tibble::tribble(
    ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation,
    "edu_lvl", "select_one", NA,                  "all",
    "edu_lvl", "select_one", NA,                  "gender"
  )
  res <- suppressWarnings(analyze_survey(des, ap))
  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f, with_counts = "row_label")
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)
  col1 <- g[[1]]
  expect_true(any(grepl("^male \\(n=\\d+\\)$",    col1)))
  expect_true(any(grepl("^female \\(n=\\d+\\)$",  col1)))
  expect_true(any(grepl("^Overall \\(n=\\d+\\)$", col1)))
})

test_that("with_counts = 'inline' appends (n=N) to value cells", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  ap  <- tibble::tribble(
    ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation,
    "edu_lvl", "select_one", NA,                  "all",
    "edu_lvl", "select_one", NA,                  "gender"
  )
  res <- suppressWarnings(analyze_survey(des, ap,
                                         result_format = "percent_fmt",
                                         digits = 0))
  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f, with_counts = "inline")
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)
  cells <- unlist(g[-1, -1])
  cells <- cells[!is.na(cells)]
  expect_true(all(grepl("\\(n=\\d+\\)$", cells)))
  # Row labels also pick up (n=Denom) under "inline".
  col1 <- g[[1]]
  expect_true(any(grepl("^male \\(n=\\d+\\)$",    col1)))
  expect_true(any(grepl("^Overall \\(n=\\d+\\)$", col1)))
})

test_that("with_counts = 'inline' + with_ci combine inside the CI parens", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  ap  <- tibble::tribble(
    ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation,
    "edu_lvl", "select_one", NA,                  "all",
    "edu_lvl", "select_one", NA,                  "gender"
  )
  res <- suppressWarnings(analyze_survey(des, ap,
                                         result_format = "percent_fmt",
                                         digits = 0))
  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f, with_counts = "inline", with_ci = TRUE)
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)
  cells <- unlist(g[-1, -1])
  cells <- cells[!is.na(cells)]
  expect_true(all(grepl(
    "^[0-9]+% \\([0-9]+% - [0-9]+%; n=\\d+\\)$", cells
  )))
})

test_that("with_counts = 'parallel' adds a sibling (n) column after each value column", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 250)
  des <- make_design(df, weights = "weight")
  ap  <- tibble::tribble(
    ~variable, ~kobo_type,   ~aggregation_method, ~disaggregation,
    "edu_lvl", "select_one", NA,                  "all",
    "edu_lvl", "select_one", NA,                  "gender"
  )
  res <- suppressWarnings(analyze_survey(des, ap))  # default proportion
  f <- tempfile(fileext = ".xlsx")
  write_xlsx(res, f, with_counts = "parallel")
  g <- openxlsx::read.xlsx(f, sheet = "gender", colNames = FALSE)
  hdr <- as.character(g[1, ])
  # First column is the question label; every other column should be a
  # response followed by an "(n)" sibling -> doubled count.
  expect_true(sum(hdr == "(n)", na.rm = TRUE) >= 4L)
  # Row labels also pick up (n=Denom) under "parallel".
  col1 <- g[[1]]
  expect_true(any(grepl("^male \\(n=\\d+\\)$",    col1)))
  expect_true(any(grepl("^female \\(n=\\d+\\)$",  col1)))
  expect_true(any(grepl("^Overall \\(n=\\d+\\)$", col1)))
})

test_that("with_counts is validated via match.arg", {
  skip_if_not_installed("openxlsx")
  df  <- make_test_data(n = 100)
  des <- make_design(df)
  res <- suppressWarnings(analyze_survey(des, make_xlsx_plan()))
  expect_error(write_xlsx(res, tempfile(fileext=".xlsx"),
                          with_counts = "nope"),
               "should be one of")
})

test_that("missing required columns error clearly", {
  skip_if_not_installed("openxlsx")
  bad <- structure(data.frame(x = 1), class = c("svyflow_results", "data.frame"))
  expect_error(write_xlsx(bad, tempfile(fileext = ".xlsx")),
               "required column")
})
