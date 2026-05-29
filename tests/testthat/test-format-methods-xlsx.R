# Tests for write_methods_xlsx() — a standalone companion workbook documenting
# the analysis run. Round-trip via openxlsx::read.xlsx and assert on labels +
# values that prove the writer is reading the right slots off the design.

# Read the Methods sheet back as a flat character matrix so we can search it.
.read_methods_flat <- function(file, sheet = "Methods") {
  df <- openxlsx::read.xlsx(file, sheet = sheet, colNames = FALSE)
  m  <- as.matrix(df)
  m[is.na(m)] <- ""
  m
}

.has_kv <- function(flat, key, value = NULL) {
  hits <- which(flat[, 1] == key)
  if (!length(hits)) return(FALSE)
  if (is.null(value)) return(TRUE)
  any(flat[hits, 2] == as.character(value))
}

.has_section <- function(flat, text) {
  any(flat[, 1] == text)
}

test_that("write_methods_xlsx writes a Methods sheet with Tier 1 sections", {
  df <- make_test_data(n = 200)
  des <- make_design(df, weights = "weight", strata = "strata")
  f <- tempfile(fileext = ".xlsx")

  write_methods_xlsx(f, design = des)
  expect_true(file.exists(f))
  expect_true(file.size(f) > 0)

  sheets <- openxlsx::getSheetNames(f)
  expect_equal(sheets, "Methods")

  flat <- .read_methods_flat(f)
  expect_true(.has_section(flat, "Session"))
  expect_true(.has_section(flat, "Data"))
  expect_true(.has_section(flat, "Survey design"))
  expect_true(.has_section(flat, "Sample sizes"))
  expect_true(.has_section(flat, "Weights summary"))
  expect_true(.has_section(flat, "Confidence intervals"))
  expect_true(.has_section(flat, "Result format"))
  expect_true(.has_section(flat, "Notes"))
})

test_that("survey design sample sizes are extracted from the design slots", {
  df <- make_test_data(n = 200)
  des <- make_design(df, weights = "weight", strata = "strata")
  f <- tempfile(fileext = ".xlsx")
  write_methods_xlsx(f, design = des)
  flat <- .read_methods_flat(f)

  # n rows = 200 (formatted with thousands separator absent at <1000).
  expect_true(.has_kv(flat, "n (rows)", "200"))
  # strata column "strata" carries the province (4 levels)
  expect_true(.has_kv(flat, "Strata column", "strata"))
  expect_true(.has_kv(flat, "Strata",
                      as.character(length(unique(df$strata)))))
})

test_that("SRS design is detected and reported as unweighted / no clusters", {
  df  <- make_test_data(n = 100)
  des <- make_design(df)
  f   <- tempfile(fileext = ".xlsx")
  write_methods_xlsx(f, design = des)
  flat <- .read_methods_flat(f)

  expect_true(.has_kv(flat, "Weighting", "unweighted"))
  expect_true(.has_kv(flat, "Strata column", "-"))
  expect_true(.has_kv(flat, "Cluster / PSU", "-"))
})

test_that("cluster design reports the cluster column name and PSU count", {
  df  <- make_test_data(n = 300)
  des <- make_design(df, weights = "weight", ids = "province")
  f   <- tempfile(fileext = ".xlsx")
  write_methods_xlsx(f, design = des)
  flat <- .read_methods_flat(f)

  expect_true(.has_kv(flat, "Cluster / PSU", "province"))
  expect_true(.has_kv(flat, "Clusters / PSUs",
                      as.character(length(unique(df$province)))))
})

test_that("Plan section is included only when plan is supplied", {
  df  <- make_test_data(n = 150)
  des <- make_design(df, weights = "weight")
  f1  <- tempfile(fileext = ".xlsx")
  f2  <- tempfile(fileext = ".xlsx")

  write_methods_xlsx(f1, design = des)
  write_methods_xlsx(f2, design = des, plan = make_test_plan())

  flat1 <- .read_methods_flat(f1)
  flat2 <- .read_methods_flat(f2)

  expect_false(.has_section(flat1, "Plan"))
  expect_true(.has_section(flat2, "Plan"))
  expect_true(.has_section(flat2, "Indicators by kobo_type"))
  expect_true(.has_section(flat2, "Indicators by aggregation_method"))
  expect_true(.has_kv(flat2, "Indicators",
                      as.character(nrow(make_test_plan()))))
})

test_that("DEFF roll-up is included only when results carries a DEFF column", {
  df   <- make_test_data(n = 200)
  des  <- make_design(df, weights = "weight")
  plan <- make_test_plan()

  res_no_deff <- suppressWarnings(analyze_survey(des, plan))
  res_deff    <- suppressWarnings(analyze_survey(des, plan, deff = TRUE))

  f1 <- tempfile(fileext = ".xlsx")
  f2 <- tempfile(fileext = ".xlsx")
  write_methods_xlsx(f1, design = des, results = res_no_deff)
  write_methods_xlsx(f2, design = des, results = res_deff)

  flat1 <- .read_methods_flat(f1)
  flat2 <- .read_methods_flat(f2)

  expect_false(.has_section(flat1, "Design effect (DEFF)"))
  expect_true(.has_section(flat2, "Design effect (DEFF)"))
  expect_true(.has_section(flat2, "Highest DEFF (top 5)"))
  expect_true(.has_kv(flat2, "Rows with DEFF"))
})

test_that("CI options round-trip into the workbook", {
  df  <- make_test_data(n = 100)
  des <- make_design(df, weights = "weight")
  f   <- tempfile(fileext = ".xlsx")
  write_methods_xlsx(f, design = des,
                     ci = ci_opts(ci_level = 0.90, df = Inf,
                                  prop_method = "logit"))
  flat <- .read_methods_flat(f)

  expect_true(.has_kv(flat, "Level",              "90%"))
  expect_true(.has_kv(flat, "Degrees of freedom", "Inf (normal)"))
  expect_true(.has_kv(flat, "Proportion method",  "logit"))
})

test_that("result_format / digits attributes on results override the args", {
  df   <- make_test_data(n = 100)
  des  <- make_design(df, weights = "weight")
  plan <- make_test_plan()
  res  <- suppressWarnings(analyze_survey(des, plan,
                                          result_format = "percent",
                                          digits = 2))
  f <- tempfile(fileext = ".xlsx")
  # Explicit args differ from the stamped attributes; attrs should win.
  write_methods_xlsx(f, design = des, results = res,
                     result_format = "proportion", digits = 4)
  flat <- .read_methods_flat(f)
  expect_true(.has_kv(flat, "Format", "percent"))
  expect_true(.has_kv(flat, "Digits", "2"))
})

test_that("weights summary picks up real weight variation", {
  df  <- make_test_data(n = 200)
  des <- make_design(df, weights = "weight")
  f   <- tempfile(fileext = ".xlsx")
  write_methods_xlsx(f, design = des)
  flat <- .read_methods_flat(f)

  # CV computed from the weight vector
  cv  <- stats::sd(df$weight) / mean(df$weight)
  expect_true(.has_kv(flat, "CV", formatC(cv, format = "f", digits = 3)))
  expect_true(.has_kv(flat, "n", "200"))
})

test_that("bad arguments fail loudly", {
  df  <- make_test_data(n = 50)
  des <- make_design(df)
  f   <- tempfile(fileext = ".xlsx")
  expect_error(write_methods_xlsx(f, design = des, plan = "not a df"),
               "data frame")
  expect_error(write_methods_xlsx(f, design = des,
                                  plan = data.frame(x = 1)),
               "kobo_type")
  expect_error(write_methods_xlsx(f, design = des,
                                  cover_notes = 42),
               "character vector")
})

test_that("Project section appears only when cover_notes is supplied", {
  df  <- make_test_data(n = 50)
  des <- make_design(df)
  f1  <- tempfile(fileext = ".xlsx")
  f2  <- tempfile(fileext = ".xlsx")

  write_methods_xlsx(f1, design = des)
  write_methods_xlsx(f2, design = des,
                     cover_notes = c("Baseline survey 2026",
                                     "Prepared for donor agency"))

  flat1 <- .read_methods_flat(f1)
  flat2 <- .read_methods_flat(f2)

  expect_false(.has_section(flat1, "Project"))
  expect_true(.has_section(flat2, "Project"))
  expect_true(any(flat2[, 1] == "Baseline survey 2026"))
  expect_true(any(flat2[, 1] == "Prepared for donor agency"))
})

test_that("named cover_notes render as 'name: value'", {
  df  <- make_test_data(n = 50)
  des <- make_design(df)
  f   <- tempfile(fileext = ".xlsx")
  write_methods_xlsx(f, design = des,
                     cover_notes = c(Project = "Baseline",
                                     Funder  = "FCDO",
                                     "Free-form footnote line"))
  flat <- .read_methods_flat(f)
  expect_true(any(flat[, 1] == "Project: Baseline"))
  expect_true(any(flat[, 1] == "Funder: FCDO"))
  expect_true(any(flat[, 1] == "Free-form footnote line"))
})

test_that("empty / NA cover_notes are dropped silently", {
  df  <- make_test_data(n = 50)
  des <- make_design(df)
  f   <- tempfile(fileext = ".xlsx")
  expect_silent(
    write_methods_xlsx(f, design = des,
                       cover_notes = c("real line", "", NA_character_))
  )
  flat <- .read_methods_flat(f)
  # The real line is there; empties / NAs did not produce blank rows.
  expect_true(any(flat[, 1] == "real line"))
})
