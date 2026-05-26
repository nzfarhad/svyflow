# Synthetic Kobo / SurveyCTO style dataset used across test files.
# Deterministic via set.seed so test failures are reproducible.

make_test_data <- function(n = 500, seed = 42) {
  set.seed(seed)

  gender   <- sample(c("male", "female"), n, replace = TRUE, prob = c(0.48, 0.52))
  province <- sample(c("kabul", "balkh", "herat", "kandahar"), n, replace = TRUE)
  edu_lvl  <- sample(c("none", "primary", "secondary", "tertiary"),
                     n, replace = TRUE)
  edu_lvl[sample(n, max(1, floor(n * 0.06)))] <- NA

  ms_opts <- c("cash", "food", "shelter", "nfis", "health")
  hh_needs_list <- lapply(seq_len(n), function(i) {
    k <- sample.int(length(ms_opts), 1)
    sort(sample(ms_opts, k))
  })
  hh_needs <- vapply(hh_needs_list, paste, character(1), collapse = "; ")
  na_idx <- sample(n, max(1, floor(n * 0.05)))
  hh_needs[na_idx] <- NA

  # Kobo-style sibling binary columns (`var/opt`)
  ms_binary <- as.data.frame(stats::setNames(
    lapply(ms_opts, function(o) {
      flag <- vapply(hh_needs_list, function(v) as.integer(o %in% v), integer(1))
      flag[na_idx] <- NA_integer_
      flag
    }),
    paste0("hh_needs/", ms_opts)
  ), check.names = FALSE)

  hh_size <- pmin(stats::rpois(n, lambda = 5) + 1L, 18L)
  hh_size[sample(n, max(1, floor(n * 0.03)))] <- NA

  income <- round(stats::rgamma(n, shape = 2, scale = 200))
  income[sample(n, max(1, floor(n * 0.02)))] <- NA

  weight <- stats::runif(n, 0.5, 2.0)

  base <- data.frame(
    gender   = gender,
    province = province,
    edu_lvl  = edu_lvl,
    hh_needs = hh_needs,
    hh_size  = hh_size,
    income   = income,
    weight   = weight,
    strata   = province,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  cbind(base, ms_binary)
}

make_test_plan <- function() {
  tibble::tribble(
    ~variable,  ~kobo_type,         ~aggregation_method, ~disaggregation, ~repeat_for,
    "gender",   "select_one",       NA,                  "all",           NA,
    "edu_lvl",  "select_one",       NA,                  "all",           NA,
    "edu_lvl",  "select_one",       NA,                  "gender",        NA,
    "hh_needs", "select_multiple",  NA,                  "all",           NA,
    "hh_needs", "select_multiple",  NA,                  "province",      NA,
    "hh_size",  "integer",          "mean",              "all",           NA,
    "hh_size",  "integer",          "mean",              "gender",        NA,
    "hh_size",  "integer",          "median",            "all",           NA,
    "income",   "integer",          "sum",               "all",           NA,
    "income",   "integer",          "firstq",            "all",           NA,
    "income",   "integer",          "thirdq",            "all",           NA,
    "income",   "integer",          "min",               "all",           NA,
    "income",   "integer",          "max",               "all",           NA,
    "hh_size",  "integer",          "mean",              "gender",        "province"
  )
}

# Same as make_test_plan() but with the optional `variable_label` and
# `disaggregation_label` columns populated. NA labels are intentional —
# they exercise the per-row fallback path in analyze_survey(use_labels).
make_test_plan_labelled <- function() {
  tibble::tribble(
    ~variable,  ~kobo_type,         ~aggregation_method, ~disaggregation, ~variable_label,    ~disaggregation_label, ~repeat_for,
    "gender",   "select_one",       NA,                  "all",           "Gender",           NA,                    NA,
    "edu_lvl",  "select_one",       NA,                  "all",           "Education level",  NA,                    NA,
    "edu_lvl",  "select_one",       NA,                  "gender",        "Education level",  "Gender",              NA,
    "hh_needs", "select_multiple",  NA,                  "all",           "Household needs",  NA,                    NA,
    "hh_size",  "integer",          "mean",              "all",           NA,                 NA,                    NA,
    "hh_size",  "integer",          "mean",              "gender",        "Household size",   "Gender",              NA
  )
}
