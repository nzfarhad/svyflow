# test_Analysis_svy.R
# Smoke test for Analysis_svy.R covering every question type, with synthetic
# data shaped like a Kobo / SurveyCTO export.
#
# Run with:  Rscript test_Analysis_svy.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source("Analysis_svy.R")

set.seed(42)

# ---- synthetic dataset (Kobo / SurveyCTO style) ------------------------------
N <- 500

# select_one columns: text codes (Kobo stores the choice list name)
gender   <- sample(c("male", "female"), N, replace = TRUE, prob = c(0.48, 0.52))
province <- sample(c("kabul", "balkh", "herat", "kandahar"), N, replace = TRUE)
# inject some NAs to exercise the all-NA short-circuit / missing-data paths
edu_lvl  <- sample(c("none", "primary", "secondary", "tertiary"),
                   N, replace = TRUE)
edu_lvl[sample(N, 30)] <- NA

# select_multiple — both the combined string column AND the sibling binary
# columns, mimicking what Kobo exports. Sep is "; " (matches the script
# default; SurveyCTO uses " " but make_design / detect_ms_options key off the
# sibling-column naming, not the separator, so this still exercises the path).
ms_opts <- c("cash", "food", "shelter", "nfis", "health")
hh_needs_list <- lapply(seq_len(N), function(i) {
  k <- sample.int(length(ms_opts), 1)
  sort(sample(ms_opts, k))
})
hh_needs <- vapply(hh_needs_list, paste, character(1), collapse = "; ")
# add NAs on a handful of rows
na_idx <- sample(N, 25)
hh_needs[na_idx] <- NA

# Kobo-style sibling binary columns: var/opt
ms_binary <- as_tibble(setNames(
  lapply(ms_opts, function(o) {
    flag <- vapply(hh_needs_list, function(v) as.integer(o %in% v), integer(1))
    flag[na_idx] <- NA_integer_
    flag
  }),
  paste0("hh_needs/", ms_opts)
))

# integer column — household size, with a tail
hh_size <- pmin(rpois(N, lambda = 5) + 1L, 18L)
hh_size[sample(N, 15)] <- NA

# A second integer column for sum/min/max sanity
income <- round(rgamma(N, shape = 2, scale = 200))
income[sample(N, 10)] <- NA

# Sampling weights and strata
weight <- runif(N, 0.5, 2.0)
strata <- province  # toy strata, fine for smoke test

df <- bind_cols(
  tibble(
    gender   = gender,
    province = province,
    edu_lvl  = edu_lvl,
    hh_needs = hh_needs,
    hh_size  = hh_size,
    income   = income,
    weight   = weight,
    strata   = strata
  ),
  ms_binary
)

cat("Synthetic data: ", nrow(df), "rows,", ncol(df), "cols\n")

# ---- analysis plan covering every (kobo_type, aggregation_method) combo ------

ap <- tribble(
  ~variable,  ~kobo_type,         ~aggregation_method, ~disaggregation, ~repeat_for,
  # select_one
  "gender",   "select_one",       NA,                  "all",           NA,
  "edu_lvl",  "select_one",       NA,                  "all",           NA,
  "edu_lvl",  "select_one",       NA,                  "gender",        NA,
  # select_multiple (no expand_multiselect call needed — siblings already exist)
  "hh_needs", "select_multiple",  NA,                  "all",           NA,
  "hh_needs", "select_multiple",  NA,                  "province",      NA,
  # integer / mean
  "hh_size",  "integer",          "mean",              "all",           NA,
  "hh_size",  "integer",          "mean",              "gender",        NA,
  # integer / median
  "hh_size",  "integer",          "median",            "all",           NA,
  # integer / sum
  "income",   "integer",          "sum",               "all",           NA,
  # integer / firstq + thirdq
  "income",   "integer",          "firstq",            "all",           NA,
  "income",   "integer",          "thirdq",            "all",           NA,
  # integer / min, max (unweighted)
  "income",   "integer",          "min",               "all",           NA,
  "income",   "integer",          "max",               "all",           NA,
  # double disaggregation via repeat_for
  "hh_size",  "integer",          "mean",              "gender",        "province"
)

# ---- run #1: unweighted SRS design -------------------------------------------

cat("\n=== Run 1: unweighted SRS design ===\n")
design_srs <- make_design(df)  # weights = NULL → SRS
res_srs    <- analyze_survey(design_srs, ap)

cat("\nUnweighted result (head):\n")
print(head(res_srs, 20))
cat("\nschema:\n")
print(colnames(res_srs))

# ---- run #2: weighted + stratified design ------------------------------------

cat("\n=== Run 2: weighted + stratified design ===\n")
design_wt <- make_design(df, weights = "weight", strata = "strata")
res_wt    <- analyze_survey(design_wt, ap)

cat("\nWeighted result (head):\n")
print(head(res_wt, 20))

# ---- run #3: expand_multiselect on a df without sibling columns --------------

cat("\n=== Run 3: expand_multiselect path ===\n")
df_no_sib <- df %>% select(-starts_with("hh_needs/"))
df_exp    <- expand_multiselect(df_no_sib, vars = "hh_needs", sep = "; ")
cat("expand_multiselect created columns:\n")
print(grep("^hh_needs___", names(df_exp), value = TRUE))
cat("ms_options attribute:\n")
print(attr(df_exp, "ms_options"))

design_exp <- make_design(df_exp)
res_exp    <- analyze_survey(
  design_exp,
  filter(ap, variable == "hh_needs" & disaggregation == "all")
)
print(res_exp)

# ---- run #4: collision handling in expand_multiselect ------------------------

cat("\n=== Run 4: expand_multiselect with name collision ===\n")
df_collide <- df_no_sib
df_collide[["hh_needs___cash"]] <- 999  # pre-existing column with the same name
# Note: detect_ms_options would now treat hh_needs___cash as an existing sibling
# and skip splitting entirely. To force the collision path, point at a fresh
# variable.
df_collide2 <- df_no_sib
df_collide2[["hh_needs___cash"]] <- 999  # not detected (different prefix style)
# detect_ms_options does match `hh_needs___cash` because `___` is in .MS_SEPS,
# so this will short-circuit on existing. That's actually the correct behavior
# we documented. Show it:
cat("With existing hh_needs___cash, detection picks it up (no split):\n")
detected <- detect_ms_options(df_collide, "hh_needs")
print(detected)

# ---- sanity checks -----------------------------------------------------------

cat("\n=== Sanity checks ===\n")

stopifnot(
  "output schema mismatch" =
    identical(
      colnames(res_srs),
      c("Disaggregation", "Disaggregation_level", "Question", "Response",
        "Aggregation_method", "Result", "SE", "CI_low", "CI_high",
        "Count", "Denominator", "repeat_for")
    )
)
cat("[ok] output schema matches public contract\n")

# Single-select proportions sum to ~100 within each disagg level
prop_check <- res_srs %>%
  filter(Aggregation_method == "perc",
         Question == "gender",
         Disaggregation == "all") %>%
  summarise(total = sum(Result, na.rm = TRUE)) %>%
  pull(total)
stopifnot("single-select proportions don't sum to ~100" = abs(prop_check - 100) < 0.5)
cat("[ok] select_one proportions sum to ~100 (got ", round(prop_check, 2), ")\n", sep = "")

# Unweighted mean must equal base R's mean
m_svy <- res_srs %>%
  filter(Question == "hh_size", Aggregation_method == "mean",
         Disaggregation == "all") %>%
  pull(Result)
m_ref <- mean(df$hh_size, na.rm = TRUE)
stopifnot("unweighted mean mismatch" = abs(m_svy - m_ref) < 1e-6)
cat("[ok] unweighted survey_mean matches base mean(hh_size): ", round(m_svy, 4), "\n", sep = "")

# Sum
s_svy <- res_srs %>%
  filter(Question == "income", Aggregation_method == "sum",
         Disaggregation == "all") %>%
  pull(Result)
s_ref <- sum(df$income, na.rm = TRUE)
stopifnot("unweighted sum mismatch" = abs(s_svy - s_ref) < 1e-6)
cat("[ok] unweighted survey_total matches base sum(income): ", s_svy, "\n", sep = "")

# Min / max are unweighted
min_row <- res_srs %>%
  filter(Question == "income", Aggregation_method == "min_unweighted")
max_row <- res_srs %>%
  filter(Question == "income", Aggregation_method == "max_unweighted")
stopifnot("min row missing"          = nrow(min_row) == 1)
stopifnot("max row missing"          = nrow(max_row) == 1)
stopifnot("min must equal raw min"   = min_row$Result == min(df$income, na.rm = TRUE))
stopifnot("max must equal raw max"   = max_row$Result == max(df$income, na.rm = TRUE))
stopifnot("min/max SE must be NA"    = is.na(min_row$SE) && is.na(max_row$SE))
cat("[ok] min_unweighted / max_unweighted match raw min/max with NA SE\n")

# Multi-select: count >= 0 for every option and denominator = sum(!is.na(hh_needs))
ms_rows <- res_srs %>%
  filter(Question == "hh_needs", Disaggregation == "all")
stopifnot("missing multi-select rows" = nrow(ms_rows) == length(ms_opts))
stopifnot("multi-select denominators inconsistent" =
            length(unique(ms_rows$Denominator)) == 1)
expected_denom <- sum(!is.na(df$hh_needs))
stopifnot("multi-select denominator wrong" =
            unique(ms_rows$Denominator) == expected_denom)
cat("[ok] multi-select rows: ", nrow(ms_rows),
    " options, denom = ", expected_denom, "\n", sep = "")

# Weighted estimates should generally differ from unweighted, but should not be
# absurdly far (weights are bounded 0.5–2.0)
m_wt <- res_wt %>%
  filter(Question == "hh_size", Aggregation_method == "mean",
         Disaggregation == "all") %>%
  pull(Result)
stopifnot("weighted mean unreasonable" = abs(m_wt - m_ref) < 1.0)
cat("[ok] weighted mean(hh_size) = ", round(m_wt, 4),
    " (unweighted = ", round(m_ref, 4), ")\n", sep = "")

# repeat_for produced rows for every province
rf_rows <- res_wt %>% filter(!is.na(repeat_for))
stopifnot("repeat_for missing" = length(unique(rf_rows$repeat_for)) ==
            length(unique(df$province)))
cat("[ok] repeat_for produced ", length(unique(rf_rows$repeat_for)),
    " province-level subsets\n", sep = "")

# CIs present and ordered for mean rows
mean_rows <- res_srs %>% filter(Aggregation_method == "mean")
stopifnot("CI columns missing for mean" =
            all(!is.na(mean_rows$SE)) &&
            all(!is.na(mean_rows$CI_low)) &&
            all(!is.na(mean_rows$CI_high)))
stopifnot("CI_low > CI_high somewhere" = all(mean_rows$CI_low <= mean_rows$CI_high))
cat("[ok] SE / CI_low / CI_high populated for mean rows and CI_low <= CI_high\n")

cat("\nAll sanity checks passed.\n")
