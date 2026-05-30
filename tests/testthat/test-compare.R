# Tests for compare_groups() and its survey-design-aware test selection.

# A panel data frame for the paired tests: one row per (unit, wave), with a
# per-unit weight repeated across the two waves and a real baseline->endline
# shift so the paired tests detect a difference.
make_panel <- function(n_units = 200, seed = 1) {
  set.seed(seed)
  base <- stats::rpois(n_units, 5) + 1
  data.frame(
    unit_id = rep(seq_len(n_units), each = 2),
    wave    = rep(c("baseline", "endline"), times = n_units),
    score   = c(rbind(base, base + stats::rnorm(n_units, 0.6, 1.5))),
    weight  = rep(stats::runif(n_units, 0.5, 2), each = 2),
    stringsAsFactors = FALSE
  )
}

test_that("numeric x binary group auto-resolves to a t-test", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, "hh_size", "gender")

  expect_s3_class(res, "svyflow_summary")
  expect_equal(nrow(res), 1L)
  expect_equal(res$Test, "ttest")
  expect_true(is.finite(res$Statistic))
  expect_true(is.finite(res$P_value))
  expect_equal(res$Effect_size_type, "Cohen_d")
  expect_equal(res$Comparison, "female vs male")
  # Significance stars match the p-value bucket.
  expect_equal(res$Significance, svyflow:::.sig_stars(res$P_value))
})

test_that("categorical x binary group auto-resolves to chi-square", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, "edu_lvl", "gender")

  expect_equal(res$Test, "chisq")
  expect_equal(res$Effect_size_type, "Cramer_V")
  expect_gte(res$Effect_size, 0)
  expect_lte(res$Effect_size, 1)
  expect_true(is.finite(res$DF2))
})

test_that("categorical x 4-level group lists all levels", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, "edu_lvl", "province")

  expect_equal(res$Test, "chisq")
  expect_match(res$Comparison, "balkh")
  expect_match(res$Comparison, "kandahar")
  expect_equal(length(strsplit(res$Comparison, ", ")[[1]]), 4L)
})

test_that("multiple indicators return one row each, in input order", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, c("hh_size", "income", "edu_lvl"), "gender")

  expect_equal(nrow(res), 3L)
  expect_equal(res$Indicator, c("hh_size", "income", "edu_lvl"))
  expect_equal(res$Test, c("ttest", "ttest", "chisq"))
})

test_that("parametric = FALSE swaps in the rank-based tests", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  expect_equal(
    compare_groups(des, "hh_size", "gender", parametric = FALSE)$Test,
    "wilcoxon"
  )
  expect_equal(
    compare_groups(des, "hh_size", "province", parametric = FALSE)$Test,
    "kruskal"
  )
})

test_that("3+ groups auto-resolves to ANOVA and matches regTermTest", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, "hh_size", "province", digits = NULL)
  expect_equal(res$Test, "anova")
  expect_true(is.finite(res$DF) && is.finite(res$DF2))

  d   <- srvyr::filter(des, !is.na(hh_size) & !is.na(province))
  fit <- survey::svyglm(hh_size ~ province, design = d)
  rt  <- survey::regTermTest(fit, "province", df = survey::degf(d))
  expect_equal(res$Statistic, as.numeric(rt$Ftest), tolerance = 1e-8)
  expect_equal(res$P_value,   as.numeric(rt$p),     tolerance = 1e-8)
})

test_that("paired t-test runs on a panel and matches svyttest(diff ~ 0)", {
  pdes <- make_design(make_panel(), weights = "weight")
  res  <- compare_groups(pdes, "score", "wave",
                         paired = TRUE, pair_by = "unit_id", digits = NULL)

  expect_equal(res$Test, "paired_ttest")
  expect_equal(res$N, 200L)

  pd <- svyflow:::.build_pair_design(pdes, "score", "wave", "unit_id")
  tt <- survey::svyttest(diff ~ 0, pd$design)
  expect_equal(res$Statistic, as.numeric(tt$statistic), tolerance = 1e-8)
  expect_equal(res$P_value,   as.numeric(tt$p.value),   tolerance = 1e-8)
})

test_that("paired test without pair_by errors", {
  pdes <- make_design(make_panel(), weights = "weight")
  expect_error(
    compare_groups(pdes, "score", "wave", paired = TRUE),
    "pair_by"
  )
})

test_that("Fisher's exact triggers on low expected counts", {
  set.seed(7)
  small <- data.frame(
    outcome = c(rep("yes", 5), rep("no", 5), rep("yes", 2), rep("no", 8)),
    grp     = c(rep("a", 10), rep("b", 10)),
    weight  = runif(20, 0.8, 1.2),
    stringsAsFactors = FALSE
  )
  sdes <- make_design(small, weights = "weight")

  expect_warning(
    res <- compare_groups(sdes, "outcome", "grp", digits = NULL),
    "Fisher"
  )
  expect_equal(res$Test, "fisher")
  expect_equal(res$Effect_size_type, "Odds_ratio")
  ft <- stats::fisher.test(table(small$outcome, small$grp))
  expect_equal(res$P_value, as.numeric(ft$p.value), tolerance = 1e-8)

  # small_sample = FALSE keeps the chi-square.
  res2 <- compare_groups(sdes, "outcome", "grp", small_sample = FALSE)
  expect_equal(res2$Test, "chisq")
})

test_that("prop_z compares two proportions (binary x binary)", {
  df <- make_test_data(500)
  df$edu_bin <- ifelse(is.na(df$edu_lvl), NA,
                       ifelse(df$edu_lvl %in% c("none", "primary"),
                              "low", "high"))
  des <- make_design(df, weights = "weight", strata = "strata")
  res <- compare_groups(des, "edu_bin", "gender", test = "prop_z")

  expect_equal(res$Test, "prop_z")
  expect_equal(res$Effect_size_type, "Cohen_h")
  expect_true(is.na(res$DF))
  expect_true(is.finite(res$Statistic))
})

test_that("explicit test on an incompatible column errors", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  expect_error(compare_groups(des, "edu_lvl", "gender", test = "ttest"),
               "numeric")
  expect_error(compare_groups(des, "hh_size", "gender", test = "chisq"),
               "categorical")
  expect_error(compare_groups(des, "edu_lvl", "gender", test = "prop_z"),
               "binary")
})

test_that("statistics match survey:: calls directly (parity)", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")

  # t-test
  d_t <- srvyr::filter(des, !is.na(hh_size) & !is.na(gender))
  tt  <- survey::svyttest(hh_size ~ gender, d_t)
  cg  <- compare_groups(des, "hh_size", "gender", digits = NULL)
  expect_equal(cg$Statistic, as.numeric(tt$statistic), tolerance = 1e-8)
  expect_equal(cg$P_value,   as.numeric(tt$p.value),   tolerance = 1e-8)

  # chi-square
  d_c <- srvyr::filter(des, !is.na(edu_lvl) & !is.na(gender))
  sc  <- survey::svychisq(~edu_lvl + gender, d_c, statistic = "F")
  cgc <- compare_groups(des, "edu_lvl", "gender", digits = NULL)
  expect_equal(cgc$Statistic, as.numeric(sc$statistic), tolerance = 1e-8)
  expect_equal(cgc$P_value,   as.numeric(sc$p.value),   tolerance = 1e-8)

  # Kruskal-Wallis
  d_k <- srvyr::filter(des, !is.na(hh_size) & !is.na(province))
  kt  <- survey::svyranktest(hh_size ~ province, d_k, test = "KruskalWallis")
  cgk <- compare_groups(des, "hh_size", "province", parametric = FALSE,
                        digits = NULL)
  expect_equal(cgk$Statistic, as.numeric(kt$statistic), tolerance = 1e-8)
  expect_equal(cgk$P_value,   as.numeric(kt$p.value),   tolerance = 1e-8)
})

test_that("ci_opts() is threaded into the t-test interval", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  d90 <- compare_groups(des, "hh_size", "gender",
                        ci = ci_opts(ci_level = 0.90), digits = NULL)
  d95 <- compare_groups(des, "hh_size", "gender", digits = NULL)
  w90 <- d90$CI_high - d90$CI_low
  w95 <- d95$CI_high - d95$CI_low
  expect_lt(w90, w95)

  # df = Inf -> normal-approximation interval.
  dInf <- compare_groups(des, "hh_size", "gender",
                         ci = ci_opts(df = Inf), digits = NULL)
  se   <- (d95$CI_high - d95$Estimate) /
            stats::qt(0.975, d95$DF)
  expect_equal(dInf$CI_high, dInf$Estimate + stats::qnorm(0.975) * se,
               tolerance = 1e-6)
})

test_that("N reflects the non-missing count after dropping NA groups", {
  df <- make_test_data(500)
  df$gender[1:50] <- NA
  des <- make_design(df, weights = "weight", strata = "strata")
  res <- compare_groups(des, "hh_size", "gender")
  expected_n <- sum(!is.na(df$hh_size) & !is.na(df$gender))
  expect_equal(res$N, expected_n)
})

test_that("a single-level group errors helpfully", {
  df <- make_test_data(200)
  df$onlyone <- "x"
  des <- make_design(df, weights = "weight", strata = "strata")
  expect_error(compare_groups(des, "hh_size", "onlyone"),
               "two groups")
})

test_that("labels appear in the output", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, "hh_size", "gender",
                        variable_labels = c(hh_size = "Household size"),
                        group_label = "Sex")
  expect_equal(res$Indicator, "Household size")
  expect_equal(res$Group, "Sex")
})

test_that("paired test on a structured design warns about collapse", {
  panel <- make_panel()
  panel$strat <- rep(c("a", "b"), length.out = nrow(panel))
  sdes <- make_design(panel, weights = "weight", strata = "strat")
  expect_warning(
    compare_groups(sdes, "score", "wave", paired = TRUE, pair_by = "unit_id"),
    "collapse"
  )
  # An unstructured (SRS-weighted) design does not warn.
  udes <- make_design(panel, weights = "weight")
  expect_no_warning(
    compare_groups(udes, "score", "wave", paired = TRUE, pair_by = "unit_id")
  )
})

test_that("Cramer's V is design-consistent (weighted, bounded)", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, "edu_lvl", "province", digits = NULL)
  expect_gte(res$Effect_size, 0)
  expect_lte(res$Effect_size, 1)

  # Matches a manual weighted Cramer's V from svytable scaled to n.
  d  <- srvyr::filter(des, !is.na(edu_lvl) & !is.na(province))
  n  <- nrow(svyflow:::.svy_data(d))
  tw <- as.matrix(survey::svytable(~edu_lvl + province, d))
  O  <- tw / sum(tw) * n
  E  <- outer(rowSums(O), colSums(O)) / n
  V  <- sqrt(sum((O - E)^2 / E) / (n * (min(dim(O)) - 1)))
  expect_equal(res$Effect_size, V, tolerance = 1e-8)
})

test_that("rank-biserial responds to sampling weights", {
  # Two groups, identical raw values but weights that emphasise different
  # halves -> the weighted rank-biserial must differ from the unweighted one.
  set.seed(99)
  x  <- c(1, 2, 3, 4, 5)
  y  <- c(3, 4, 5, 6, 7)
  wx <- c(10, 10, 1, 1, 1)
  wy <- c(1, 1, 1, 10, 10)
  rb_w <- svyflow:::.weighted_rank_biserial(x, wx, y, wy)
  rb_u <- svyflow:::.weighted_rank_biserial(x, rep(1, 5), y, rep(1, 5))
  expect_false(isTRUE(all.equal(rb_w, rb_u)))
  expect_gte(rb_w, -1); expect_lte(rb_w, 1)
})

test_that("result has the svyflow_summary class and prints a header", {
  des <- make_design(make_test_data(500), weights = "weight", strata = "strata")
  res <- compare_groups(des, "hh_size", "gender")
  expect_s3_class(res, "svyflow_summary")
  expect_output(print(res), "Indicator")
})
