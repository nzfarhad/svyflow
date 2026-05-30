# ----------------------------------------------------------------------------
# Group comparison / significance testing
#
# compare_groups() selects and runs the right survey-design-aware test for an
# indicator and returns a tidy `svyflow_summary` tibble, one row per
# indicator. The test selected by `test = "auto"` is:
#
#   Numeric indicator:
#     2 groups
#       paired       -> paired t-test (param) / Wilcoxon signed-rank (nonparam)
#       independent  -> independent t-test (param) / Mann-Whitney U (nonparam)
#     3+ groups      -> ANOVA (param) / Kruskal-Wallis (nonparam)
#   Categorical indicator:
#     any expected cell count < 5  -> Fisher's exact (weights ignored; warned)
#     otherwise                    -> Rao-Scott chi-square (svychisq, F)
#     binary x binary              -> z-test for two proportions (opt-in only)
# ----------------------------------------------------------------------------

# Internal: significance stars from a p-value.
.sig_stars <- function(p) {
  if (is.na(p))      return("")
  if (p < 0.001)     return("***")
  if (p < 0.01)      return("**")
  if (p < 0.05)      return("*")
  ""
}

# Internal: render the group levels into a Comparison string.
.comparison_str <- function(levels) {
  if (length(levels) == 2) paste(levels, collapse = " vs ")
  else paste(levels, collapse = ", ")
}

# Internal: one-row result tibble in the public schema (minus the columns the
# wrapper fills in: Indicator, Group, Significance).
.cmp_row <- function(test, comparison, statistic = NA_real_, df = NA_real_,
                     df2 = NA_real_, estimate = NA_real_, ci_low = NA_real_,
                     ci_high = NA_real_, effect_size = NA_real_,
                     effect_size_type = NA_character_, p_value = NA_real_,
                     n = NA_integer_) {
  tibble::tibble(
    Comparison       = comparison,
    Test             = test,
    Statistic        = as.numeric(statistic),
    DF               = as.numeric(df),
    DF2              = as.numeric(df2),
    Estimate         = as.numeric(estimate),
    CI_low           = as.numeric(ci_low),
    CI_high          = as.numeric(ci_high),
    Effect_size      = as.numeric(effect_size),
    Effect_size_type = as.character(effect_size_type),
    P_value          = as.numeric(p_value),
    N                = as.integer(n)
  )
}

# Internal: the group levels present after dropping rows missing either the
# indicator or the grouping column.
.group_levels <- function(d, group) {
  sort(unique(as.character(.svy_data(d)[[group]])))
}

# Internal: number of distinct non-missing values of an indicator (used to
# decide whether a column is "binary").
.n_levels <- function(d, v) {
  length(unique(stats::na.omit(.svy_data(d)[[v]])))
}

# ----------------------------------------------------------------------------
# Test resolution
# ----------------------------------------------------------------------------

# Decide which test to run for one indicator. Throws on incompatible explicit
# requests. `glv` is the vector of group levels on complete cases.
.resolve_test <- function(d, v, group, test, paired, parametric,
                          small_sample, glv) {
  is_num  <- is.numeric(.svy_data(d)[[v]])
  n_lev   <- length(glv)
  v_lev   <- .n_levels(d, v)

  needs_numeric <- c("ttest", "paired_ttest", "wilcoxon", "anova", "kruskal")
  needs_categ   <- c("chisq", "fisher")

  if (test != "auto") {
    if (test %in% needs_numeric && !is_num) {
      cli::cli_abort(c(
        "Test {.val {test}} requires a numeric indicator.",
        "x" = "Indicator {.val {v}} is not numeric."
      ))
    }
    if (test %in% needs_categ && is_num) {
      cli::cli_abort(c(
        "Test {.val {test}} requires a categorical indicator.",
        "x" = "Indicator {.val {v}} is numeric."
      ))
    }
    if (test == "prop_z") {
      if (v_lev != 2 || n_lev != 2) {
        cli::cli_abort(c(
          "Test {.val prop_z} requires a binary indicator and a binary group.",
          "x" = "Indicator {.val {v}} has {v_lev} level{?s}; \\
                 group {.val {group}} has {n_lev} level{?s}."
        ))
      }
    }
    if (test %in% c("ttest", "wilcoxon", "paired_ttest", "prop_z") &&
        !(test == "wilcoxon" && !paired && n_lev >= 2) && n_lev != 2) {
      # All of these are two-group tests.
      cli::cli_abort(c(
        "Test {.val {test}} compares exactly two groups.",
        "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
      ))
    }
    if (test %in% c("ttest", "paired_ttest", "prop_z") && n_lev != 2) {
      cli::cli_abort(c(
        "Test {.val {test}} compares exactly two groups.",
        "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
      ))
    }
    if (test == "wilcoxon" && !paired && n_lev != 2) {
      cli::cli_abort(c(
        "Unpaired {.val wilcoxon} compares exactly two groups.",
        "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
      ))
    }
    if (test %in% c("anova", "kruskal") && n_lev < 2) {
      cli::cli_abort(c(
        "Test {.val {test}} needs at least two groups.",
        "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
      ))
    }
    if (paired && n_lev != 2) {
      cli::cli_abort(c(
        "Paired comparisons require exactly two groups.",
        "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
      ))
    }
    return(test)
  }

  # --- test == "auto" --------------------------------------------------------
  if (is_num) {
    if (paired && n_lev != 2) {
      cli::cli_abort(c(
        "Paired comparisons require exactly two groups.",
        "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
      ))
    }
    if (n_lev < 2) {
      cli::cli_abort(c(
        "A comparison needs at least two groups.",
        "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
      ))
    }
    if (n_lev == 2) {
      if (paired) return(if (parametric) "paired_ttest" else "wilcoxon")
      return(if (parametric) "ttest" else "wilcoxon")
    }
    return(if (parametric) "anova" else "kruskal")
  }

  # Categorical indicator.
  if (n_lev < 2) {
    cli::cli_abort(c(
      "A comparison needs at least two groups.",
      "x" = "Group {.val {group}} has {n_lev} level{?s} after dropping NAs."
    ))
  }
  if (!identical(small_sample, FALSE) && .has_small_cells(d, v, group)) {
    return("fisher")
  }
  "chisq"
}

# Internal: does the (unweighted) contingency table have any expected cell
# count below 5? Expected counts under independence: E_ij = R_i * C_j / N.
.has_small_cells <- function(d, v, group) {
  df  <- .svy_data(d)
  tab <- table(df[[v]], df[[group]])
  if (any(dim(tab) < 2)) return(FALSE)
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  any(expected < 5)
}

# Internal: the sampling weights of a design, aligned with .svy_data(design).
# Falls back to all-ones if the design carries no usable probabilities.
.design_weights <- function(design) {
  w <- tryCatch(as.numeric(1 / design$prob), error = function(e) NULL)
  if (is.null(w) || length(w) != nrow(.svy_data(design)) || any(!is.finite(w))) {
    w <- rep(1, nrow(.svy_data(design)))
  }
  w
}

# Internal: does the design carry real multistage structure (more than one
# stratum, or clusters with fewer PSUs than rows)? Used to warn when the
# paired pivot has to collapse that structure.
.design_has_structure <- function(design) {
  st <- design$strata
  has_strata <- !is.null(st) && length(unique(as.data.frame(st)[[1]])) > 1
  cl <- design$cluster
  has_clusters <- !is.null(cl) && ncol(as.data.frame(cl)) >= 1 &&
    length(unique(as.data.frame(cl)[[1]])) < nrow(as.data.frame(cl))
  isTRUE(has_strata) || isTRUE(has_clusters)
}

# Internal: design-consistent Cramer's V. Uses the weighted contingency
# proportions (via survey::svytable) scaled to the actual sample size n, so
# the Pearson statistic reflects the weighted association while V stays bounded
# in [0, 1]. Consistent with the design-based svychisq() used for the test.
.cramers_v_weighted <- function(d, v, group, n) {
  fml <- stats::as.formula(paste0("~ `", v, "` + `", group, "`"))
  tw  <- tryCatch(survey::svytable(fml, d), error = function(e) NULL)
  if (is.null(tw)) return(NA_real_)
  tw <- as.matrix(tw)
  if (any(dim(tw) < 2) || sum(tw) <= 0) return(NA_real_)
  O <- tw / sum(tw) * n                       # weighted proportions -> counts
  E <- outer(rowSums(O), colSums(O)) / n
  chi2 <- sum((O - E)^2 / E)
  sqrt(chi2 / (n * (min(dim(O)) - 1)))
}

# Internal: weighted estimate of P(Y > X) + 0.5 P(Y == X) from two weighted
# samples. O((n1 + n2) log) via cumulative x-weights with explicit tie
# handling. Used for the design-consistent rank-biserial effect size.
.weighted_pgt <- function(x, wx, y, wy) {
  ag  <- tapply(wx, x, sum)
  ux  <- as.numeric(names(ag))
  o   <- order(ux)
  ux  <- ux[o]
  agw <- as.numeric(ag)[o]
  cw0 <- c(0, cumsum(agw))                     # cw0[k+1] = weight of x <= ux[k]
  idx <- findInterval(y, ux)                   # ux[idx] <= y < ux[idx + 1]
  w_le <- cw0[idx + 1]                          # weight of x <= y
  m    <- match(y, ux)
  w_eq <- ifelse(is.na(m), 0, agw[m])           # weight of x == y
  w_lt <- w_le - w_eq                           # weight of x  < y
  sum(wy * (w_lt + 0.5 * w_eq)) / (sum(wx) * sum(wy))
}

# Internal: design-consistent rank-biserial correlation, r = 1 - 2 P(Y > X),
# with P(Y > X) estimated from the sampling weights.
.weighted_rank_biserial <- function(x, wx, y, wy) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  1 - 2 * .weighted_pgt(x, wx, y, wy)
}

# ----------------------------------------------------------------------------
# Per-test implementations. Each returns a one-row tibble from .cmp_row().
# `d` is the design filtered to rows with both the indicator and group present.
# ----------------------------------------------------------------------------

# Independent t-test. Computed via svyglm so the CI level / df bundle is fully
# honoured (svyttest fixes those internally); the t-statistic, df and p-value
# are identical to survey::svyttest() on the same design.
.compare_ttest <- function(d, v, group, glv, n, ci) {
  fml  <- stats::as.formula(paste0("`", v, "` ~ `", group, "`"))
  fit  <- survey::svyglm(fml, design = d, family = stats::gaussian())
  est  <- unname(stats::coef(fit)[2])
  se   <- unname(survey::SE(fit)[2])
  # svyglm's residual df is what survey::svyttest() reports (degf - 1 for a
  # two-group test); use it so Statistic / DF / P_value match svyttest exactly.
  dfree <- if (is.null(ci$df)) fit$df.residual else ci$df
  tstat <- est / se
  pval  <- 2 * stats::pt(-abs(tstat), dfree)
  mult  <- stats::qt((1 + ci$ci_level) / 2, dfree)

  # Pooled, design-aware Cohen's d.
  s <- dplyr::summarise(
    srvyr::group_by(d, !!rlang::sym(group)),
    m = srvyr::survey_mean(as.numeric(.data[[v]]), na.rm = TRUE),
    v = srvyr::survey_var(as.numeric(.data[[v]]), na.rm = TRUE),
    n = srvyr::unweighted(dplyr::n())
  )
  pooled_var <- sum((s$n - 1) * s$v) / (sum(s$n) - 2)
  d_eff <- if (isTRUE(pooled_var > 0)) est / sqrt(pooled_var) else NA_real_

  .cmp_row("ttest", .comparison_str(glv),
           statistic = tstat, df = dfree,
           estimate = est, ci_low = est - mult * se, ci_high = est + mult * se,
           effect_size = d_eff, effect_size_type = "Cohen_d",
           p_value = pval, n = n)
}

# Paired t-test. Pivots to per-pair differences, rebuilds a weighted design on
# the pairs, then tests mean(diff) = 0.
.compare_paired_ttest <- function(design, v, group, glv, pair_by, ci) {
  pd   <- .build_pair_design(design, v, group, pair_by)
  pdes <- pd$design
  m    <- survey::svymean(~diff, pdes, na.rm = TRUE)
  est  <- unname(stats::coef(m)[1])
  se   <- unname(survey::SE(m)[1])
  # Matches survey::svyttest(diff ~ 0, ...): the one-sample design-based
  # t-test uses degf(design) - 1.
  dfree <- if (is.null(ci$df)) survey::degf(pdes) - 1 else ci$df
  tstat <- est / se
  pval  <- 2 * stats::pt(-abs(tstat), dfree)
  mult  <- stats::qt((1 + ci$ci_level) / 2, dfree)

  sd_diff <- sqrt(as.numeric(survey::svyvar(~diff, pdes, na.rm = TRUE))[1])
  d_eff   <- if (isTRUE(sd_diff > 0)) est / sd_diff else NA_real_

  .cmp_row("paired_ttest", .comparison_str(glv),
           statistic = tstat, df = dfree,
           estimate = est, ci_low = est - mult * se, ci_high = est + mult * se,
           effect_size = d_eff, effect_size_type = "Cohen_d",
           p_value = pval, n = pd$n_pairs)
}

# Mann-Whitney U (unpaired) or signed-rank (paired).
.compare_wilcoxon <- function(d, design, v, group, glv, n, paired, pair_by) {
  if (paired) return(.compare_wilcoxon_paired(design, v, group, glv, pair_by))

  fml <- stats::as.formula(paste0("`", v, "` ~ `", group, "`"))
  res <- survey::svyranktest(fml, design = d, test = "wilcoxon")

  df  <- .svy_data(d)
  w   <- .design_weights(d)
  gx  <- as.character(df[[group]]) == glv[1]
  gy  <- as.character(df[[group]]) == glv[2]
  rb  <- tryCatch(
    .weighted_rank_biserial(as.numeric(df[[v]])[gx], w[gx],
                            as.numeric(df[[v]])[gy], w[gy]),
    error = function(e) NA_real_)

  # Estimate is the difference of the design-weighted group medians (not the
  # Hodges-Lehmann estimator); see the Effect sizes section in ?compare_groups.
  med_diff <- tryCatch({
    s  <- dplyr::summarise(
      srvyr::group_by(d, !!rlang::sym(group)),
      med = srvyr::survey_median(as.numeric(.data[[v]]), na.rm = TRUE,
                                 vartype = NULL)
    )
    mv <- stats::setNames(s$med, as.character(s[[group]]))
    unname(mv[[glv[2]]] - mv[[glv[1]]])
  }, error = function(e) NA_real_)

  .cmp_row("wilcoxon", .comparison_str(glv),
           statistic = unname(res$statistic), df = unname(res$parameter),
           estimate = med_diff,
           effect_size = rb, effect_size_type = "Rank_biserial",
           p_value = res$p.value, n = n)
}

# Wilcoxon signed-rank on per-pair differences (design-weighted rank test on
# |diff| split by the sign of diff).
.compare_wilcoxon_paired <- function(design, v, group, glv, pair_by) {
  pd    <- .build_pair_design(design, v, group, pair_by)
  diffs <- pd$diffs
  nz    <- diffs[diffs != 0]

  if (length(unique(nz > 0)) < 2) {
    # All non-zero differences share a sign: the two-group rank test is
    # undefined. Report the matched-pairs rank-biserial and a NA test.
    stat <- NA_real_; dff <- NA_real_; pval <- NA_real_
  } else {
    pf   <- data.frame(abs_diff = abs(diffs),
                       pos      = factor(diffs > 0),
                       .w       = pd$w)
    des2 <- make_design(pf, weights = ".w")
    res  <- survey::svyranktest(abs_diff ~ pos, des2, test = "wilcoxon")
    stat <- unname(res$statistic); dff <- unname(res$parameter)
    pval <- res$p.value
  }

  # Matched-pairs rank-biserial correlation, with each pair's rank
  # contribution weighted by its sampling weight (ranks themselves are
  # unweighted; documented as an approximation in ?compare_groups).
  w_nz <- pd$w[diffs != 0]
  rr   <- rank(abs(nz))
  Rtot <- sum(w_nz * rr)
  rb   <- if (Rtot > 0)
            (sum((w_nz * rr)[nz > 0]) - sum((w_nz * rr)[nz < 0])) / Rtot
          else NA_real_

  .cmp_row("wilcoxon", .comparison_str(glv),
           statistic = stat, df = dff,
           estimate = stats::median(diffs),
           effect_size = rb, effect_size_type = "Rank_biserial",
           p_value = pval, n = pd$n_pairs)
}

# Design-based one-way ANOVA via svyglm + regTermTest (Wald F).
.compare_anova <- function(d, v, group, glv, n, ci) {
  fml <- stats::as.formula(paste0("`", v, "` ~ `", group, "`"))
  fit <- survey::svyglm(fml, design = d, family = stats::gaussian())
  dfree <- if (is.null(ci$df)) survey::degf(d) else ci$df
  rt  <- survey::regTermTest(fit, group, df = dfree)

  # Approximate, design-informed eta-squared from per-group weighted means /
  # variances and unweighted group sizes.
  s <- dplyr::summarise(
    srvyr::group_by(d, !!rlang::sym(group)),
    m = srvyr::survey_mean(as.numeric(.data[[v]]), na.rm = TRUE),
    v = srvyr::survey_var(as.numeric(.data[[v]]), na.rm = TRUE),
    n = srvyr::unweighted(dplyr::n())
  )
  N         <- sum(s$n)
  m_overall <- sum(s$n * s$m) / N
  ss_b      <- sum(s$n * (s$m - m_overall)^2)
  ss_w      <- sum((s$n - 1) * s$v)
  eta_sq    <- if ((ss_b + ss_w) > 0) ss_b / (ss_b + ss_w) else NA_real_

  .cmp_row("anova", .comparison_str(glv),
           statistic = unname(rt$Ftest), df = unname(rt$df),
           df2 = unname(rt$ddf),
           effect_size = eta_sq, effect_size_type = "Eta_sq",
           p_value = unname(rt$p), n = n)
}

# Kruskal-Wallis rank test (design-based).
.compare_kruskal <- function(d, v, group, glv, n) {
  fml <- stats::as.formula(paste0("`", v, "` ~ `", group, "`"))
  res <- survey::svyranktest(fml, design = d, test = "KruskalWallis")
  H   <- unname(res$statistic)
  eps_sq <- if (n > 1) H * (n + 1) / (n^2 - 1) else NA_real_

  .cmp_row("kruskal", .comparison_str(glv),
           statistic = H, df = unname(res$parameter),
           effect_size = eps_sq, effect_size_type = "Epsilon_sq",
           p_value = res$p.value, n = n)
}

# Rao-Scott chi-square (default categorical test). Effect size is a
# design-consistent Cramer's V from the weighted contingency proportions.
.compare_chisq <- function(d, v, group, glv, n) {
  fml <- stats::as.formula(paste0("~ `", v, "` + `", group, "`"))
  res <- survey::svychisq(fml, design = d, statistic = "F")
  V   <- .cramers_v_weighted(d, v, group, n)

  .cmp_row("chisq", .comparison_str(glv),
           statistic = unname(res$statistic),
           df = unname(res$parameter[1]), df2 = unname(res$parameter[2]),
           effect_size = V, effect_size_type = "Cramer_V",
           p_value = res$p.value, n = n)
}

# Fisher's exact test on the unweighted table (no survey-aware Fisher exists;
# weights are ignored, which is documented and warned about by the caller).
.compare_fisher <- function(d, v, group, glv, n) {
  df  <- .svy_data(d)
  tab <- table(df[[v]], df[[group]])

  if (all(dim(tab) == 2)) {
    res <- stats::fisher.test(tab)
    .cmp_row("fisher", .comparison_str(glv),
             estimate = unname(res$estimate),
             ci_low = res$conf.int[1], ci_high = res$conf.int[2],
             effect_size = unname(res$estimate),
             effect_size_type = "Odds_ratio",
             p_value = res$p.value, n = n)
  } else {
    res <- stats::fisher.test(tab, simulate.p.value = TRUE)
    chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
    V   <- sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
    .cmp_row("fisher", .comparison_str(glv),
             effect_size = unname(V), effect_size_type = "Cramer_V",
             p_value = res$p.value, n = n)
  }
}

# Two-proportion z-test (binary indicator x binary group). Implemented as a
# t-test on a 0/1 recode with df = Inf so the t collapses to a z.
.compare_prop_z <- function(d, v, group, glv, n, ci) {
  uv  <- sort(unique(.svy_data(d)[[v]]))
  ref <- uv[2]
  d2  <- dplyr::mutate(d, .bin = as.numeric(.data[[v]] == ref))

  fml <- stats::as.formula(paste0(".bin ~ `", group, "`"))
  fit <- survey::svyglm(fml, design = d2, family = stats::gaussian())
  est <- unname(stats::coef(fit)[2])
  se  <- unname(survey::SE(fit)[2])
  zstat <- est / se
  pval  <- 2 * stats::pnorm(-abs(zstat))
  mult  <- stats::qnorm((1 + ci$ci_level) / 2)

  s <- dplyr::summarise(
    srvyr::group_by(d2, !!rlang::sym(group)),
    p = srvyr::survey_mean(.data$.bin, na.rm = TRUE)
  )
  pv <- stats::setNames(s$p, as.character(s[[group]]))
  p1 <- pv[[glv[1]]]; p2 <- pv[[glv[2]]]
  phi <- function(p) 2 * asin(sqrt(pmin(pmax(p, 0), 1)))
  h   <- phi(p2) - phi(p1)

  .cmp_row("prop_z", .comparison_str(glv),
           statistic = zstat, df = NA_real_,
           estimate = est, ci_low = est - mult * se, ci_high = est + mult * se,
           effect_size = h, effect_size_type = "Cohen_h",
           p_value = pval, n = n)
}

# ----------------------------------------------------------------------------
# Paired-design builder
# ----------------------------------------------------------------------------

# Pivot to per-pair differences and rebuild a weighted design on the pairs.
# The multistage structure (strata / clusters / FPC) is collapsed: per-pair
# FPC is undefined in the panel case, so the paired view is treated as a
# weighted sample of independent pair differences. The per-pair weight is the
# first level's weight (a warning fires if the two levels disagree).
.build_pair_design <- function(design, v, group, pair_by) {
  df    <- .svy_data(design)
  w_vec <- tryCatch(as.numeric(1 / design$prob), error = function(e) NULL)
  if (is.null(w_vec) || length(w_vec) != nrow(df)) w_vec <- rep(1, nrow(df))

  if (.design_has_structure(design)) {
    cli::cli_warn(paste0("Paired test collapses the design to weighted ",
                         "independent pair differences; strata / clusters ",
                         "are dropped, so the SE may be anti-conservative if ",
                         "pairs were clustered."))
  }

  keep <- !is.na(df[[pair_by]]) & !is.na(df[[group]]) &
          !is.na(df[[v]]) & !is.na(w_vec)
  g    <- as.character(df[[group]])[keep]
  val  <- as.numeric(df[[v]])[keep]
  pid  <- as.character(df[[pair_by]])[keep]
  wv   <- w_vec[keep]

  lvls <- sort(unique(g))
  if (length(lvls) != 2) {
    cli::cli_abort(c(
      "Paired comparisons require exactly two groups.",
      "x" = "Group {.val {group}} has {length(lvls)} level{?s} on complete pairs."
    ))
  }
  l1 <- lvls[1]; l2 <- lvls[2]

  diffs <- numeric(0); ws <- numeric(0); warn_w <- FALSE
  for (p in unique(pid)) {
    idx <- pid == p
    g_p <- g[idx]; val_p <- val[idx]; w_p <- wv[idx]
    if (!(l1 %in% g_p) || !(l2 %in% g_p)) next
    v1 <- val_p[g_p == l1][1]; v2 <- val_p[g_p == l2][1]
    w1 <- w_p[g_p == l1][1];   w2 <- w_p[g_p == l2][1]
    if (!isTRUE(all.equal(w1, w2))) warn_w <- TRUE
    diffs <- c(diffs, v2 - v1)
    ws    <- c(ws, w1)
  }
  if (warn_w) {
    cli::cli_warn(paste0("Weights differ within some pairs; using the first ",
                         "level's weight per pair."))
  }
  if (length(diffs) < 2) {
    cli::cli_abort(c(
      "Not enough complete pairs to run a paired test.",
      "i" = "Found {length(diffs)} pair{?s} with both group levels present."
    ))
  }

  pair_df <- data.frame(diff = diffs, .w = ws)
  list(design  = make_design(pair_df, weights = ".w"),
       n_pairs = length(diffs),
       diffs   = diffs,
       w       = ws)
}

# ----------------------------------------------------------------------------
# Validation
# ----------------------------------------------------------------------------

.validate_compare_args <- function(df, variables, group, test, paired,
                                   pair_by, parametric, small_sample,
                                   variable_labels, group_label) {
  if (!is.character(variables) || length(variables) < 1) {
    cli::cli_abort("`variables` must be a character vector of length >= 1.")
  }
  bad_v <- setdiff(variables, names(df))
  if (length(bad_v) > 0) {
    cli::cli_abort(c("Indicator column(s) not found in the data:",
                     "x" = "{.val {bad_v}}"))
  }
  if (!is.character(group) || length(group) != 1) {
    cli::cli_abort("`group` must be a single column name.")
  }
  if (!(group %in% names(df))) {
    cli::cli_abort(c("Grouping column not found in the data:",
                     "x" = "{.val {group}}"))
  }
  if (!is.logical(paired) || length(paired) != 1 || is.na(paired)) {
    cli::cli_abort("`paired` must be a single TRUE or FALSE.")
  }
  if (!is.logical(parametric) || length(parametric) != 1 || is.na(parametric)) {
    cli::cli_abort("`parametric` must be a single TRUE or FALSE.")
  }
  if (!is.null(pair_by)) {
    if (!is.character(pair_by) || length(pair_by) != 1) {
      cli::cli_abort("`pair_by` must be a single column name (or NULL).")
    }
    if (!(pair_by %in% names(df))) {
      cli::cli_abort(c("`pair_by` column not found in the data:",
                       "x" = "{.val {pair_by}}"))
    }
  }
  if (!(identical(small_sample, "auto") || isTRUE(small_sample) ||
        isFALSE(small_sample))) {
    cli::cli_abort('`small_sample` must be "auto", TRUE, or FALSE.')
  }
  if (!is.null(variable_labels)) {
    if (!is.character(variable_labels) || is.null(names(variable_labels))) {
      cli::cli_abort("`variable_labels` must be a named character vector.")
    }
    bad_n <- setdiff(names(variable_labels), variables)
    if (length(bad_n) > 0) {
      cli::cli_abort(c("`variable_labels` names must be among `variables`:",
                       "x" = "{.val {bad_n}}"))
    }
  }
  if (!is.null(group_label) &&
      (!is.character(group_label) || length(group_label) != 1)) {
    cli::cli_abort("`group_label` must be a single string (or NULL).")
  }
  invisible(TRUE)
}

# ----------------------------------------------------------------------------
# Public entry point
# ----------------------------------------------------------------------------

#' Compare groups with the right survey-design-aware significance test
#'
#' Selects and runs an appropriate hypothesis test for each indicator,
#' comparing it across the levels of a grouping variable, and returns a tidy
#' one-row-per-indicator table. With `test = "auto"` (the default) the test is
#' chosen from the indicator type, the number of groups, and the `paired` /
#' `parametric` flags:
#'
#' \describe{
#'   \item{Numeric indicator, 2 groups, independent}{`ttest` (parametric) or
#'     `wilcoxon` (Mann-Whitney U).}
#'   \item{Numeric indicator, 2 groups, paired}{`paired_ttest` (parametric) or
#'     paired `wilcoxon` (signed-rank). Requires `pair_by`.}
#'   \item{Numeric indicator, 3+ groups}{`anova` (parametric) or `kruskal`
#'     (Kruskal-Wallis).}
#'   \item{Categorical indicator}{`chisq` (Rao-Scott F chi-square). Switches
#'     to `fisher` (Fisher's exact) when any expected cell count is below 5
#'     and `small_sample != FALSE`.}
#' }
#'
#' Any branch can be forced with `test`. `prop_z` (a two-proportion z-test) is
#' never picked automatically -- it is mathematically a 2x2 chi-square -- and
#' must be requested explicitly.
#'
#' All tests except `fisher` respect the survey design (weights, strata,
#' clusters). Fisher's exact has no survey-aware form, so it is run on the
#' **unweighted** contingency table; a note is emitted when it is used.
#'
#' @param design A [srvyr::tbl_svy] survey design (typically from
#'   [make_design()]).
#' @param variables Character vector of one or more indicator column names.
#' @param group Character. The grouping column name.
#' @param test Which test to run. `"auto"` (default) resolves per the decision
#'   tree above; otherwise one of `"ttest"`, `"paired_ttest"`, `"wilcoxon"`,
#'   `"anova"`, `"kruskal"`, `"chisq"`, `"fisher"`, `"prop_z"`.
#' @param paired Logical. For a numeric two-group comparison, run the paired
#'   variant. Requires `pair_by`. Default `FALSE`.
#' @param pair_by Character. The per-unit identifier column linking the two
#'   observations of a pair (e.g. a household ID measured at baseline and
#'   endline). Required when the resolved test is paired.
#' @param parametric Logical. Use the parametric numeric test (t-test / ANOVA)
#'   rather than its rank-based counterpart (Wilcoxon / Kruskal-Wallis).
#'   Default `TRUE`. Ignored for categorical indicators.
#' @param small_sample `"auto"` (default), `TRUE`, or `FALSE`. When `"auto"`
#'   or `TRUE`, a categorical comparison switches from chi-square to Fisher's
#'   exact if any expected cell count is below 5. `FALSE` always keeps the
#'   chi-square.
#' @param variable_labels Optional named character vector of display labels;
#'   names must be a subset of `variables`. Used for the `Indicator` column.
#' @param group_label Optional single string used for the `Group` column.
#'   Defaults to the `group` column name.
#' @param digits Number of decimal places for the rounding applied to the
#'   numeric output columns. Default `3`.
#' @param ci A [ci_opts()] bundle. `ci_level` and `df` feed the t / paired-t
#'   confidence intervals and p-values (e.g. `ci_opts(df = Inf)` for a
#'   normal-approximation interval). Default `ci_opts()`.
#'
#' @return A tibble of class `svyflow_summary`, one row per indicator, with
#'   columns `Indicator`, `Group`, `Comparison`, `Test`, `Statistic`, `DF`,
#'   `DF2`, `Estimate`, `CI_low`, `CI_high`, `Effect_size`,
#'   `Effect_size_type`, `P_value`, `Significance`, `N`.
#'
#' @section Test statistics and degrees of freedom:
#'   The `Statistic`, `DF`, `DF2` and `P_value` columns come straight from the
#'   design-based engine (`survey::svyttest()`, `svyglm()` +
#'   `regTermTest()`, `svychisq()`, `svyranktest()`), so weights, strata and
#'   clusters are fully respected. The independent t-test matches
#'   `survey::svyttest()` exactly. `prop_z` is, by definition, a normal-theory
#'   z-test (`DF = NA`): with few PSUs a design-df t-test (`test = "ttest"`)
#'   is the safer choice. Fisher's exact has no survey-aware form, so it runs
#'   on the **unweighted** table and a warning is emitted when it is selected
#'   automatically.
#'
#' @section Effect sizes:
#'   Effect sizes are descriptive magnitudes reported alongside each test.
#'   Most are design-consistent; two are documented approximations:
#'   \describe{
#'     \item{`Cohen_d` (t-test, paired t-test)}{Mean difference over the
#'       pooled standard deviation. The group variances are design-based
#'       (`srvyr::survey_var()`); they are pooled using the unweighted group
#'       degrees of freedom, so `d` is a design-informed but not fully
#'       design-weighted standardisation.}
#'     \item{`Rank_biserial` (Wilcoxon)}{Design-consistent: \eqn{r = 1 - 2
#'       \hat{P}(Y > X)} for the unpaired case, with \eqn{\hat{P}} estimated
#'       from the sampling weights. The paired case weights each pair's signed
#'       rank contribution by its sampling weight (the ranks themselves are
#'       unweighted -- an approximation).}
#'     \item{`Eta_sq` (ANOVA)}{An **approximate** design-informed
#'       eta-squared from per-group design-based means and variances combined
#'       with unweighted group sizes. For designs with highly variable weights
#'       it can over- or understate the effect; treat it as indicative.}
#'     \item{`Epsilon_sq` (Kruskal-Wallis)}{Derived from the design-based
#'       \eqn{H} statistic.}
#'     \item{`Cramer_V` (chi-square)}{Design-consistent: computed from the
#'       weighted contingency proportions (scaled to `n`), bounded in
#'       \eqn{[0, 1]}. For the Fisher r x c fallback it is computed from the
#'       unweighted table, matching the unweighted test.}
#'     \item{`Odds_ratio` (Fisher 2x2), `Cohen_h` (prop_z)}{From the Fisher
#'       result and the two design-weighted proportions respectively.}
#'   }
#'   For the Wilcoxon test, `Estimate` is the difference of the design-weighted
#'   group medians, not the Hodges-Lehmann estimator.
#'
#' @examples
#' df  <- data.frame(
#'   hh_size = stats::rpois(200, 5) + 1,
#'   gender  = sample(c("m", "f"), 200, TRUE),
#'   region  = sample(c("n", "s", "e"), 200, TRUE),
#'   weight  = runif(200, 0.5, 2)
#' )
#' des <- make_design(df, weights = "weight")
#'
#' # Auto: numeric x 2 groups -> t-test
#' compare_groups(des, "hh_size", "gender")
#'
#' # Auto: numeric x 3 groups -> ANOVA
#' compare_groups(des, "hh_size", "region")
#'
#' # Rank-based variants
#' compare_groups(des, "hh_size", "gender", parametric = FALSE)
#'
#' @seealso [analyze_survey()], [summarize_mean()], [ci_opts()]
#' @export
compare_groups <- function(design,
                           variables,
                           group,
                           test = c("auto", "ttest", "paired_ttest",
                                    "wilcoxon", "anova", "kruskal",
                                    "chisq", "fisher", "prop_z"),
                           paired       = FALSE,
                           pair_by      = NULL,
                           parametric   = TRUE,
                           small_sample = "auto",
                           variable_labels = NULL,
                           group_label  = NULL,
                           digits       = 3,
                           ci           = ci_opts()) {
  test <- match.arg(test)
  ci   <- .as_ci_opts(ci)
  df   <- .svy_data(design)

  .validate_compare_args(df, variables, group, test, paired, pair_by,
                         parametric, small_sample, variable_labels,
                         group_label)

  group_disp <- if (is.null(group_label)) group else group_label

  rows <- purrr::map_dfr(variables, function(v) {
    # Drop rows missing either the indicator or the grouping column.
    d   <- srvyr::filter(design,
                         !is.na(.data[[v]]) & !is.na(.data[[group]]))
    glv <- .group_levels(d, group)
    if (length(glv) < 1) {
      cli::cli_abort(c(
        "No non-missing data to compare for indicator {.val {v}}."
      ))
    }
    n_use <- nrow(.svy_data(d))

    resolved <- .resolve_test(d, v, group, test, paired, parametric,
                              small_sample, glv)

    # Paired tests need pair_by.
    if (resolved %in% c("paired_ttest") ||
        (resolved == "wilcoxon" && paired)) {
      if (is.null(pair_by)) {
        cli::cli_abort(c(
          "A paired comparison needs `pair_by`.",
          "x" = "Resolved test {.val {resolved}} for {.val {v}} is paired \\
                 but `pair_by` is NULL."
        ))
      }
    }

    if (resolved == "fisher" && test == "auto") {
      cli::cli_warn(c(
        "!" = "{.val {v}}: expected cell count < 5; using Fisher's exact \\
               test on the unweighted table (survey weights ignored)."
      ))
    }

    out <- switch(
      resolved,
      ttest        = .compare_ttest(d, v, group, glv, n_use, ci),
      paired_ttest = .compare_paired_ttest(design, v, group, glv, pair_by, ci),
      wilcoxon     = .compare_wilcoxon(d, design, v, group, glv, n_use,
                                       paired, pair_by),
      anova        = .compare_anova(d, v, group, glv, n_use, ci),
      kruskal      = .compare_kruskal(d, v, group, glv, n_use),
      chisq        = .compare_chisq(d, v, group, glv, n_use),
      fisher       = .compare_fisher(d, v, group, glv, n_use),
      prop_z       = .compare_prop_z(d, v, group, glv, n_use, ci)
    )

    ind <- if (!is.null(variable_labels) && v %in% names(variable_labels))
             unname(variable_labels[[v]]) else v
    out$Indicator <- ind
    out$Group     <- group_disp
    out
  })

  # Round the numeric columns, add significance stars, set column order.
  num_cols <- c("Statistic", "DF", "DF2", "Estimate", "CI_low", "CI_high",
                "Effect_size", "P_value")
  if (!is.null(digits)) {
    for (cc in num_cols) rows[[cc]] <- round(rows[[cc]], digits)
  }
  rows$Significance <- vapply(rows$P_value, .sig_stars, character(1))

  ordered <- rows[, c("Indicator", "Group", "Comparison", "Test",
                      "Statistic", "DF", "DF2", "Estimate", "CI_low",
                      "CI_high", "Effect_size", "Effect_size_type",
                      "P_value", "Significance", "N"), drop = FALSE]

  new_svyflow_summary(ordered)
}
