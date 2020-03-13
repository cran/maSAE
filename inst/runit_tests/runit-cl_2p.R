message("Tests for the clustered two-phase design")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(
    devtools::as.package(".")["path"], "inst", "runit_tests",
    "setup.R"
  ))
}
data(list = paste0("s", 0:2), package = "maSAE")

s12 <- bind_data(s1, s2)
# adapt data for forestinventory
s12$g[is.na(s12$g)] <- "a"
s12$phase <- s12$phase1 + s12$phase2

# fake some weights
sum_rel_dev <- rowSums((s12[, c(paste0("x", 1:3))] -
  colMeans(s12[, c(paste0("x", 1:3))])) / colMeans(s12[, c(paste0("x", 1:3))]))
set.seed(56467)
s12$weight <- rnorm(nrow(s12), mean = 3 + sum_rel_dev, sd = 0.01)
s12$weight[s12$weight < 0] <- 0
s12$weight[s12$weight > 1] <- 1
formula.s1 <- y ~ x1 + x2 + x3
tm <- aggregate(cbind(x1, x2, x3) ~ g, data = s12, "mean")
tm_partially <- tm[, -c(2, 4)]
truemeans.G <- cbind("Intercept" = 1, tm[TRUE, paste0("x", 1:3)])
rownames(truemeans.G) <- tm$g



test_nonex_noweights <- function() {
  message("Tests for non-exhaustive auxiliary data without weights")

  maSAE <- predict(suppressMessages(saObj(
    data = s12, f = update(formula.s1, ~ . | g),
    s2 = "phase2", cluster = "clustid"
  )),
  use_lm = FALSE
  )
  ##### % extended pseudo synthetic estimator
  fi <-
    structure(list(area = c("a", "b"), estimate = c(
      388.925862054566,
      392.339527092439
    ), ext_variance = c(87.6471489397849, 288.170951914057), g_variance = c(254.329302626316, 807.322394330049), n1 = c(
      344,
      344
    ), n2 = c(68, 68), n1G = c(242, 102), n2G = c(47, 21), r.squared = c(
      0.886960953238245,
      0.886960953238245
    )), row.names = c(NA, -2L), class = "data.frame")

  RUnit::checkIdentical(clean(fi), clean(maSAE))

  ##### %  pseudo synthetic estimator
  fi <-
    structure(list(area = c("a", "b"), estimate = c(
      391.100826482191,
      387.574968963254
    ), ext_variance = c(NA_real_, NA_real_), g_variance = c(
      228.097762567531,
      575.59806719354
    ), n1 = c(344, 344), n2 = c(68, 68), n1G = c(
      242,
      102
    ), n2G = c(47, 21), r.squared = c(0.886694654412209, 0.886694654412209)), row.names = c(NA, -2L), class = "data.frame")
  RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psynth"))
  ##### %  pseudo small area estimator
  fi <-
    structure(list(area = c("a", "b"), estimate = c(
      389.072092128464,
      392.286868107391
    ), ext_variance = c(87.4395646223382, 289.159523123657), g_variance = c(315.504433559558, 864.523602876943), n1 = c(
      344,
      344
    ), n2 = c(68, 68), n1G = c(242, 102), n2G = c(47, 21), r.squared = c(
      0.886694654412209,
      0.886694654412209
    )), row.names = c(NA, -2L), class = "data.frame")
  RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psmall"))
}
if (interactive()) test_nonex_noweights()

no_test_nonex_weights <- function() {
  message("Tests for non-exhaustive auxiliary data with weights")
  maSAE <- predict(saObj(
    data = s12, f = update(formula.s1, ~ . | smallarea),
    s2 = "s2", auxiliaryWeights = "boundary_weights"
  ),
  use_lm = FALSE
  )

  # % extended pseudo synthetic estimator
  RUnit::checkIdentical(clean(fi), clean(maSAE))
  # %  pseudo synthetic estimator
  RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psynth"))
  # %  pseudo small area estimator
  RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psmall"))
}

test_ex_weights <- function() {
  message("Tests for exhaustive auxiliary data with weights")
  old_options <- options(warn = 2)
  RUnit::checkException(predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | g),
    s2 = "s2",
    smallAreaMeans = tm,
    cluster = "clustid",
    auxiliaryWeights = "boundary_weights"
  ),
  use_lm = FALSE
  ))
  options(old_options)
  suppressWarnings(weighted <- predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | g),
    s2 = "phase2",
    smallAreaMeans = tm,
    cluster = "clustid",
    auxiliaryWeights = "weight"
  ),
  use_lm = FALSE
  ))
  not_weighted <- predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | g),
    s2 = "phase2",
    cluster = "clustid",
    smallAreaMeans = tm,
    auxiliaryWeights = NULL
  ),
  use_lm = FALSE
  )
  RUnit::checkTrue(!identical(not_weighted, weighted))
}

test_ex_noweights <- function() {
  message("Tests for exhaustive auxiliary data without weights")

  maSAE <- predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | g),
    s2 = "phase2", cluster = "clustid",
    smallAreaMeans = tm
  ),
  use_lm = FALSE
  )

  ##### % extended pseudo synthetic estimator
  fi <-
    structure(list(area = c("a", "b"), estimate = c(
      388.925862054565,
      392.339527092439
    ), ext_variance = c(87.6471489397849, 288.170951914057), g_variance = c(94.0347482218164, 315.021024873811), n1 = c(
      Inf,
      Inf
    ), n2 = c(68, 68), n1G = c(Inf, Inf), n2G = c(47, 21), r.squared = c(
      0.886960953238245,
      0.886960953238245
    )), row.names = c(NA, -2L), class = "data.frame")
  RUnit::checkIdentical(clean(fi), clean(maSAE))
  ##### % pseudo synthetic estimator
  fi <-
    structure(list(area = c("a", "b"), estimate = c(
      391.100826482191,
      387.574968963254
    ), ext_variance = c(NA_real_, NA_real_), g_variance = c(
      66.8108732113946,
      80.0708154929257
    ), n1 = c(Inf, Inf), n2 = c(68, 68), n1G = c(
      Inf,
      Inf
    ), n2G = c(47, 21), r.squared = c(0.886694654412209, 0.886694654412209)), row.names = c(NA, -2L), class = "data.frame")
  RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psynth"))
  ##### % pseudo small area estimator
  fi <-
    structure(list(area = c("a", "b"), estimate = c(
      389.072092128464,
      392.286868107391
    ), ext_variance = c(87.4066709920268, 288.925535683403), g_variance = c(154.217544203421, 368.996351176329), n1 = c(
      Inf,
      Inf
    ), n2 = c(68, 68), n1G = c(Inf, Inf), n2G = c(47, 21), r.squared = c(
      0.886694654412209,
      0.886694654412209
    )), row.names = c(NA, -2L), class = "data.frame")

  RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psmall"))
}

test_partially <- function() {
  # see runit-uc_20.R function test_partially() for an explanation
  not_weighted <- predict(saObj(
    data = s12, f = update(formula.s1, ~ . | g),
    s2 = "phase2", smallAreaMeans = tm_partially, cluster = "clustid"
  ),
  use_lm = FALSE
  )
  expectation <-
    structure(list(small_area = structure(1:2, .Label = c("a", "b"), class = "factor"), prediction = c(388.925862054566, 392.339527092439), variance = c(136.100887183427, 459.497317094639), psynth = c(
      391.100826482191,
      387.574968963254
    ), var_psynth = c(103.174388911079, 109.132901007312), psmall = c(389.072092128464, 392.286868107392), var_psmall = c(
      190.581059903106,
      398.058436690715
    )), row.names = c(NA, -2L), class = "data.frame")
  RUnit::checkIdentical(clean(not_weighted, FALSE), clean(expectation, FALSE))
  if (FALSE) {
    weighted <- predict(saObj(
      data = s12, f = update(formula.s1, ~ . | g),
      s2 = "phase2", smallAreaMeans = tm_partially, cluster = "clustid",
      auxiliaryWeights = "weight"
    ),
    use_lm = FALSE
    )
    RUnit::checkIdentical(clean(not_weighted, FALSE), expectation)
    get_figs <- function(x) {
      return(apply(clean(x, FALSE)[, -1], 2, as.numeric))
    }
    RUnit::checkTrue(!isTRUE(all.equal(get_figs(not_weighted), get_figs(weighted))))
  }
}

if (FALSE) {
  ## This is the mimick I used to get fixed estimations from package
  ## forestinventory as reference.
  truemeans.G_partially <- truemeans.G[, -c(2, 4)]
  fi <- forestinventory::twophase(formula.s1,
    data = s12,
    phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
    psmall = TRUE,
    exhaustive = truemeans.G_partially,
    small_area = list(
      sa.col = "g", areas = c("a", "b"),
      unbiased = TRUE
    )
  )[["estimation"]]
  dump("fi")
}
