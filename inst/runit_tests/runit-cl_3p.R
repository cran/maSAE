message("Tests for the clustered three-phase design")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(
    devtools::as.package(".")["path"], "inst", "runit_tests",
    "setup.R"
  ))
}
data(list = paste0("s", 0:2), package = "maSAE")

s0$x1 <- s0$x3 <- NULL
s012 <- bind_data(s1, s2, s0)
# adapt data for forestinventory
s012$g[is.na(s012$g)] <- "a"
s012$phase <- s012$phase1 + s012$phase2

formula.s1 <- y ~ x1 + x2 + x3
formula.s0 <- y ~ x2
tm <- aggregate(cbind(x1, x2, x3) ~ g, data = s012, "mean")
tm_partially <- tm[, -c(2, 4)]
truemeans.G <- cbind("Intercept" = 1, tm[TRUE, paste0("x", 1:3)])
truemeans.G_partially <- truemeans.G[, -c(2, 4)]
rownames(truemeans.G) <- tm$g
fitp <- forestinventory::threephase


# when we use full means, we go back to twophase.
# maSAE does so automatically, forestinventory throws an error, so we need to
# adapt the data set for forestinventory.
three_as_two <- s012
three_as_two$phase[three_as_two$phase == 0] <- 1


test_nonex_noweights <- function() {
  message("Tests for non-exhaustive auxiliary data without weights")

  maSAE <- predict(suppressMessages(saObj(
    data = s012, f = update(formula.s1, ~ . | g),
    s1 = "phase1", s2 = "phase2", cluster = "clustid"
  )),
  use_lm = FALSE
  )
  ##### % extended pseudo synthetic estimator
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- fitp(formula.s0, formula.s1,
      data = s012,
      phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
      cluster = "clustid",
      small_area = list(
        sa.col = "g", areas = c("a", "b"),
        unbiased = TRUE
      )
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
  fi <-
    structure(list(area = c("a", "b"), estimate = c(
      385.031841932114,
      404.967740283593
    ), ext_variance = c(137.417365329949, 451.0187603266), g_variance = c(144.439678550074, 493.600475190784), n0 = c(
      3446L,
      3446L
    ), n1 = c(344L, 344L), n2 = c(68L, 68L), n0G = c(
      2584L,
      862L
    ), n1G = c(242L, 102L), n2G = c(47L, 21L), r.squared_reduced = c(
      0.530634582506201,
      0.530634582506201
    ), r.squared_full = c(0.886960953238245, 0.886960953238245)), row.names = c(NA, -2L), class = "data.frame")
  RUnit::checkIdentical(clean(fi), clean(maSAE))

  ##### %  pseudo synthetic estimator
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- fitp(formula.s0, formula.s1,
      data = s012,
      phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
      cluster = "clustid",
      small_area = list(
        sa.col = "g", areas = c("a", "b"),
        unbiased = FALSE
      )
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
  # RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psynth"))
  ##### %  pseudo small area estimator
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- fitp(formula.s0, formula.s1,
      data = s012,
      phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
      cluster = "clustid",
      psmall = TRUE,
      small_area = list(
        sa.col = "g", areas = c("a", "b"),
        unbiased = TRUE
      )
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
  # RUnit::checkIdentical(clean(fi), clean(maSAE, which = "psmall"))
}
if (interactive()) test_nonex_noweights()

no_test_nonex_weights <- function() {
  message("Tests for non-exhaustive auxiliary data with weights")
  maSAE <- predict(saObj(
    data = s012, f = update(formula.s1, ~ . | g),
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

no_test_ex_weights <- function() {
  message("Tests for exhaustive auxiliary data with weights")
  old_options <- options(warn = 2)
  RUnit::checkException(predict(saObj(
    data = s012,
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
    data = s012,
    f = update(formula.s1, ~ . | g),
    s2 = "phase2",
    smallAreaMeans = tm,
    cluster = "clustid",
    auxiliaryWeights = "weight"
  ),
  use_lm = FALSE
  ))
  not_weighted <- predict(saObj(
    data = s012,
    f = update(formula.s1, ~ . | g),
    s2 = "phase2",
    cluster = "clustid",
    smallAreaMeans = tm,
    auxiliaryWeights = NULL
  ),
  use_lm = FALSE
  )
  RUnit::checkIdentical(not_weighted, weighted)
}

test_ex_noweights <- function() {
  message("Tests for exhaustive auxiliary data without weights")

  maSAE <- predict(saObj(
    data = s012,
    f = update(formula.s1, ~ . | g),
    s2 = "phase2", cluster = "clustid",
    smallAreaMeans = tm
  ),
  use_lm = FALSE
  )

  ##### % extended pseudo synthetic estimator
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- forestinventory::twophase(formula.s1,
      data = three_as_two,
      phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
      cluster = "clustid",
      exhaustive = truemeans.G,
      small_area = list(
        sa.col = "g", areas = c("a", "b"),
        unbiased = TRUE
      )
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
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
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- forestinventory::twophase(formula.s1,
      data = three_as_two,
      phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
      cluster = "clustid",
      exhaustive = truemeans.G,
      small_area = list(
        sa.col = "g", areas = c("a", "b"),
        unbiased = FALSE
      )
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
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
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- forestinventory::twophase(formula.s1,
      data = three_as_two,
      phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
      cluster = "clustid",
      exhaustive = truemeans.G,
      psmall = TRUE,
      small_area = list(
        sa.col = "g", areas = c("a", "b"),
        unbiased = TRUE
      )
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
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

no_test_partially <- function() {
  # see runit-uc_2p.R function test_partially() for an explanation.

  # here, I set tm_partially to something different than the sample mean of
  # the s0 data, else it yields exaxctly the same results as twophase, which
  # is true but confusing.
  tm_partially <- tm[, -c(2, 3)]

  not_weighted <- predict(suppressMessages(saObj(
    data = s012, f = update(formula.s1, ~ . | g),
    s1 = "phase1", s2 = "phase2",
    smallAreaMeans = tm_partially,
    cluster = "clustid"
  )),
  use_lm = FALSE
  )

  if (interactive()) {
    dump_file <- tempfile()
    expectation <- clean(not_weighted)
    dump("expectation", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
  expectation <-
    structure(c("a", "b", "388.9259", "392.3395", "117.2678", "330.1792"), .Dim = 2:3, .Dimnames = list(c("1", "2"), NULL))

  RUnit::checkIdentical(clean(not_weighted, FALSE), expectation)
  if (FALSE) {
    weighted <- predict(saObj(
      data = s012, f = update(formula.s1, ~ . | g),
      auxiliaryWeights = "weights",
      s2 = "phase2", smallAreaMeans = tm_partially, cluster = "clustid"
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
