message("Tests for the unclustered three-phase design")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(devtools::as.package(".")["path"], "inst", "runit_tests", "setup.R"))
}
data(grisons, package = "forestinventory")

## -----------------------------------------------------------------------------
formula.rm <- formula.s0 <- tvol ~ mean # reduced model:
formula.fm <- formula.s1 <- tvol ~ mean + stddev + max + q75 # full model

## -----------------------------------------------------------------------------
truemeans.G <- data.frame(
  Intercept = rep(1, 4),
  mean = c(12.85, 12.21, 9.33, 10.45)
)
rownames(truemeans.G) <- c("A", "B", "C", "D")

## data adjustments
s12_3p <- grisons[grisons[["phase_id_3p"]] %in% c(1, 2), ]
s0 <- grisons[grisons[["phase_id_3p"]] == 0, ]
s12_3p[["s1"]] <- s12_3p[["phase_id_3p"]] %in% c(1, 2)
s12_3p[["s2"]] <- s12_3p[["phase_id_3p"]] == 2
s0[["s1"]] <- s0[["s2"]] <- FALSE
predictors_s0 <- all.vars(formula.s0)[-1]
predictors_s1 <- all.vars(formula.s1)[-1]
eval(parse(text = (paste0(
  "s0$",
  setdiff(predictors_s1, predictors_s0),
  " <- NA"
))))
s012 <- rbind(s0, s12_3p)
tm <- truemeans.G
tm[["smallarea"]] <- row.names(tm)
tm[["Intercept"]] <- NULL
fitp <- forestinventory::threephase

test_nonex_weights <- function() {
  message("Tests for non-exhaustive auxiliary data with weights")
  maSAE <- predict(saObj(
    data = s012, f = update(formula.s1, ~ . | smallarea),
    s2 = "s2", s1 = "s1", auxiliaryWeights = "boundary_weights"
  ),
  use_lm = FALSE
  )

  # % extended pseudo synthetic estimator
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- fitp(formula.s0,
      formula.s1,
      data = grisons,
      phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
      small_area = list(
        sa.col = "smallarea", areas = c("A", "B", "C", "D"),
        unbiased = TRUE
      ),
      boundary_weights = "boundary_weights"
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
  fi <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      395.188225915315,
      389.832923269855, 321.996683640107, 365.493828898401
    ), ext_variance = c(
      1901.21070248208,
      1846.99516028223, 722.74130563735, 2248.93947947105
    ), g_variance = c(
      1858.20421523992,
      1816.6551714768, 763.073102771117, 1930.68807706402
    ), n0 = c(
      306L,
      306L, 306L, 306L
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L,
      40L, 40L, 40L
    ), n0G = c(94L, 81L, 66L, 65L), n1G = c(
      38L, 34L,
      28L, 28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.545482386468315,
      0.535463726304091, 0.528229139374251, 0.533999575080949
    ), r.squared_full = c(
      0.724291342046218,
      0.717151185696584, 0.717237478898247, 0.726882000974667
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(fi), clean(maSAE))

  # %  pseudo synthetic estimator
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- fitp(formula.s0,
      formula.s1,
      data = grisons,
      phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
      small_area = list(
        sa.col = "smallarea", areas = c("A", "B", "C", "D"),
        unbiased = FALSE
      ),
      boundary_weights = "boundary_weights"
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
  fi <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      421.525203297324,
      397.199671796286, 312.711109106712, 328.677330000184
    ), ext_variance = c(
      NA_real_,
      NA_real_, NA_real_, NA_real_
    ), g_variance = c(
      725.944853381872,
      828.551187834526, 640.940025020516, 594.011884391591
    ), n0 = c(
      306L,
      306L, 306L, 306L
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L,
      40L, 40L, 40L
    ), n0G = c(94L, 81L, 66L, 65L), n1G = c(
      38L, 34L,
      28L, 28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.527362966601392,
      0.527362966601392, 0.527362966601392, 0.527362966601392
    ), r.squared_full = c(
      0.716660757889513,
      0.716660757889513, 0.716660757889513, 0.716660757889513
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  check_variance_prediciton(clean(fi), clean(maSAE, which = "psynth"))
  # %  pseudo small area estimator
  if (interactive()) {
    ## This is how I got fixed estimations from package
    ## forestinventory as reference.
    fi <- fitp(formula.s0,
      formula.s1,
      data = grisons,
      phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
      small_area = list(
        sa.col = "smallarea", areas = c("A", "B", "C", "D"),
        unbiased = TRUE
      ),
      psmall = TRUE,
      boundary_weights = "boundary_weights"
    )[["estimation"]]
    dump_file <- tempfile()
    dump("fi", file = dump_file)
    cat(sep = "\n", readLines(dump_file))
  }
  fi <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      394.970300401189,
      390.550390681263, 321.922899307449, 364.022508379404
    ), ext_variance = c(
      1903.59586455414,
      1854.69271724789, 721.724701839275, 2241.96741262956
    ), g_variance = c(
      2310.75305595557,
      1740.44340011131, 1016.95867663148, 2438.66947424925
    ), n0 = c(
      306L,
      306L, 306L, 306L
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L,
      40L, 40L, 40L
    ), n0G = c(94L, 81L, 66L, 65L), n1G = c(
      38L, 34L,
      28L, 28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.527362966601392,
      0.527362966601392, 0.527362966601392, 0.527362966601392
    ), r.squared_full = c(
      0.716660757889513,
      0.716660757889513, 0.716660757889513, 0.716660757889513
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")

  check_variance_prediciton(clean(fi), clean(maSAE, which = "psmall"))
}

test_nonex_noweights <- function() {
  message("Tests for non-exhaustive auxiliary data without weights")
  maSAE <- predict(saObj(
    data = s012, f = update(formula.s1, ~ . | smallarea),
    s2 = "s2", s1 = "s1"
  ),
  use_lm = FALSE
  )

  ##### % extended pseudo synthetic estimator

  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      393.555975137013,
      389.142873601616, 321.438459172502, 363.820033982439
    ), ext_variance = c(
      1901.21070248208,
      1846.99516028223, 722.74130563735, 2248.93947947105
    ), g_variance = c(
      1864.51840966647,
      1817.94263880365, 760.657170580104, 1930.19162909943
    ), n0 = c(
      306L,
      306L, 306L, 306L
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L,
      40L, 40L, 40L
    ), n0G = c(94L, 81L, 66L, 65L), n1G = c(
      38L, 34L,
      28L, 28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.545482386468315,
      0.535463726304091, 0.528229139374251, 0.533999575080949
    ), r.squared_full = c(
      0.724291342046218,
      0.717151185696584, 0.717237478898247, 0.726882000974667
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE))
  ##### % pseudo synthetic estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      419.881801576212,
      396.482742306776, 312.178809114443, 327.295451473312
    ), ext_variance = c(
      NA_real_,
      NA_real_, NA_real_, NA_real_
    ), g_variance = c(
      729.715808950881,
      828.268117715722, 643.881775373443, 600.752520121852
    ), n0 = c(
      306L,
      306L, 306L, 306L
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L,
      40L, 40L, 40L
    ), n0G = c(94L, 81L, 66L, 65L), n1G = c(
      38L, 34L,
      28L, 28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.527362966601392,
      0.527362966601392, 0.527362966601392, 0.527362966601392
    ), r.squared_full = c(
      0.716660757889513,
      0.716660757889513, 0.716660757889513, 0.716660757889513
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")

  check_variance_prediciton(clean(forestinventory), clean(maSAE, which = "psynth"))


  ##### % pseudo small estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      393.326898680076,
      389.833461191753, 321.39059931518, 362.640629852532
    ), ext_variance = c(
      1903.59586455414,
      1854.69271724789, 721.724701839275, 2241.96741262956
    ), g_variance = c(
      2314.52401152457,
      1740.16032999251, 1019.90042698441, 2445.41010997951
    ), n0 = c(
      306L,
      306L, 306L, 306L
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L,
      40L, 40L, 40L
    ), n0G = c(94L, 81L, 66L, 65L), n1G = c(
      38L, 34L,
      28L, 28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.527362966601392,
      0.527362966601392, 0.527362966601392, 0.527362966601392
    ), r.squared_full = c(
      0.716660757889513,
      0.716660757889513, 0.716660757889513, 0.716660757889513
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  check_variance_prediciton(clean(forestinventory), clean(maSAE, which = "psmall"))
}


full_means <- cbind(tm, stddev = 1, max = 2, q75 = 3)

test_ex_weights <- function() {
  message("Tests for exhaustive auxiliary data with weights")
  RUnit::checkException(suppressWarnings(saObj(
    data = s012, f = update(formula.s1, ~ . | smallarea),
    s1 = "s1", s2 = "s2", smallAreaMeans = full_means,
    auxiliaryWeights = "boundary_weights"
  )))
}
test_ex_noweights <- function() {
  message("Tests for exhaustive auxiliary data without weights")
  RUnit::checkException(suppressWarnings(saObj(
    data = s012, f = update(formula.s1, ~ . | smallarea),
    s1 = "s1", s2 = "s2", smallAreaMeans = full_means
  )))
}


no_test_partially_weights <- function() {
  message("Tests for partially exhaustive auxiliary data with weights")
  maSAE <- predict(saObj(
    data = s012, f = update(formula.s1, ~ . | smallarea),
    s1 = "s1", s2 = "s2", smallAreaMeans = tm,
    auxiliaryWeights = "boundary_weights"
  ), use_lm = FALSE)

  forestinventory <-
    structure(list(
      area = c("A", "B", "C", "D"),
      estimate = c(
        382.640520604325,
        368.901320772678, 325.372002265592, 388.032483229118
      ), ext_variance = c(
        1642.05511149775,
        1501.2108134244, 640.223217604633, 1961.13218937406
      ), g_variance = c(
        1518.74073444941,
        1530.57590559564, 543.268061963863, 1756.09058562859
      ), n0 = c(
        Inf,
        Inf, Inf, Inf
      ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
        40L, 40L,
        40L, 40L
      ), n0G = c(Inf, Inf, Inf, Inf), n1G = c(
        38L, 34L, 28L,
        28L
      ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
        0.545482386468315,
        0.535463726304091, 0.528229139374251, 0.533999575080949
      ), r.squared_full = c(
        0.724291342046218,
        0.717151185696584, 0.717237478898247, 0.726882000974667
      )
    ), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE))

  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      409.339018090812,
      375.460822322501, 316.134393432719, 350.794884171777
    ), ext_variance = c(
      NA_real_,
      NA_real_, NA_real_, NA_real_
    ), g_variance = c(
      410.752867668512,
      461.825004366177, 411.819554007835, 425.906714085627
    ), n0 = c(
      Inf,
      Inf, Inf, Inf
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L, 40L,
      40L, 40L
    ), n0G = c(Inf, Inf, Inf, Inf), n1G = c(
      38L, 34L, 28L,
      28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.527362966601392,
      0.527362966601392, 0.527362966601392, 0.527362966601392
    ), r.squared_full = c(
      0.716660757889513,
      0.716660757889513, 0.716660757889513, 0.716660757889513
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, "psynth"))
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      382.784115194676,
      368.811541207478, 325.346183633456, 386.140062550997
    ), ext_variance = c(
      1642.80462303232,
      1509.68001091265, 639.685552910564, 1957.49641885476
    ), g_variance = c(
      1995.56107024221,
      1373.71721664296, 787.838205618801, 2270.56430394329
    ), n0 = c(
      Inf,
      Inf, Inf, Inf
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L, 40L,
      40L, 40L
    ), n0G = c(Inf, Inf, Inf, Inf), n1G = c(
      38L, 34L, 28L,
      28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.527362966601392,
      0.527362966601392, 0.527362966601392, 0.527362966601392
    ), r.squared_full = c(
      0.716660757889513,
      0.716660757889513, 0.716660757889513, 0.716660757889513
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, "psmall"))
}

no_test_partially_noweights <- function() {
  message("Tests for partially exhaustive auxiliary data without weights")
  maSAE <- predict(saObj(
    data = s012, f = update(formula.s1, ~ . | smallarea),
    s1 = "s1", s2 = "s2", smallAreaMeans = tm
  ),
  use_lm = FALSE
  )
  forestinventory <-
    structure(list(
      area = c("A", "B", "C", "D"),
      estimate = c(
        380.798235741967,
        368.865835469893, 325.708059273519, 389.358524557748
      ), ext_variance = c(
        1642.05511149775,
        1501.2108134244, 640.223217604633, 1961.13218937406
      ), g_variance = c(
        1524.80608552508,
        1530.62155658642, 541.023475626502, 1753.99861192807
      ), n0 = c(
        Inf,
        Inf, Inf, Inf
      ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
        40L, 40L,
        40L, 40L
      ), n0G = c(Inf, Inf, Inf, Inf), n1G = c(
        38L, 34L, 28L,
        28L
      ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
        0.545482386468315,
        0.535463726304091, 0.528229139374251, 0.533999575080949
      ), r.squared_full = c(
        0.724291342046218,
        0.717151185696584, 0.717237478898247, 0.726882000974667
      )
    ), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE))
  forestinventory <-
    structure(list(
      area = c("A", "B", "C", "D"),
      estimate = c(
        407.491633714341,
        375.423701090071, 316.509083297678, 352.356794554042
      ), ext_variance = c(
        NA_real_,
        NA_real_, NA_real_, NA_real_
      ), g_variance = c(
        414.370846709973,
        461.989487009112, 414.140181991281, 431.103960616678
      ), n0 = c(
        Inf,
        Inf, Inf, Inf
      ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
        40L, 40L,
        40L, 40L
      ), n0G = c(Inf, Inf, Inf, Inf), n1G = c(
        38L, 34L, 28L,
        28L
      ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
        0.527362966601392,
        0.527362966601392, 0.527362966601392, 0.527362966601392
      ), r.squared_full = c(
        0.716660757889513,
        0.716660757889513, 0.716660757889513, 0.716660757889513
      )
    ), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, "psynth"))
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      380.936730818205,
      368.774419975048, 325.720873498416, 387.701972933262
    ), ext_variance = c(
      1642.80462303232,
      1509.68001091265, 639.685552910564, 1957.49641885476
    ), g_variance = c(
      1999.17904928367,
      1373.8816992859, 790.158833602247, 2275.76155047434
    ), n0 = c(
      Inf,
      Inf, Inf, Inf
    ), n1 = c(128L, 128L, 128L, 128L), n2 = c(
      40L, 40L,
      40L, 40L
    ), n0G = c(Inf, Inf, Inf, Inf), n1G = c(
      38L, 34L, 28L,
      28L
    ), n2G = c(12L, 11L, 8L, 9L), r.squared_reduced = c(
      0.527362966601392,
      0.527362966601392, 0.527362966601392, 0.527362966601392
    ), r.squared_full = c(
      0.716660757889513,
      0.716660757889513, 0.716660757889513, 0.716660757889513
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, "psmall"))
}


if (FALSE) {
  ## This is the mimick I used to get fixed estimations from package forestinventory.
  forestinventory <- forestinventory::threephase(formula.s0,
    formula.s1,
    data = grisons,
    phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
    small_area = list(
      sa.col = "smallarea", areas = c("A", "B", "C", "D"),
      unbiased = TRUE
    )
  )$estimation
  forestinventory <- forestinventory::threephase(formula.s0,
    formula.s1,
    data = grisons,
    phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
    small_area = list(
      sa.col = "smallarea", areas = c("A", "B", "C", "D"),
      unbiased = TRUE
    ),
    boundary_weights = "boundary_weights"
  )$estimation
  dump("forestinventory")
  forestinventory::threephase(formula.rm, formula.fm,
    data = grisons,
    phase_id = list(
      phase.col = "phase_id_3p",
      s1.id = 1, terrgrid.id = 2
    ),
    small_area = list(
      sa.col = "smallarea", areas = c("A", "B"),
      unbiased = FALSE
    ),
    exhaustive = truemeans.G
  )$estimation
  object <- saObj(
    data = s012, f = update(formula.s1, ~ . | smallarea),
    s1 = "s1", s2 = "s2", smallAreaMeans = tm
  )
  forestinventory <- forestinventory::threephase(formula.s0,
    formula.s1,
    data = grisons,
    phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
    small_area = list(
      sa.col = "smallarea", areas = c("A", "B", "C", "D"),
      unbiased = FALSE
    )
  )$estimation
  forestinventory <- forestinventory::threephase(formula.s0,
    formula.s1,
    data = grisons,
    phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
    small_area = list(
      sa.col = "smallarea", areas = c("A", "B", "C", "D"),
      unbiased = TRUE
    ),
    psmall = TRUE
  )$estimation
  dump("forestinventory")
}
