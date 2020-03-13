message("Tests for the unclustered two-phase design")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(devtools::as.package(".")["path"], "inst", "runit_tests", "setup.R"))
}

data(grisons, package = "forestinventory")

## -----------------------------------------------------------------------------
formula.s0 <- tvol ~ mean # reduced model:
formula.s1 <- tvol ~ mean + stddev + max + q75 # full model


## -----------------------------------------------------------------------------
truemeans.G <- data.frame(
  Intercept = rep(1, 4),
  mean = c(12.85, 12.21, 9.33, 10.45),
  stddev = c(9.31, 9.47, 7.90, 8.36),
  max = c(34.92, 35.36, 28.81, 30.22),
  q75 = c(19.77, 19.16, 15.40, 16.91)
)
rownames(truemeans.G) <- c("A", "B", "C", "D")

# data adjustments
s1 <- grisons[grisons[["phase_id_2p"]] == 1, ]
s2 <- grisons[grisons[["phase_id_2p"]] == 2, ]
s12 <- rbind(s1, s2)
s12$s1 <- s12$phase_id_2p %in% c(1, 2)
s12$s2 <- s12$phase_id_2p == 2

tm <- truemeans.G
tm[["smallarea"]] <- row.names(tm)
tm[["Intercept"]] <- NULL
tm_partially <- tm[, -c(1:3)]

test_nonex_weights <- function() {
  message("Tests for non-exhaustive auxiliary data with weights")
  maSAE <- predict(saObj(
    data = s12, f = update(formula.s1, ~ . | smallarea),
    s2 = "s2", auxiliaryWeights = "boundary_weights"
  ),
  use_lm = FALSE
  )

  # % extended pseudo synthetic estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      391.935649149001,
      419.723068204099, 328.859956933459, 373.949709760812
    ), ext_variance = c(
      995.560244435755,
      1214.60533511289, 916.226556579646, 1272.70556994598
    ), g_variance = c(
      1017.63272121133,
      1019.19125748109, 1036.7911545727, 1110.24538462587
    ), n1 = c(
      306,
      306, 306, 306
    ), n2 = c(67, 67, 67, 67), n1G = c(94, 81, 66, 65), n2G = c(19, 17, 15, 16), r.squared = c(
      0.652650335602544,
      0.642885419397165, 0.643001792107126, 0.655617766440169
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")

  RUnit::checkIdentical(clean(forestinventory), clean(maSAE))

  # %  pseudo synthetic estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      421.886277346691,
      418.73992333989, 332.735077222885, 334.503615184589
    ), ext_variance = c(
      NA_real_,
      NA_real_, NA_real_, NA_real_
    ), g_variance = c(
      546.865129856003,
      566.336052185175, 491.776216244558, 417.031538591464
    ), n1 = c(
      306,
      306, 306, 306
    ), n2 = c(67, 67, 67, 67), n1G = c(94, 81, 66, 65), n2G = c(19, 17, 15, 16), r.squared = c(
      0.642877054009429,
      0.642877054009429, 0.642877054009429, 0.642877054009429
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, which = "psynth"))
  # %  pseudo small area estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      393.971278413612,
      419.641588226808, 328.898758907616, 370.291205895834
    ), ext_variance = c(
      1009.03354558997,
      1214.03537755011, 919.879850597901, 1299.64291904742
    ), g_variance = c(
      1308.1174516816,
      1259.47226248986, 1334.89931620916, 1393.07961144221
    ), n1 = c(
      306,
      306, 306, 306
    ), n2 = c(67, 67, 67, 67), n1G = c(94, 81, 66, 65), n2G = c(19, 17, 15, 16), r.squared = c(
      0.642877054009429,
      0.642877054009429, 0.642877054009429, 0.642877054009429
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, which = "psmall"))
}

test_nonex_noweights <- function() {
  message("Tests for non-exhaustive auxiliary data without weights")

  maSAE <- predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | smallarea),
    s2 = "s2"
  ), use_lm = FALSE)
  ##### % extended pseudo synthetic estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      391.160515610514,
      419.67462884089, 328.011650582291, 371.059582784188
    ), ext_variance = c(
      995.560244435755,
      1214.60533511289, 916.226556579646, 1272.70556994598
    ), g_variance = c(
      1016.95574515815,
      1019.26980564541, 1035.09075526481, 1112.73456000826
    ), n1 = c(
      306,
      306, 306, 306
    ), n2 = c(67, 67, 67, 67), n1G = c(94, 81, 66, 65), n2G = c(19, 17, 15, 16), r.squared = c(
      0.652650335602544,
      0.642885419397165, 0.643001792107126, 0.655617766440169
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")

  RUnit::checkIdentical(clean(forestinventory), clean(maSAE))

  ##### %  pseudo synthetic estimator

  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      421.05550455786,
      418.690833706389, 331.88706364263, 331.640938942728
    ), ext_variance = c(
      NA_real_,
      NA_real_, NA_real_, NA_real_
    ), g_variance = c(
      547.910365640619,
      564.478236805245, 492.814550605055, 417.794180319389
    ), n1 = c(
      306,
      306, 306, 306
    ), n2 = c(67, 67, 67, 67), n1G = c(94, 81, 66, 65), n2G = c(19, 17, 15, 16), r.squared = c(
      0.642877054009429,
      0.642877054009429, 0.642877054009429, 0.642877054009429
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, which = "psynth"))
  ##### %  pseudo small area estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      393.140505624781,
      419.592498593307, 328.050745327361, 367.428529653973
    ), ext_variance = c(
      1009.03354558997,
      1214.03537755011, 919.879850597901, 1299.64291904742
    ), g_variance = c(
      1309.16268746621,
      1257.61444710993, 1335.93765056966, 1393.84225317013
    ), n1 = c(
      306,
      306, 306, 306
    ), n2 = c(67, 67, 67, 67), n1G = c(94, 81, 66, 65), n2G = c(19, 17, 15, 16), r.squared = c(
      0.642877054009429,
      0.642877054009429, 0.642877054009429, 0.642877054009429
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, which = "psmall"))
}

test_ex_weights <- function() {
  message("Tests for exhaustive auxiliary data with weights")
  old_options <- options(warn = 2)
  RUnit::checkException(predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | smallarea),
    s2 = "s2",
    smallAreaMeans = tm,
    auxiliaryWeights = "boundary_weights"
  ),
  use_lm = FALSE
  ))
  options(old_options)
  suppressWarnings(weighted <- predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | smallarea),
    s2 = "s2",
    smallAreaMeans = tm,
    auxiliaryWeights = "boundary_weights"
  ),
  use_lm = FALSE
  ))
  not_weighted <- predict(saObj(
    data = s12,
    f = update(formula.s1, ~ . | smallarea),
    s2 = "s2",
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
    data = s12,
    f = update(formula.s1, ~ . | smallarea),
    s2 = "s2",
    smallAreaMeans = tm
  ),
  use_lm = FALSE
  )

  ##### % extended pseudo synthetic estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      372.692974632121,
      387.511613484442, 334.831400088368, 405.966717762401
    ), ext_variance = c(
      744.365784378973,
      693.857562845081, 838.395307705685, 940.31485465495
    ), g_variance = c(
      696.573912633751,
      708.110501854425, 801.430309727381, 890.953619217217
    ), n1 = c(
      Inf,
      Inf, Inf, Inf
    ), n2 = c(67, 67, 67, 67), n1G = c(
      Inf, Inf, Inf,
      Inf
    ), n2G = c(19, 17, 15, 16), r.squared = c(
      0.652650335602544,
      0.642885419397165, 0.643001792107126, 0.655617766440169
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE))
  ##### % pseudo synthetic estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      402.543391979474,
      386.516889702736, 338.747452357679, 366.328686870604
    ), ext_variance = c(
      NA_real_,
      NA_real_, NA_real_, NA_real_
    ), g_variance = c(
      224.753455262853,
      228.022615839058, 232.680636228253, 207.53865428268
    ), n1 = c(
      Inf,
      Inf, Inf, Inf
    ), n2 = c(67, 67, 67, 67), n1G = c(
      Inf, Inf, Inf,
      Inf
    ), n2G = c(19, 17, 15, 16), r.squared = c(
      0.642877054009429,
      0.642877054009429, 0.642877054009429, 0.642877054009429
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, which = "psynth"))
  ##### % pseudo small area estimator
  forestinventory <-
    structure(list(area = c("A", "B", "C", "D"), estimate = c(
      374.628393046395,
      387.418554589653, 334.91113404241, 402.116277581849
    ), ext_variance = c(
      761.252321825593,
      693.136210304687, 843.123099964604, 976.048072850744
    ), g_variance = c(
      986.005777088446,
      921.158826143745, 1075.80373619286, 1183.58672713342
    ), n1 = c(
      Inf,
      Inf, Inf, Inf
    ), n2 = c(67, 67, 67, 67), n1G = c(
      Inf, Inf, Inf,
      Inf
    ), n2G = c(19, 17, 15, 16), r.squared = c(
      0.642877054009429,
      0.642877054009429, 0.642877054009429, 0.642877054009429
    )), row.names = c(
      NA,
      -4L
    ), class = "data.frame")
  RUnit::checkIdentical(clean(forestinventory), clean(maSAE, which = "psmall"))
}

test_partially <- function() {
  # I do not know how to do two-phase partially exhaustive using
  # forestinventory. This fails:
  tryCatch(
    {
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
    },
    error = identity
  )
  # Thus I don't have reference data. So I just take my results and make sure
  # that weighting matters.

  weighted <- predict(saObj(
    data = s12, f = update(formula.s1, ~ . | smallarea),
    s2 = "s2", smallAreaMeans = tm_partially,
    auxiliaryWeights = "boundary_weights"
  ),
  use_lm = FALSE
  )
  expectation <-
    structure(c(
      "A", "B", "C", "D", "372.0717", "398.9897", "329.7628",
      "385.0420", "716.9027", "898.5370", "850.1451", "995.5185", "403.1495",
      "397.6410", "333.6635", "345.3305", "258.6937", "248.5087", "288.9749",
      "244.8478", "375.2345", "398.5427", "329.8272", "381.1181", "1019.9460",
      " 941.6449", "1132.0980", "1220.8959"
    ), .Dim = c(4L, 7L), .Dimnames = list(
      c("1", "2", "3", "4"), NULL
    ))
  RUnit::checkIdentical(clean(weighted, FALSE), expectation)

  not_weighted <- predict(saObj(
    data = s12, f = update(formula.s1, ~ . | smallarea),
    s2 = "s2", smallAreaMeans = tm_partially
  ), use_lm = FALSE)
  expectation <-
    structure(c(
      "A", "B", "C", "D", "370.7609", "399.4659", "330.3102",
      "384.9273", "716.3790", "898.5984", "848.8257", "997.5646", "401.8134",
      "398.1258", "334.2506", "345.1768", "259.5159", "247.0576", "289.7945",
      "245.5435", "373.8984", "399.0275", "330.4143", "380.9644", "1020.7682",
      " 940.1939", "1132.9176", "1221.5916"
    ), .Dim = c(4L, 7L), .Dimnames = list(
      c("1", "2", "3", "4"), NULL
    ))
  RUnit::checkIdentical(clean(not_weighted, FALSE), expectation)
  get_figs <- function(x) {
    return(apply(clean(x, FALSE)[, -1], 2, as.numeric))
  }
  RUnit::checkTrue(!isTRUE(all.equal(get_figs(not_weighted), get_figs(weighted))))
}


if (FALSE) {
  ## This is the mimick I used to get fixed estimations from package forestinventory.
  forestinventory <- forestinventory::twophase(
    formula = formula.s1,
    data = grisons,
    phase_id = list(
      phase.col = "phase_id_2p",
      terrgrid.id = 2
    ),
    small_area = list(
      sa.col = "smallarea",
      areas = c("A", "B", "C", "D"),
      unbiased = TRUE
    ),
    exhaustive = tm_partially
  )$estimation

  forestinventory <- twophase(
    formula = formula.s1,
    data = grisons,
    phase_id = list(
      phase.col = "phase_id_2p",
      terrgrid.id = 2
    ),
    small_area = list(
      sa.col = "smallarea",
      areas = c("A", "B", "C", "D"),
      unbiased = FALSE
    ),
    exhaustive = truemeans.G
  )$estimation

  forestinventory <- twophase(
    formula = formula.s1,
    data = grisons,
    phase_id = list(
      phase.col = "phase_id_2p",
      terrgrid.id = 2
    ),
    small_area = list(
      sa.col = "smallarea",
      areas = c("A", "B", "C", "D"),
      unbiased = TRUE
    ),
    psmall = TRUE,
    exhaustive = truemeans.G
  )$estimation
  dump("forestinventory")
  forestinventory::twophase(formula.fm,
    data = grisons,
    phase_id = list(
      phase.col = "phase_id_2p",
      terrgrid.id = 2
    ),
    small_area = list(
      sa.col = "smallarea", areas = c("A", "B"),
      unbiased = FALSE
    ),
    exhaustive = truemeans.G
  )$estimation
}
