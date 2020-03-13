message("Tests for the clustered two-phase design")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(
    devtools::as.package(".")["path"], "inst", "runit_tests",
    "setup.R"
  ))
}
library("forestinventory")
library("maSAE")

clean <- function(x, which = NULL) {
  if (identical(FALSE, which)) {
    res <- as.matrix(unname(x[TRUE, TRUE]))
  } else {
    if (is.null(which)) {
      if (all(c("small_area", "prediction", "variance") %in% names(x))) {
        res <- as.matrix(unname(x[TRUE, 1:3]))
      } else {
        res <- as.matrix(unname(x[["estimation"]][TRUE, c("area", "estimate", "g_variance")]))
      }
    } else {
      res <- as.matrix(unname(x[TRUE, c(1, grep(which, names(x)))]))
    }
  }
  return(res)
}

compare <- function(maSAE, forestinventory, message = NULL) {
  if (!isTRUE(all.equal(clean(maSAE), clean(forestinventory)))) {
    message <- c(
      "Differing results from maSAE and forestinventory: ",
      message
    )
    warning(message)
    return(FALSE)
  }
  return(TRUE)
}

data(grisons, package = "forestinventory")

formula.s0 <- tvol ~ mean # reduced model:
formula.s1 <- tvol ~ mean + stddev + max + q75 # full model


truemeans.G <- data.frame(
  Intercept = rep(1, 4),
  mean = c(12.85, 12.21, 9.33, 10.45)
)
rownames(truemeans.G) <- c("A", "B", "C", "D")

s12_3p <- grisons[grisons[["phase_id_3p"]] %in% c(1, 2), ]
s12_3p$s2 <- s12_3p$phase_id_3p == 2
s12_3p$phase_id_2p <- NULL
s12_3p$phase_id_3p <- NULL
tm <- truemeans.G
tm[["smallarea"]] <- row.names(tm)
tm[["Intercept"]] <- NULL


test_partially_exhaustive_auxiliary_information <- function() {
  extsynth_3p <- threephase(formula.s0, formula.s1,
    data = grisons,
    phase_id = list(
      phase.col = "phase_id_3p",
      s1.id = 1, terrgrid.id = 2
    ),
    small_area = list(
      sa.col = "smallarea", areas = c("A", "B", "C", "D"),
      unbiased = TRUE
    ),
    exhaustive = truemeans.G,
    boundary_weights = "boundary_weights"
  )

  ## -----------------------------------------------------------------------------
  maSAE <- predict(saObj(
    data = s12_3p,
    f = update(formula.s1, ~ . | smallarea),
    auxiliaryWeights = "boundary_weights",
    s2 = "s2", smallAreaMeans = tm
  ))
  RUnit::checkEquals(clean(maSAE), clean(extsynth_3p))
}
