message("#% Tests against forestinventory")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(
    devtools::as.package(".")["path"], "inst", "runit_tests",
    "setup.R"
  ))
}

message("##% Tests for using zberg")
suppressWarnings(rm("s1", "s2", "s12"))
data("zberg", package = "forestinventory")
set.seed(1234)
zberg[["weights"]] <- sample(c(rep(1, 5), 0.2), nrow(zberg), replace = TRUE)
s1 <- zberg[zberg[["phase_id_2p"]] == 1, ]
s2 <- zberg[zberg[["phase_id_2p"]] == 2, ]
s12 <- rbind(s1, s2)
s12[["s1"]] <- s12[["phase_id_2p"]] %in% c(1, 2)
s12[["s2"]] <- s12[["phase_id_2p"]] == 2

test_unclustered_unweighted <- function() {
  fi <- forestinventory::twophase(
    formula = basal ~ stade + couver + melange, data = zberg,
    phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
    small_area = list(
      sa.col = "ismallold", areas = c("1"),
      unbiased = TRUE
    )
  )$estimation
  object <- maSAE::saObj(
    data = s12,
    f = basal ~ stade + couver + melange | ismallold,
    s2 = "s2"
  )
  ma <- maSAE::predict(object)
  RUnit::checkEquals(ma[2, c(2, 3)], fi[c(2, 4)], check.attributes = FALSE)
}
if (interactive()) test_unclustered_unweighted()

test_unclustered_weighted <- function() {
  fi <- forestinventory::twophase(
    formula = basal ~ stade + couver + melange, data = zberg,
    phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
    boundary_weights = "weights",
    small_area = list(
      sa.col = "ismallold", areas = c("1"),
      unbiased = TRUE
    )
  )$estimation
  object <- saObj(
    data = s12, f = basal ~ stade + couver + melange | ismallold,
    auxiliaryWeights = "weights",
    s2 = "s2"
  )
  ma <- maSAE::predict(object)
  RUnit::checkEquals(ma[2, c(2, 3)], fi[c(2, 4)], check.attributes = FALSE)
}
if (interactive()) test_unclustered_weighted()
