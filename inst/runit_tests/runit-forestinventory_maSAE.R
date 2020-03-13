message("#% Tests against forestinventory")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(
    devtools::as.package(".")["path"], "inst", "runit_tests",
    "setup.R"
  ))
}

message("##% Tests for clustered sampling using s1, s2")
suppressWarnings(rm("s1", "s2", "s12"))
data("s1", "s2", package = "maSAE")
s12 <- bind_data(s1, s2)
# adapt for forestinventory
s12[["g"]][is.na(s12[["g"]])] <- "a"
s12[["phase"]] <- s12[["phase1"]] + s12[["phase2"]]
test_clustered_unweighted <- function() {
  ma <- maSAE::predict(saObj(
    data = s12, f = y ~ x1 + x2 + x3 | g,
    s2 = "phase2", cluster = "clustid"
  ))
  fi <- forestinventory::twophase(y ~ x1 + x2 + x3,
    data = s12,
    cluster = "clustid",
    phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
    small_area = list(
      sa.col = "g", areas = c("a", "b"),
      unbiased = TRUE
    )
  )$estimation
  RUnit::checkEquals(ma[c(2, 3)], fi[c(2, 4)], check.attributes = FALSE)
}
if (interactive()) test_clustered_unweighted()
