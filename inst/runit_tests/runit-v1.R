message("Ensure 1.0.0 and 2.0.0 give same predictions and variances")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(
    devtools::as.package(".")["path"], "inst", "runit_tests",
    "setup.R"
  ))
}

suppressWarnings(rm(s1, s2, s0))
data("s1", "s2", "s0", package = "maSAE")
s0$x1 <- s0$x3 <- NULL
s12 <- bind_data(s1, s2)
s012 <- bind_data(s1, s2, s0)
tm <- data.frame(x1 = c(150, 200), x2 = c(23, 23), x3 = c(7, 7.5), g = c("a", "b"))
tm_p <- data.frame(x2 = c(23, 23), g = c("a", "b"))

test_lm_unclustered <- function() {

  # % two-phase
  ## % partially exhaustive
  object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm_p)
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)

  ## % exhaustive
  object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm)
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)

  ## % non-exhaustive
  object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2")
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)

  # % three-phase
  object <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, s1 = "phase1", s2 = "phase2")
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)
}

test_lm_clustered <- function() {
  # % two-phase
  ## % partially exhaustive
  object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm_p, cluster = "clustid")
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)

  ## % exhaustive
  object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm, cluster = "clustid")
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)

  ## % non-exhaustive
  object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", cluster = "clustid")
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)

  # % three-phase
  object <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, s1 = "phase1", s2 = "phase2", cluster = "clustid")
  out <- maSAE::predict(object)
  out_v1 <- maSAE::predict(object, version = "1.0.0")
  RUnit::checkEquals(out[1:3], out_v1, check.attributes = FALSE)
}
if (interactive()) test_lm_clustered()
