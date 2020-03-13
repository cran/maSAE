message("Tests for using lm")
if (interactive()) {
  pkgload::load_all(".")
  source(file.path(
    devtools::as.package(".")["path"], "inst", "runit_tests",
    "setup.R"
  ))
}

fake_weights <- function(df) {
  df[["weights"]] <- 1
  df[["weights"]][df[["x2"]] == 0] <- 0.12
  return(df)
}
suppressWarnings(rm(s1, s2, s0))
data("s1", "s2", "s0", package = "maSAE")
s0$x1 <- s0$x3 <- NULL
s0 <- fake_weights(s0)
s1 <- fake_weights(s1)
s2 <- fake_weights(s2)
s12 <- bind_data(s1, s2)
s012 <- bind_data(s1, s2, s0)
tm <- data.frame(x1 = c(150, 200), x2 = c(23, 23), x3 = c(7, 7.5), g = c("a", "b"))
tm_p <- data.frame(x2 = c(23, 23), g = c("a", "b"))

test_means <- function() {
  reference <-
    list(mean = c(
      `(Intercept)` = 1, x1 = 166.17146030866, x2 = 23.897813052293,
      x3 = 7.88834951456311
    ), cov = structure(c(
      0, 0, 0, 0, 0, 205.852275432253,
      2.9031468575511, 2.34338834386355, 0, 2.9031468575511, 0.380376712016751,
      0.0978142770118694, 0, 2.34338834386355, 0.0978142770118694,
      0.0875309504815286
    ), .Dim = c(4L, 4L), .Dimnames = list(c(
      "(Intercept)",
      "x1", "x2", "x3"
    ), c("(Intercept)", "x1", "x2", "x3"))))
  object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm_p)
  result <- maSAE:::estimate_means(
    df = cbind(1, object@data[, paste0("x", 1:3)]), index_s = object@data[[object@s2]],
    index_g = TRUE,
    weights = NULL, lm = TRUE
  )
  RUnit::checkEquals(result, reference)
}

if (interactive()) test_means()
