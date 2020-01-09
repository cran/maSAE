testthat::context("Testing classes")
testthat::test_that("sadObj", {
  testthat::expect_error(new("sadObj.virtual"), regexp = "is not a defined class")
  testthat::expect_error(new("sadObj", f = y ~ x:z), "formula must not contain interactions")
  testthat::expect_error(new("sadObj", f = y ~ x * z), "formula must not contain interactions")

  testthat::expect_error(new("sadObj", f = y ~ x + z), "formula must contain a ' | smallArea' term")
  testthat::expect_error(new("sadObj", f = y ~ x + z | g), "predictand y not found in data")

  testthat::expect_error(
    new("sadObj", f = y ~ x + z | g, data = data.frame(y = "a")),
    "has got to be numeric"
  )
  testthat::expect_error(
    new("sadObj", f = y ~ x + z | g, data = data.frame(y = 1)),
    "smallArea g not found in data"
  )
  testthat::expect_error(
    new("sadObj", f = y ~ x + z | g, data = data.frame(y = 1, g = "a")),
    "formula has to be of structure predictand ~ NULL | smallArea"
  )
})

testthat::test_that("saeObj", {
  testthat::expect_error(new("saeObj", f = y ~ x:z), "formula must not contain interactions")
  testthat::expect_error(new("saeObj", f = y ~ x * z), "formula must not contain interactions")

  testthat::expect_error(new("saeObj", f = y ~ x + z), "formula must contain a ' | smallArea' term")
  testthat::expect_error(new("saeObj", f = y ~ x + z | g), "predictand y not found in data")


  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = "a")),
    "has got to be numeric"
  )

  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1)),
    "smallArea g not found in data"
  )

  testthat::expect_error(
    new("saeObj", f = y ~ NULL | g, data = data.frame(y = 1, g = "a")),
    "got no predictor"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, g = "a")),
    "not all predictors found in data"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, g = "a")),
    "not all predictors found in data"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a")),
    "got neither s2 nor smallAreaMeans"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = "a", g = "a")),
    "all predictors have got to be numeric"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1), cluster = "cl"),
    "need s2 for cluster sampling"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a"), s2 = "stp"),
    " s2 indicator stp not found in data"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stp = 1), s2 = "stp"),
    "has got to be of class 'logical'"
  )
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stp = TRUE, stp0 = FALSE), s2 = "stp", s1 = "stp0"),
"s2 is not a subset of s1")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stp = TRUE), cluster = "cl", s2 = "stp"),
"clustering indicator cl not found in data")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a"), smallAreaMeans = data.frame(g = "b", x = 1, z = 1)),
"found extraneous smallAreas in smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a"), smallAreaMeans = data.frame(g = "a", x = 1)),
"got neither s2 nor exhaustive smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a"), smallAreaMeans = data.frame(g = "a", x = 1, z = 1, k = 1)),
"smallAreaMeans don't really fit")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a"), smallAreaMeans = data.frame(g = "a", x = 1, z = NA)), "Can't deal with missing data in smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1:2, x = 1:2, z = 1:2, g = c("a", "b")), smallAreaMeans = data.frame(g = "a", x = 1, z = 1)), "missing smallAreas in smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1, stp = TRUE), cluster = "cl", s2 = "stp", include = "inc"), "inclusion indicator inc not found in data")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1, stp = TRUE, inc = 1), cluster = "cl", s2 = "stp", include = "inc"), "inc has got to be of class 'logical'")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1, stp = TRUE, inc = TRUE), s2 = "stp", include = "inc"), "inclusion indicator inc only valid for clustered data")
testthat::expect_error(new("saeObj",
  f = y ~ x + z | g,
  data = data.frame(y = 1, x = 1, z = 1, g = "a", s1 = TRUE),
  smallAreaMeans = data.frame(g = "a", x = 1, z = 1),
  s1 = "s1"
), "giving smallAreaMeans and s1 doesn't make sense")
})
