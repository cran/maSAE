if (interactive()) pkgload::load_all()
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
    "small_area g not found in data"
  )
  testthat::expect_error(
    new("sadObj", f = y ~ x + z | g, data = data.frame(y = 1, g = "a")),
    "formula has to be of structure predictand ~ NULL | smallArea"
  )
})

testthat::test_that("saeObj creation", {
    expectation <- list(value = new("saeObj", smallAreaMeans = NULL, s1 = "s1", 
                                    s2 = "s2", 
                                    data = structure(list(y = 1, x = 1, z = 1, 
                                                          g = structure(1L, 
                                                                        .Label = "a", 
                                                                        class = "factor"), 
                                                          s1 = TRUE, s2 = TRUE, 
                                                          w = 1, include = TRUE, 
                                                          clust = structure(1L, 
                                                                            .Label = "B", 
                                                                            class = "factor")), 
                                                     class = "data.frame", 
                                                     row.names = c(NA, -1L)), 
                                    f = y ~ x + z | g, cluster = "clust", 
                                    include = "include", auxiliaryWeights = "w"), 
                        visible = TRUE)
    result  <- new("saeObj",
            f = y ~ x + z | g,
            data = data.frame(y = 1, x = 1, z = 1, g = "a", s1 = TRUE, s2 = TRUE, w = 1,
                              include = TRUE, clust = "B", stringsAsFactors = TRUE),
            s1 = "s1",
            s2 = "s2",
            include = "include",
            cluster = "clust",
            auxiliaryWeights = "w"
            )
    testthat::expect_identical(expectation[[1]], result)
})


testthat::test_that("saeObj", {
   #% auxiliary weights
   testthat::expect_error(new("saeObj",
                              f = y ~ x + z | g,
                              data = data.frame(y = 1, x = 1, z = 1, g = "a", 
                                                s1 = TRUE, s2 = TRUE,
                                                include = TRUE, clust = "B", stringsAsFactors = TRUE),
                              s1 = "s1", s2 = "s2", include = "include",

                              cluster = "clust", auxiliaryWeights = "w"),
                          "auxiliary weight w not found in data"
                          )
   testthat::expect_error(new("saeObj",
                              f = y ~ x + z | g,
                              data = data.frame(y = 1, x = 1, z = 1, g = "a", 
                                                s1 = TRUE, s2 = TRUE, w = -1e06,
                                                include = TRUE, clust = "B", stringsAsFactors = TRUE),
                              s1 = "s1", s2 = "s2", include = "include",
                              cluster = "clust", auxiliaryWeights = "w"),
                          "range of auxiliary weights in w has to be in"
   )
   testthat::expect_error(new("saeObj",
                              f = y ~ x + z | g,
                              data = data.frame(y = 1, x = 1, z = 1, g = "a", 
                                                s1 = TRUE, s2 = TRUE, w = 1+1e06,
                                                include = TRUE, clust = "B", stringsAsFactors = TRUE),
                              s1 = "s1", s2 = "s2", include = "include",
                              cluster = "clust", auxiliaryWeights = "w"),
                          "range of auxiliary weights in w has to be in"
   )
   testthat::expect_error(new("saeObj",
                              f = y ~ x + z | g,
                              data = data.frame(y = 1, x = 1, z = 1, g = "a", 
                                                s1 = TRUE, s2 = TRUE, w = "a",
                                                include = TRUE, clust = "B", stringsAsFactors = TRUE),
                              s1 = "s1", s2 = "s2", include = "include",
                              cluster = "clust", auxiliaryWeights = "w"),
                          "w has got to be of class 'numeric'"
   )
  #%                     

  testthat::expect_error(new("saeObj", f = y ~ x:z), "formula must not contain interactions")
  testthat::expect_error(new("saeObj", f = y ~ x * z), "formula must not contain interactions")

  testthat::expect_error(new("saeObj", f = y ~ x + z), "formula must contain a ' | smallArea' term")
  testthat::expect_error(new("saeObj", f = y ~ x + z | g), "predictand y not found in data")


  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = "a", stringsAsFactors = TRUE)),
    "has got to be numeric"
  )

  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, stringsAsFactors = TRUE)),
    "small_area g not found in data"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ NULL | g, data = data.frame(y = 1, g = "a", stringsAsFactors = TRUE)),
    "got no predictor"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, g = "a", stringsAsFactors = TRUE)),
    "not all predictors found in data"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, g = "a", stringsAsFactors = TRUE)),
    "not all predictors found in data"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stringsAsFactors = TRUE)),
    "got neither s2 nor smallAreaMeans"
  )
#  testthat::expect_error(
#    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = "a", g = "a", stringsAsFactors = TRUE)),
#    "all predictors have got to be numeric"
#  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1, stringsAsFactors = TRUE), cluster = "cl"),
    "need s2 for cluster sampling"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stringsAsFactors = TRUE), s2 = "stp"),
    " s2 indicator stp not found in data"
  )
  testthat::expect_error(
    new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stp = 1, stringsAsFactors = TRUE), s2 = "stp"),
    "has got to be of class 'logical'"
  )
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stp = TRUE, stp0 = FALSE, stringsAsFactors = TRUE), s2 = "stp", s1 = "stp0"),
"s2 is not a subset of s1")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stp = TRUE, stringsAsFactors = TRUE), cluster = "cl", s2 = "stp"),
"clustering indicator cl not found in data")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stringsAsFactors = TRUE), smallAreaMeans = data.frame(g = "b", x = 1, z = 1, stringsAsFactors = TRUE)),
"found extraneous smallAreas in smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stringsAsFactors = TRUE), smallAreaMeans = data.frame(g = "a", x = 1, stringsAsFactors = TRUE)),
"got neither s2 nor exhaustive smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stringsAsFactors = TRUE), smallAreaMeans = data.frame(g = "a", x = 1, z = 1, k = 1, stringsAsFactors = TRUE)),
"smallAreaMeans don't really fit")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", stringsAsFactors = TRUE), smallAreaMeans = data.frame(g = "a", x = 1, z = NA, stringsAsFactors = TRUE)), "Can't deal with missing data in smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1:2, x = 1:2, z = 1:2, g = c("a", "b"), stringsAsFactors = TRUE), smallAreaMeans = data.frame(g = "a", x = 1, z = 1, stringsAsFactors = TRUE)), "missing smallAreas in smallAreaMeans")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1, stp = TRUE, stringsAsFactors = TRUE), cluster = "cl", s2 = "stp", include = "inc"), "inclusion indicator inc not found in data")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1, stp = TRUE, inc = 1, stringsAsFactors = TRUE), cluster = "cl", s2 = "stp", include = "inc"), "inc has got to be of class 'logical'")
testthat::expect_error(new("saeObj", f = y ~ x + z | g, data = data.frame(y = 1, x = 1, z = 1, g = "a", cl = 1, stp = TRUE, inc = TRUE, stringsAsFactors = TRUE), s2 = "stp", include = "inc"), "inclusion indicator inc only valid for clustered data")
testthat::expect_error(new("saeObj",
  f = y ~ x + z | g,
  data = data.frame(y = 1, x = 1, z = 1, g = "a", s1 = TRUE, stringsAsFactors = TRUE),
  smallAreaMeans = data.frame(g = "a", x = 1, z = 1, stringsAsFactors = TRUE),
  s1 = "s1"
), "Got both all true means and s1")
})
