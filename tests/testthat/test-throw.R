testthat::context("Testing maSAE:::throw()")
testthat::test_that("throw the maSAE exception", {
  error_message <- "hello, testthat"
  string <- "hello, testthat"
  testthat::expect_error(
    maSAE:::throw(string),
    error_message
  )
})
