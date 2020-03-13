clean <- function(x, which = NULL) {
  if (isFALSE(which)) {
    res <- as.matrix(unname(x[TRUE, TRUE]))
  } else {
    if (is.null(which)) {
      if (all(c("small_area", "prediction", "variance") %in% names(x))) {
        res <- as.matrix(unname(x[TRUE, 1:3]))
      } else {
        res <- as.matrix(unname(x[TRUE, c("area", "estimate", "g_variance")]))
      }
    } else {
      res <- as.matrix(unname(x[TRUE, c(1, grep(which, names(x)))]))
    }
  }
  return(res)
}

# I have functions where maSAE and forestinventory get different variances, so I
# would like to check on the predictions at least.
check_variance_prediciton <- function(x, y) {
  prediction_variance <- tryCatch(RUnit::checkIdentical(x, y),
    error = identity
  )
  if (inherits(prediction_variance, "error")) {
    warning("Variances differ, checking for predictions only!", immediate. = TRUE)
    RUnit::checkIdentical(x[, 1:2], y[, 1:2])
  } else {
    RUnit::checkTrue(prediction_variance)
  }
}
