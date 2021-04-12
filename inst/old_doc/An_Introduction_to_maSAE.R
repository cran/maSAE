### R code from vignette source 'An_Introduction_to_maSAE.Rnw'

###################################################
### code chunk number 1: An_Introduction_to_maSAE.Rnw:117-255
###################################################

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
s12 <- maSAE::bind_data(s1, s2)
s012 <- maSAE::bind_data(s1, s2, s0)
tm <- data.frame(x1 = c(150, 200), x2 = c(23, 23), x3 = c(7, 7.5), g = c("a", "b"))
tm_p <- data.frame(x2 = c(23, 23), g = c("a", "b"))

#% unclustered

##% un-weighted
###% two-phase
####% partially exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm_p)
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm)
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% non-exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

###% three-phase
object <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, s1 = "phase1", s2 = "phase2")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

##% weighted
###% two-phase
####% partially exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, 
                       s2 = "phase2", smallAreaMeans = tm_p, 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, 
                       s2 = "phase2", smallAreaMeans = tm, 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% non-exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, 
                       s2 = "phase2", 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

###% three-phase
object <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, 
                       s1 = "phase1", s2 = "phase2", 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

#% clustered
##% un-weighted
###% two-phase
####% partially exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm_p, cluster = "clustid")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm, cluster = "clustid")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% non-exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", cluster = "clustid")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

###% three-phase
object <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, s1 = "phase1", s2 = "phase2", cluster = "clustid")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)


##% weighted
###% two-phase
####% partially exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm_p, cluster = "clustid", 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", smallAreaMeans = tm, cluster = "clustid", 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

####% non-exhaustive 
object <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "phase2", cluster = "clustid", 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
outlm <- maSAE::predict(object, use_lm = TRUE)
RUnit::checkEquals(out, outlm)

###% three-phase
object <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, s1 = "phase1", s2 = "phase2", cluster = "clustid", 
                       auxiliaryWeights = "weights")
(out <- maSAE::predict(object, use_lm = FALSE))
(outlm <- maSAE::predict(object, use_lm = TRUE))
RUnit::checkEquals(out, outlm)




