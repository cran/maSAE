### R code from vignette source 'maSAE.Rnw'

###################################################
### code chunk number 1: maSAE.Rnw:58-59
###################################################
library("maSAE")


###################################################
### code chunk number 2: maSAE.Rnw:63-65
###################################################
data("s2")
data("s1")


###################################################
### code chunk number 3: maSAE.Rnw:70-71
###################################################
s12 <- bind_data(s1, s2)


###################################################
### code chunk number 4: maSAE.Rnw:74-76
###################################################
saeO <- saObj(data = s12, f = y ~x1 + x2 + x3 | g,
              s2 = "phase2")


###################################################
### code chunk number 5: maSAE.Rnw:79-80
###################################################
predict(saeO)


###################################################
### code chunk number 6: maSAE.Rnw:88-91
###################################################
data("s0")
s0$x1 <- s0$x3 <- NULL
s012 <- bind_data(s1, s2, s0)


###################################################
### code chunk number 7: maSAE.Rnw:94-96
###################################################
predict(saObj(data = s012,  f = y ~x1 + x2 + x3 | g,
              s2 = "phase2", s1 = "phase1"))


###################################################
### code chunk number 8: maSAE.Rnw:103-107
###################################################
tm1 <- as.data.frame(tapply(s012$x2, s012$g, mean))
names(tm1)[1] <- c("x2"); tm1$g <- row.names(tm1)
predict(saObj(data = s12, f = y ~x1 + x2 + x3 | g,
              s2 = "phase2", smallAreaMeans = tm1))


###################################################
### code chunk number 9: maSAE.Rnw:115-120
###################################################
preds <- paste("x",1:3, sep="")
tm <- as.data.frame(rbind(colMeans(subset(s12, g == "a")[, preds]),
                          colMeans(subset(s12, g == "b")[, preds])
                          )
); tm$g=c("a", "b")


###################################################
### code chunk number 10: maSAE.Rnw:123-125
###################################################
predict(saObj(data = s12, f = y ~x1 + x2 + x3 | g,
              s2 = "phase2", smallAreaMeans = tm))


###################################################
### code chunk number 11: maSAE.Rnw:131-138
###################################################
source("Rao.R")
library(nlme)
dat <- subset(s2, ! is.na(s2$g))
dat <- dat[with(dat, order(g)), TRUE]
aLmeObj <- lme(y ~x1 + x2 + x3, data = dat, random =  ~1 | g)
foo <- new(Class = "sae", lmeObj = aLmeObj, domain.df = tm)
sae(foo)


###################################################
### code chunk number 12: maSAE.Rnw:150-151
###################################################
 grep(".*clust", capture.output(str(s1)), value = TRUE)


###################################################
### code chunk number 13: maSAE.Rnw:159-161 (eval = FALSE)
###################################################
## predict(saObj(data = s12, f = y ~x1 + x2 + x3 | g,
##               s2 = "phase2", cluster = "clustid"))


###################################################
### code chunk number 14: maSAE.Rnw:163-166 (eval = FALSE)
###################################################
## predict(saObj(data = s012,  f = y ~x1 + x2 + x3 | g,
##               s2 = "phase2", s1 = "phase1",
##               cluster = "clustid"))


###################################################
### code chunk number 15: maSAE.Rnw:168-171 (eval = FALSE)
###################################################
## predict(saObj(data = s12, f = y ~x1 + x2 + x3 | g,
##               s2 = "phase2", smallAreaMeans = tm1,
##               cluster = "clustid"))


###################################################
### code chunk number 16: maSAE.Rnw:173-176 (eval = FALSE)
###################################################
## predict(saObj(data = s12, f = y ~x1 + x2 + x3 | g,
##               s2 = "phase2", smallAreaMeans = tm,
##               cluster = "clustid"))


