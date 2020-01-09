data("s0", package = "maSAE")
data("s1", package = "maSAE")
data("s2", package = "maSAE")
testthat::context("Testing design based")
testthat::test_that("Testing design", {
                        saeO <-maSAE::saObj(data = s2, f = y ~ NULL | g)
                        result <- maSAE::predict(saeO)
                        expectation <- structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = c(394.009810276032, 439.116356034433
), variance = c(1721.07692428521, 1312.61172404567)), row.names = c(NA,
-2L), class = "data.frame", type = "design-based")
                        testthat::expect_equal(result, expectation)

                        ## assuming the data are clustered:
                        saeO <- maSAE::saObj(data = s2, f = y ~ NULL | g, cluster = "clustid")
                        result <- maSAE::predict(saeO)
                        expectation <- structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = c(394.009810276032, 439.116356034433
), variance = c(3712.26128994839, 2197.24438754629)), row.names = c(NA,
-2L), class = "data.frame", comment = "include is NULL, automatically adding it as TRUE to data.", type = "design-based")
                        testthat::expect_equal(result, expectation)

})

testthat::context("Testing unclustered")
testthat::test_that("non-exhaustive", {
message("## add sample indicators to s2")
s2$s1 <- TRUE
s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)
saeO <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "s2")
result <- maSAE::predict(saeO)

expectation <- structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(378.859044896576, 
391.826233791547), reference = "a2.35"), variance = structure(c(487.367953897024, 
417.344217739991), reference = "a2.36")), row.names = c(NA, -2L
), references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "non-exhaustive", clustered = FALSE, class = "data.frame")
                        testthat::expect_equal(result, expectation)
                        
})

testthat::test_that("three-phase", {
                        message("## add sample indicators to s2")
s2$s1 <- TRUE
s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)

message("## s0 has all 3 potential predictors, we keep one only")
s0$x1 <- s0$x3 <- NULL
message("## add sample indicators to s1")
s0$s1 <- s0$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s0$", setdiff(names(s12), names(s0)), " <- NA", sep = ""))))
message("## union s12 and s0 data")
s012 <- rbind(s0, s12)
saeO <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, s1 = "s1", s2 = "s2")
result <- maSAE::predict(saeO)
expectation <-
structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(397.186639960918, 
404.219728060002), reference = "c2.23"), variance = structure(c(311.807799109917, 
271.903595772526), reference = "c2.24")), row.names = c(NA, -2L
), references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "three-phase sampling", clustered = FALSE, class = "data.frame")
                        testthat::expect_equal(result, expectation)
})
testthat::test_that("partially exhaustive", {
                        message("## add sample indicators to s2")
s2$s1 <- TRUE
s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)

message("## s0 has all 3 potential predictors, we keep one only")
s0$x1 <- s0$x3 <- NULL
message("## add sample indicators to s1")
s0$s1 <- s0$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s0$", setdiff(names(s12), names(s0)), " <- NA", sep = ""))))
message("## union s12 and s0 data")
s012 <- rbind(s0, s12)
                        tm <- as.data.frame(tapply(s012$x2, s012$g, mean))
                        tm$g <- row.names(tm)
                        names(tm) <- c("x2", "g")
                        result <- maSAE::predict(maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "s2", smallAreaMeans = tm))
                        expectation <- structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(397.186639960918, 
404.219728060002), reference = "b2.30"), variance = structure(c(295.1857212266, 
254.217676455386), reference = "b2.31")), row.names = c(NA, -2L
), references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "partially exhaustive", clustered = FALSE, class = "data.frame")
                        testthat::expect_equal(result, expectation)
})

testthat::test_that("exhaustive", {

                        message("## add sample indicators to s2")
s2$s1 <- TRUE
s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)
message("## true Means as sample means from s12")
preds <- paste("x", 1:3, sep = "")
tm <- as.data.frame(
  rbind(
    colMeans(subset(s12, g == "a")[, preds]),
    colMeans(subset(s12, g == "b")[, preds])
  )
)
tm$g <- c("a", "b")
result <- maSAE::predict(maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "s2", smallAreaMeans = tm))

expectation <-
structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(378.859044896576, 
391.826233791547), reference = "a2.31"), variance = structure(c(223.129500601688, 
142.031819200991), reference = "a2.33")), row.names = c(NA, -2L
), references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "exhaustive", clustered = FALSE, class = "data.frame")
                        testthat::expect_equal(result, expectation)
                        
})


testthat::context("Testing unclustered")
testthat::test_that("non-exhaustive", {
message("## add sample indicators to s2")
s2$s1 <- s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)
saeO <- maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "s2", cluster = "clustid")
result <- maSAE::predict(saeO)
expectation <- structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(381.572883441901, 
392.339527092439), reference = "a2.46"), variance = structure(c(950.459374917922, 
807.322394330037), reference = "a2.47")), row.names = c(NA, -2L
), comment = "include is NULL, automatically adding it as TRUE to data.", references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "non-exhaustive", clustered = TRUE, class = "data.frame")
                        testthat::expect_equal(result, expectation)

})
testthat::test_that("three-phase", {
s2$s1 <- TRUE
s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)
message("## s0 has all 3 potential predictors, we keep one only")
s0$x1 <- s0$x3 <- NULL
message("## add sample indicators to s1")
s0$s1 <- s0$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s0$", setdiff(names(s12), names(s0)), " <- NA", sep = ""))))
message("## union s12 and s0 data")
s012 <- rbind(s0, s12)
saeO <- maSAE::saObj(data = s012, f = y ~ x1 + x2 + x3 | g, s1 = "s1", s2 = "s2", cluster = "clustid")
result <- maSAE::predict(saeO)
expectation <- 
structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(400.306466144986, 
404.967740283593), reference = "c1.53"), variance = structure(c(600.947704153426, 
493.600475190784), reference = "c1.55")), row.names = c(NA, -2L
), comment = c("include is NULL, automatically adding it as TRUE to data.", 
"b1 p.22 gives strange formulae for R_c and R1_c, I\n\t\t\t replace hat{Y} with Y, resulting in formulae analogous to b2\n\t\t\t p.18 with Zeta replaced by Zeta1. Confirmed by D.M., personal\n\t\t\t communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change\n\t\t\t hat{eR}_{1,c} to use the clustered version of the parameter\n\t\t\t estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change hat{eR}_{c} to use the clustered version of the parameter estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"b1.52 is skrewed, I replace hat{bar{Z}}_{G,1} with  hat{bar{Z}}_{c,G} and bar{Z}^(1) with bar{Zeta}^(1)_{G} in analogy with c2.24. Confirmed by D.M.", 
"b1 p.22 gives strange formulae for R_c and R1_c, I\n\t\t\t replace hat{Y} with Y, resulting in formulae analogous to b2\n\t\t\t p.18 with Zeta replaced by Zeta1. Confirmed by D.M., personal\n\t\t\t communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change\n\t\t\t hat{eR}_{1,c} to use the clustered version of the parameter\n\t\t\t estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change hat{eR}_{c} to use the clustered version of the parameter estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"b1.52 is skrewed, I replace hat{bar{Z}}_{G,1} with  hat{bar{Z}}_{c,G} and bar{Z}^(1) with bar{Zeta}^(1)_{G} in analogy with c2.24. Confirmed by D.M."
), references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "three-phase sampling", clustered = TRUE, class = "data.frame")

                        testthat::expect_equal(result, expectation)
})
testthat::test_that("partially exhaustive", {
s2$s1 <- TRUE
s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)
message("## s0 has all 3 potential predictors, we keep one only")
s0$x1 <- s0$x3 <- NULL
message("## add sample indicators to s1")
s0$s1 <- s0$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s0$", setdiff(names(s12), names(s0)), " <- NA", sep = ""))))
message("## union s12 and s0 data")
s012 <- rbind(s0, s12)
tm <- as.data.frame(tapply(s012$x2, s012$g, mean))
tm$g <- row.names(tm)
names(tm) <- c("x2", "g")
result <- maSAE::predict(maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "s2", cluster = "clustid", smallAreaMeans = tm))

expectation <-
structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(400.306466144986, 
404.967740283592), reference = "b1.50"), variance = structure(c(570.693284365626, 
461.927161122278), reference = "b1.52")), row.names = c(NA, -2L
), comment = c("include is NULL, automatically adding it as TRUE to data.", 
"b1 p.22 gives strange formulae for R_c and R1_c, I\n\t\t\t replace hat{Y} with Y, resulting in formulae analogous to b2\n\t\t\t p.18 with Zeta replaced by Zeta1. Confirmed by D.M., personal\n\t\t\t communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change\n\t\t\t hat{eR}_{1,c} to use the clustered version of the parameter\n\t\t\t estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change hat{eR}_{c} to use the clustered version of the parameter estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"b1.52 is skrewed, I replace hat{bar{Z}}_{G,1} with  hat{bar{Z}}_{c,G} and bar{Z}^(1) with bar{Zeta}^(1)_{G} in analogy with c2.24. Confirmed by D.M.", 
"b1 p.22 gives strange formulae for R_c and R1_c, I\n\t\t\t replace hat{Y} with Y, resulting in formulae analogous to b2\n\t\t\t p.18 with Zeta replaced by Zeta1. Confirmed by D.M., personal\n\t\t\t communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change\n\t\t\t hat{eR}_{1,c} to use the clustered version of the parameter\n\t\t\t estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"c1 p.21/b1 p.22: In analogy to a1.25 I change hat{eR}_{c} to use the clustered version of the parameter estimate. Confirmed by D.M., personal communication, 2014-01-08", 
"b1.52 is skrewed, I replace hat{bar{Z}}_{G,1} with  hat{bar{Z}}_{c,G} and bar{Z}^(1) with bar{Zeta}^(1)_{G} in analogy with c2.24. Confirmed by D.M."
), references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "partially exhaustive", clustered = TRUE, class = "data.frame")
                        testthat::expect_equal(result, expectation)
})
testthat::test_that("exhaustive", {
data("s1", package = "maSAE")
message("## add sample indicators to s2")
s2$s1 <- s2$s2 <- TRUE
message("## add sample indicators to s1")
s1$s1 <- TRUE
s1$s2 <- FALSE
message("## prepare s1 data")
eval(parse(text = (paste("s1$", setdiff(names(s2), names(s1)), " <- NA", sep = ""))))
message("## union s1 and s2 data")
s12 <- rbind(s1, s2)
preds <- paste("x", 1:3, sep = "")
tm <- as.data.frame(
  rbind(
    colMeans(subset(s12, g == "a")[, preds]),
    colMeans(subset(s12, g == "b")[, preds])
  )
)
tm$g <- c("a", "b")
result <- maSAE::predict(maSAE::saObj(data = s12, f = y ~ x1 + x2 + x3 | g, s2 = "s2", cluster = "clustid", smallAreaMeans = tm))

expectation <-
structure(list(smallArea = structure(1:2, .Label = c("a", "b"
), class = "factor"), prediction = structure(c(381.572883441901, 
392.339527092439), reference = "a2.48"), variance = structure(c(435.592781479774, 
315.021024873806), reference = "a2.49")), row.names = c(NA, -2L
), comment = "include is NULL, automatically adding it as TRUE to data.", references = c("READ ME using  cat(sep = '\n', attr(NAME, 'references')[2])  ", 
" I cite 6 manuscripts:\n\na2 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nIn: Canadian Journal of Forest Research 43.5 (2013), pp. 441-449.\ndoi: 10.1139/cjfr-2012-0381.\n\na1 Daniel Mandallaz. Design-based properties of some small-area estimators in\nforest inventory with two-phase sampling.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2012.\ndoi: 10.3929/ethz-a-007318974.\n\nb2 Daniel Mandallaz, Jochen Breschan, and Andreas Hill. New regression\nestimators in forest inventories with two-phase sampling and partially\nexhaustive information: a design-based Monte Carlo approach with applications\nto small-area estimation.\nIn: Canadian Journal of Forest Research 43.11 (2013), pp. 1023-1031.\ndoi: 10.1139/cjfr-2013-0181.\n\nb1 Daniel Mandallaz. Regression estimators in forest inventories with twophase\nsampling and partially exhaustive information with applications to small-area\nestimation.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-007623322.\n\nc2 Daniel Mandallaz. A three-phase sampling extension of the generalized\nregression estimator with partially exhaustive information.\nIn: Canadian Journal of Forest Research 44.4 (2014), pp. 383-388.\ndoi: 10.1139/cjfr-2013-0449.\n\nc1 Daniel Mandallaz. Regression estimators in forest inventories with threephase\nsampling and two multivariate components of auxiliary information.\nTech. rep. Eidgenoessische Technische Hochschule Zuerich,\nDepartement Umweltsystemwissenschaften, 2013.\ndoi: 10.3929/ethz-a-009990020.\n\nThere's three topics ('a', 'b' and 'c') with two versions each,\n(1) a detailed report published via http://e-collection.library.ethz.ch/ and\n(2) a paper in CFJR.\nI use topicversion to cite them. i.e. a1 is the report on topic a.\nI cite equations from the manuscripts using\ntopicversion.equationnumber[.equationpart][.equationversion]\nwhere 'equationnumber. is the equation number from the manuscript,\nfor multiline equations 'equationpart' is the line of the equation indexed\nby letters\nand for equations differing by an index, say k, equationversion gives the\nvalue of the index.\nSo a2.13.b refers to the second line of equation (13) in\nthe CJFR-paper 'Design-based properties of some small-area estimators ...'\nand b2.5.2 refers to the version for k=2 of equation (5) in the CJFR-paper\n'New regression estimators ...'\n"
), auxilliary.data = "exhaustive", clustered = TRUE, class = "data.frame")
                        testthat::expect_equal(result, expectation)
})
