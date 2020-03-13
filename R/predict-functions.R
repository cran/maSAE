predict_version <- function(object, version = NULL, use_lm = NA) {
    if (is.null(version)) version <- getOption("maSAE_version")
    if (is.null(version)) version <- "2.0.0"
    if (identical(version, "1.0.0")) {
        predict_v1(object)
    } else {
        predict_v2(object, use_lm = use_lm)
    }
}

determine_prediction_type <- function(object) {
    vars <- determine_variables(object)
    predictand <- vars[["predictand"]]
    predictors <- vars[["predictors"]]
    small_area <- vars[["small_area"]]
    rm(vars)
    #% which type of auxiliary data do we have?
    if (is.null(methods::slot(object, "smallAreaMeans"))) {
        is_exhaustive <- FALSE
        if (is.null(methods::slot(object, "s1"))) {
            pred_type <- "non-exhaustive"
            is_partially <- FALSE
            is_3phase <- FALSE
        } else {
            pred_type <- "partially"
            is_partially <- TRUE
            is_3phase <- TRUE
        }
    } else {
        is_exhaustive <- TRUE
        is_3phase <- FALSE
        if (
            length(colnames(methods::slot(object, "smallAreaMeans"))) == length(c(predictors, small_area)) &&
                all(sort(colnames(methods::slot(object, "smallAreaMeans"))) == sort(c(predictors, small_area)))
            ) {
            pred_type <- "exhaustive"
            is_partially <- FALSE
        } else {
            pred_type <- "partially"
            is_partially <- TRUE
        }
    }
    #% is it clustered?
    is_clustered <- !is.null(methods::slot(object, "cluster"))
    #% is it weighted?
    if (!is.null(methods::slot(object, "auxiliaryWeights"))) {
        weights <- methods::slot(object, "data")[[methods::slot(object, "auxiliaryWeights")]]
    } else {
        # set them NULL, for there is a function stats::weights which will be
        # called otherwise!
        weights <- NULL
    }
    return(list(prediction_type = pred_type, is_partially = is_partially,
                is_exhaustive = is_exhaustive, is_3phase = is_3phase, 
                is_clustered = is_clustered, weights = weights
                ))
}

determine_variables <- function(object) {
    varnames <- all.vars(methods::slot(object, "f"))
    small_area <- varnames[length(varnames)]
    predictors <- varnames[-c(1, which(varnames == small_area))]
    predictand <- varnames[1]
    return(list(small_area = small_area, 
                predictors = predictors, predictand = predictand))
}
check_predictor_values <- function(object, prediction_type) {
    vars <- determine_variables(object)
    predictand <- vars[["predictand"]]
    predictors <- vars[["predictors"]]
    small_area <- vars[["small_area"]]
    rm(vars)

    ##% missing predictand in s2
    if (is.null(methods::slot(object, "s2"))) {
        pred_vals <- methods::slot(object, "data")[, predictand]
    } else {
        pred_vals <- methods::slot(object, "data")[methods::slot(object, "data")[, methods::slot(object, "s2")], predictand]
    }
    !any(is.na(pred_vals)) || stop("can't handle missing values for predictand in s2")
    ## missing predictor in s2
    if (is.null(methods::slot(object, "s2"))) {
        pred_vals <- methods::slot(object, "data")[, predictors]
    } else {
        pred_vals <- methods::slot(object, "data")[methods::slot(object, "data")[, methods::slot(object, "s2")], predictors]
    }
    !any(is.na(pred_vals)) || stop("can't handle missing values for predictors in s2")
    ##% missing predictor in s1
    if (prediction_type %in% c("non-exhaustive", "partially")) {
        if (is.null(methods::slot(object, "s1"))) {
            pred_vals <- methods::slot(object, "data")[, predictors]
        } else {
            pred_vals <- methods::slot(object, "data")[methods::slot(object, "data")[, methods::slot(object, "s1")], predictors]
        }
        !any(is.na(pred_vals)) || stop("can't handle missing values for predictors in s1")
    }
    ##% missing predictor in s0
    # works with three-phase implementation - no checks done!
}

get_z <- function(data, predictors, weights, cluster = NULL, use_lm = FALSE) {
    z <- data[c(predictors)]
    if (!is.null(weights)) {
        z <- z * weights
    }
    if (is.null(cluster)) {
        z <- cbind("Intercept" = 1,
                   data[predictors])
    } else {
        if (!isTRUE(use_lm)) {
            z <- cbind("Intercept" = 1,
                       as.data.frame(stats::aggregate(z,
                                                      list(cluster = cluster),
                                                      mean)[, -1]))
        } else {
            z <- cbind("Intercept" = 1, z)
            formula <- stats::DF2formula(z)
            old_options <- options(na.action = "na.pass")
            z <- stats::model.matrix(formula, z)
            options(old_options)
            z <- stats::aggregate(z, list(cluster = cluster), mean)[, -1]
            names(z) <- sub("\\((Intercept)\\)", "\\1", names(z))
        }
    }
    return(z)
}

get_means <- function(object, predictors, small_area, index) {
    true_means <- methods::slot(object, "smallAreaMeans")
    g_index <- true_means[[small_area]] == index
    mz_g <- c(1, as.numeric(true_means[g_index, predictors])) ## a2 p.443
    emz_g <-  c(mz_g, 1) ## a1 p.18
    # make them lists to conform with the estimated version
    mz_g <- list(mean = mz_g, cov = NULL)
    emz_g <- list(mean = emz_g, cov = NULL)
    return(list(mz_g = mz_g, emz_g = emz_g))
}


get_a <- function(x, m) {
    result <- t(as.matrix(as.numeric(m) * x)) %*% as.matrix(x) / nrow(x)
    return(result)
}
cov_matrix_beta <- function(r, z, m) {
    a <- get_a(z, m)
    p <-  (t(as.numeric(m^2) * as.numeric(r^2) * as.matrix(z))) %*% as.matrix(z)  / nrow(z)^2
    result <- solve(a) %*% p %*% solve(a)
    return(result)
}
cov_matrix_alpha <- function(r, zs2, zs1, ms2, ms1) {
    a <- get_a(zs1, ms1)
    p <-  (t(as.numeric(ms2^2) * as.numeric(r^2) * as.matrix(zs2))) %*% as.matrix(zs2)  / nrow(zs2)^2
    result <- solve(a) %*% p %*% solve(a)
    return(result)
}
get_u <- function(z, y, m) {
    result <- colMeans(m * z * y)
    return(result)
}
estimate_means <- function(df, index_s, index_g, weights, lm) {
    z <- as.data.frame(df[index_s & index_g, TRUE])
    if (!is.null(weights)) {
        tmp <- class(weights) # preserve the class!
        weights <- weights[index_s & index_g]
        class(weights) <- tmp
    }
    is_clustered <- inherits(weights, "cluster")
    if (!lm) {
        z <- as.matrix(z)
    } else {
        # use the design matrix returned my model.matrix for factors!
        # This is a bit spooky. I assume z to contain predictors only (either z
        # or z^{(1)} in Mandallaz' notation.
        formula <- stats::DF2formula(z)
        z <- stats::model.matrix(formula, z)
    }
    if (!is.null(weights)) {
        mz <- colSums(z * weights / sum(weights))
    } else {
        mz <- colMeans(z)
    }
    # covariance matrix (a2_24, a2_37, a2_41, a2_45)
    n <- nrow(z)
    cz  <- t(t(z) - mz) # this is equal to,
    # but faster than: cz <- t(apply(z, 1, function(x) x - mz)),
    if (is_clustered) {
        relative_m <- (weights / mean(weights))^2
    } else {
        # TODO: is that true, if there are weights?!
        relative_m <- 1
    }
    sigma <- (t(relative_m * cz) %*% cz) / n / (n - 1)
    return(list(mean = mz, cov = sigma))
}


pred_synth <- function(y, z, mz, is_partially, is_3phase,
                       lm = FALSE, n = NULL, m = NULL) {
    if (is.null(m)) m <- 1 # all weights equal
    if (!is.null(n)) ratio_n <- n[["s2"]] / n[["s1"]]
    if (identical(names(z), c("z", "z1", "z1s1"))) {
        z1s1 <- z[["z1s1"]]
        z1 <- z[["z1"]]
        z <- z[["z"]]
    }
    if (identical(names(mz), c("mz", "mz1", "tmz1"))) {
        tmz1 <- mz[["tmz1"]]
        mz1 <- mz[["mz1"]]
        mz <- mz[["mz"]]
    }
    if (identical(names(m), c("m", "m1"))) {
        m1 <- m[["m1"]]
        m <- m[["m"]]
    }
    ## estimate regression coefficients
    if (isTRUE(lm)) {
        # z is already a design matrix, but not for factors, so I get rid of the
        # intercept's column in z:
        mod <- lm(y ~ .,
                  as.data.frame(cbind(z[which(names(z) != "Intercept")], "y" = y)),
                  na.action = stats::na.exclude, x = TRUE, weights = m)
        beta <- stats::coef(mod)
        r <- stats::residuals(mod)
        z <- mod[["x"]] # use the model's design matrix
        if (is_partially) {
            mod1 <- lm(y ~ .,  # z already is a design matrix
                       as.data.frame(cbind(z1[which(names(z1) != "Intercept")], "y" = y)),
                      na.action = stats::na.exclude,
                      x = TRUE, weights = m)
            alpha <- stats::coef(mod1)
            r1 <- stats::residuals(mod1)
            z1 <- mod1[["x"]] # use the model's design matrix
        }
    } else {
        u <- get_u(z, y, m)
        ia <- solve(get_a(z, m))
        beta <- as.numeric(ia %*% u)
        r <- y - as.numeric(as.matrix(z) %*% beta) # a2 p.443 & b2 p.1025
        if (is_partially) {
            ia1 <- solve(get_a(z1, m))
            u1 <- get_u(as.data.frame(z1), y, m)
            alpha <- as.numeric(ia1 %*% u1) # b2.5.2
            r1 <- y - as.numeric(as.matrix(z1) %*% alpha)
        }
    }
    ## design-based variance-covariance matrix
    cov_beta <- cov_matrix_beta(r, z, m)
    cov_z <- mz[["cov"]]
    mz <- mz[["mean"]]

    prediction <- mz %*% beta
    variance <- mz %*% cov_beta %*% mz
    if (is_partially) {
        prediction <- prediction + (tmz1[["mean"]] - mz1[["mean"]]) %*% alpha
        cov_alpha <- cov_matrix_alpha(r1, z1, z1s1, m, m1)
        variance  <- (1 - ratio_n) * variance +
            ratio_n * tmz1[["mean"]] %*% cov_alpha %*%  tmz1[["mean"]]
        if (is_3phase) {
            variance <- variance + alpha %*% tmz1[["cov"]] %*% alpha
        }
    } else {
        if (!is.null(cov_z)) {
            variance <- variance + beta %*% cov_z %*% beta
        }
    }
    result <- list(prediction = prediction,
                   variance = variance)

    result <- lapply(result, function(x) as.numeric(x))
    result <- c(result, list(r = r), list(beta = beta))
    if (isTRUE(lm)) {
    result <- c(result, list(model = mod))
    }
    return(result)
}

predict_v2 <- function(object, use_lm) {
    vars <- determine_variables(object)
    predictand <- vars[["predictand"]]
    predictors <- vars[["predictors"]]
    small_area <- vars[["small_area"]]
    rm(vars)
    if (!(is.logical(use_lm) && !is.na(use_lm))) {
        if (all(apply(methods::slot(object, "data")[, c(predictors)], 2, is.numeric))) {
            use_lm <- FALSE
        } else {
            use_lm <- TRUE
        }
    }

    pred_type <- determine_prediction_type(object)
    prediction_type <- pred_type[["prediction_type"]]
    is_partially <- pred_type[["is_partially"]]
    is_exhaustive <- pred_type[["is_exhaustive"]]
    is_3phase <- pred_type[["is_3phase"]]
    is_clustered <- pred_type[["is_clustered"]]
    weights <- pred_type[["weights"]]
    rm(pred_type)
    check_predictor_values(object, prediction_type) 


    #% set up data
    if (is_clustered) {
        include <- methods::slot(object, "data")[, methods::slot(object, "include")]
        cluster <- methods::slot(object, "data")[, methods::slot(object, "cluster")]
        m <- tapply(as.numeric(include),
                    cluster,
                    sum, na.rm = TRUE)
        y <- methods::slot(object, "data")[, predictand]
        y  <- stats::aggregate(y, list(cluster), sum)[, -1] / m


        z <- get_z(data = methods::slot(object, "data"), use_lm = use_lm,
                   predictors = predictors, weights = weights, cluster = cluster)
        if (is.null(methods::slot(object, "s1"))) {
            s1 <- rep(TRUE, length(m))
        } else {
            s1 <- tapply(methods::slot(object, "data")[, methods::slot(object, "s1")],
                         methods::slot(object, "data")[, methods::slot(object, "cluster")], unique)
        }
        if (is.null(methods::slot(object, "s2"))) {
            s2 <- rep(TRUE, length(m))
        } else {
            s2 <- tapply(methods::slot(object, "data")[, methods::slot(object, "s2")],
                         methods::slot(object, "data")[, methods::slot(object, "cluster")], unique)
        }

        if (is_partially) {
            if (is_3phase) {
                ## find those predictors which are not all NA where s1 is FALSE
                z <- get_z(data = methods::slot(object, "data"), use_lm = use_lm,
                           predictors = predictors, weights = weights, cluster = cluster)
                not_na_in_s0 <- !is.na(methods::slot(object, "data")[!s1, predictors])
                predictors1 <- predictors[apply(not_na_in_s0, 2, all)]
                rm(not_na_in_s0)
            } else {
                not_small_area <- -which(colnames(methods::slot(object, "smallAreaMeans")) == small_area)
                predictors1 <- colnames(methods::slot(object, "smallAreaMeans"))[not_small_area]
                rm(not_small_area)
            }
            z1 <- get_z(data = methods::slot(object, "data"), use_lm = use_lm,
                       predictors = predictors1, weights = weights, cluster = cluster)
        }
        # The empirical means will have to be weighted by m:
        weights <- as.numeric(m)
        # But we need to make a distinction between cluster weights and
        # auxiliaryWeights
        class(weights) <- c(class(weights), "cluster")

    } else {
        y <- methods::slot(object, "data")[, predictand]
        m <- rep(1, length(y))
        cluster <- methods::slot(object, "cluster")
        z <- get_z(data = methods::slot(object, "data"), use_lm = use_lm,
                   predictors = predictors, weights = weights, cluster = cluster)
        if (is.null(methods::slot(object, "s2"))) {
            s2 <- rep(TRUE, nrow(methods::slot(object, "data")))
        } else {
            s2 <- methods::slot(object, "data")[, methods::slot(object, "s2")]
        }
        if (is.null(methods::slot(object, "s1"))) {
            s1 <- rep(TRUE, nrow(methods::slot(object, "data")))
        } else {
            s1 <- methods::slot(object, "data")[, methods::slot(object, "s1")]
        }
        if (is_partially) {
            if (is_3phase) {
                ## find those predictors which are not all NA where s1 is FALSE
                not_na_in_s0 <- !is.na(methods::slot(object, "data")[!s1, predictors])
                predictors1 <- predictors[apply(not_na_in_s0, 2, all)]
                rm(not_na_in_s0)
            } else {
                not_small_area <- -which(colnames(methods::slot(object, "smallAreaMeans")) == small_area)
                predictors1 <- colnames(methods::slot(object, "smallAreaMeans"))[not_small_area]
                rm(not_small_area)
            }
            z1 <- get_z(data = methods::slot(object, "data"), use_lm = use_lm,
                       predictors = predictors1, weights = weights, cluster = cluster)
        }
    }
    out <- data.frame()
    groups <- unique(methods::slot(object, "data")[, small_area])
    groups <- sort(groups[!is.na(groups)])

    for (group in groups) {
        if (interactive() && !exists("group"))
            group <- groups[1]

        if (is_clustered) {
            if (is.factor(methods::slot(object, "data")[, small_area])) {
                smallarea <- tapply(as.character(methods::slot(object, "data")[, small_area]),
                                    methods::slot(object, "data")[, methods::slot(object, "cluster")],
                                    unique)
            } else {
                smallarea <- tapply(methods::slot(object, "data")[, small_area],
                                    methods::slot(object, "data")[, methods::slot(object, "cluster")],
                                    unique)
            }
            g <- smallarea == group
        } else {

            g <- methods::slot(object, "data")[, small_area] == group
        }
        g[is.na(g)] <- FALSE
        ez <- cbind(z, as.numeric(g)) ## a2 p.444
        if (is_partially) {
            ez1 <- cbind(z1, as.numeric(g))
        }
        n_s2g <- sum(as.numeric(s2 & g))


        #% Setup the means (and possibly their covariance matrices)
        ## a1 p.18
        if (is_exhaustive) {
            if (is_partially) {
                mz1_g <- estimate_means(df = z1, index_s = s1,
                                        index_g = g, weights = weights,
                                        lm = use_lm)
                mez1_g <- estimate_means(df = ez1, s1, g, weights, lm = use_lm)
                mz_g <- estimate_means(df = z, s1, g, weights, lm = use_lm)
                mez_g <- estimate_means(df = ez, s1, g, weights, lm = use_lm)
                tmp <- get_means(object = object, small_area = small_area,
                                 predictors = predictors1, index = group)
                tmz1_g <- tmp[["mz_g"]]
                tmez1_g <- tmp[["emz_g"]]
            } else {
                tmp <- get_means(object = object, small_area = small_area,
                                 predictors = predictors, index = group)
                mz_g <- tmp[["mz_g"]]
                emz_g <- tmp[["emz_g"]]
            }
            rm(tmp)
        } else {
            if (is_3phase) {
                mz1_g <- estimate_means(df = z1, s1, g, weights, lm = use_lm)
                mez1_g <- estimate_means(df = ez1, s1, g, weights, lm = use_lm)
                mz_g <- estimate_means(df = z, s1, g, weights, lm = use_lm)
                mez_g <- estimate_means(df = ez, s1, g, weights, lm = use_lm)
                tmz1_g <- estimate_means(df = z1, TRUE, g, weights, lm = use_lm)
                tmez1_g <- estimate_means(df = ez1, TRUE, g, weights, lm = use_lm)
            } else {
                mz_g <- estimate_means(df = z, index_s = s1,
                                       index_g = g, weights, lm = use_lm)
                emz_g <- estimate_means(df = ez, s1, g, weights, lm = use_lm)
            }
        }
        #% predict synthetic
        if (!is_partially) {
            synth <- pred_synth(y = y[s2], z = z[s2, TRUE], mz = mz_g,
                                is_partially, is_3phase, m = m[s2],
                                lm = use_lm)
        } else {
            synth <- pred_synth(y = y[s2],
                                z = list("z" = z[s2, TRUE],
                                         "z1" = z1[s2, TRUE],
                                         "z1s1" = z1[s1, TRUE]),
                                mz = list("mz" = mz_g, "mz1" = mz1_g,
                                          "tmz1" = tmz1_g),
                                m = list("m" = m[s2],
                                         "m1" = m[s1]),
                                is_partially, is_3phase,
                                n = c("s1" = sum(s1), "s2" = sum(s2)),
                                lm = use_lm
                                )
        }
        synthetic_prediction <- synth[["prediction"]]
        synthetic_variance <- synth[["variance"]]
        #% predict small
        if (isTRUE(use_lm)) {
            # predict for all y using the model, needs to be returned by
            # pred_synth.
            r <- y - predict(synth[["model"]], newdata = z)
        } else {
            r <- y - as.numeric(as.matrix(z) %*% synth[["beta"]]) # a2 p.443 & b2 p.1025
        }
        if (is_clustered) { # a2_44
            mr <- sum(m[s2 & g] * r[s2 & g]) / sum(m[s2 & g]) # a2, p.445 following a2_45
            small_prediction <- synthetic_prediction + mr
        } else {
            small_prediction <- synthetic_prediction + mean(r[s2 & g])
        }
        if (is_clustered) { # a2_45
            small_variance <- synthetic_variance +
                1 / n_s2g * 1 / (n_s2g - 1) *
                sum((m[s2 & g] / mean(m[s2 & g]))^2 * (r[s2 & g] - mr)^2)
        } else {
            small_variance <- synthetic_variance + 1 / n_s2g * stats::var(r[s2 & g])
        }

        #% predict extended
        if (!is_partially) {
            extended <- pred_synth(y = y[s2], z = ez[s2, TRUE], mz = emz_g,
                                   is_partially, is_3phase, m = m[s2],
                                   lm = use_lm)
        } else {
            extended <- pred_synth(y = y[s2],
                                   z = list("z" = ez[s2, TRUE],
                                            "z1" = ez1[s2, TRUE],
                                            "z1s1" = ez1[s1, TRUE]),
                                   mz = list("mz" = mez_g, "mz1" = mez1_g,
                                             "tmz1" = tmez1_g),
                                   m = list("m" = m[s2],
                                            "m1" = m[s1]),
                                   is_partially, is_3phase,
                                   n = c("s1" = sum(s1), "s2" = sum(s2)),
                                   lm = use_lm)
        }

        extend_prediction <- extended[["prediction"]]
        extended_variance <- extended[["variance"]]
        result <- data.frame(small_area = group,
                             prediction = extend_prediction, variance = extended_variance,
                             psynth = synthetic_prediction, var_psynth = synthetic_variance,
                             psmall = small_prediction, var_psmall = small_variance)
        out <- rbind(out, result)
    }
    return(out)
}
