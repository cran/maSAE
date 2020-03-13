## -----------------------------------------------------------------------------
library("forestinventory")
library("maSAE")

## -----------------------------------------------------------------------------
clean <- function(x, which = NULL) {
    if (identical(FALSE, which)) {
        res <- as.matrix(unname(x[TRUE, TRUE]))
    } else {
        if (is.null(which)) {
            if (all(c( "small_area", "prediction", "variance") %in% names(x))) {
                res <- as.matrix(unname(x[TRUE, 1:3]))
            } else {
                res <- as.matrix(unname(x[["estimation"]][TRUE, c("area", "estimate", "g_variance")]))
            }
        } else {
            res <- as.matrix(unname(x[TRUE, c(1, grep(which, names(x)))]))
        }
    }
    return(res)
} 

compare <- function(maSAE, forestinventory, message = NULL) {
    if(! isTRUE(all.equal(clean(maSAE), clean(forestinventory), 
                          check.attributes = FALSE))) {
        message <- c("Differing results from maSAE and forestinventory: ", 
                     message)
        warning(message)
        return(FALSE)
    } 
    return(TRUE)
}

## -----------------------------------------------------------------------------
data(grisons, package = "forestinventory")

## -----------------------------------------------------------------------------
formula.s0 <- tvol ~ mean # reduced model:
formula.s1 <- tvol ~ mean + stddev + max + q75 # full model
formula.clust.s0 <- basal ~ stade
formula.clust.s1 <- basal ~ stade + couver + melange


## -----------------------------------------------------------------------------
truemeans.G <- data.frame(Intercept = rep(1, 4),
                          mean = c(12.85, 12.21, 9.33, 10.45),
                          stddev = c(9.31, 9.47, 7.90, 8.36),
                          max = c(34.92, 35.36, 28.81, 30.22),
                          q75 = c(19.77, 19.16, 15.40, 16.91))
rownames(truemeans.G) <- c("A", "B", "C", "D")

# data adjustments
s1 <- grisons[grisons[["phase_id_2p"]] == 1, ]
s2 <- grisons[grisons[["phase_id_2p"]] == 2, ]
s12 <- rbind(s1, s2)
s12$s1 <- s12$phase_id_2p %in% c(1, 2)
s12$s2 <- s12$phase_id_2p == 2

tm <- truemeans.G
tm[["smallarea"]] <- row.names(tm)
tm[["Intercept"]] <- NULL

truemeans.G.partially <- truemeans.G[, -which(names(truemeans.G) %in% c("stddev", "mean"))]
tm.partially <- tm[, -which(names(tm) %in% c("stddev", "mean"))]


## -----------------------------------------------------------------------------
summary(twophase(formula = formula.s1, 
                 data = grisons,
                 phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                 small_area = list(sa.col = "smallarea", 
                                   areas = c("A", "B","C", "D"),
                                   unbiased = TRUE),
                 boundary_weights = "boundary_weights"
                 ))

## -----------------------------------------------------------------------------
maSAE <- predict(saObj(data = s12,
                       f = update(formula.s1, ~ . | smallarea),
                       auxiliaryWeights = "boundary_weights",
                       s2 = 's2')
)

forestinventory <- twophase(formula = formula.s1, 
                            data = grisons,
                            phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                            small_area = list(sa.col = "smallarea", 
                                              areas = c("A", "B","C", "D"),
                                              unbiased = TRUE),
                            boundary_weights = "boundary_weights"
                            )

## -----------------------------------------------------------------------------
compare(maSAE, forestinventory, "two-phase, ext. pseudo sae")

## -----------------------------------------------------------------------------
wrap_two <- function(...) {
    dots <- list(...)
    dots$small_area$unbiased <- TRUE
    ex <- do.call(twophase, dots)$estimation
    dots$psmall <- TRUE
    small <- do.call(twophase, dots)$estimation
    dots$psmall <- FALSE
    dots$small_area$unbiased <- FALSE
    synth <- do.call(twophase, dots)$estimation
    cbind(ex[TRUE, c("estimate", "g_variance")], 
          synth[TRUE, c("estimate", "g_variance")], 
          small[TRUE, c("estimate", "g_variance")])

}
mbmb <- microbenchmark::microbenchmark
mb <- mbmb(
           forestinventory = wrap_two(formula = formula.s1, 
                                    data = grisons,
                                    phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
                                    small_area = list(sa.col = "smallarea", 
                                                      areas = c("A", "B","C", "D"),
                                                      unbiased = TRUE)
                                    ), 
           maSAE = predict(saObj(data = s12, 
                                 f = update(formula.s1, ~ . | smallarea),
                                 s2 = 's2'
                                 ))[, -1],
           check = "equivalent"
           )

## -----------------------------------------------------------------------------
print(mb)
microbenchmark:::autoplot.microbenchmark(mb)


## -----------------------------------------------------------------------------
forestinventory <- twophase(formula = formula.s1, 
                            data = grisons,
                            phase_id = list(phase.col = "phase_id_2p", 
                                            terrgrid.id = 2),
                            small_area = list(sa.col ="smallarea", 
                                              areas = c("A", "B", "C", "D"),
                                              unbiased = TRUE),
                            exhaustive = truemeans.G)
summary(forestinventory)

## -----------------------------------------------------------------------------
maSAE <- predict(saObj(data = s12, 
                       f = update(formula.s1, ~ . | smallarea),
                       s2 = 's2',
                       smallAreaMeans = tm))
compare(maSAE, forestinventory, "two-phase, ext. sae")

## -----------------------------------------------------------------------------
mb <- mbmb(
           forestinventory = wrap_two(formula = formula.s1, 
                                            data = grisons,
                                            phase_id = list(phase.col = "phase_id_2p", 
                                                            terrgrid.id = 2),
                                            small_area = list(sa.col ="smallarea", 
                                                              areas = c("A", "B", "C", "D"),
                                                              unbiased = TRUE),
                                            exhaustive = truemeans.G),
           maSAE = predict(saObj(data = s12, 
                                       f = update(formula.s1, ~ . | smallarea),
                                       s2 = 's2',
                                       smallAreaMeans = tm))[, -1],

           check = "equivalent")
print(mb)
microbenchmark:::autoplot.microbenchmark(mb)

## -----------------------------------------------------------------------------
truemeans.G <- data.frame(Intercept = rep(1, 4),
                          mean = c(12.85, 12.21, 9.33, 10.45))
rownames(truemeans.G) <- c("A", "B", "C", "D")

## data adjustments
s12_3p <- grisons[grisons[["phase_id_3p"]] %in% c(1,2), ]
s0 <- grisons[grisons[["phase_id_3p"]] ==0 , ]
s12_3p$s1 <- s12_3p$phase_id_3p %in% c(1, 2)
s12_3p$s2 <- s12_3p$phase_id_3p == 2
s0$s1 <- s0$s2 <- FALSE
predictors_s0 <- all.vars(formula.s0)[-1]
predictors_s1 <- all.vars(formula.s1)[-1]
eval(parse(text=(paste0("s0$",
                        setdiff(predictors_s1, predictors_s0),
                        " <- NA"))))
s012 <- rbind(s0, s12_3p)
tm <- truemeans.G
tm[["smallarea"]] <- row.names(tm)
tm[["Intercept"]] <- NULL


## -----------------------------------------------------------------------------
summary(threephase(formula.s0,
                   formula.s1,
                   data = grisons,
                   phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                   small_area=list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                   unbiased = TRUE),
                   boundary_weights = "boundary_weights"
                   ))

## -----------------------------------------------------------------------------
forestinventory <- threephase(formula.s0,
                              formula.s1,
                              data = grisons,
                              phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                              small_area=list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                              unbiased = TRUE),
                              boundary_weights = "boundary_weights"
                              )

maSAE <- predict(saObj(data = s012, 
                       f = update(formula.s1, ~ . | smallarea),
                       s1 = 's1',
                       auxiliaryWeights = "boundary_weights",
                       s2 = 's2'))


## -----------------------------------------------------------------------------
compare(maSAE, forestinventory, "three-phase, ext. pseudo sae")

## -----------------------------------------------------------------------------
wrap_three <- function(...) {
    dots <- list(...)
    dots$small_area$unbiased <- TRUE
    ex <- do.call(threephase, dots)$estimation
    dots$psmall <- TRUE
    small <- do.call(threephase, dots)$estimation
    dots$psmall <- FALSE
    dots$small_area$unbiased <- FALSE
    synth <- do.call(threephase, dots)$estimation
    cbind(ex[TRUE, c("estimate", "g_variance")], 
          synth[TRUE, c("estimate", "g_variance")], 
          small[TRUE, c("estimate", "g_variance")])

}
mb <- mbmb(
           forestinventory = wrap_three(formula.s0,
                              formula.s1,
                              data = grisons,
                              phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                              small_area=list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                              unbiased = TRUE)
                              ),
           maSAE = predict(suppressMessages(saObj(data = s012, 
                       f = update(formula.s1, ~ . | smallarea),
                       s1 = 's1',
                       s2 = 's2')))[, -1],

           check = "equivalent")
print(mb)
microbenchmark:::autoplot.microbenchmark(mb)

## -----------------------------------------------------------------------------
try(twophase(formula = formula.s1, 
             data = grisons,
             phase_id = list(phase.col = "phase_id_2p", 
                             terrgrid.id = 2),
             small_area = list(sa.col ="smallarea", 
                               areas = c("A", "B", "C", "D"),
                               unbiased = TRUE),
             exhaustive = truemeans.G.partially))
predict(saObj(data = s12, 
              f = update(formula.s1, ~ . | smallarea),
              s2 = 's2',
              smallAreaMeans = tm.partially))


## -----------------------------------------------------------------------------

summary(forestinventory <- threephase(formula.s0,
                              formula.s1,
                              data = grisons,
                              phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                              small_area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                                unbiased = TRUE),
                              exhaustive = truemeans.G))



try(predict(saObj(data = s012, 
              f = update(formula.s1, ~ . | smallarea),
              s2 = 's2',
              s1 = 's1',
              smallAreaMeans = tm)))


## -----------------------------------------------------------------------------
extsynth_3p <- threephase(formula.s0, formula.s1, data = grisons,
                          phase_id = list(phase.col = "phase_id_3p",
                                          s1.id = 1, terrgrid.id = 2),
                          small_area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                            unbiased = TRUE),
                          exhaustive = truemeans.G,
                          boundary_weights = "boundary_weights"
                          )
extsynth_3p$estimation

## -----------------------------------------------------------------------------
s12_3p$s1 <- NULL
s12_3p$phase_id_2p <- NULL
s12_3p$phase_id_3p <- NULL
maSAE <- predict(saObj(data = s12_3p, 
                       f = update(formula.s1, ~ . | smallarea),
                       auxiliaryWeights = "boundary_weights",
                       s2 = 's2', smallAreaMeans = tm)
)
compare(maSAE, extsynth_3p, "three-phase, ext. sae")


## -----------------------------------------------------------------------------
mb <- mbmb(
           forestinventory = wrap_three(formula.s0,
                              formula.s1,
                              data = grisons,
                              phase_id = list(phase.col = "phase_id_3p", s1.id = 1, terrgrid.id = 2),
                              small_area = list(sa.col = "smallarea", areas = c("A", "B", "C", "D"),
                                                unbiased = TRUE),
                              exhaustive = truemeans.G),
           maSAE = predict(suppressMessages(saObj(data = s12_3p, 
                       f = update(formula.s1, ~ . | smallarea),
                       s2 = 's2', smallAreaMeans = tm)))[, -1],
           check = "equivalent")
print(mb)
microbenchmark:::autoplot.microbenchmark(mb)


## -----------------------------------------------------------------------------

suppressWarnings(rm("s1" ,"s2", "s12"))
data("s1", "s2", package = "maSAE")
s12 <- bind_data(s1, s2)
# adapt for forestinventory
s12[["g"]][is.na(s12[["g"]])] <- "a"
s12[["phase"]]  <-  s12[["phase1"]] + s12[["phase2"]]
maSAE <- predict(suppressMessages(saObj(data = s12, f = y ~x1 + x2 + x3 | g,
              s2 = "phase2", cluster = "clustid")))
extpsynth.clust <- twophase(y ~x1 + x2 + x3,
         data = s12,
         cluster = "clustid",
         phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
         small_area = list(sa.col = "g", areas = c("a", "b"),
                           unbiased = TRUE))
compare(maSAE, extpsynth.clust, "three-phase, ext. sae")
mb <- mbmb(
           forestinventory = clean(twophase(y ~x1 + x2 + x3,
         data = s12,
         cluster = "clustid",
         phase_id = list(phase.col = "phase", s1.id = 1, terrgrid.id = 2),
         small_area = list(sa.col = "g", areas = c("a", "b"),
                           unbiased = TRUE))),
           maSAE = clean(predict(suppressMessages(saObj(data = s12, f = y ~x1 + x2 + x3 | g,
              s2 = "phase2", cluster = "clustid")))),

           check = "equivalent")
print(mb)
microbenchmark:::autoplot.microbenchmark(mb)


## -----------------------------------------------------------------------------
  data("zberg", package = "forestinventory")
  forestinventory <- forestinventory::twophase(
    formula = basal ~ stade + couver + melange, data = zberg,
    phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
    cluster = "cluster",
    small_area = list(
      sa.col = "ismallold", areas = c("1"),
      unbiased = TRUE
    )
  )
  s1 <- zberg[zberg[["phase_id_2p"]] == 1, ]
  s2 <- zberg[zberg[["phase_id_2p"]] == 2, ]
  s12 <- rbind(s1, s2)
  s12[["s1"]] <- s12[["phase_id_2p"]] %in% c(1, 2)
  s12[["s2"]] <- s12[["phase_id_2p"]] == 2
  object <- maSAE::saObj(
    data = s12,
    f = basal ~ stade + couver + melange | ismallold,
    s2 = "s2",
    cluster = "cluster"
  )
  maSAE <- maSAE::predict(object)
  compare(maSAE[2,], forestinventory, "clustered, ext. sae")

## -----------------------------------------------------------------------------
  forestinventory <- forestinventory::twophase(
    formula = basal ~ stade + couver + melange, data = zberg,
    phase_id = list(phase.col = "phase_id_2p", terrgrid.id = 2),
    small_area = list(
      sa.col = "ismallold", areas = c("1"),
      unbiased = TRUE
    )
  )
  object <- maSAE::saObj(
    data = s12,
    f = basal ~ stade + couver + melange | ismallold,
    s2 = "s2",
  )
  maSAE <- maSAE::predict(object)
  compare(maSAE[2,], forestinventory, "clustered, ext. sae")

