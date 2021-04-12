#' Class \code{"characterOrNULL"}
#'
#' the  _union_ of classes \code{character} and  \code{NULL}
#'
#' used by \linkS4class{savObj}, \linkS4class{saeObj}
#'
#' @name characterOrNULL-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @seealso  \code{\link[methods:setClassUnion]{?methods::setClassUnion}}
#' @keywords classes
#' @keywords internal
#' @family classes
#' @rdname maSAE-internal
#' @examples
#'
#' showClass("characterOrNULL")
setClassUnion("characterOrNULL", c("character", "NULL"))

#' Class \code{"data.frameOrNULL"}
#'
#' the  _union_ of classes \code{data.frame} and  \code{NULL}
#'
#' used by \linkS4class{saeObj}
#'
#' @name data.frameOrNULL-class
#' @docType class
#' @seealso  \code{\link[methods:setClassUnion]{?methods::setClassUnion}}
#' @family classes
#' @keywords classes
#' @keywords internal
#' @rdname maSAE-internal
#' @examples
#'
#' showClass("data.frameOrNULL")
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))

#' Small Area Virtual Object Class
#'
#' Common slots for classes \code{sadObj} and \code{saeObj}.
#'
#'
#' @name savObj-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @family classes
#' @slot data See \code{"\linkS4class{saeObj}"}.
#' @slot f  See \code{"\linkS4class{saeObj}"}.
#' @slot cluster  See \code{"\linkS4class{saeObj}"}.
#' @slot include  See \code{"\linkS4class{saeObj}"}.
#' @note The slots are described in
#' \code{"\link[=saeObj-class]{class?maSAE::saeObj}"}, since this is the main class
#' of the package.
#' @seealso \code{"\link[stats:formula]{?stats::formula}"},
#' \code{"\link[=sadObj-class]{class?maSAE::sadObj}"} and
#' \code{"\link[=saeObj-class]{class?maSAE::saeObj}"}.
#' @keywords internal
#' @keywords classes
#' @export
#' @examples
#'
#' showClass("savObj")
setClass(
  Class = "savObj",
  contains = "VIRTUAL",
  slots = c(
    data = "data.frame",
    f = "formula",
    cluster = "characterOrNULL",
    include = "characterOrNULL"
  ),
  validity = function(object) {
    if (
      length(grep("*", methods::slot(object, "f"), fixed = TRUE)) != 0 |
        length(grep(":", methods::slot(object, "f"), fixed = TRUE)) != 0
    ) {
      return("formula must not contain interactions")
    }
    if (length(grep("|", methods::slot(object, "f"), fixed = TRUE)) != 1) {
      return("formula must contain a ' | small_area' term")
    }
    varnames <- all.vars(methods::slot(object, "f"))
    small_area <- varnames[length(varnames)]
    predictand <- varnames[1]
    predictand %in% colnames(methods::slot(object, "data")) || return(paste("predictand ", predictand, " not found in data.", sep = ""))
    is.numeric(methods::slot(object, "data")[, predictand]) || return(paste("data$", predictand, " has got to be numeric.", sep = ""))
    small_area %in% colnames(methods::slot(object, "data")) || return(paste("small_area ", small_area, " not found in data.", sep = ""))
    if (!is.null(methods::slot(object, "cluster"))) {
        methods::slot(object, "cluster") %in% colnames(methods::slot(object, "data")) || return(paste("clustering indicator ", methods::slot(object, "cluster"), " not found in data.", sep = ""))
      any(is.na(methods::slot(object, "data")[, methods::slot(object, "cluster")])) &&
        return(paste(
          "clustering indicator ",
          methods::slot(object, "cluster"), "contains NA, can't deal with",
          "missing cluster indicators, use constructor function saObj."
        ))
    }
    if (!is.null(methods::slot(object, "include"))) {
      !is.null(methods::slot(object, "cluster")) ||
          return(paste("inclusion indicator ", methods::slot(object, "include"),
                       " only valid for clustered data.", sep = ""))
      methods::slot(object, "include") %in% colnames(methods::slot(object, "data")) ||
          return(paste("inclusion indicator ", methods::slot(object, "include"),
                       " not found in data.", sep = ""))
      if (class(methods::slot(object, "data")[, methods::slot(object, "include")]) != "logical") {
        return(paste(methods::slot(object, "include"),
                     " has got to be of class 'logical'.", sep = ""))
      }
    }
  }
)

#' Small Area Design-based Objects
#'
#' A class for design-based estimation only.
#'
#' See \code{"\linkS4class{saeObj}"}. The fixed effects part of
#' \code{f} has to be NULL: design-based estimation knows no fixed effects.
#'
#' @family classes
#' @name sadObj-class
#' @aliases sadObj-class
#' @docType class
#' @section Extends: Class \code{"\linkS4class{savObj}"}, directly.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("sadObj", ...)} or via the constructor function  \code{"\link[=saObj]{?maSAE::saObj}"}.
#' @slot data  See \code{"\linkS4class{saeObj}"}.
#' @slot f See \code{"\linkS4class{saeObj}"}.
#' @slot cluster See \code{"\linkS4class{saeObj}"}.
#' @slot include See \code{"\linkS4class{saeObj}"}.
#' @note The slots are described in
#' \code{"\link[=saeObj-class]{class?maSAE::saeObj}"}, since this is the main class
#' of the package.
#'
#' @section Methods:
#' \code{\link{predict}}
#' @seealso \code{"\linkS4class{saeObj}"}
#' \code{"\link[=saObj]{?maSAE::saObj}"}
#' @keywords classes
#' @export
#' @examples
#' showClass("sadObj")
setClass(
  Class = "sadObj",
  contains = "savObj",
  validity = function(object) {
    if (length(all.vars(methods::slot(object, "f"))) > 2) {
      return("formula has to be of structure predictand ~ NULL | small_area")
    }
    varnames <- all.vars(methods::slot(object, "f"))
    small_area <- varnames[length(varnames)]
    predictand <- varnames[1]
    include <- methods::slot(object, "data")[, methods::slot(object, "include")]

    if (is.null(methods::slot(object, "include"))) {
      !any(is.na(methods::slot(object, "data")[, predictand])) ||
          return("can't handle missing values for predictand")
    } else {
        !any(is.na(methods::slot(object, "data")[, predictand])) ||
            return("can't handle missing values for predictand")
        !any(is.na(methods::slot(object, "data")[include, predictand])) ||
            return("can't handle missing values for predictand")
    }
  }
)

#' Small Area Estimation Objects
#'
#' Class for small area estimation, the one you're probably looking for.
#'
#' \code{cluster} optionally gives the name of a variable in slot \code{data}
#' from which the cluster information for clustered sample designs is to be
#' read.
#' See Manadallaz 2013, p. 445 for Details.\cr
#' \code{include} optionally gives the name of a variable in slot \code{data}
#' from which the inclusion indicator for cluster points is to be read.
#' See Manadallaz 2013, p. 445 for Details on \eqn{I_f}.\cr
#' Also see the \bold{Details} for \code{\link{predict}}.
#'
#' @name saeObj-class
#' @aliases saeObj-class
#' @docType class
#'
#' @section Extends: Class \code{"\linkS4class{savObj}"}, directly.
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("saeObj", ...)} or via the constructor function
#' \code{"\link[=saObj]{?maSAE::saObj}"} (recommended).
#'
#' @slot smallAreaMeans An \emph{optional} \code{"data.frame"}
#'    giving the true means of fixed effects for the small areas.
#'    Must have a column with the random effect defining the small areas in
#'    slot
#'    \code{data}.
#' @slot s1 An \emph{optional} \code{"character"} string giving the name of a
#'    variable in slot \code{data} indicating that an observation (a row in
#'    slot
#'    \code{data}) belongs to subset 1.
#' @slot s2 An \emph{optional} \code{"character"} string giving the name of a
#'    variable in slot \code{data} indicating that an observation (a row in
#'    slot
#'    \code{data}) belongs to subset 2.
#' @slot data Object of class \code{"data.frame"} to use for prediction,
#' typically
#'    consisting of a predictand and one or more predictors (zero or more fixed
#'    effects and one random effect defining the small areas).
#'    See \bold{Details} for optional clustering variable and/or inclusion
#'    indicator.
#' @slot f Object of class \code{"formula"} a linear mixed effects model
#' formula.
#' @slot cluster An \emph{optional} \code{"character"} string giving the name of
#' the
#'    clustering variable in slot \code{data}.
#' @slot include An \emph{optional} \code{"character"} string giving the name of
#' the
#'    inclusion indicator in slot \code{data}.
#' @slot auxiliaryWeights An \emph{optional} \code{"character"} string giving
#' the name of the
#'    auxiliary weights in slot \code{data}. You will need it, if your
#'    auxiliary
#'    data does not have full spatial support for each observation (for example
#'    when a shapefile does not completely cover all gird cells used to compute
#'    auxiliary data on).
#'    See
#'    \code{vignette("forestinventory_vignette", package = "forestinventory")}
#'    for details.
#'
#'
#' @section Methods: \code{\link{predict}}
#' @references
#' \cite{
#' Mandallaz, D. 2013
#' Design-based properties of some small-area estimators in forest
#' inventory with two-phase sampling.
#' Canadian Journal of Forest Research \bold{43}(5), pp. 441--449.
#' \doi{10.1139/cjfr-2012-0381}.
#' }
#'
#' @family classes
#' @seealso \code{"\link[stats:formula]{?stats::formula}"},
#' \code{"\link[=sadObj-class]{class?maSAE::saObj}"},
#' \code{"\link[=savObj-class]{class?maSAE::savObj}"},
#' \code{"\link[=saObj]{?maSAE::saObj}"} and
#' \code{"\link[=predict]{?maSAE::predict}"}
#' @keywords classes
#' @export
#' @examples
#' showClass("saeObj")
setClass(
  Class = "saeObj",
  contains = "savObj",
  slots = c(
    smallAreaMeans = "data.frameOrNULL",
    s1 = "characterOrNULL",
    s2 = "characterOrNULL",
    auxiliaryWeights = "characterOrNULL"
  ),
  validity = function(object) {
    varnames <- all.vars(methods::slot(object, "f"))
    small_area <- varnames[length(varnames)]
    predictors <- varnames[2:(length(varnames) - 1)]

    if (!is.null(methods::slot(object, "smallAreaMeans")) &&
        !is.null(methods::slot(object, "auxiliaryWeights"))) {
        if (all(predictors %in% names(methods::slot(object, "smallAreaMeans"))))
            if (is.null(methods::slot(object, "cluster"))) {
                warning("Got both all true means and auxiliary weights. Will ignore the latter.")
            }
    }
    predictors <- varnames[-c(1, which(varnames == small_area))]
    length(predictors) > 0 || return(paste("got no predictor(s).", sep = ""))
    all(predictors %in% colnames(methods::slot(object, "data"))) || return(paste("not all predictors found in data.", sep = ""))
    if (!is.null(methods::slot(object, "s1"))) {
      if (class(methods::slot(object, "data")[, methods::slot(object, "s1")]) != "logical") {
        return(paste("data$", methods::slot(object, "s1"), " has got to be of class 'logical'.", sep = ""))
      }
      s1_to_s0 <- sum(methods::slot(object, "data")[, methods::slot(object, "s1")]) / nrow(methods::slot(object, "data"))
      if (s1_to_s0 > 0.1) {
        message(paste("n(s0) >> n(s1) should hold, but you've given s1 resulting in n(s1)/n(s0) = ", s1_to_s0))
      }
    }
    if (!is.null(methods::slot(object, "s2"))) {
        methods::slot(object, "s2") %in% colnames(methods::slot(object, "data")) || return(paste("s2 indicator ", methods::slot(object, "s2"), " not found in data.", sep = ""))
      if (class(methods::slot(object, "data")[, methods::slot(object, "s2")]) != "logical") {
        return(paste("data$", methods::slot(object, "s2"), " has got to be of class 'logical'.", sep = ""))
      }
    } else {
      if (!is.null(methods::slot(object, "cluster"))) {
        return("need s2 for cluster sampling.")
      }
      if (is.null(methods::slot(object, "smallAreaMeans"))) {
        return("got neither s2 nor smallAreaMeans.")
      } else {
        if (!all(predictors %in% colnames(methods::slot(object, "smallAreaMeans")))) {
          return("got neither s2 nor exhaustive smallAreaMeans.")
        }
      }
    }
    if (!is.null(methods::slot(object, "s1")) && !is.null(methods::slot(object, "s2"))) {
      if (any(methods::slot(object, "data")[, methods::slot(object, "s2")] & !methods::slot(object, "data")[, methods::slot(object, "s1")])) {
        return("s2 is not a subset of s1!")
      }
    }
    if (!is.null(methods::slot(object, "smallAreaMeans"))) {
      if (!is.null(methods::slot(object, "s1"))) {
        if (all(predictors %in% names(methods::slot(object, "smallAreaMeans")))) {
            return("Got both all true means and s1. If the null phase was exhaustive, then you don't need it's data!")
        } else {
            return("Got both partial true means and s1. There is no theoretical description for this so far!")
        }
      }
      if (any(is.na(methods::slot(object, "smallAreaMeans")))) {
        return("Can't deal with missing data in smallAreaMeans")
      }
      if (!all(methods::slot(object, "smallAreaMeans")[, small_area] %in%
        unique(methods::slot(object, "data")[, small_area][!is.na(methods::slot(object, "data")[, small_area])]))) {
        return("found extraneous smallAreas in smallAreaMeans")
      }
      if (!all(unique(methods::slot(object, "data")[, small_area][!is.na(methods::slot(object, "data")[, small_area])]) %in%
        methods::slot(object, "smallAreaMeans")[, small_area])) {
        return("missing smallAreas in smallAreaMeans")
      }
      if (!all(colnames(methods::slot(object, "smallAreaMeans")) %in% c(predictors, small_area))) {
        return(paste("smallAreaMeans don't really fit.",
                     "Check predictor and small_area naming and",
                     "remove all unnecessary variables from the frame."))
      }
    }
    if (!is.null(methods::slot(object, "auxiliaryWeights"))) {
        weights <- methods::slot(object, "data")[[methods::slot(object, "auxiliaryWeights")]]
        methods::slot(object, "auxiliaryWeights") %in% colnames(methods::slot(object, "data")) ||
            return(paste("auxiliary weight ",
                         methods::slot(object, "auxiliaryWeights"), " not found in data.",
                         sep = ""))
        if (!is.numeric(weights)) {
            return(paste(methods::slot(object, "auxiliaryWeights"), " has got to be of class 'numeric'.", sep = ""))
        } else {
            r <- range(weights)
            r[1] >= 0 && r[2] <= 1 ||
                return(paste("range of auxiliary weights in",
                             methods::slot(object, "auxiliaryWeights"),
                             "has to be in [0, 1]."))
        }
    }

  }
)
