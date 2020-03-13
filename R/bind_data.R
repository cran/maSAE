#' Merge Data Sets
#'
#' You will usually have different data sets for the first, second and possibly
#' a null phase. This functions binds them into a data sets suitable to create
#' an object of class \code{\linkS4class{saeObj}}.
#' @note This is experimental!
#' @param s1 The first phase \code{\link{data.frame}}.
#' @param s2 The second phase \code{\link{data.frame}}.
#' @param s0 Possibly a null phase \code{\link{data.frame}}.
#' @keywords internal
#' @export
#' @return A \code{\link{data.frame}} with added boolean variables \code{phase1}
#' and \code{phase2}.
#' @examples
#' data(list= paste0("s", 1:2), package = "maSAE")
#' str(s1)
#' str(s2)
#' str(bind_data(s1, s2))
bind_data <- function(s1, s2, s0 = NULL) {
    s1$phase1 <- TRUE
    s1$phase2 <- FALSE
    s2$phase1 <- s2$phase2 <- TRUE
    s1[TRUE, setdiff(names(s2), names(s1))] <- NA
    data <- rbind(s1, s2)
    if (!is.null(s0)) {
        s0$phase1 <- s0$phase2 <- FALSE
        s0[TRUE, setdiff(names(data), names(s0))] <- NA
        data <- rbind(s0, data)
    }
    return(data)
}
