#' Convert coxnet objects
#'
#' These functions convert `coxnet` objects into popular survival objects like
#' `survival::coxph` or `rms::cph`.
#'
#' @param object object that should be converted.
#'
#' @rdname as
#' @author Sebastian Gibb
#' @export
as.coxph <- function(object, ...) {
    UseMethod("as.coxph")
}

#' @rdname as
#' @param s `character`/`numeric`, value(s) of the penality parameter `lambda`.
#' @param x `matrix`, as in `glmnet`.
#' @param y response as in `glmnet`.
#' @param \dots further arguments passed to the underlying function
#' (`survival::coxph` or `rms::cph`).
#' @seealso [`glmnet::glmnet()`]
#' @method as.coxph rcv.glmnet
#' @export
as.coxph.rcv.glmnet <- function(object, s, x, y, ...) {
    as.coxph(object$glmnet.fit, s = .s2numeric(object, s), x = x, y = y, ...)
}

#' @rdname as
#' @method as.coxph coxnet
#' @export
as.coxph.coxnet <- function(object, s, x, y, ...) {
    requireNamespace("survival")
    if (!is(y, "Surv") && is(y, "matrix") && ncol(y) == 2L)
        y <- survival::Surv(y[, 1L], y[, 2L])
    glmnet:::mycoxph(object = object, s = s, x = x, y = y, ...)
}

#' @rdname as
#' @export
as.cph <- function(object, ...) {
    UseMethod("as.cph")
}

#' @rdname as
#' @seealso [`rms::cph()`]
#' @method as.cph rcv.glmnet
#' @export
as.cph.rcv.glmnet <- function(object, s, x, y, ...) {
    as.cph(object$glmnet.fit, s = .s2numeric(object, s), x = x, y = y, ...)
}

#' @rdname as
#' @importFrom stats as.formula predict
#' @param time.inc `numeric(1)`, time increment, see [`rms::cph()`] for details.
#' @method as.cph coxnet
#' @export
as.cph.coxnet <- function(object, s, x, y, time.inc = 30, ...) {
    requireNamespace("rms")
    requireNamespace("survival")
    if (!is(y, "Surv") && is(y, "matrix") && ncol(y) == 2L)
        y <- survival::Surv(y[, 1L], y[, 2L])

    ## taken from glmnet version 4.1-1 glmnet:::mycoxph
    args <- list(...)
    glmnet_call_names <- names(object$call)[-1]
    args$object <- object
    args$newx <- x
    args$s <- s
    args$x <- x
    args$y <- y
    if ("offset" %in% glmnet_call_names) {
        args$newoffset <- rep(0, length.out = nrow(x))
    }
    eta <- do.call(predict, args)
    coxphargs <- list(
        formula = "y ~ X1", data = data.frame(y, eta),
        init = 1, iter = 0, x = TRUE, y = TRUE, surv = TRUE,
        time.inc = time.inc
    )
    if ("strata" %in% names(attributes(y))) {
        coxphargs$data$strata <- attr(y, "strata")
        coxphargs$formula <- paste(coxphargs$formula, "+ strata(strata)")
    }
    if ("weights" %in% glmnet_call_names) {
        coxphargs$weights <- args$weights
    }
    if ("offset" %in% glmnet_call_names) {
        coxphargs$data$offset <- args$offset
        coxphargs$formula <- paste(coxphargs$formula, "+ offset(offset)")
    }
    coxphargs$formula <- as.formula(coxphargs$formula)
    do.call(rms::cph, coxphargs)
}
