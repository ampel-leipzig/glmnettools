#' Repeated Cross-Validation for cv.glmnet
#'
#' This functions returns the best lambda of repeated [`glmnet::cv.glmnet()`]
#' calls.
#'
#' @param x `matrix`, as in `cv.glmnet`.
#' @param y response as in `cv.glmnet`.
#' @param lambda `numeric`, optional user-supplied lambda sequence; default is
#' `NULL` and `glmnet` chooeses its own sequence.
#' @param nrepcv `integer(1)`, number of repeated cross-validations (outer
#' loop).
#' @param nfolds `integer`, number of folds, same as in `cv.glmnet`.
#' @param \dots further arguments passed to `cv.glmnet`.
#' @param trace.it `integer`, if `trace.it = 1`, then a progress bar is
#' displayed.
#' @param mc.cores `integer`, number of cores used for parallelisation.
#'
#' @return An object of class `rcv.glmnet` that extends the `cv.glmnet`
#' class.
#'
#' @author Sebastian Gibb
#' @seealso [`glmnet::cv.glmnet()`]
#' @references
#' Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
#'  Regularization Paths for Generalized Linear Models via Coordinate
#'  Descent. Journal of Statistical Software, 33(1), 1-22. URL
#'  \url{https://www.jstatsoft.org/v33/i01/}.
#'
#'  Noah Simon, Jerome Friedman, Trevor Hastie, Rob Tibshirani (2011).
#'  Regularization Paths for Cox's Proportional Hazards Model via
#'  Coordinate Descent. Journal of Statistical Software, 39(5), 1-13. URL
#'  \url{https://www.jstatsoft.org/v39/i05/}.
#'
#' @importFrom parallel mclapply
#' @importFrom glmnet cv.glmnet
#' @usage rcv.glmnet(
#'     x, y,
#'     lambda = NULL,
#'     nrepcv = 100L, nfolds = 10L,
#'     ...,
#'     trace.it = interactive(), mc.cores = getOption("mc.cores", 1L)
#' )
#' @export rcv.glmnet
#' @examples
#' # Examples taken from ?"glmnet::cv.glmnet"
#' set.seed(1010)
#' n <- 1000
#' p <- 100
#' nzc <- trunc(p/10)
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(nzc)
#' fx <- x[, seq(nzc)] %*% beta
#' eps <- rnorm(n) * 5
#' y <- drop(fx + eps)
#' set.seed(1011)
#' # nrepcv should usually be higher but to keep the runtime of the example low
#' # we choose 2 here
#' rcvob <- rcv.glmnet(x, y, nrepcv = 2)
#' plot(rcvob)
#' title("Gaussian Family", line = 2.5)
#' coef(rcvob)
#' predict(rcvob, newx = x[1:5, ], s = "lambda.min")
rcv.glmnet <- function(x, y, lambda = NULL, nrepcv = 100L, nfolds = 10L, ...,
                          trace.it = interactive(),
                          mc.cores = getOption("mc.cores", 1L)) {
    if (as.logical(trace.it) && requireNamespace("pbapply")) {
        rcv <- pbapply::pblapply(
            integer(nrepcv),
            function(i)cv.glmnet(
                x = x, y = y, lambda = lambda, nfolds = nfolds, ...
            ),
            cl = mc.cores
        )
    } else {
        rcv <- parallel::mclapply(
            integer(nrepcv),
            function(i)cv.glmnet(
                x = x, y = y, lambda = lambda, nfolds = nfolds, ...
            ),
            mc.cores = mc.cores
        )
    }

    cvm <- cvsd <- matrix(
        NA_real_, nrow = length(rcv[[1L]]$lambda), ncol = nrepcv
    )

    for (i in seq(along=rcv)) {
        cvm[, i] <- rcv[[i]]$cvm
        cvsd[, i] <- rcv[[i]]$cvsd
    }

    cvm <- rowMeans(cvm)
    cvsd <- rowMeans(cvsd)
    i <- which.min(cvm)

    index <- matrix(
        c(i, which(cvm < (cvm[i] + cvsd[i]))[1L]),
        nrow = 2L, ncol = 1L, dimnames = list(c("min", "1se"), "Lambda")
    )

    out <- list(
        call = match.call(),
        cvm = cvm,
        cvsd = cvsd,
        cvup = cvm + cvsd,
        cvlo = cvm - cvsd,
        index = index,
        nrepcv = nrepcv,
        nfolds = nfolds,
        lambda.min = rcv[[1L]]$lambda[index["min",]],
        lambda.1se = rcv[[1L]]$lambda[index["1se",]]
    )
    out <- c(out, rcv[[1L]][c("glmnet.fit", "lambda", "name", "nzero")])
    class(out) <- c("rcv.glmnet", class(rcv[[1L]]))
    out
}

#' @noRd
#' @importFrom graphics title
#' @method plot rcv.glmnet
#' @export
plot.rcv.glmnet <- function(x, ...) {
    NextMethod()
    title(
        sub = paste(
            "Averaged across", x$nrepcv, "repeated cross validations",
            "each with", x$nfolds, "folds."
        ),
        adj = 0L
    )
}
