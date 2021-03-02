test_that("rcv.glmnet", {
    # taken from glmnet::cv.glmnet
    set.seed(1010)
    n <- 1000
    p <- 100
    nzc <- trunc(p/10)
    x <- matrix(rnorm(n * p), n, p)
    beta <- rnorm(nzc)
    fx <- x[, seq(nzc)] %*% beta
    eps <- rnorm(n) * 5
    y <- drop(fx + eps)
    set.seed(1011)
    cv <- list(
        cv1 = cv.glmnet(x, y, nfolds = 3),
        cv2 = cv.glmnet(x, y, nfolds = 3)
    )
    set.seed(1011)
    rcv <- rcv.glmnet(
        x, y, nrepcv = 2, nfolds = 3, mc.cores = 1, trace.it = FALSE
    )
    expect_equal(
        rcv$cvm,
        colMeans(do.call(rbind, list(cv[[1]]$cvm, cv[[2]]$cvm)))
    )
    expect_equal(
        rcv$index,
        matrix(
            c(25, 17), nrow = 2,
            dimnames = list(c("min", "1se"), c("Lambda"))
        )
    )

#' # each base::graphics plot function must be wrapped by an anonymous function
#' # that could be called by `vdiffr::expect_doppelganger()`
#' # use `vdiffr::manage_cases()` to add new/verify changed plots
#' vdiffr::manage_cases()
    vdiffr::expect_doppelganger("rcv.glmnet-plot", function()plot(rcv))
})
