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

test_that("predict.rcv.glmnet", {
    # taken from glmnet::cv.glmnet
    set.seed(10101)
    n <- 500
    p <- 30
    nzc <- trunc(p / 10)
    x <- matrix(rnorm(n * p), n, p)
    beta <- rnorm(nzc)
    fx <- x[, seq(nzc)] %*% beta / 3
    hx <- exp(fx)
    ty <- rexp(n, hx)
    tcens <- rbinom(n = n, prob = 0.3, size = 1)  # censoring indicator
    y <- cbind(time = ty, status = 1 - tcens)

    expect_error(
            predict(
                rcv.glmnet(
                    x, y[, "status"], nrepcv = 1, nfolds = 3,
                    mc.cores = 1, trace.it = FALSE
                ),
                type = "survival"
            ),
            "family = .*cox.*"
    )

    rcvobj <- rcv.glmnet(
        x, y, family = "cox", nrepcv = 1, nfolds = 3,
        mc.cores = 1, trace.it = FALSE
    )
    expect_error(
        predict(rcvobj, type = "survival", s = c(0.1, 0.2)), "length 1"
    )

    expect_equal(
        predict(rcvobj, newx = x[1:5,], type = "link", s = "lambda.1se"),
        predict(
            rcvobj$glmnet.fit, newx = x[1:5,],
            type = "link", s = rcvobj[["lambda.1se"]]
        )
    )

    skip_if_not_installed("survival")
    requireNamespace("survival")

    expect_equal(
        predict(
            rcvobj,
            x = x, y = survival::Surv(y[, "time"], y[, "status"]),
            newx = x[1:5,],
            type = "survival", s = "lambda.1se", times = c(0, 7)
        ),
        matrix(
            c(rep(1, 5), 0.1312533, 0.05047742, 0.002480262, 0.01006882,
              0.02348179),
            byrow = TRUE, nrow = 2, ncol = 5,
            dimnames = list(NULL, c(as.character(1:5)))
        )
    )

    expect_equal(
        dim(predict(
            rcvobj,
            x = x, y = survival::Surv(y[, "time"], y[, "status"]),
            newx = x[1:5,],
            type = "survival", s = "lambda.1se"
        )), c(343, 5)
    )
})
