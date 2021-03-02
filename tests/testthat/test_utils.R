test_that(".s2numeric", {
    expect_error(.s2numeric("foo"), "cv.glmnet.* object")

    set.seed(1010)
    n <- 200
    p <- 30
    x <- matrix(rnorm(n * p), n, p)
    nzc <- trunc(p/10)
    beta <- rnorm(nzc)
    fx <- x[, seq(nzc)] %*% beta
    eps <- rnorm(n) * 5
    y <- drop(fx + eps)

    rcv <- rcv.glmnet(
        x, y, nrepcv = 1, nfolds = 3, mc.cores = 1, trace.it = FALSE
    )
    expect_error(.s2numeric(rcv, "foo"))
    expect_equal(.s2numeric(rcv, "lambda.min"), rcv[["lambda.min"]])
    expect_equal(.s2numeric(rcv, c(0.1, 0.2)), c(0.1, 0.2))
})
