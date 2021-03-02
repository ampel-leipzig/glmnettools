#' Convert s into its numeric equivalent
#'
#' This function converts s/lambda to its numeric equivalent.
#'
#' @param object `cv.glmnet`, object.
#' @param s `numeric`/`character`.
#' @return `numeric`:
#' @importFrom methods is
#' @noRd
.s2numeric <- function(object, s = c("lambda.1se", "lambda.min")) {
    if(!is(object, "cv.glmnet"))
        stop("'object' has to be an 'cv.glmnet' object.")
    if (is.character(s))
        object[[match.arg(s)]]
    else
        s
}
