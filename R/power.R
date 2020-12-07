##' calculate power in one-sample log-rank test
##'
##' @title power calculation in one-sample log-rank test
##' @param alpha type I error rate
##' @param n sample size
##' @param ta length of accrual period, during which patients are recruited
##' @param tf length of follow-up time, during which patients are monitored
##' @param m0 median survival time of the historical control sample, which shall be obtained from previous research.
##' @param delta expected effect size.
##' @param k shape parameter of survival functions (assumed to be Weibull).
##' @return power in one-sample log-rank test
##' @export
##' @import stats
##' @examples
##' power(alpha = 0.05, n = 88,  ta = 5, tf = 3, m0 = 9, delta = 4/7, k = 1.22)

power <- function(alpha, n, ta, tf, m0, delta, k) {
    p0 <- Fp0(alpha, ta, tf, m0, delta, k)
    p1 <- delta * p0

    p00 <- Fp00(alpha, ta, tf, m0, delta, k)
    p01 <- delta * p00

    w <- p1 - p0
    sigma <- (p1 - p1^2 + 2 * p00 - p0^2 - 2 * p01 + 2 * p0 * p1)^0.5

    power <- round(pnorm(-p0^0.5 * qnorm(1 - alpha)/sigma - n^0.5 * w/sigma),3)

}

