##' calculate sample size in one-sample log-rank test
##'
##' @title sample size calculation in one-sample log-rank test
##' @param alpha type I error rate
##' @param power desired power for your study
##' @param ta length of accrual period, during which patients are recruited
##' @param tf length of follow-up time, during which patients are monitored
##' @param m0 median survival time of the historical control sample, which shall be obtained from previous research.
##' @param delta expected effect size.
##' @param k shape parameter of survival functions (assumed to be Weibull).
##' @return sample size in one-sample log-rank test
##' @export
##' @import stats
##' @examples
##' SampleSize(alpha = 0.05, power = 0.8, ta = 5, tf = 3, m0 = 9, delta = 0.57, k = 1.22)

SampleSize <- function(alpha, power, ta, tf, m0, delta, k) {
    p0 <- Fp0(alpha, ta, tf, m0, delta, k)
    p1 <- delta * p0

    p00 <- Fp00(alpha, ta, tf, m0, delta, k)
    p01 <- delta * p00

    w <- p1 - p0
    sigma <- (p1 - p1^2 + 2 * p00 - p0^2 - 2 * p01 + 2 * p0 * p1)^0.5
    n <- ceiling((p0^0.5 * qnorm(1 - alpha) + sigma * qnorm(power))^2/w^2)
}
