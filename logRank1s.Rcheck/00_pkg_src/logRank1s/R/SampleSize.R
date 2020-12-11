##' calculate sample size in one-sample log-rank test
##'
##' @title sample size calculation in one-sample log-rank test
##' @param alpha type I error rate, by default alpha = 0.05
##' @param power the desired power for the study you are planning
##' @param ta length of accrual period, during which patients are recruited
##' @param tf length of follow-up time, during which patients are monitored
##' @param m0 median survival time of the standard population or historical control, which can be obtained from previous literature or estimated directly from the standard population.
##' @param delta hazard ratio between the sample of interest and the standard population or historical control.
##' @param k shape parameter of survival functions (by default k = 1), can be obtained from the standard population or historical control.
##' @return sample size in one-sample log-rank test, depends on the desired power
##' @export
##' @import stats
##' @examples
##' n <- SampleSize(alpha = 0.05, power = 0.8, ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
##' # n = 88
##'
##'@references
##' Wu, J. R. (2015). Sample size calculation for the one-sample log-rank test. Pharmaceutical   Statistics, 14, 26–33. https://doi.org/10.1002/pst.1654

SampleSize <- function(alpha = 0.05, power, ta, tf, m0, delta, k=1) {
    stopifnot(is.numeric(alpha), is.numeric(power),
              is.numeric(ta), is.numeric(tf),
              is.numeric(m0), is.numeric(delta),
              is.numeric(k), alpha > 0 & alpha < 1,
              power > 0 & power < 1, ta > 0, tf > 0,
              m0 > 0, delta > 0, k > 0)

    p0 <- Fp0(alpha, ta, tf, m0, delta, k)
    p1 <- delta * p0

    p00 <- Fp00(alpha, ta, tf, m0, delta, k)
    p01 <- delta * p00

    w <- p1 - p0
    sigma <- (p1 - p1^2 + 2 * p00 - p0^2 - 2 * p01 + 2 * p0 * p1)^0.5
    n <- ceiling((p0^0.5 * qnorm(1 - alpha) + sigma * qnorm(power))^2/w^2)
}
