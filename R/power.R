##' calculate power in one-sample log-rank test
##'
##' @title power calculation in one-sample log-rank test
##' @param alpha type I error rate, by default alpha = 0.05.
##' @param n sample size
##' @param ta length of accrual period, during which patients are recruited
##' @param tf length of follow-up time, during which patients are monitored
##' @param m0 median survival time of the standard population or historical control, which can be obtained from previous literature or estimate directly from the standard population.
##' @param delta hazard ratio between the sample of interest and the standard population or historical control
##' @param k shape parameter of survival functions (by default k = 1), from the standard population or historical control.
##' @return power in one-sample log-rank test
##' @export
##' @import stats
##' @examples
##' Power <- power(alpha = 0.05, n = 88,  ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
##' # Power = 0.803
##'
##'@references
##' Wu, J. R. (2015). Sample size calculation for the one-sample log-rank test. Pharmaceutical   Statistics, 14, 26–33. https://doi.org/10.1002/pst.1654

power <- function(alpha = 0.05, n, ta, tf, m0, delta, k=1) {
    stopifnot(is.numeric(alpha), is.numeric(n),
              is.numeric(ta), is.numeric(tf),
              is.numeric(m0), is.numeric(delta),
              is.numeric(k), alpha > 0 & alpha < 1,
              n > 0, ta > 0, tf > 0, m0 > 0, delta > 0, k > 0,
              n %% 1 == 0)

    p0 <- Fp0(alpha, ta, tf, m0, delta, k)
    p1 <- delta * p0

    p00 <- Fp00(alpha, ta, tf, m0, delta, k)
    p01 <- delta * p00

    w <- p1 - p0
    sigma <- (p1 - p1^2 + 2 * p00 - p0^2 - 2 * p01 + 2 * p0 * p1)^0.5

    power <- round(pnorm(-p0^0.5 * qnorm(1 - alpha)/sigma - n^0.5 * w/sigma),3)
    return(power)
}

