##' calculate empirical type I error and power in one-sample log-rank test
##'
##' @title empirical type I error and power
##' @param n sample size
##' @param parameter if true, calculate alpha, otherwise calculate power
##' @param B number of iterations used in simulation
##' @param k shape parameter of survival functions (assumed to be Weibull).
##' @param delta expected effect size.
##' @param m0 median survival time of the historical control sample, which shall be obtained from previous research.
##' @param ta length of accrual period, during which patients are recruited
##' @param tf length of follow-up time, during which patients are monitored
##' @return type I error or power in one-sample log-rank test
##' @export
##' @import stats
##' @examples
##' Simulation(n = 534, parameter = TRUE, B = 10000, ta = 3, tf = 1, m0 = 1, delta = 1/1.2, k = 0.1)
##' Simulation(n = 534, parameter = FALSE, B = 10000, ta = 3, tf = 1, m0 = 1, delta = 1/1.2, k = 0.1)


Simulation <- function(n, parameter, B=10000, k, delta, m0, ta, tf) {
    tau <- ta + tf

    # parameter = T, calculate alpha; ow, calculate power
    lambda <- ifelse(parameter == TRUE,
                     (log(2))^(1/k)/m0,
                     (log(2))^(1/k)/m0 *delta^(1/k))  #lambda under H1

    indicator <- rep(NA, n)
    L <- rep(NA, B)
    culH <- rep(NA, n)

    H0 <- function(x) log(2) * (x/m0)^k


    for (b in 1:B) {

        set.seed(b)

        Ts <- rweibull(n, shape = k, scale = 1/lambda)

        Cs <- runif(n, min = tf, max = tau)

        Xs <- pmin(Ts, Cs)


        for (i in 1:n) {
            indicator[i] <- Ts[i] < Cs[i]
            culH[i] <- H0(Xs[i])
        }

        O <- sum(indicator)

        E <- sum(culH)

        L[b] <- (O - E)/sqrt(E)

    }

    count <- L < (-qnorm(.95))


    value <- sum(count)/B

    return(value)

    ifelse(parameter == T,
                      message("empirical type I error = ", value, "."),
                      message("empirical power = ", value, "."))
}
