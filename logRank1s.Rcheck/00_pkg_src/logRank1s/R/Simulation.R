##' calculate empirical type I error and/or empirical power in one-sample log-rank test via simulation
##'
##' @title empirical type I error and power
##' @param n sample size
##' @param parameter logical value, if TRUE then calculate empirical alpha, if FALSE calculate empirical power
##' @param B number of iterations used in simulation (by default B = 1000)
##' @param k shape parameter of survival functions (by default k = 1) from the standard population or historical control
##' @param delta hazard ratio between the sample of interest and the standard population or historical control
##' @param m0 median survival time of the standard population or historical control
##' @param ta length of accrual period, during which patients are recruited
##' @param tf length of follow-up time, during which patients are monitored
##' @return empirical type I error or empirical power in one-sample log-rank test
##' @export
##' @import stats
##' @examples
##' a <- Simulation(n = 534, parameter = TRUE, B = 10000, ta = 3, tf = 1,
##' m0 = 1, delta = 1/1.2, k = 0.1)
##' # parameter = T, calculate empirical type I error
##' # a = 0.0472
##'
##' b <- Simulation(n = 534, parameter = FALSE, B = 10000, ta = 3, tf = 1,
##' m0 = 1, delta = 1/1.2, k = 0.1)
##' # parameter = T, calculate empirical power
##' # b = 0.9052
##'
##'@references
##' Wu, J. R. (2015). Sample size calculation for the one-sample log-rank test. Pharmaceutical   Statistics, 14, 26–33. https://doi.org/10.1002/pst.1654


Simulation <- function(n, parameter, B=1000, ta, tf, m0, delta, k = 1) {
    stopifnot(is.numeric(n), is.logical(parameter), is.numeric(B),
              is.numeric(ta), is.numeric(tf),
              is.numeric(m0), is.numeric(delta),
              is.numeric(k), B > 0,
              n > 0, ta > 0, tf > 0, m0 > 0, delta > 0, k > 0,
              n %% 1 == 0, B %% 1 == 0)

    tau <- ta + tf
    # if T, use lambda = lambda0 under H0; if F, use lambda = lambda1 under H1
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

    # m <- ifelse(parameter == T,
    #                  paste("empirical type I error = ", value, "."),
    #                  paste("empirical power = ", value, "."))
    # message(m)

    return(value)
}
