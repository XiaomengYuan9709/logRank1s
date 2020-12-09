
Fp00 <- function(alpha, ta, tf, m0, delta, k) {
    tau <- ta + tf

    F1p00 <- function(t) {



        # survival function of standard sample assuming Weibull distribution
        # S0(t) = exp(-log(2)*(t/m0)^k)

        # hazard function assuming Weibull distribution
        # lambda0(t) = k*(log(2)*t^(k-1)/m0^k

        # cumulative hazard H0(t) = log(2)(t/m0)^k

        # S0(t)^delta*H0(t)*lambda0(t)
        (exp(-log(2) * (t/m0)^k)^delta) * (log(2) * (t/m0)^k) * (k * log(2) *
            t^(k - 1)/m0^k)

    }

    F2p00 <- function(t) {

        # (tau-t)*S0(t)^delta*H0(t)*lambda0(t)
        (tau - t) * (exp(-log(2) * (t/m0)^k)^delta) * (log(2) * (t/m0)^k) *
            (k * log(2) * t^(k - 1)/m0^k)

    }

    p00 <- integrate(F1p00, 0, tf)$value + (1/ta) * integrate(F2p00, tf, tau)$value


}
