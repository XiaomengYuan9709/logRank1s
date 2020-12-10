# calculate p0 = sigma0^2

Fp0 <- function(alpha, ta, tf, m0, delta, k) {
    tau <- ta + tf

    F1p0 <- function(t) {


        # survival function of standard sample assuming Weibull distribution S0(t) =
        # exp(-log(2)*(t/m0)^k)

        # hazard function assuming Weibull distribution lambda0(t)
        # k*log(2)*t^(k-1)/m0^k


        # S0(t)^delta*lambda0(t)
        (exp(-log(2) * (t/m0)^k))^delta * (k * log(2) * t^(k - 1)/m0^k)

    }


    F2p0 <- function(t) {


        # (tau-t)*S0(t)^delta*lambda0(t)
        (tau - t) * (exp(-log(2) * (t/m0)^k))^delta * (k * log(2) * t^(k - 1)/m0^k)

    }

    p0 <- integrate(F1p0, 0, tf)$value + (1/ta) * integrate(F2p0, tf, tau)$value

}

