## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(logRank1s)

## ----eval=F-------------------------------------------------------------------
#  # install from tar.gz
#  install.packages(file.path("~/Desktop","logRank1s_0.0.0.9000.tar.gz"),repos=NULL,type="source")
#  
#  # install from Github
#  devtools::install_github("XiaomengYuan9709/logRank1s")

## -----------------------------------------------------------------------------
# sample size for power = 0.8
n1 <- SampleSize(alpha = 0.05, power = 0.8, ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
n1

# sample size for power = 0.85
n2 <- SampleSize(alpha = 0.05, power = 0.85, ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
n2

# sample size for power = 0.9
n3 <- SampleSize(alpha = 0.05, power = 0.9, ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
n3

## -----------------------------------------------------------------------------
# n = 88
Power1 <- power(alpha = 0.05, n = 88,  ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
Power1

# n = 100
Power2 <- power(alpha = 0.05, n = 100,  ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
Power2

# n = 117
Power3 <- power(alpha = 0.05, n = 117,  ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
Power3

## -----------------------------------------------------------------------------
# setting parameter = T indicates calculating type I error.
alpha <- Simulation(n = 534, parameter = TRUE, B = 10000, ta = 3, tf = 1, m0 = 1, delta = 1/1.2, k = 0.1)

alpha


## -----------------------------------------------------------------------------
# setting parameter = F indicates calculating power.
power_empirical <- Simulation(n = 534, parameter = FALSE, B = 10000, ta = 3, tf = 1, m0 = 1, delta = 1/1.2, k = 0.1)

power_empirical


