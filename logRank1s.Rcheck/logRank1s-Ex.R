pkgname <- "logRank1s"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('logRank1s')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("SampleSize")
### * SampleSize

flush(stderr()); flush(stdout())

### Name: SampleSize
### Title: sample size calculation in one-sample log-rank test
### Aliases: SampleSize

### ** Examples

n <- SampleSize(alpha = 0.05, power = 0.8, ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
# n = 88




cleanEx()
nameEx("Simulation")
### * Simulation

flush(stderr()); flush(stdout())

### Name: Simulation
### Title: empirical type I error and power
### Aliases: Simulation

### ** Examples

a <- Simulation(n = 534, parameter = TRUE, B = 10000, ta = 3, tf = 1,
m0 = 1, delta = 1/1.2, k = 0.1)
# parameter = T, calculate empirical type I error
# a = 0.0472

b <- Simulation(n = 534, parameter = FALSE, B = 10000, ta = 3, tf = 1,
m0 = 1, delta = 1/1.2, k = 0.1)
# parameter = T, calculate empirical power
# b = 0.9052




cleanEx()
nameEx("power")
### * power

flush(stderr()); flush(stdout())

### Name: power
### Title: power calculation in one-sample log-rank test
### Aliases: power

### ** Examples

Power <- power(alpha = 0.05, n = 88,  ta = 5, tf = 3, m0 = 9, delta = 1/1.75, k = 1.22)
# Power = 0.803




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
