% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Simulation.R
\name{Simulation}
\alias{Simulation}
\title{empirical type I error and power}
\usage{
Simulation(n, parameter, B = 1000, ta, tf, m0, delta, k = 1)
}
\arguments{
\item{n}{sample size}

\item{parameter}{logical value, if TRUE then calculate empirical alpha, if FALSE calculate empirical power}

\item{B}{number of iterations used in simulation (by default B = 1000)}

\item{ta}{length of accrual period, during which patients are recruited}

\item{tf}{length of follow-up time, during which patients are monitored}

\item{m0}{median survival time of the standard population or historical control}

\item{delta}{hazard ratio between the sample of interest and the standard population or historical control}

\item{k}{shape parameter of survival functions (by default k = 1) from the standard population or historical control}
}
\value{
empirical type I error or empirical power in one-sample log-rank test
}
\description{
calculate empirical type I error and/or empirical power in one-sample log-rank test via simulation
}
\examples{
a <- Simulation(n = 534, parameter = TRUE, B = 10000, ta = 3, tf = 1,
m0 = 1, delta = 1/1.2, k = 0.1)
# parameter = T, calculate empirical type I error
# a = 0.0472

b <- Simulation(n = 534, parameter = FALSE, B = 10000, ta = 3, tf = 1,
m0 = 1, delta = 1/1.2, k = 0.1)
# parameter = T, calculate empirical power
# b = 0.9052

}
\references{
Wu, J. R. (2015). Sample size calculation for the one-sample log-rank test. Pharmaceutical   Statistics, 14, 26–33. https://doi.org/10.1002/pst.1654
}
