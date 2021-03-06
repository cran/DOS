\name{schoket}
\alias{schoket}
\concept{Observational study}
\concept{Sensitivity analysis}
\concept{Matched pairs}
\concept{Causal inference}
\concept{Nonresponders}
\concept{Design sensitivity}
\concept{Design of Observational Studies}
\docType{data}
\title{
DNA Damage in Aluminum Production Workers
}
\description{
This data set is from Schoket et al. (1991) and is discussed in Chapter 16 of Design of Observational Studies (2010).  The data describe 25 aluminum production workers (w) and 25 controls (c) matched for age and smoking. The outcome is a measure of genetic damage, namely DNA adducts per 10^8 nucleotides.  The data are used as an example in Chapter 16 of Design of Observational Studies, where the data illustrate the possibility that some treated individuals are strongly affected by treatment, while others are unaffected, so the average treatment effect may be small, but the effect on affected individuals may be large and evident in data.
}
\usage{data("schoket")}
\format{
  A data frame with 25 observations on the following 9 variables.
  \describe{
    \item{\code{pair}}{Pair number, 1 to 25.}
    \item{\code{idw}}{Worker ID from Schoket et al. (1991).}
    \item{\code{agew}}{Worker age in years}
    \item{\code{smokingw}}{Worker cigarettes per day}
    \item{\code{adductsw}}{Worker DNA adducts}
    \item{\code{idc}}{Control ID from Schoket et al. (1991).}
    \item{\code{agec}}{Control age in years}
    \item{\code{smokingc}}{Control cigarettes per day}
    \item{\code{adductsc}}{Control DNA adducts}
  }
}
\source{
The data are from Schoket et al. (1991).  The data are used as an example in Chapter 16 of
Design of Observational Studies (2010).
}
\references{
Conover, W. J. and Salsburg, D. S. (1988). Locally most powerful tests for detecting treatment effects when only a subset of patients can be expected to" respond" to treatment. Biometrics, 189-196.

Rosenbaum, P. R. (2007). Confidence intervals for uncommon but dramatic responses to treatment. Biometrics, 63(4), 1164-1171.

Rosenbaum, P. R. (2010). Design of Observational Studies.
New York: Springer.  This example is discussed in Chapter 16.

Rosenbaum, P. R. (2011). A new U statistic with superior design sensitivity in matched observational studies. Biometrics, 67(3), 1017-1027.

Schoket, B., Phillips, D. H., Hewer, A. and Vincze, I. (1991). 32P-postlabelling detection of aromatic DNA adducts in peripheral blood lymphocytes from aluminium production plant workers. Mutation Research/Genetic Toxicology, 260(1), 89-98.

Stephenson, W. R. (1981). A general class of one-sample nonparametric test statistics based on subsamples. Journal of the American Statistical Association, 76(376), 960-966.

}
\examples{
data(schoket)
attach(schoket)
plot(sort(adductsc),sort(adductsw),ylim=c(0,6.4),xlim=c(0,6.4),
   xlab="DNA adducts for controls",ylab="DNA adducts for workers",
   main="Quantile-Quantile Plot") # Compare with Chapter 16
abline(0,1) # line of equality
legend(4,1,lty=1,"x=y")
boxplot(adductsw,adductsc,ylim=c(0,6.4),ylab="DNA adducts",names=c("Worker","Control"))
d<-adductsw-adductsc
senWilcox(d,gamma=1)
senWilcox(d,gamma=1.5) # sensitive to gamma=1.5
}
\keyword{datasets}
