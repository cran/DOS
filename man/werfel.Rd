\name{werfel}
\alias{werfel}
\concept{Observational study}
\concept{Matched pairs}
\concept{Causal inference}
\concept{Sensitivity analysis}
\concept{Design of Observational Studies}
\docType{data}
\title{
Welding Fumes and DNA Damage
}
\description{
This data set from Werfel et al. (1998) describes 39 electric arc welders and 39 controls
matched for age and smoking.  All are men.  The outcome is a measure of genetic damage;
specifically, erpcp_p is a measure of DNA single strand breakage and DNA-protein cross-links using
elution rates through polycarbonate filters with proteinase K.
}
\usage{data("werfel")}
\format{
  A data frame with 39 observations on the following 6 variables.
  \describe{
    \item{\code{cage}}{Age in years of the control in a matched pair.}
    \item{\code{csmoke}}{NS=nonsmoker, S=smoker for the control in a pair}
    \item{\code{cerpc_p}}{erpcp_p for the control in a pair}
    \item{\code{sage}}{Age in years of the welder in a matched pair.}
    \item{\code{ssmoke}}{NS=nonsmoker, S=smoker for the welder in a pair}
    \item{\code{serpc_p}}{erpcp_p for the welder in a pair}
  }
}

\source{
The data are from Werfel et al. (1998).  It is used as an example in Section 3.5 of
Design of Observational Studies.  It is also discussed in Fogarty (2016) and
Rosenbaum (2007, 2015).
}
\references{
Fogarty, C. B. (2016). Sensitivity analysis for the average treatment effect in paired observational studies. arXiv preprint arXiv:1609.02112.

Rosenbaum, P. R. (2007). Sensitivity analysis for M estimates, tests,
and confidence intervals in matched observational studies. Biometrics, 63(2), 456-464.

Rosenbaum, P. R. (2010). Design of Observational Studies.
New York: Springer.  This example is discussed in Section 3.5.

Rosenbaum, P. R. (2015). Two R packages for sensitivity analysis in observational studies. Observational Studies, 1(1), 1-17.

Werfel, U., Langen, V., Eickhoff, I., Schoonbrood, J., Vahrenholz, C., Brauksiepe, A., Popp, W. and Norpoth, K.
(1998). Elevated DNA single-strand breakage frequencies in lymphocytes of welders exposed to chromium and nickel. Carcinogenesis, 19(3), 413-418.
}
\examples{
data(werfel)
d<-werfel$serpc_p-werfel$cerpc_p

# Reproduces the approximate one-sided P-value computed in Section 3.5 of Rosenbaum (2010).
senWilcox(d,gamma=3)

# Agrees with the usual large sample Wilcoxon procedures when gamma=1.
senWilcox(d,gamma=1,conf.int=TRUE,alternative="twosided")
stats::wilcox.test(d,conf.int=TRUE,exact=FALSE,correct=FALSE)
}
\keyword{datasets}
