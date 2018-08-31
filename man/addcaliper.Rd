\name{addcaliper}
\alias{addcaliper}
\concept{Caliper matching}
\concept{Observational study}
\concept{Causal inference}
\concept{Propensity score}
\concept{Optimal matching}
\title{
Implement a Caliper Using a Penalty Function in Optimal Matching
}
\description{
A caliper forces a close match on a continuous covariate, often the propensity score (Rosenbaum and Rubin 1985).  Alternatively or additionally, a caliper may be imposed on a prognostic score estimated from some other independent data set (Hansen 2008).  Rosenbaum and Rubin (1985) suggested minimizing the Mahalanobis distance within calipers defined by the propensity score.  If the caliper is implemented using a penalty function, the caliper cannot create infeasibility.   Implementation of a caliper using a penalty function is discussed in Chapter 8 of Design of Observational Studies (2010).
}
\usage{
addcaliper(dmat, z, p, caliper = 0.1, penalty = 1000)
}
\arguments{
  \item{dmat}{
	A distance matrix with one row for each treated individual and one column for each control.  Often, this is either the Mahalanobis distance based on covariates, mahal(), or else a robust variant produced by smahal().
}
  \item{z}{
	z is a vector that is 1 for a treated individual and 0 for a control.  The number of treated subjects, sum(z), must equal the number of rows of dmat, and the number of potential controls, sum(1-z), must equal the number of columns of dmat.
}
  \item{p}{
	A vector covariate to which the caliper will be applied.  Often, p is the propensity score.  length(p) must equal length(z).
}
  \item{caliper}{
	A positive number.  A penalty will be added to row i and column j of the distance matrix dmat if treated unit i and control unit j have values of p that differ in absolute value by more than caliper*sd(p), the default being a tenth of the standard deviation of p.  This is different from the code in Section 13.11 of Rosenbaum (2010), where caliper defaulted to 0.2, rather than 0.1.
}
  \item{penalty}{
	A positive number.  penalty determines the magnitude of the penalty that is added to the distance matrix when the caliper is violated.  Let pt and pc be the values of p for a treated individual and a control.  If |pt-pc| <= caliper*sd(p), then nothing is added to dmat for these individuals.  Otherwise, define
gap = |pt-pc|-caliper*sd(p); then, the distance between these individuals in dmat is increased by gap*penalty.  So there is no penalty inside the caliper, but dmat is increased rapidly as violation of the caliper increases in magnitude.  A reasonable value of penalty will depend upon the units in which p is measured, and the default values are reasonable starting points for the propensity score expressed as a probability.
}
}
\details{
  By calling addcaliper several times, calipers may be imposed on several variables, say a propensity score and age.  If you use several penalties, say from addcaliper, addalmostexact, and fine, then you need to pay some attention to their relative magnitudes to ensure that they express your view of the relative importance of the conditions they impose.
}
\value{
  Returns the penalized distance matrix.
}
\references{

Hansen, B. B. and Klopfer, S. O. (2006). Optimal full matching and related designs via network flows. Journal of computational and Graphical Statistics, 15(3), 609-627. (optmatch package)

Hansen, B. B. (2007). Flexible, optimal matching for observational studies. R News, 7, 18-24. (optmatch package)

Hansen, B. B. (2008). The prognostic analogue of the propensity score. Biometrika, 95(2), 481-488.

Rosenbaum, P. R. (2010). Design of Observational Studies.
New York: Springer.  The method and example are discussed in Section 9.2.

Rosenbaum, P. R. and Rubin, D. B. (1985). Constructing a control group using multivariate matched sampling methods that incorporate the propensity score. The American Statistician, 39, 33-38.

}
\author{
Paul R. Rosenbaum
}

\examples{
data(costa)
z<-1*(costa$welder=="Y")
aa<-1*(costa$race=="A")
smoker=1*(costa$smoker=="Y")
age<-costa$age
x<-cbind(age,aa,smoker)
dmat<-mahal(z,x)
# Mahalanobis distances
round(dmat[,1:6],2) # Compare with Table 8.5 in Design of Observational Studies (2010)
# Impose propensity score calipers
prop<-glm(z~age+aa+smoker,family=binomial)$fitted.values # propensity score
# Mahalanobis distanced penalized for violations of a propensity score caliper.
# This version is used for numerical work.
dmat<-addcaliper(dmat,z,prop,caliper=.5)
round(dmat[,1:6],2) # Compare with Table 8.5 in Design of Observational Studies (2010)
\dontrun{
# Find the minimum distance match within propensity score calipers.
optmatch::pairmatch(dmat,data=costa)
}
# Conceptual versions with infinite distances for violations of propensity caliper.
dmat[dmat>20]<-Inf
round(dmat[,1:6],2) # Compare with Table 8.5 in Design of Observational Studies (2010)
}

