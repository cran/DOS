\name{mahal}
\alias{mahal}
\concept{Mahalanobis distance}
\concept{Observational study}
\concept{Causal inference}
\concept{Propensity score}
\concept{Optimal matching}
\title{
Mahalanobis Distance Matrix for Optimal Matching
}
\description{
Computes a Mahalanobis distance matrix between treated individuals and potential controls.  The method is discussed in Chapter 8 of Design of Observational Studies (2010).
}
\usage{
mahal(z, X)
}
\arguments{
  \item{z}{
	z is a vector that is 1 for a treated individual and 0 for a control.
}
  \item{X}{
	A matrix of continuous or binary covariates.  The number of rows of X must equal the length of z.
}
}
\value{
  The distance matrix has one row for each treated individual (z=1) and one column for each potential control (z=0).  The row and column names of the distance matrix refer to the position in z, 1, 2, ..., length(z).
}
\references{

Hansen, B. B. and Klopfer, S. O. (2006). Optimal full matching and related designs via network flows. Journal of computational and Graphical Statistics, 15(3), 609-627. (optmatch package)

Hansen, B. B. (2007). Flexible, optimal matching for observational studies. R News, 7, 18-24. (optmatch package)

Rosenbaum, P. R. (2010). Design of Observational Studies.
New York: Springer.  The method and example are discussed in Chapter 8.

Rosenbaum, P. R. and Rubin, D. B. (1985). Constructing a control group using multivariate matched sampling methods that incorporate the propensity score. The American Statistician, 39, 33-38.

Rubin, D. B. (1980). Bias reduction using Mahalanobis-metric matching. Biometrics, 36, 293-298.
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

