\name{smahal}
\alias{smahal}
\concept{Mahalanobis distance}
\concept{Observational study}
\concept{Causal inference}
\concept{Propensity score}
\concept{Optimal matching}
\concept{Design of Observational Studies}
\title{
Robust Mahalanobis Distance Matrix for Optimal Matching
}
\description{
Computes a robust Mahalanobis distance matrix between treated individuals and potential controls.  The usual Mahalnaobis distance may ignore a variable because it contains one extreme outlier, and it pays too
much attention to rare binary variables, but smahal addresses both issues.
}
\usage{
smahal(z, X)
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
  The robust distance matrix has one row for each treated individual (z=1) and one column for each potential control (z=0).  The row and column names of the distance matrix refer to the position in z, 1, 2, ..., length(z).
}
\details{
The usual Mahalnaobis distance may ignore a variable because it contains one extreme outlier, and it pays too
much attention to rare binary variables, but smahal addresses both issues.

To address outliers, each column of x is replaced by a column of ranks, with average ranks used for ties.  This prevents one outlier from inflating the variance for a column, thereby making large differences count little in the Mahalanobis distance.

Rare binary variables have very small variances, p(1-p) for small p, so in the usual Mahalanobis distance, a mismatch for a rare binary variable is counted as very important.  If you were matching for US states as 49 binary variables, mismatching for California would not be very important, because p(1-p) is not so small, but mismatching for Wyoming is very important because p(1-p) is very small.  To combat this, the variances of the ranked columns are rescaled so they are all the same, all equal to the variance of untied ranks.  See Chapter 8 of Design of Observational Studies (2010).
}
\references{

Rosenbaum, P. R. (2010). Design of Observational Studies.
New York: Springer.  The method and example are discussed in Chapter 8.

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
dmat<-smahal(z,x)
# Mahalanobis distances
round(dmat[,1:6],2) # Compare with Table 8.6 in Design of Observational Studies (2010)
# Impose propensity score calipers
prop<-glm(z~age+aa+smoker,family=binomial)$fitted.values # propensity score
# Mahalanobis distanced penalized for violations of a propensity score caliper.
# This version is used for numerical work.
dmat<-addcaliper(dmat,z,prop,caliper=.5)
round(dmat[,1:6],2) # Compare with Table 8.6 in Design of Observational Studies (2010)
\dontrun{
# Find the minimum distance match within propensity score calipers.
optmatch::pairmatch(dmat,data=costa)
}
# Conceptual versions with infinite distances for violations of propensity caliper.
dmat[dmat>20]<-Inf
round(dmat[,1:6],2) # Compare with Table 8.6 in Design of Observational Studies (2010)
}

