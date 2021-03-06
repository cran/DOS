smahal <- function (z, X)
{
  #Check input
  stopifnot(is.vector(z))
  stopifnot(all((z==0)|(z==1)))
  if (is.vector(X)) X<-matrix(X,length(X),1)
  if (is.data.frame(X)) X<-as.matrix(X)
  stopifnot(is.matrix(X))
  stopifnot(length(z)==(dim(X)[1]))

  #Preliminary computations
  n <- dim(X)[1]
  rownames(X) <- 1:n
  k <- dim(X)[2]
  m <- sum(z)
  for (j in 1:k) X[, j] <- rank(X[, j])
  cv <- stats::cov(X)
  vuntied <- stats::var(1:n)
  rat <- sqrt(vuntied/diag(cv))
  cv <- diag(rat) %*% cv %*% diag(rat)

  #Compute distances
  out <- matrix(NA, m, n - m)
  Xc <- X[z == 0, ]
  Xt <- X[z == 1, ]
  rownames(out) <- rownames(X)[z == 1]
  colnames(out) <- rownames(X)[z == 0]
  icov <- MASS::ginv(cv)
  for (i in 1:m) out[i, ] <- stats::mahalanobis(Xc, Xt[i, ], icov,
                                         inverted = T)
  out
}
