linearreg <- function(n,p){
  cat("n =", n, "\n")
  cat("p =", p, "\n")
  z1 <- rnorm(n*p)
  x <- matrix(c(rep(1,n),z1), nrow=n, ncol=(p+1))
  cat("x =", "\n")
  print(x)
  z2 <- rnorm(n)
  y <- matrix(z2, nrow=n, ncol=1)
  cat("y =", "\n")
  print(y)
  beta <- solve(t(x) %*% x) %*% t(x) %*% y
  j <- y - x%*%beta
  sigma_sqaure <- norm(j, type="2") / (n-p-1)
  variance <- sigma_sqaure * solve(t(x) %*% x)
  error <- diag(variance)
  beta_0 <- rep(0, p+1)
  t <- beta - beta_0 / error
  p <- pnorm(t, lower.tail = FALSE)
  return( cat("beta =", beta, "\n", "standard errors =", error, "\n",
              "t-statistics =", t, "\n", "p-values =", p, "\n") )
}
