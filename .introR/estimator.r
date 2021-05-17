# rm(list = ls())
# Standard MV MLE  --------------------------
#--------------------------------------------
ll_multN <- function(theta,X) {

  # theta = starting_pars
  # X = X #is an nxk dataset

  # MLE: L = - (nk/2)*log(2*pi) - (n/2)*log(det(Sigma)) - (1/2)*sum_i(t(X_i-mu)^2 %*% Sigma^-1 %*% (X_i-mu)^2)
  # summation over i is performed using a apply call for efficiency

  n <- nrow(X)
  k <- ncol(X)

  # construct model-implied covariance and mean vector
  lmeans <- grepl("lmean", names(theta))
  latent_mean <- theta[lmeans]

  lambda <- matrix(
    c(1, 1, 1,
      0, 1, 2), ncol = 2)

  phi <- grepl("phi", names(theta))
  phi <- theta[phi]
  phi <-
    matrix(
      c(phi[1], phi[2],
        phi[2], phi[3]), ncol = 2)

  errors <- grepl("error", names(theta))
  errors <- theta[errors]
  errors <- diag(errors, k)

  Sigma <- lambda %*% phi %*% t(lambda) + errors
  mu.vec <- latent_mean %*% t(lambda)

  # compute summation
  sum_i <- sum(apply(X, 1, function(x) (matrix(x,1,k)-mu.vec)%*%solve(Sigma)%*%t(matrix(x,1,k)-mu.vec)))

  # compute log likelihood
  logl <- -.5*n*k*log(2*pi) - .5*n*log(det(Sigma))
  logl <- logl - .5*sum_i
  return(-logl)
}

n <- 300 # number of data points
k <- 3 # number of variables

lambda <-
  matrix(
    c(1, 1, 1,
      0, 1, 2), ncol = 2)
phi <-
  matrix(
    c(1, 0.2,
      0.2, 0.1), ncol = 2)
errors <- diag(0.5, 3)
Sigma.tru <- lambda %*% phi %*% t(lambda) + errors

latent_mean <- c(3, 0.5)
mu.tru <- lambda %*% latent_mean

# Generate simulated dataset
X <- MASS::mvrnorm(n, mu = mu.tru, Sigma = Sigma.tru, empirical = T)
colMeans(X)
cov(X)

starting_pars <-
  c(lmean_inp = c(0, 0),
    phi_inp = c(1, 0, .1),
    error_inp = 0.5
  )
optim.out <-
  optim(par = starting_pars,
        fn = ll_multN,
        X = X,
        hessian = T,
        method = "L-BFGS-B",
        lower = -Inf,
        upper = Inf)

starting_pars <-
  c(lmean_inp = c(0, 0),
    phi_inp = c(1, 0, 1),
    error_inp = 0.5
  )

nlminb.out <-
  nlminb(start = starting_pars,
         objective = ll_multN,
         X = X,
         # gradient=GRADIENT,
         lower = -Inf,
         upper = Inf)
# hessian_est <- numDeriv::hessian(func = ll_multN, theta = starting_pars, x = X)

# Standard Errors

#
fisher_info <- solve(optim.out$hessian)
prop_sigma <- sqrt(diag(fisher_info))
upper <- optim.out$par+1.96*prop_sigma
lower <- optim.out$par-1.96*prop_sigma


#
my.hessian <- function(Y, nlminb.out) {
  # Y <- X; nlminb.out <- nlminb.out$par
  # construct model-implied covariance and mean vector
  theta <- nlminb.out
  lmeans <- grepl("lmean", names(theta))
  latent_mean <- theta[lmeans]

  lambda <- matrix(
    c(1, 1, 1,
      0, 1, 2), ncol = 2)

  phi <- grepl("phi", names(theta))
  phi <- theta[phi]
  phi <-
    matrix(
      c(phi[1], phi[2],
        phi[2], phi[3]), ncol = 2)

  errors <- grepl("error", names(theta))
  errors <- theta[errors]
  errors <- diag(errors, k)

  Sigma <- lambda %*% phi %*% t(lambda) + errors
  mu <- latent_mean %*% t(lambda)

  if (!is.matrix(Y)) Y <- as.matrix(Y)
  # Create Matrices
  inv_sig <- solve(Sigma)
  Y_residual <- Y - matrix(1,nrow(Y)) %*% mu # sweep(as.matrix(Y), 2, mu)
  d <- Dn(ncol(Y))
  # Create second derivatives
  d2m <- -nrow(Y)*solve(Sigma)
  d2s <- -t(d) %*%  (inv_sig %x% ( inv_sig %*% crossprod(Y_residual) %*%
                                     inv_sig  - .5*nrow(Y)*inv_sig )) %*% d
  d2ms <- - t(inv_sig %x% colSums(Y_residual %*% inv_sig) )  %*% d
  # Compute Hessian
  rbind(cbind(d2m,d2ms), cbind(t(d2ms),d2s))
}

Dn <- function(n){
  mat <- diag(n)
  index <- seq(n * (n+1) / 2)
  mat[lower.tri(mat,T)] <- index
  mat[upper.tri(mat)]   <- t(mat)[upper.tri(mat)]
  mat <- outer(c(mat), index, function(x, y) ifelse( x == y, 1, 0))
  return(mat)
}

hessian_est <- my.hessian(X, p$par)
SE_est <- sqrt(diag(solve(-hessian_est)))



