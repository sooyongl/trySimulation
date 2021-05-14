#' generate data
#' @examples
#' gen.data <- genData(N = 100, cov_size = 0, n_indi = 3)
#' @export
genData <- function(N, cov_size, n_indi) {

  # factor_loadings <- matrix(
  #   c(1,0,
  #     1,0,
  #     1,0,
  #     0,1,
  #     0,1,
  #     0,1 ),
  #   ncol = 2
  # )

  factor_loadings <- matrix(
    c(rep(c(1,0), n_indi), rep(c(0,1), n_indi)),
    ncol = 2,
    byrow = T
  )

  latent_matrix <- matrix(
    c(1, cov_size,
      cov_size, 1),
    ncol = 2
  )

  error_terms <- diag(0.5, n_indi*2)

  model_impled <- factor_loadings %*% latent_matrix %*% t(factor_loadings) + error_terms

  mean_vector <- rep(0, n_indi*2)

  g.data <- mvrnorm(N, mean_vector, Sigma = model_impled)
  g.data <- data.frame(g.data)
  return(g.data)
}

#' generally generate data
# ggenData <- function(
#   n = 1000,
#   Phi = 1, lam_x = 1, error_x = 0,
#
#   lam_y = matrix(
#     c(1.0, 0.0,
#       1.0, 1.0,
#       1.0, 2.0,
#       1.0, 3.0), nrow = 4, byrow = T),
#
#   I1 =
#     matrix(c(1,0,
#              0,1), nrow = 2),
#   Beta =
#     matrix(c(0, 0,
#              0, 0), nrow = 2, byrow = T),
#   Gamma =
#     matrix(c(0,
#              0) , nrow = 2),
#   Psi = matrix(c(1,  0.2,
#                  0.2,  1), nrow = 2),
#   error_y = diag(4),
#   x = 0,
#   eta_mu = cbind(c(5, -.5))
# )
# {
#
#   # Cov information
#
#   end1 <- lam_y %*% solve(I1 - Beta) %*% ((Gamma %*% Phi %*% t(Gamma)) + Psi ) %*% solve(t(I1 - Beta)) %*% t(lam_y) + error_y
#
#   cov1 <- lam_x %*% Phi %*% t(Gamma) %*% solve(t(I1 - Beta)) %*% t(lam_y)
#   exg1 <- lam_x %*% Phi %*% t(lam_x) + t(error_x)
#
#   cov2 <- lam_y %*% solve(I1 - Beta) %*% Gamma %*% Phi %*% t(lam_x)
#
#   cov_mat <- rbind(cbind(end1, cov2), cbind(cov1, exg1))
#
#   # Mean information
#   eta <- eta_mu + Gamma * x
#   y <- lam_y %*% eta
#
#   mean_vec <- c(y, x)
#
#   # if(!checkPositiveDefinite(cov_mat))
#   #   stop("The covariance matrix is not positive definite.")
#
#   res_data <- data.frame(mvtnorm::rmvnorm(
#     n,
#     mean = mean_vec,
#     sigma = cov_mat)
#   ) %>% setNames(paste0("V", 1:(dim(lam_y)[1]+length(x))))
#
#   res <- list(cov_mat = cov_mat, mean_vec = mean_vec, res_data = res_data)
#   return(res)
# }
