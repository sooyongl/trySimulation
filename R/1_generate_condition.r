#' Generation a condition table
#'
#' \code{\link{makeCondition}} returns a condition table
#' @param n_sample a integer vector indicating total sample sizes.
#' @param cov_size a double vector indicating the size of latent covariance.
#' @param n_indi a integer vector indicating the number of indicators each latent factor.
#' @param n_rep a integer scalar for the number of replication
#' @param full a logical scalar. If \code{full} = TRUE, \code{makeCondition} returns a condition list including the replication numbers.
#' @return a data frame containing the conditions.
#' @examples
#' condition_list <- makeCondition(n_sample = c(500), cov_size = c(0, 0.5), n_indi = c(3, 6), cut_size = c(5,7), full = T)
#' @export
makeCondition <- function(n_sample, cov_size, n_indi, cut_size, n_rep = 10, full = T) {
  con_list <-
    crossing(n_sample, cov_size, n_indi, cut_size) %>%
    mutate(condition_number = row_number())

  if(full)
    con_list <-
      crossing(con_list, n_rep = 1:n_rep) %>%
      arrange(n_rep, condition_number)
  return(con_list)
}
