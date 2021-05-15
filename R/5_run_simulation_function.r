#' run simulation
#' @examples
#' sim_result <- runSimulation(condition_list)
#' @export
runSimulation <- function(condition_full, seed = 1) {

  total_n <- dim(condition_full)[1]

  res <- vector("list", total_n)
  for(tn in 1:total_n) {

    n_sample         <- condition_full$n_sample[tn]
    cov_size         <- condition_full$cov_size[tn]
    n_indi           <- condition_full$n_indi[tn]
    cut_size         <- condition_full$cut_size[tn]
    condition_number <- condition_full$condition_number[tn]
    n_rep            <- condition_full$n_rep[tn]

    set.seed(seed + n_rep + condition_number)
    original.data <- genData(N = n_sample, cov_size = cov_size, n_indi = n_indi)
    cut.data <- dataCut(original.data, cut_size)

    .fit <- analysis(cut.data)

    est <- extractEst(.fit)

    res[[tn]] <- cbind(condition_full[tn, ], est)
  }

  res <- do.call('rbind', res)
  return(res)
}
