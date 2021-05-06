#' get power and type I error data
#' @export
getPower <- function(results) {
  results %>%
    filter(str_detect(type, "F1~~F2")) %>%
    mutate(rate = if_else(pvalue < 0.05, 1, 0)) %>%
    group_by(n_sample, cov_size, n_indi, cut_size) %>%
    summarise(mean_rate = mean(rate))
}

# power_res <- getPower(sim_result)

#' get bias and rmse data
#' @export
getBias <- function(results) {
  results %>%
    mutate(pop_value = case_when(cov_size == 0 & type == "F1~~F2" ~ 0,
                                 cov_size == 0.5 & type == "F1~~F2" ~ 0.5,
                                 TRUE ~ 1),
           type = case_when(str_detect(type, "=~") ~ "factor_loading",
                            type == "F1~~F2" ~ "latent_cov",
                            TRUE ~ "latent_var")) %>%
    group_by(n_sample, cov_size, n_indi, cut_size, type) %>%
    summarise(
      Bias = mean(est - pop_value),
      RMSE = sqrt( mean((est - pop_value)^2) )
    )
}

# bias_res <- getBias(sim_result)
