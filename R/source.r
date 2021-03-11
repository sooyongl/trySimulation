# suppressPackageStartupMessages(library(lavaan))
# suppressPackageStartupMessages(library(tidyverse))
# suppressPackageStartupMessages(library(MASS))
# suppressPackageStartupMessages(library(glue))

#' generation condition
#' @export
makeCondition <- function(n_sample, cov_size, n_indi, cut_size, n_rep, full = T) {
  con_list <-
    crossing(n_sample, cov_size, n_indi, cut_size) %>%
    mutate(condition_number = row_number())

  if(full)
    con_list <- crossing(con_list, n_rep = 1:n_rep) %>% arrange(n_rep, condition_number)

  return(con_list)
}

# condition_list <- makeCondition(n_sample = c(100, 500), cov_size = c(0, 0.5), n_indi = c(3, 6), cut_size = c(3,5,7), n_rep = 5, full = T)

#' generate data
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

  g.data <- MASS::mvrnorm(N, mean_vector, Sigma = model_impled)
  g.data <- data.frame(g.data)
  return(g.data)
  }

# .data <- genData(N = 100, cov_size = 0, n_indi = 3)

#' cut data
#' @export
dataCut <- function(.data, cut_size) {

  # cut(x = .data[,1], breaks = cut_size, labels = F)

  cut.data <- apply(.data, 2, function(x) cut(x = x, breaks = cut_size, labels = F))

  cut.data <- data.frame(cut.data)

  return(cut.data)
}

# cut5.data <- dataCut(.data, cut_size = 3)
# cov(.data)
# cov(cut5.data)

#' run a single replication
#' @export
analysis <- function(.data) {

  n_indi <- dim(.data)[2]/2

  for_f1 <- paste0("X", 1:n_indi)
  for_f2 <- paste0("X", (n_indi+1):(2*n_indi))

  for_f1 <- paste(for_f1, collapse = " + ")
  for_f2 <- paste(for_f2, collapse = " + ")

  .model <- glue('
      F1 =~ {for_f1}
      F2 =~ {for_f2}

      F1 ~~ F2
  ')

  .fit <- lavaan::sem(model = .model,
                      data  = .data )

  # summary(.fit)
  return(.fit)
  }

# .fit <- analysis(.data)

#' get estimates
#' @export
getEst <- function(.fit) {

  parameterestimates(.fit) %>%
    # filter(str_detect(op, "~~") & str_detect(lhs, "F1") & str_detect(rhs, "F2")) %>%
    filter(str_detect(op, "=~") | str_detect(lhs, "F1|F2")) %>%
    filter(se != 0) %>%
    mutate(type = paste0(lhs, op, rhs)) %>%
    dplyr::select(type, est, pvalue)
}

# getEst(.fit)

#' run simulation
#' @export
runSimulation <- function(condition_full) {

  total_n <- dim(condition_full)[1]

  res <- vector("list", total_n)
  for(tn in 1:total_n) {

    n_sample         <- condition_full$n_sample[tn]
    cov_size         <- condition_full$cov_size[tn]
    n_indi           <- condition_full$n_indi[tn]
    cut_size         <- condition_full$cut_size[tn]
    condition_number <- condition_full$condition_number[tn]
    n_rep            <- condition_full$n_rep[tn]

    original.data <- genData(N = n_sample, cov_size = cov_size, n_indi = n_indi)
    cut.data <- dataCut(original.data, cut_size)

    .fit <- analysis(cut.data)

    est <- getEst(.fit)

    res[[tn]] <- cbind(condition_full[tn, ], est)
    }

  res <- do.call('rbind', res)
  return(res)
}

# sim_result <- runSimulation(condition_list)

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

#' make plots for results
#' @export
makePlot <- function(results, what = "RMSE", font_size) {

  if(what == "RMSE" | what == "Bias" ) {
    results %>%
      filter(cov_size != 0) %>%
      mutate_at(vars(matches("_")), as.factor) %>%
      ggplot(aes(x = cut_size, y = !!as.name(what))) +
      geom_point(aes(colour = n_sample), size = 2, alpha = 0.8) +
      facet_grid(type ~ n_indi) +
      theme_bw(base_size = font_size)

  } else {
    if(tolower(what) == "power") {
      results %>%
        filter(cov_size != 0) %>%
        mutate_at(vars(matches("_")), as.factor) %>%
        ggplot(aes(x = cut_size, y = mean_rate)) +
        geom_point(aes(colour = n_sample), size = 2, alpha = 0.8) +
        facet_grid(. ~ n_indi) +
        theme_bw(base_size = font_size)

    } else {
      results %>%
        filter(cov_size == 0) %>%
        mutate_at(vars(matches("_")), as.factor) %>%
        ggplot(aes(x = cut_size, y = mean_rate)) +
        geom_point(aes(colour = n_sample), size = 2, alpha = 0.6) +
        facet_grid(n_indi ~ .) +
        theme_bw(base_size = font_size)
    }
  }
}

# makePlot(bias_res, what = "RMSE", font_size = 12)
# makePlot(power_res, what = "power", font_size = 12)
# makePlot(power_res, what = "typeIerror", font_size = 12)


#' generate table
#' @export
makeTable <- function() {
    tibble(a = 1:10, b = 1:10)

}


