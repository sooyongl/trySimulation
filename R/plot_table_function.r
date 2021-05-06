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


