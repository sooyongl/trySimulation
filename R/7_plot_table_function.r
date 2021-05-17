#' make plots for results
#' @examples
#' makePlot(bias_res, what = "RMSE", font_size = 12)
#' makePlot(power_res, what = "power", font_size = 12)
#' makePlot(power_res, what = "typeIerror", font_size = 12)
#' @export
makePlot <- function(results, what = "Bias", font_size = 14, point_size = 2, alpha = 1) {

  theme_set(theme_bw(base_size = font_size))

  if(what == "RMSE" | what == "Bias" ) {
    p1 <-
      results %>%
      filter(cov_size != 0) %>%
      mutate_at(vars(matches("_")), as.factor) %>%
      ggplot(aes(x = cut_size, y = !!as.name(what)))

  } else {
    if(tolower(what) == "power") {
      p1 <-
        results %>%
        filter(cov_size != 0) %>%
        mutate_at(vars(matches("n_s|cov_|cut_")), as.factor) %>%
        ggplot(aes(x = cut_size, y = mean_rate)) +
        geom_hline(yintercept = .8)

    } else {
      p1 <-
        results %>%
        filter(cov_size == 0) %>%
        mutate_at(vars(matches("n_s|cov_|cut_")), as.factor) %>%
        ggplot(aes(x = cut_size, y = mean_rate)) +
        geom_hline(yintercept = .05) +
        annotate("rect", xmin=-Inf, xmax=Inf,
                 ymin=.03, ymax=0.07,
                 fill="green", alpha=0.05)
    }
  }


  p2 <- p1 +
    geom_point(aes(colour = n_sample, shape = n_sample),
               size = point_size, alpha = alpha) +
    scale_x_discrete(name = "Cut size") +
    scale_shape_discrete(name = "Sample size") +
    scale_colour_manual(name = "Sample size",
                        values = c("#EB322F","#2F88ED")) +
    theme(strip.background =element_rect(fill="#EDEDED", colour = "black"))

  if(what == "RMSE" | what == "Bias" ) {
    p2 + facet_grid(n_indi ~ type)
  } else {
    p2 + facet_grid(. ~ n_indi)
  }
}

#' generate table
#' makeTable(bias_res, what = "RMSE", font_size = 12)
#' makeTable(power_res, what = "power", font_size = 12)
#' makeTable(power_res, what = "typeIerror", font_size = 12)
#' @export
makeTable <- function() {
    tibble(a = 1:10, b = 1:10)

}


