devtools::install_github("https://github.com/sooyongl/trySimulation")
library(trySimulation); library(tidyverse); library(MASS)
library(lavaan); library(glue)

# for(source.function in list.files("R") { source(source.function)})

condition_list <-
    makeCondition(
      n_sample = c(100, 500),
      cov_size = c(0, 0.5),
      n_indi = c(3, 6),
      cut_size = c(3,5,7),
      n_rep = 50,
      full = T)

sim_result <- runSimulation(condition_list)

saveRDS(sim_result, file = ".test/sim_result.rds")

sim_result <- readRDS(file = ".test/sim_result.rds")

power_res <- getPower(sim_result)
bias_res <- getAccuracy(sim_result)

# saveRDS(power_res, file = ".simulation/power_res.rds")
# save(power_res, bias_res, file = ".simulation/results.rdata")

makePlot(bias_res, what = "RMSE", font_size = 12, point_size = 4)
makePlot(bias_res, what = "Bias", font_size = 12)
makePlot(results = power_res, what = "power", font_size = 12)
makePlot(power_res, what = "typeIerror", font_size = 12)
