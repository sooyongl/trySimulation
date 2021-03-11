library(trySimulation)

condition_list <- makeCondition(n_sample = c(100, 500), cov_size = c(0, 0.5), n_indi = c(3, 6), cut_size = c(3,5,7), n_rep = 5, full = T)

sim_result <- runSimulation(condition_list)

power_res <- getPower(sim_result)
bias_res <- getBias(sim_result)

makePlot(bias_res, what = "RMSE", font_size = 12)
makePlot(power_res, what = "power", font_size = 12)
makePlot(power_res, what = "typeIerror", font_size = 12)
