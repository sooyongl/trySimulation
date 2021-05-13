library(trySimulation)

condition_list <-
  makeCondition(
    n_sample = c(100, 500),
    cov_size = c(0, 0.5),
    n_indi = c(3, 6),
    cut_size = c(3,5,7),
    n_rep = 5,
    full = F)

gen.data <- genData(
  N = 10,
  cov_size = 0,
  n_indi = 3)


cut5.data <- dataCut(
  gen.data,
  cut_size = 5)

.model <- '
      F1 =~ X1 + X2 + X3
      F2 =~ X4 + X5 + X6
      F1 ~~ F2
  '
.fit <- lavaan::sem(model = .model,
                    data  = .data)
summary(.fit, fit.measures=TRUE)

.fit <- analysis(cut5.data)

