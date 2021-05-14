# rm(list = ls())
# pacakge needed
library(MplusAutomation); library(data.table); library(tidyverse);
library(trySimulation); library(glue)

# file path
project.path <- getwd()
mplus_path <- file.path(project.path,".introR", "mplus_materials")
dir.create(path = mplus_path)

# write_delim(use_data, "Run Mplus in R/use_data", delim = "\t", col_names = F)
# fwrite(use_data, "Run Mplus in R/use_data.txt", sep = "\t", col.names = F)
# write.table(use_data, file = "Run Mplus in R/use_data.txt",
#             row.names=FALSE,
#             col.names=FALSE,
#             sep = "\t", quote = FALSE)
# write_delim(use_data, "Run Mplus in R/use_data.txt",
#             delim = " ",
#             col_names = F)

use_data <- trySimulation::genData(
  N = 100,
  cov_size = 0.2,
  n_indi = 4)

write_tsv(use_data, file.path(mplus_path, "test_data.dat"), col_names = F)

Mplus.model <- '

data: file is test_data.dat;
variable: names are v1-v8;

model:
  F1 by v1-v4;
  F2 by v5-v8;
  F1 with F2;

'
#
writeLines(Mplus.model, file.path(mplus_path, "test_input.inp"))
file.show(file.path(mplus_path, "test_input.inp"))

inp_path <- file.path(mplus_path, "test_input.inp")
inp_file_name <- glue("Mplus {inp_path}")
system(
  inp_file_name,
  show.output.on.console = F,
  invisible = F,
  wait = TRUE
)

out_path <- file.path(mplus_path, "test_input.out")
file.show(out_path)

read_lines(out_path)


model1 <- MplusAutomation::readModels(out_path)

tibble(inf = rownames(t(model1$summaries)), value = t(model1$summaries)[,1]) %>%
  filter( inf %in% c("Parameters","ChiSqM_Value","ChiSqM_DF","CFI","TLI","RMSEA_Estimate"))

model1$parameters$unstandardized %>%
  as_tibble() %>%
  rename_at(vars(contains("$")), ~ paste0(1:6, "_new"))

model1$parameters$unstandardized %>%
  filter(str_detect(paramHeader, "BY"))

model1$parameters$unstandardized %>%
  filter(str_detect(paramHeader, "WITH"))

model1$parameters$unstandardized %>%
  filter(str_detect(paramHeader, pattern = "^Variances"))

model1$parameters$unstandardized %>%
  filter(str_detect(paramHeader, "Residual"))


