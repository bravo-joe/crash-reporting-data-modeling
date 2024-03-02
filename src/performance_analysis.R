# Beginning of performance_analysis.R
library(tidyverse)
library(data.table)
library(yardstick)
set.seed(525)  # Setting seed

# Read the configuration file
config <- config::get()

# Load data into variable
results_tbl <- fread(
  file = config$results_tbl,
  stringsAsFactors = TRUE,
  header = TRUE
  )
results_tbl

# Begin gathering some of the important performance metrics
cm <- conf_mat(results_tbl, actual, predicted)
# First plot attempt
autoplot(cm, type = "heatmap")

results_tbl %>%
  roc_auc(actual, predicted)

# Kappa and accuracy metrics
perf_metr_1 <- metrics(results_tbl, actual, predicted)
perf_metr_1

# Precision macro metric
prec_macro <- precision(results_tbl, actual, predicted)
prec_macro

# Combine last two data frames
perf_metr_2 <- rbind(perf_metr_1, prec_macro) 
perf_metr_2

# Precision micro metric
prec_micro <- precision(results_tbl, actual, predicted, estimator = "micro")
prec_micro

# Combine last two data frames
perf_metr_3 <- rbind(perf_metr_2, prec_micro) 
perf_metr_3

#convert tibble to data frame
perf_metr_3 <- as.data.frame(perf_metr_3)

# Output performance metrics table to csv file
write.csv(perf_metr_3, config$perf_metrics, row.names=FALSE)

# End of performance_analysis.R