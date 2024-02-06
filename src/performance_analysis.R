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


# End of performance_analysis.R
