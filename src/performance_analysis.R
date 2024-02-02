# Beginning of performance_analysis.R
library(tidyverse)
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




# End of performance_analysis.R
