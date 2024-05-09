# Beginning of roc-auc-scratchwork.R
###########################################################################
# A script to prototype the different ROC-AUC curves for the five possible
# classes in the outcome variable.
###########################################################################
# Import necessary libraries
library(data.table)
library(ggplot2)
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

# Now, import pROC library and plot first class
library(pROC)

# Definte object to plot
# results_tbl$predicted changed to numeric for multiclass.roc
#roc_obj <- pROC::multiclass.roc(results_tbl$actual, as.numeric(results_tbl$predicted))
roc_obj <- pROC::multiclass.roc(
  results_tbl$predicted,
  as.numeric(results_tbl$actual)
  )

# ROC plots
# Plotting logic
plot_1 <- plot.roc(
  roc_obj$rocs[[1]],
  print.auc = T,
  legacy.axes = T
  )
plot_1

plot_2 <- plot.roc(
  roc_obj$rocs[[2]],
  col = "darkgreen",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)
plot_2

plot_3 <- plot.roc(
  roc_obj$rocs[[3]],
  col = "coral",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)

plot_4 <- plot.roc(
  roc_obj$rocs[[4]],
  col = "darkorchid1",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)

plot_5 <- plot.roc(
  roc_obj$rocs[[5]],
  col = "brown2",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)

plot_6 <- plot.roc(
  roc_obj$rocs[[6]],
  col = "cadetblue4",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)

plot_7 <- plot.roc(
  roc_obj$rocs[[7]],
  col = "darkcyan",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)
 
plot_8 <- plot.roc(
  roc_obj$rocs[[8]],
  col = "chartreuse2",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)

plot_9 <- plot.roc(
  roc_obj$rocs[[9]],
  col = "darkslategray3",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)

plot_10 <- plot.roc(
  roc_obj$rocs[[10]],
  col = "deeppink4",
  lwd = 3.5,
  print.auc = T,
  legacy.axes = T
)

# End of roc-auc-scratchwork.R
