# Beginning of confusion_matrix_heatmap.R
#library(tidyverse)
library(shiny)
source("mod-cm.R")
library(data.table)
library(yardstick)
set.seed(525)  # Setting seed

# Read the configuration file
config <- config::get(file="../src/config.yml")

# Load data into variable
results_tbl <- fread(
  file = config$results,
  stringsAsFactors = TRUE,
  header = TRUE
)
results_tbl

cm <- conf_mat(results_tbl, actual, predicted)
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

#ggplot(plt, aes(Prediction, Truth, fill=Freq)) +
#  geom_tile() + geom_text(aes(label=Freq)) +
#  scale_fill_gradient(low="white", high="#009194") +
#  labs(x="Truth", y="Prediction")

# Compute performance metrics
sensitivity(plt, plt$Truth, plt$Prediction, estimator="macro")
  
ui <- fluidPage(
  # App Title
  titlePanel("Crash Data Modeling"),
  chartUI(id = "chart1")
)

server <- function(input, output, session) {
  chartServer(
    id = "chart1",
    x = plt$Prediction,
    y = plt$Truth,
    freq = plt$Freq,
    title = "Confusion Matrix"
  )
}

shinyApp(ui = ui, server = server)

# Beginning of confusion_matrix_heatmap.R