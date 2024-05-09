# Beginning of confusion_matrix_heatmap.R
library(shinydashboard)
#library(shiny)
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

# Performance metrics
perf_metrics <- fread(
  file = config$perf_metrics,
  stringsAsFactors = TRUE,
  header = TRUE
)
perf_metrics

ui <- dashboardPage(
  # App Title
  dashboardHeader(title = "Crash Data Modeling"),
  dashboardSidebar(),
  dashboardBody(
    # Need Dashboard Body
    fluidRow(
      chartUI(id = "chart1")
    ),
    fluidRow(
      # A static valueBox
      valueBox(perf_metrics$.estimate[[1]], "Accuracy", icon = icon("list"))
    )
  )
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