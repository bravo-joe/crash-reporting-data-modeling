# Beginning of mod-cm.R
# Import necessary libraries
library(ggplot2)
library(shiny)

chartUI <- function(id) {
  # Create unique variable name
  # namespace
  ns <- NS(id)
  fluidRow(
    plotOutput(outputId = ns("confusion_matrix"))
  )
}

chartServer <- function(id, x, y, title) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      # Convert input data to data.frame
      df <- reactive({
        data.frame(
          x = x,
          y = y
        )
      })
      # Render a plot
      output$confusion_matrix <- renderPlot({
        ggplot(df(), aes(x = x, y = y, fill = Freq)) +
          geom_tile() + geom_text(aes(label=Freq)) +
          scale_fill_gradient(low="white", high="#009194") +
          labs(x="Truth", y="Prediction", title = title)
      })
    }
  )
}

# End of mod-cm.R