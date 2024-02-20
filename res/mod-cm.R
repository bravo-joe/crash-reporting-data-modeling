# Beginning of mod-cm.R
# Import necessary libraries
library(ggplot2)
library(shiny)

chartUI <- function(id) {
  # Create unique variable name
  # namespace
  ns <- NS(id)
  fluidRow(
    plotOutput(outputId = ns("conf_matrix"))
  )
}

chartServer <- function(id, x, y, freq, title) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      # Convert input data to data.frame
      df <- reactive({
        data.frame(
          x = x,
          y = y,
          freq = freq
        )
      })
      # Render a plot
      output$conf_matrix <- renderPlot({
        ggplot(df(), aes(x = x, y = y, fill = freq)) +
          geom_tile() + geom_text(aes(label=freq)) +
          scale_fill_gradient(low="white", high="#009194") +
          labs(x="Truth", y="Prediction", title = title)
      })
    }
  )
}

# End of mod-cm.R
