library(shiny)
library(ggplot2)
library(shinythemes)
library(randomForest)

# Load your data and model from "project.R"
source("jo_k.R")

# Define the UI
ui <- fluidPage(
  titlePanel("Random Forest Model with Shiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Plot Type:", choices = c("Scatter Plot", "Bar Chart")),
      selectInput("x_axis", "X-Axis:", ""),
      selectInput("y_axis", "Y-Axis:", ""),
      numericInput("location", "Location:", min = 0, max = 7, value = 1),
      numericInput("total_sqft", "ts:", min = 1, max = 5000, value = 1000),
      numericInput("bath", "bath:", min = 1, max = 7, value = 1),
      numericInput("size1", "size:", min = 1, max = 2, value = 1),
      
      actionButton("generate_plot", "Generate Plot"),
      actionButton("predict_button", "Predict"),
      br(),
      h3("Random Forest Model Results:"),
      textOutput("prediction_result")
    ),
    mainPanel(
      plotOutput("generated_plot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  data <- df # Assuming df is loaded from project.R
  rf_model <- rf_model # Assuming your_rf_model is loaded from project.R
  
  observe({
    if (input$plot_type == "Scatter Plot" || input$plot_type == "Bar Chart") {
      updateSelectInput(session, "x_axis", choices = colnames(data))
      updateSelectInput(session, "y_axis", choices = colnames(data))
    }
  })
  
  observeEvent(input$generate_plot, {
    if (input$plot_type == "Scatter Plot") {
      output$generated_plot <- renderPlot({
        ggplot(data, aes_string(x = input$x_axis, y = input$y_axis)) +
          geom_point() +
          labs(title = paste("Scatter Plot of", input$x_axis, "vs.", input$y_axis))
      })
    } else if (input$plot_type == "Bar Chart") {
      output$generated_plot <- renderPlot({
        ggplot(data, aes_string(x = input$x_axis, y = input$y_axis)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Bar Chart of", input$x_axis, "vs.", input$y_axis))
      })
    }
  })
  
  observeEvent(input$predict_button, {
    isolate({
      if (!is.null(input$location) && !is.null(input$total_sqft) && !is.null(input$bath) && !is.null(input$size1)) {
        # Create a new data frame with the numeric Age, Category, Size, Gender, and Channel
        new_data <- data.frame(location = input$location, total_sqft = input$total_sqft, bath = input$bath, size1= input$size1)
        
        # Predict using the random forest model
        predictions <- predict(rf_model, new_data)
        # Extract the first prediction
        prediction <- predictions[1]
        output$prediction_result <- renderText({
          paste("price is:", round(as.numeric(prediction), 2))
        })
      } else {
        output$prediction_result <- renderText({
          "Please enter Age, Category, Size, Gender, and Channel to predict the Amount."
        })
      }
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)