library(shiny)
library(devtools)
load_all()


ui <- fluidPage(
  titlePanel("Unit Converter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("conversion_type",
                  "Conversion Type:",
                  c("Distance", "Weight", "Temperature")),
      numericInput("value",
                   "Value:",
                   value = 1),
      textInput("from_unit",
                "From Unit:"),
      textInput("to_unit",
                "To Unit:"),
      actionButton("convert", "Convert")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    req(input$convert)
    isolate({
      if (input$conversion_type == "Distance") {
        converted_value <- convert_distance(input$value, input$from_unit, input$to_unit)
      } else if (input$conversion_type == "Weight") {
        converted_value <- convert_weight(input$value, input$from_unit, input$to_unit)
      } else {
        converted_value <- convert_temperature(input$value, input$from_unit, input$to_unit)
      }

      paste(input$value, input$from_unit, "is equal to", round(converted_value, 2), input$to_unit)
    })
  })
}

shinyApp(ui = ui, server = server)
