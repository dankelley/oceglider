ui <- fluidPage(
  textAreaInput("caption", "Caption", "Data Summary", width = "1000px"),
  verbatimTextOutput("value")
)
server <- function(input, output) {
  output$value <- renderText({ input$caption })
}
shinyApp(ui, server)

