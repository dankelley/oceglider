library(shiny)

ui <- fluidPage(
  tags$head(tags$script("
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'go') {
        var text = prompt('Write me something nice:');
        Shiny.onInputChange('mytext', text);
      }
    });"
  )),
  actionButton("go", "Click for prompt"),
  textOutput("txt")
)

server <- function(input, output, session) {
  output$txt <- renderText( {
    input$mytext
  })
}

shinyApp(ui, server)

