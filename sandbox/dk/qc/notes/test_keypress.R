library(shiny)
runApp( list(ui = bootstrapPage(
  verbatimTextOutput("results"),
  tags$script('
    $(document).on("keypress", function (e) {
       Shiny.onInputChange("mydata", e.which);
       Shiny.onInputChange("keypressTrigger", Math.random());
});
  ') 
) , server = function(input, output, session) {
      output$results = renderText({
          if (!is.null(input$mydata)) {
              key <- intToUtf8(input$mydata)
              if (key == 'q') stop("experiment complete") else key
          } else {
              "click window and then type letters ('q' to quit)."
          }
      })
  }
  ))
