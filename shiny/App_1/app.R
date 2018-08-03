library(shiny)
ui <- basicPage(
  "The current library path is:", 
  .libPaths()
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)