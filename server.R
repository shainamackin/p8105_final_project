library(shiny)
library(ggplot2)

server <- function(input, output) {
  shinyApp(
    server = function(input, output) {
      
    },
  )
  rmarkdown::run(
    file = "index.html",
    shiny_args = list(
      port = 8080,
      launch.browser = TRUE
      )
  )
}