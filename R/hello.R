meldaJs <- function() {
  system.file("extdata/bundle.js", package = "meldakb")
}

meldaHtml <- function(){
  system.file("extdata/template.html", package = "meldakb")
}

meldaKB <- function(){
  library(shiny)

  ui <- fluidPage(htmlTemplate(meldaHtml(),
                               exitButton =tags$button(
                                 id = 'close',
                                 type = "button",
                                 class = "btn btn-primary btn-sm action-button",
                                 "Exit"
                               ),
                               bundle = includeScript(meldaJs())

  ))

  server <- function(input, output, session) {
    observe({
      if (input$close) stopApp()
    })
  }
  runApp(list(ui = ui, server = server))
}
