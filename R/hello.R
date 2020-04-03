meldaJs <- function() {
  system.file("extdata/bundle.js", package = "meldakb")
}

meldaHtml <- function(){
  system.file("extdata/template.html", package = "meldakb")
}

meldaKB <- function(){
  library(shiny)

  ui <- htmlTemplate(meldaHtml(),
                     exitButton =tags$button(
                       id = 'Close',
                       type = "button",
                       class = "btn action-button",
                       onclick = "setTimeout(function(){window.close();window.parent.postMessage('disconnected', '*');},500);",
                       "Close window"
                     ),
                     bundle = includeScript(meldaJs())
  )

  server <- function(input, output, session) {}
  shinyApp(list(ui = ui, server = server))
}
meldaKB()
