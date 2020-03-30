meldaKB <- function(){
  library(shiny)

  ui <- htmlTemplate("template.html",
                     exitButton =tags$button(
                       id = 'Close',
                       type = "button",
                       class = "btn action-button",
                       onclick = "setTimeout(function(){window.close();window.parent.postMessage('disconnected', '*');},500);",
                       "Close window"
                     ),
                     bundle = includeScript("www/node_modules/knowledge-base/dist/bundle.js")
  )

  server <- function(input, output, session) {}
  shinyApp(ui = ui, server = server)
}

