library(shiny)
library(miniUI)
library(DT)
library(httr)

meldaKB <- function(){
  ui <- miniPage(
        textInput("search","Search in Packages,Methods, Authors","",width = '100%'),

    miniTabstripPanel(
      miniTabPanel(id = "pkg","Packages",icon = icon("table"),
                   miniContentPanel(
                     (DT::dataTableOutput("packagesTable"))),
      ),
      miniTabPanel("Method",icon = icon("table"),
                   miniContentPanel(
                     (DT::dataTableOutput("methodsTable")))
      ),
      miniTabPanel("Author", icon = icon("table"),
                   miniContentPanel(
                     ((DT::dataTableOutput("authorsTable")))
                   ))
    )
  )

  server <- function(input, output, session) {

    url <- "https://kbdev.melda.io/"
    rv <- reactiveValues()
    rv$methods = data.frame()
    rv$authors = data.frame()
    rv$packages = data.frame()
    rv$packageName = ''

    observe({
      req(input$search)
      res <- httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100"))
      resAuthor <-  httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100"))

      rv$packages <- data.frame(name = sapply(content(res)$packages,function(x) x$name )
                                ,description = sapply(content(res)$packages,function(x) x$description ),
                                version = sapply(content(res)$packages,function(x) x$version ))

      rv$methods = data.frame(name = sapply(content(res)$methods,function(x) x$name),
                              title = sapply(content(res)$methods,function(x) x$title),
                              description = sapply(content(res)$methods,function(x) x$description))

      rv$authors = data.frame(name = sapply(content(resAuthor)$packages,function(x) x$author )
                              ,Package = sapply(content(resAuthor)$packages,function(x) x$name ))

    })

    output$packagesTable <- DT::renderDataTable({
      req(rv$packages)
      rv$packages
    })

    output$methodsTable <- DT::renderDataTable({
      req(rv$methods)
      datatable(
        rv$methods, selection =  "none", class="cell-border strip hover"
      ) %>% formatStyle(0, cursor = 'pointer')

    })

    output$authorsTable <- DT::renderDataTable({
      req(rv$authors)
      rv$authors
    })
  }
  runGadget(shinyApp(ui, server),  viewer = paneViewer())
}
