meldaKB <- function(){
  library(shiny)
  library(miniUI)
  library(DT)
  library(httr)

  ui <- miniUI::miniPage(
    miniTitleBar(htmlOutput("meldaLogo",style="align:left"),
                 right = actionButton("exitBtn","Exit")),
    textInput("search","","",placeholder = "Enter a keyword",width = '100%'),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(id = "pkg","Packages",icon = icon("table"),
                   miniUI::miniContentPanel(padding = 0,
                     (dataTableOutput("packagesTable",
                                      height="220px")),
                     shiny::htmlOutput("packageDetail"))
                   ),
      miniUI::miniTabPanel("Methods",icon = icon("table"),
                   miniUI::miniContentPanel(padding = 0,
                     (dataTableOutput("methodsTable",
                                      height="220px")),
                     shiny::htmlOutput("methodDetail"))
      ),

        miniUI::miniTabPanel("Authors", icon = icon("table"),
                             miniUI::miniContentPanel(padding = 0,
                                                      ((DT::dataTableOutput("authorsTable",
                                                                            height="220px"))),
                                                      shiny::htmlOutput("authorDetail")))


          )
  )

  server <- function(input, output, session) {

    url <- "https://kbdev.melda.io/"
    rv <- reactiveValues()

    rv$methods = data.frame()
    rv$authors = data.frame()
    rv$packages = data.frame()
    rv$packageDetail = data.frame()
    rv$packageMethods = data.frame()

    rv$methodDetail = ''
    rv$authorDetail = ""
    rv$packageName = ''
    rv$authorPackageName = ''
    rv$packageDetailOutput = ''
    rv$authorDetailOutput = ''
    rv$methodDetailOutput = ''
    rv$methodPackageName = ''
    rv$methodMethodName = ''

    getPackageDetail <- function(){
      rv$packageDetailOutput = ""
      packageRes <- httr::GET( paste0( url, "api/", "package-detail?package=", rv$packageName))
      contents <- content(packageRes)$result

      for ( i in 1:length(contents)){
        if( typeof(contents[[i]] ) == "character") {
          rv$packageDetailOutput <- paste(rv$packageDetailOutput,
                                          "<br>","<strong>",
                                          names(contents[i]),
                                          "</strong>",
                                          contents[[i]] )
        }
      }

      for(i in 1:length(content(packageRes)$result$methods)){
        rv$packageDetailOutput <- paste(rv$packageDetailOutput,'<hr><ul>',
                                        "<li><strong>Name</strong>:",
                                        content(packageRes)$result$methods[[i]]$name,
                                        "</li><br>",
                                        "<li><strong>Description</strong>",
                                        content(packageRes)$result$methods[[i]]$description,
                                        "</li><br>",
                                        "<li><strong>Arguments</strong>",
                                        content(packageRes)$result$methods[[i]]$argument,
                                        "</li><br>",
                                        '</ul><hr>')
      }
    }

    getauthorPackageDetail <- function(){
      rv$authorDetailOutput = ""
      authorRes <- httr::GET( paste0( url, "api/", "package-detail?package=", rv$authorPackageName))
      contents <- content(authorRes)$result

      for ( i in 1:length(contents)){
        if( typeof(contents[[i]] ) == "character") {
         rv$authorDetailOutput <- paste(rv$authorDetailOutput,
                                        "<br>","<strong>",
                                         names(contents[i]),
                                        "</strong>",
                                          contents[[i]] )
       }
      }

      for(i in 1:length(content(authorRes)$result$methods)){
        rv$authorDetailOutput <- paste(rv$authorDetailOutput,'<hr><ul>',
                                       "<li><strong>Name</strong>:",
                                       content(authorRes)$result$methods[[i]]$name,
                                       "</li><br>",
                                       "<li><strong>Description</strong>",
                                       content(authorRes)$result$methods[[i]]$description,
                                       "</li><br>",
                                       "<li><strong>Arguments</strong>",
                                       content(authorRes)$result$methods[[i]]$argument,
                                       "</li><br>",
                                       '</ul><hr>')
        }
    }

    getMethodDetail <- function(){
      rv$methodDetailOutput <- ""
      methodRes <- httr::GET( paste0( url, "api/", "method-detail?package=",
                                       rv$methodPackageName,"&method=",
                                      rv$methodMethodName))
      contents <- content(methodRes)$result

      for ( i in 1:length(contents)){
        if( typeof(contents[[i]] ) == "character") {
          rv$methodDetailOutput <- paste(rv$methodDetailOutput,
                                         "<br>","<strong>",
                                         names(contents[i]),
                                         "</strong>",
                                         contents[[i]] )
        }
      }

      req(content(methodRes)$result$argument)
      rv$methodDetailOutput <- paste( rv$methodDetailOutput,
                                      "<br>","<strong><center>",
                                      "Arguments</center></strong>")

      for(i in 1:length(content(methodRes)$result$argument)){
        rv$methodDetailOutput <- paste(rv$methodDetailOutput,
                                        "<strong>Name</strong>:",
                                        content(methodRes)$result$argument[[i]]$name,
                                        "<br><strong>Description</strong>",
                                        content(methodRes)$result$argument[[i]]$description,
                                        "<br><strong>Argument Id</strong>",
                                        content(methodRes)$result$argument[[i]]$argumentId,
                                        '<br>')
      }
    }

    observeEvent( input$packagesTable_cell_clicked , {
      req( length( input$packagesTable_cell_clicked) > 0)
      if( input$packagesTable_cell_clicked$col == 1){
        info = input$packagesTable_cell_clicked
        rv$packageName <- info$value
        getPackageDetail()
      }
    })

    observeEvent(input$authorsTable_cell_clicked , {
      req( length(input$authorsTable_cell_clicked) > 0 )
      print(input$authorsTable_cell_clicked)
      if( input$authorsTable_cell_clicked$col == 2){
        info = input$authorsTable_cell_clicked
        rv$authorPackageName <- info$value
        getauthorPackageDetail()
      }
    })

    observeEvent(input$methodsTable_cell_clicked , {
      req(length(input$methodsTable_cell_clicked) > 0)
        if( input$methodsTable_cell_clicked$col == 1){
          methodColNum = as.numeric(input$methodsTable_cell_clicked$col)
          methodRowNum = as.numeric(input$methodsTable_cell_clicked$row)
          packageColNum = methodColNum + 3
          packageRowNum = methodRowNum

          rv$methodPackageName <- rv$methods[packageRowNum,packageColNum]
          rv$methodMethodName <- rv$methods[methodRowNum,methodColNum]
          rv$methodMethodName <- URLencode(as.character(rv$methodMethodName))
          rv$methodPackageName <- URLencode(as.character(rv$methodPackageName))
          getMethodDetail()
        }
      })

    observe({
      if(input$exitBtn){
        stopApp()
      }

    })
    observe({
      req(input$search)
      resPackage <- httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100&in=package"))
      resMethod <- httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100&in=method"))
      resAuthor <-  httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100&in=author"))

      rv$packages <- data.frame(package = sapply(httr::content(resPackage)$packages,function(x) x$name ),
                                description = sapply(httr::content(resPackage)$packages,function(x) x$description ),
                                version = sapply(httr::content(resPackage)$packages,function(x) x$version ))

      rv$methods = data.frame(method = sapply(httr::content(resMethod)$methods,function(x) x$name),
                              title = sapply(httr::content(resMethod)$methods,function(x) x$title),
                              description = sapply(httr::content(resMethod)$methods,function(x) x$description),
                              package = sapply(content(resMethod)$methods,function(x) x$packageName))

      rv$authors = data.frame(author = sapply(httr::content(resAuthor)$packages,function(x) x$author )
                              ,package = sapply(httr::content(resAuthor)$packages,function(x) x$name ))


      output$packageDetail <- renderText({
        req(rv$packageDetailOutput)
        HTML(rv$packageDetailOutput)
      })

      output$authorDetail <- renderText({
        req(rv$authorDetailOutput)
        HTML(rv$authorDetailOutput)
      })

      output$methodDetail <- renderText({
        req(rv$methodDetailOutput)
        HTML(rv$methodDetailOutput)
      })
    })

    output$packagesTable <- DT::renderDataTable({
      req(rv$packages)
      data.frame(rv$packages)

      },class = 'display compact',
        selection = "single",
      options = list(
        dom = 'tp',
        pageLength = 5,
        columnDefs = list(list(
          targets = c(1,2,3),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 50 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
            "}")
        ))), callback = JS('table.page(3).draw(false);')
      )

    output$methodsTable <- DT::renderDataTable({
      req(rv$methods)
      rv$methods
      },class = "display compact",
      selection = "single",
      options = list(
        pageLength = 5,
        dom = 'tp',
        columnDefs = list(list(
          targets = c(1,2,3),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 14 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 14) + '...</span>' : data;",
            "}")
        ))), callback = JS('table.page(3).draw(false);'))

    output$authorsTable <- DT::renderDataTable({
      req(rv$authors)
      rv$authors
    },class = "display compact",
      selection = "single",
    options = list(
      dom = 'tp',
      pageLength = 5,
      columnDefs = list(list(
        targets = c(1,2),
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 14 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 14) + '...</span>' : data;",
          "}")
      ))), callback = JS('table.page(3).draw(false);'))

    output$meldaLogo <-
      renderText({
        c('<a href="https://www.melda.io/" style="float:left">',
          '<img src="',
          "https://i0.wp.com/www.melda.io/wp-content/uploads/2018/10/melda_final.png?w=1080&ssl=1",
          '"',' ,style="width:"7%", width="7%"',
          '>Search in melda.io Knowledge Base</a>',
          '<a href="https://www.melda.io/" style="float:left">'
        )
      })
  }
  runGadget(shinyApp(ui, server),  viewer = paneViewer())
}
meldaKB()

