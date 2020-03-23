meldaKB <- function(){
  library(shiny)
  library(miniUI)
  library(DT)
  library(httr)

  ui <- miniUI::miniPage(
    miniTitleBar(htmlOutput("meldaLogo")),
    textInput("search","","",placeholder = "Enter a keyword",width = '100%'),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(id = "pkg","Packages",icon = icon("table"),
                   miniUI::miniContentPanel(
                     (DT::dataTableOutput("packagesTable")),
                     shiny::htmlOutput("packageDetail"))
                   ),
      miniUI::miniTabPanel("Method",icon = icon("table"),
                   miniUI::miniContentPanel(
                     (DT::dataTableOutput("methodsTable")),
                     shiny::htmlOutput("methodDetail"))
      ),
      miniUI::miniTabPanel("Author", icon = icon("table"),
                   miniUI::miniContentPanel(
                     ((DT::dataTableOutput("authorsTable"))),
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

    getPackageDetail <- function(input,output){
      packageRes <- httr::GET( paste0( url, "api/", "package-detail?package=", rv$packageName))

      rv$packageDetailOutput <- paste("<br>",'<strong><center>Package Detail</center></strong>',"<br>",
                                      "<br>","<strong>Name: </strong>",
                                      rv$packageName,
                                      "<br>","<strong>Imports: </strong>",
                                      paste(content(packageRes)$result$imports,collapse = ""),
                                      "<br>","<strong>Depends: </strong>",
                                      paste(content(packageRes)$result$depends,collapse = ""),
                                      "<br>","<strong>Author: </strong>",
                                      paste(content(packageRes)$result$author,collapse = ""),
                                      "<br>","<strong>Id: </strong>",
                                      paste(content(packageRes)$result$packageId,collapse = ""),
                                      "<br>","<strong>Description: </strong>",
                                      paste(content(packageRes)$result$description,collapse = ""),
                                      "<br>","<strong>Published Date: </strong>",
                                      paste(content(packageRes)$result$published,collapse = ""),
                                      "<br>","<strong>Version: </strong>",
                                      paste(content(packageRes)$result$version,collapse = ""),
                                      "<br>","<strong>Maintainer: </strong>",
                                      paste(content(packageRes)$result$maintainer,collapse = ""),
                                      "<br>","<strong>Licence: </strong>",
                                      paste(content(packageRes)$result$licence,collapse = ""),
                                      "<br>","<strong>Suggests: </strong>",
                                      paste(content(packageRes)$result$suggests,collapse = ""),
                                      "<br>","<strong>Materials: </strong>",
                                      paste(content(packageRes)$result$materials,collapse = ""),
                                      "<br>","<strong>Needs Compilation: </strong>",
                                      paste(content(packageRes)$result$needscompilation,collapse = ""),
                                      "<br>","<strong>Cran Checks: </strong>",
                                      paste(content(packageRes)$result$cranchecks,collapse = ""),
                                      "<br>","<strong><center> Methods </center> </strong>" ,"<br>")

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

    getauthorPackageDetail <- function(input,output){
      authorRes <- httr::GET( paste0( url, "api/", "package-detail?package=", rv$authorPackageName))

      rv$authorDetailOutput <- paste("<br>","<strong>Package Detail</strong>","<br>",
                                     "<br>","<strong>Name: </strong>",
                                     rv$packageName,
                                     "<br>","<strong>Imports: </strong>",
                                     paste(content(authorRes)$result$imports,collapse = ""),
                                     "<br>","<strong>Depends: </strong>",
                                     paste(content(authorRes)$result$depends,collapse = ""),
                                     "<br>","<strong>Author: </strong>",
                                     paste(content(authorRes)$result$author,collapse = ""),
                                     "<br>","<strong>Id: </strong>",
                                     paste(content(authorRes)$result$packageId,collapse = ""),
                                     "<br>","<strong>Description: </strong>",
                                     paste(content(authorRes)$result$description,collapse = ""),
                                     "<br>","<strong>Published Date: </strong>",
                                     paste(content(authorRes)$result$published,collapse = ""),
                                     "<br>","<strong>Version: </strong>",
                                     paste(content(authorRes)$result$version,collapse = ""),
                                     "<br>","<strong>Maintainer: </strong>",
                                     paste(content(authorRes)$result$maintainer,collapse = ""),
                                     "<br>","<strong>Licence: </strong>",
                                     paste(content(authorRes)$result$licence,collapse = ""),
                                     "<br>","<strong>Suggests: </strong>",
                                     paste(content(authorRes)$result$suggests,collapse = ""),
                                     "<br>","<strong>Materials: </strong>",
                                     paste(content(authorRes)$result$materials,collapse = ""),
                                     "<br>","<strong>Needs Compilation: </strong>",
                                     paste(content(authorRes)$result$needscompilation,collapse = ""),
                                     "<br>","<strong>Cran Checks: </strong>",
                                     paste(content(authorRes)$result$cranchecks,collapse = "",
                                    "<br>","<strong><center> Methods </center> </strong>" ,"<br>")
      )

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

    getMethodDetail <- function(input,output){
      methodRes <- httr::GET( paste0( url, "api/", "method-detail?package=",
                                       rv$methodPackageName,"&method=",
                                      rv$methodMethodName))

            rv$methodDetailOutput <- paste("<br>","<strong><center>Method Detail</center></strong>","<br>",
                                     "<br>","<strong>Usage</strong>:",
                                     paste(content(methodRes)$result$usage,collapse=""),
                                     "<br>","<strong>Name</strong>:",
                                     paste(content(methodRes)$result$name,collapse=""),
                                     "<br>","<strong>Method</strong> Id:",
                                     paste(content(methodRes)$result$methodId,collapse=""),
                                     "<br>","<strong>Description</strong>",
                                     paste(content(methodRes)$result$description,collapse=""),
                                     "<br>","<strong>Alias</strong>",
                                     paste(content(methodRes)$result$alias,collapse=""),
                                     "<br>","<strong>Title</strong>",
                                     paste(content(methodRes)$result$title,collapse="")
      )

      req(content(methodRes)$result$argument)
      rv$methodDetailOutput <- paste("<br>","<strong><center>Arguments</center></strong>")
      for(i in 1:length(content(methodRes)$result$argument)){
        rv$methodDetailOutput <- paste(rv$methodDetailOutput,'<hr><ul>',
                                        "<li><strong>Name</strong>:",
                                        content(methodRes)$result$argument[[i]]$name,
                                        "</li><br>",
                                        "<li><strong>Description</strong>",
                                        content(methodRes)$result$argument[[i]]$description,
                                        "</li><br>",
                                        "<li><strong>Argument Id</strong>",
                                        content(methodRes)$result$argument[[i]]$argumentId,
                                        "</li><br>",
                                        '</ul><hr>')
      }
    }

    observeEvent( input$packagesTable_cell_clicked , {
      req( length( input$packagesTable_cell_clicked) > 0)
      if( input$packagesTable_cell_clicked$col == 1){
        info = input$packagesTable_cell_clicked
        rv$packageName <- info$value
        getPackageDetail(input,output)
      }
    })

    observeEvent(input$authorsTable_cell_clicked , {
      req( length(input$authorsTable_cell_clicked) > 0 )
      print(input$authorsTable_cell_clicked)
      if( input$authorsTable_cell_clicked$col == 2){
        info = input$authorsTable_cell_clicked
        rv$authorPackageName <- info$value
        getauthorPackageDetail(input,output)
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
          getMethodDetail(input,output)
        }
      })

    observe({
      req(input$search)
      resPackage <- httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100&in=package"))
      resMethod <- httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100&in=method"))
      resAuthor <-  httr::GET(paste0(url,"search?q=",URLencode(input$search),"&size=100&in=author"))

      rv$packages <- data.frame(name = sapply(httr::content(resPackage)$packages,function(x) x$name )
                                ,description = sapply(httr::content(resPackage)$packages,function(x) x$description ),
                                version = sapply(httr::content(resPackage)$packages,function(x) x$version ))

      rv$methods = data.frame(name = sapply(httr::content(resMethod)$methods,function(x) x$name),
                              title = sapply(httr::content(resMethod)$methods,function(x) x$title),
                              description = sapply(httr::content(resMethod)$methods,function(x) x$description),
                              package = sapply(content(resMethod)$methods,function(x) x$packageName))

      rv$authors = data.frame(name = sapply(httr::content(resAuthor)$packages,function(x) x$author )
                              ,Package = sapply(httr::content(resAuthor)$packages,function(x) x$name ))


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
      rv$packages
    })

    output$methodsTable <- DT::renderDataTable({
      req(rv$methods)
      rv$methods
      })

    output$authorsTable <- DT::renderDataTable({
      req(rv$authors)
      rv$authors
    })

    output$meldaLogo <-
      renderText({
        c('<a href="https://www.melda.io/">',
          '<img src="',
          "https://i0.wp.com/www.melda.io/wp-content/uploads/2018/10/melda_final.png?w=1080&ssl=1",
          '"',' ,style="width:"7%", width="7%"',
          '>Search in melda.io Knowledge Base</a>',
          '<a href="https://www.melda.io/">'
        )
      })
  }
  runGadget(shinyApp(ui, server),  viewer = paneViewer())
}

