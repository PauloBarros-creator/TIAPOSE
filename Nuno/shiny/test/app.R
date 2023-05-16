library(shiny)

u <- shinyUI(pageWithSidebar(
  
  headerPanel("Entering Vectors in Shiny"),
  sidebarPanel(
    textInput('vec1', 'Enter a vector (comma delimited)', "0,1,2")
  ),
  
  mainPanel(
    h4('You entered'),
    verbatimTextOutput("oid1"),
    verbatimTextOutput("oid2")
  )
))

s <- shinyServer(function(input, output) {
  
  output$oid1 <- renderPrint({
    cat("As string:\n")
    cat(input$vec1)
  }
  )
  
  output$oid2<-renderPrint({
    x <- as.numeric(unlist(strsplit(input$vec1,",")))
    cat("As atomic vector:\n")
    print(x)
  }
  )
}
)
shinyApp(ui = u, server = s)