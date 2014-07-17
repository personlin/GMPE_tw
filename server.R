
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

GMPEs <- c("WU2001", "CB2004", "JB2004", "JR2004", "KA2004", "NCREE2006", "TS2006", "LIN2009", "LIN2011")

source("helpers.R")

shinyServer(function(input, output) {

  output$text1 <- renderText({
    input$goButton
    isolate({
      paste("You have select these GMPEs:", input$gmpes)
    })
  })
  
  output$text2 <- renderText({
    paste("You have select Magnitude:", input$mag)
  })
  
  output$text3 <- renderText({
    paste("You have select distance range:", input$rangeR[1], "to", input$rangeR[2], "km.")
  })

  output$plot1 <- renderPlot({
    input$goButton
    plot_gmpe(input$mag, input$rangeR[2], input$rangeR[1], input$gmpes)
  })
})
