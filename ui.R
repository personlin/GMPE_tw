
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
GMPEs <- c("WU2001", "CB2004", "JB2004", "JR2004", "KA2004", "NCREE2006", "TS2006", "LIN2009", "LIN2011")

shinyUI(fluidPage(

  # Application title
  titlePanel("My Shiny App: GMPE_tw"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h4("Select GMPE:"),
      selectInput('gmpes', 'Click blow to select the GMPE that you want.', GMPEs, multiple=TRUE, selectize=TRUE),
      br(),
      br(),
      sliderInput("mag", label = h4("Magnitude (Mw)"),
                  min = 3, max = 8, value = 6.5, step=0.1),
      
      sliderInput("rangeR", label = h4("Range of distance (km):"), min = 1, max = 400, value = c(1,150)),
      br()
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1"),
      textOutput("text1"),
      textOutput("text2")
    )
  )
))
