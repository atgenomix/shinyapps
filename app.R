#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
source("modules/rnaseq.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  tabsetPanel(
    tabPanel("RNAseq",rnaseqUI("RNAseq")),
    tabPanel("DNAseq",rnaseqUI("DNAseq")),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  rnaseqServer("RNAseq")
  rnaseqServer("DNAseq")
}

# Run the application 
shinyApp(ui = ui, server = server)
