#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

install.packages("sparklyr")
install.packages("pysparklyr")
pysparklyr::install_pyspark()

library(shiny)
library(sparklyr)
library(dplyr)
library(glue)
library(DBI)
library(pheatmap)

# Connect to SeqsLab
sc <- spark_connect(
  master = "sc://localhost",
  method = "spark_connect"
)

# list available databases (runs)
dbs <- src_databases(sc)

# Define UI for RNA-seq application
rnaseqUI <- function(id) {
  ns <- NS(id)
  
  selectInput(ns("db"), label = "Run name:", choices = dbs)
  
  tagList(
    plotOutput(ns("heatmap"))
  )
}

# Define server logic required to draw RNA-seq charts
rnaseqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    read_table <- function(pattern, db, tbls) {
      n <- tbls[grepl(pattern, tbls)]
      sdf_sql(sc, glue("SELECT * FROM {db}.{n}"))
    }
    
    tbls <- reactive({
      tbl_change_db(sc, input$db)
      dbGetQuery(sc, glue("SHOW TABLES IN {input$db}"))
    })
    
    tbl_go <- reactive({
      read_table("^go_delta", input$db, tbls())
    })
    
    tbl_gosea <- reactive({
      read_table("^gosea_delta", input$db, tbls())
    })
    
    tbl_gopvalue <- reactive({
      read_table("^gopvalue_delta", input$db, tbls())
    })
    
    tbl_exacttest <- reactive({
      read_table("^exacttest_delta", input$db, tbls())
    })
    
    tbl_normalcounts <- reactive({
      read_table("^normalcounts_delta", input$db, tbls())
    })
    
    output$heatmap <- renderPlot({
      pheatmap(tbl_normalcounts())
    }, res = 96)
  })
}
