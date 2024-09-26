#' Run the shiny application
#' @param ... further arguments that can be passed to \code{\link[shiny]{shinyApp}} options
#' @return shinyApp object, see also \code{\link[shiny]{shinyApp}}
#' @importFrom shiny shinyApp
#' @import sparklyr
#' @export
app_rnaseq <- function(...) {
  shinyApp(
    ui = rnaseq_ui(),
    server = rnaseq_server(),
    options = list(...)
  )
}

# Define UI for application that draws a histogram
rnaseq_ui <- function() {
  # Connect to SeqsLab
  sc <- spark_connect(
    master = "sc://172.18.0.1:15002",
    method = "spark_connect",
    version = "3.5"
  )
  # list available databases (runs)
  # dbs <- src_databases(sc, col="namespace")
  dbs <- dbGetQuery(sc, "SHOW DATABASES")
  fluidPage(
    tabsetPanel(
      tabPanel("RNAseq", rnaseqUI("RNAseq", dbs)),
      tabPanel("DNAseq", rnaseqUI("DNAseq", dbs)),
    )
  )
}

# Define server logic required to draw a histogram
rnaseq_server <- function() {
  # Connect to SeqsLab
  sc <- spark_connect(
    master = "sc://172.18.0.1:15002",
    method = "spark_connect",
    version = "3.5"
  )
  function(input, output, session) {
    rnaseqServer("RNAseq")
    rnaseqServer("DNAseq")
  }
}