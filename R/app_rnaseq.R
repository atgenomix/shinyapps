#' Run the shiny application
#' @param ... further arguments that can be passed to \code{\link[shiny]{shinyApp}} options
#' @return shinyApp object, see also \code{\link[shiny]{shinyApp}}
#' @importFrom shiny shinyApp
#' @import sparklyr
#' @export
launch_app_rnaseq <- function(master = "sc://172.18.0.1:15002", ...) {
  # Connect to SeqsLab
  sc <- spark_connect(
    master = master,
    method = "spark_connect",
    version = "3.5"
  )
  shinyApp(
    ui = rnaseq_ui(sc),
    server = rnaseq_server(sc),
    options = list(...)
  )
}

# Define UI for application that draws a histogram
rnaseq_ui <- function(sc) {
  # list available databases (runs)
  # dbs <- src_databases(sc, col="namespace")
  dbs <- dbGetQuery(sc, "SHOW DATABASES")
  fluidPage(
    tabsetPanel(
      tabPanel("RNAseq", mod_rnaseq_ui("RNAseq", sc, dbs)),
      tabPanel("DNAseq", mod_rnaseq_ui("DNAseq", sc, dbs)),
    )
  )
}

# Define server logic required to draw a histogram
rnaseq_server <- function(sc) {
  function(input, output, session) {
    mod_rnaseq_server("RNAseq", sc)
    mod_rnaseq_server("DNAseq", sc)
  }
}