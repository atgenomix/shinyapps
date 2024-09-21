#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

required_pkgs <- c("shiny", "sparklyr", "pysparklyr", "dplyr", "glue", "DBI", "pheatmap", "bslib", "ggplot2")
install.packages(setdiff(required_pkgs, rownames(installed.packages())))
invisible(lapply(required_pkgs, library, character.only = TRUE))

# Define UI for application that draws a histogram
uiFunction <- function() {
    library(sparklyr)
    library(DBI)
    sc <- spark_connect(master="sc://172.18.0.1:15002", method="spark_connect", version="3.5.1")
    dbs <- dbGetQuery(sc, "SHOW DATABASES")
    fluidPage(
        tabsetPanel(
            tabPanel("RNAseq", rnaseqUI("RNAseq", dbs)),
            tabPanel("DNAseq", rnaseqUI("DNAseq", dbs)),
        )
    )
}

# Define server logic required to draw a histogram
serverFunction <- function() {
    library(sparklyr)
    sc <- spark_connect(master="sc://172.18.0.1:15002", method="spark_connect", version="3.5.1")
    function(input, output, session) {
        rnaseqServer("RNAseq", sc)
        rnaseqServer("DNAseq", sc)
    }
}

# Define UI for RNA-seq application
rnaseqUI <- function(id, dbs) {
    library(shiny)
    library(bslib)
    ns <- NS(id)
    tagList(
        selectInput(ns("db"), label = "Run name:", choices = dbs),
        sliderInput(
            ns("logFC_slider"),
            "Exclude Range of Fold Change",
            min = -10, max = 10, step = 0.1,
            value = c(-0.6, 0.6)
        ),
        input_task_button(ns("generate_plot"), "Generate Plot"),
        plotOutput(ns("volcano")),
        plotOutput(ns("heatmap"))
    )
}

# Define server logic required to draw RNA-seq charts
rnaseqServer <- function(id, sc) {
  moduleServer(id, function(input, output, session) {
    library(DBI)
    library(ggplot2)
    library(glue)
    library(pheatmap)

    read_table <- function(pattern, db, tbls) {
      n <- tbls$tableName[grepl(pattern, tbls$tableName)]
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
      read_table("^normcounts_delta", input$db, tbls())
    })
    
    logFC_min <- reactive({
      as.numeric(input$logFC_slider[1])
    })
    
    logFC_max <- reactive({
      as.numeric(input$logFC_slider[2])
    })
    
    query_genes <- reactive({
      logFC_min <- logFC_min()
      logFC_max <- logFC_max()
      tbl_exacttest() %>%
        filter(logFC >= logFC_max | logFC <= logFC_min) %>%
        filter(PValue <= 0.05) %>%
        filter(FDR <= 0.05) %>%
        select(genes) %>%
        collect() %>%
        .$genes
    })
    
    output$heatmap <- renderPlot({
      query_genes <- query_genes()
      tbl_normalcounts() %>%
        filter(genes %in% query_genes) %>%
        #mutate_if(is.numeric, ~ log2(. + 1)) %>%
        collect() %>%
        {
          pheatmap(.[, -1], labels_row = .$genes)
        }
    }) %>% bindEvent(input$generate_plot)
    
    output$volcano <- renderPlot({
      logFC_min <- logFC_min()
      logFC_max <- logFC_max()
      tbl_exacttest() %>%
        select(c("logFC", "PValue")) %>%
        mutate(negLogPval = -log10(PValue)) %>%
        mutate(diffexpressed = case_when(
          logFC >= logFC_max & PValue <= 0.05 ~ "Upregulated",
          logFC <= logFC_min & PValue <= 0.05 ~ "Downregulated",
          TRUE ~ "Not significant"
        )) %>%
        collect() %>%
        ggplot(., aes(x = logFC, y = negLogPval, col = diffexpressed)) +
        geom_point() +
        theme_minimal() +
        labs(
          title = "Volcano Plot RNA Seq",
          x = "Log2 Fold Change",
          y = "-Log10 P-value"
        ) +
        geom_vline(xintercept = c(logFC_min, logFC_max), col = "gray", linetype = "dashed") +
        geom_hline(yintercept = -log10(0.05), col = "gray", linetype = "dashed") +
        geom_point(size = 1) +
        scale_color_manual(
          values = c("#00AFBB", "grey", "#bb0c00"),
          labels = c("Downregulated", "Not significant", "Upregulated")
        ) +
        labs(
          color = "Expressed", # legend_title,
          x = expression("log"[2] * "FC"), y = expression("-log"[10] * "p-value")
        )
    }) %>% bindEvent(input$generate_plot)
  })
}
