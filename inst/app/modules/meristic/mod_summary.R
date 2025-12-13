mod_summary_ui_meristic <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Summary Statistics"),
    hr(),
    verbatimTextOutput(ns("summary_text")),
    br(),
    h4("Summary Table by OTU (Mean ± SD, Min–Max)"),
    DTOutput(ns("summary_table")),
    hr(),
    p(strong("Note:"), "See the",
      strong("Visualization"), "tab for scatter, box, and violin plots."),
    downloadButton(ns("download_summary"), "Download Table"),
    hr(),
  )
}

mod_summary_server_meristic <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$summary_text <- renderPrint({
      df <- isolate(data_r())
      req(df)
      n_otus <- length(unique(df[[1]]))
      sample_size_per_otu <- table(df[[1]])
      n_traits <- ncol(df) - 1
      
      cat("Number of OTUs:", n_otus, "\n\n")
      cat("Sample size per OTU:\n")
      print(sample_size_per_otu)
      cat("\nNumber of Traits:", n_traits, "\n")
    })
    
    summary_stats <- reactive({
      df <- isolate(data_r())
      req(df)
      df[[1]] <- as.factor(df[[1]])
      
      trait_cols <- sapply(df, is.numeric)
      numeric_data <- names(df)[trait_cols]
      numeric_data <- setdiff(numeric_data, names(df)[1])
      
      grouped_summary <- lapply(split(df, df[[1]]), function(sub_df) {
        sapply(numeric_data, function(col) {
          x <- sub_df[[col]]
          m <- round(mean(x, na.rm = TRUE), 2)
          sd <- round(sd(x, na.rm = TRUE), 2)
          min_val <- round(min(x, na.rm = TRUE), 2)
          max_val <- round(max(x, na.rm = TRUE), 2)
          paste0(m, " ± ", sd, " (", min_val, "–", max_val, ")")
        })
      })
      
      summary_df <- do.call(rbind, grouped_summary)
      data.frame(OTU = rownames(summary_df), summary_df, row.names = NULL, check.names = FALSE)
    })
    
    output$summary_table <- renderDT({
      datatable(summary_stats(), options = list(dom = 'tip', scrollX = TRUE))
    })
    
    # pdated to write .xlsx with proper UTF-8 output
    output$download_summary <- downloadHandler(
      filename = function() {
        paste0("summary_statistics_by_OTU_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- summary_stats()
        openxlsx::write.xlsx(df, file = file, overwrite = TRUE)
      }
    )
  })
}
