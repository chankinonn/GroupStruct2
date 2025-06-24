
mod_summary_ui_morphometric <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Summary Statistics"),
    hr(),
    verbatimTextOutput(ns("raw_summary_text")),
    br(),
    h4("Summary Table by OTU (Mean ± SD, Min–Max)"),
    DTOutput(ns("raw_summary_table")),
    downloadButton(ns("download_raw_summary"), "Download Table"),
    hr()
  )
}

mod_summary_server_morphometric <- function(id, raw_data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$raw_summary_text <- renderPrint({
      df <- raw_data_r() # Use the raw data
      if (is.null(df)) {
        cat("Upload data in the 'Input Data' module to see raw data summary.\n")
        return()
      }
      req(df)
      n_otus <- length(unique(df[[1]]))
      sample_size_per_otu <- table(df[[1]])
      n_traits <- ncol(df) - 1
      
      cat("Number of OTUs:", n_otus, "\n\n")
      cat("Sample size per OTU:\n")
      print(sample_size_per_otu)
      cat("\nNumber of Traits:", n_traits, "\n")
      cat("\nNote: This summary is based on the raw, unadjusted data.\n")
    })
    
    raw_summary_stats <- reactive({
      df <- raw_data_r()
      req(df)
      df[[1]] <- as.factor(df[[1]])
      
      trait_cols <- sapply(df, is.numeric)
      numeric_data <- names(df)[trait_cols]
      numeric_data <- setdiff(numeric_data, names(df)[1]) # Exclude OTU column
      
      grouped_summary <- lapply(split(df, df[[1]]), function(sub_df) {
        sapply(numeric_data, function(col) {
          x <- sub_df[[col]]
          m <- round(mean(x, na.rm = TRUE), 2)
          sd <- round(sd(x, na.rm = TRUE), 2)
          min_val <- round(min(x, na.rm = TRUE), 2)
          max_val <- round(max(x, na.rm = TRUE), 2)
          # Use proper characters
          paste0(m, " ± ", sd, " (", min_val, "–", max_val, ")")
        })
      })
      
      summary_df <- do.call(rbind, grouped_summary)
      data.frame(OTU = rownames(summary_df), summary_df, row.names = NULL, check.names = FALSE)
    })
    
    output$raw_summary_table <- renderDT({
      if (is.null(raw_data_r())) {
        return(datatable(data.frame(Message = "Upload data in the 'Input Data' module to see raw data summary table.")))
      }
      datatable(raw_summary_stats(), options = list(dom = 'tip', scrollX = TRUE))
    })
    
    # Updated to write .xlsx instead of CSV
    output$download_raw_summary <- downloadHandler(
      filename = function() {
        paste0("morphometric_raw_summary_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- raw_summary_stats()
        openxlsx::write.xlsx(df, file = file, overwrite = TRUE)
      }
    )
    
    return(raw_data_r)
  })
}
