
mod_summary_ui_combined <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Summary Statistics"),
    hr(),
    verbatimTextOutput(ns("raw_summary_text")),
    br(),
    h4("Summary Table by OTU (Mean ± SD, Min–Max)"),
    p("Categorical variables, if present, are not included in the summary table"),
    DTOutput(ns("raw_summary_table")),
    downloadButton(ns("download_raw_summary"), "Download Table"),
    uiOutput(ns("categorical_note")),  # show omitted categorical variables
    hr()
  )
}

mod_summary_server_combined <- function(id, raw_data_list_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    combined_df_and_cols <- reactive({
      req(raw_data_list_r())
      raw_data_list_r()
    })
    
    df_reactive <- reactive({
      req(combined_df_and_cols()$data)
      combined_df_and_cols()$data
    })
    
    group_col_name_reactive <- reactive({
      req(combined_df_and_cols()$group_col)
      combined_df_and_cols()$group_col
    })
    
    # Raw Data Summary Outputs
    output$raw_summary_text <- renderPrint({
      df <- df_reactive()
      group_col_name <- group_col_name_reactive()
      req(df, group_col_name)
      
      n_otus <- length(unique(df[[group_col_name]]))
      sample_size_per_otu <- table(df[[group_col_name]])
      
      numeric_trait_cols <- names(df)[sapply(df, is.numeric)]
      numeric_trait_cols <- setdiff(numeric_trait_cols, group_col_name)
      n_traits <- length(numeric_trait_cols)
      
      non_numeric_cols <- names(df)[!sapply(df, is.numeric)]
      non_group_categoricals <- setdiff(non_numeric_cols, group_col_name)
      
      cat("Number of OTUs:", n_otus, "\n\n")
      cat("Sample size per OTU:\n")
      print(sample_size_per_otu)
      cat("\nNumber of Numeric Traits (Meristic + Morphometric):", n_traits, "\n")
      cat("\nNote: This summary includes only numeric traits (meristic and morphometric).\n")
      
      if (length(non_group_categoricals) > 0) {
        cat("The following column(s) were excluded from summary because they are categorical:\n")
        cat(paste(non_group_categoricals, collapse = ", "), "\n")
      }
    })
    
    raw_summary_stats <- reactive({
      df <- df_reactive()
      group_col_name <- group_col_name_reactive()
      req(df, group_col_name)
      
      df[[group_col_name]] <- as.factor(df[[group_col_name]])
      
      numeric_data_cols <- names(df)[sapply(df, is.numeric)]
      numeric_data_cols <- setdiff(numeric_data_cols, group_col_name)
      
      if (length(numeric_data_cols) == 0) {
        return(data.frame(Message = "No numeric traits found for summary."))
      }
      
      df_numeric_only <- df %>% select(all_of(group_col_name), all_of(numeric_data_cols))
      
      grouped_summary <- lapply(split(df_numeric_only, df_numeric_only[[group_col_name]]), function(sub_df) {
        sapply(numeric_data_cols, function(col) {
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
    
    output$raw_summary_table <- renderDT({
      req(df_reactive())
      datatable(raw_summary_stats(), options = list(dom = 'tip', scrollX = TRUE))
    })
    
    # Updated to export Excel-safe UTF-8 .xlsx with proper symbols
    output$download_raw_summary <- downloadHandler(
      filename = function() {
        paste0("combined_raw_summary_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- raw_summary_stats()
        openxlsx::write.xlsx(df, file = file, overwrite = TRUE)
      }
    )
    
    output$categorical_note <- renderUI({
      df <- df_reactive()
      group_col_name <- group_col_name_reactive()
      req(df, group_col_name)
      
      non_numeric_cols <- names(df)[!sapply(df, is.numeric)]
      non_group_categoricals <- setdiff(non_numeric_cols, group_col_name)
      
      if (length(non_group_categoricals) > 0) {
        HTML(paste0(
          "<br><strong>Note:</strong> The following column(s) were excluded from the summary because they are categorical: ",
          paste(non_group_categoricals, collapse = ", "),
          "."
        ))
      } else {
        NULL
      }
    })
    
    return(raw_data_list_r)
  })
}
