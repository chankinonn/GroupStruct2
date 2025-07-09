
mod_data_combined_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Mixed Data (Meristic, Morphological, Categorical)"),
    p("The first column should be Group/OTU names (e.g., species or population)."),
    p(strong("Missing values and singletons are not allowed.")),
    p("A preview of the data will be shown as soon as it is uploaded."),
    fileInput(ns("file_upload"), "Upload file (.csv, .tsv, or .txt)", accept = c(".csv", ".tsv", ".txt")),
    actionButton(ns("load_example"), "Load Example Meristic + Morphometric + Categorical Dataset"),
    uiOutput(ns("upload_status_message")),
    br(),
    p("Note on the example dataset: The second colum (SVL) represents body size. Colums 3 (HH) to 16 (TW) are morphometric traits and should be size corrected. Colums 17 (SL) to 27 (TL4) are meristic traits and should not be size corrected. Colums 28 (iris color) to 32 (drk on body) are categorical traits."),
    p("Additional details on this dataset can be found at: Grismer et al. (2022). Phylogenetic and multivariate analyses of Gekko smithii Gray, 1842 recover a new species from Peninsular Malaysia and support the resurrection of G. albomaculatus (Giebel, 1861) from Sumatra. Vertebrate Zoology, 72, 47–80. https://doi.org/10.3897/vz.72.e77702)"), 
    hr(),
    h4("Outlier Detection"),
    p("The Boxplot IQR method is used to detect values exceeding 1.5×IQR within each OTU. Useful when comparing across species/populations with heterogeneous distributions. Requires ≥4 samples per group to work well."),
    p(strong("Outliers will be flagged but NOT REMOVED. It is up to the user to determine what to do with them."), style = "color: red;"),
    actionButton(ns("detect_outliers"), "Detect Outliers"),
    verbatimTextOutput(ns("outlier_report")),
    hr(),
    h4("Data Preview"),
    DTOutput(ns("data_preview")),
    hr()
  )
}


mod_data_combined_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    processed_combined_data_r <- reactiveVal(NULL)
    
    observeEvent(input$file_upload, {
      req(input$file_upload)
      
      file_path <- input$file_upload$datapath
      file_name <- input$file_upload$name
      
      df_raw <- tryCatch({
        ext <- tolower(tools::file_ext(file_name))
        if (ext == "csv") {
          readr::read_csv(file_path, show_col_types = FALSE)
        } else if (ext == "tsv") {
          readr::read_tsv(file_path, show_col_types = FALSE)
        } else {
          stop("Unsupported file type. Please upload a .csv or .tsv file.")
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", paste("File reading error: ", e$message))
        })
        return(NULL)
      })
      
      if (is.null(df_raw)) {
        processed_combined_data_r(NULL)
        return()
      }
      
      if (ncol(df_raw) < 2) {
        msg <- "Error: Data must have at least two columns (Group/OTU and at least one trait)."
        showNotification(msg, type = "error", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", msg)
        })
        processed_combined_data_r(NULL)
        return()
      }
      
      group_col_name <- names(df_raw)[1]
      
      if (all(is.na(df_raw[[group_col_name]]) | df_raw[[group_col_name]] == "")) {
        msg <- paste0("Error: The first column ('", group_col_name, "', Group/OTU) cannot be empty or contain only missing values.")
        showNotification(msg, type = "error", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", msg)
        })
        processed_combined_data_r(NULL)
        return()
      }
      
      df_raw[[group_col_name]] <- as.factor(df_raw[[group_col_name]])
      
      trait_cols <- names(df_raw)[-1]
      categorical_cols_found <- c()
      
      if (length(trait_cols) > 0) {
        for (col_name in trait_cols) {
          if (any(is.na(df_raw[[col_name]]))) {
            showNotification(paste0("Warning: Trait column '", col_name, "' contains missing values (NA). These will need to be handled in downstream modules like Allometric Correction or MFA."), type = "warning", duration = 8)
          }
          
          if (!is.numeric(df_raw[[col_name]])) {
            df_raw[[col_name]] <- as.factor(df_raw[[col_name]])
            categorical_cols_found <- c(categorical_cols_found, col_name)
          }
        }
      } else {
        msg <- "Warning: No trait columns detected beyond Group/OTU. You may need to add more columns for meaningful analysis."
        showNotification(msg, type = "warning", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-warning", msg)
        })
      }
      
      processed_combined_data_r(list(
        data = df_raw,
        group_col = group_col_name,
        categorical_cols = categorical_cols_found
      ))
      
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success", "File uploaded and validated successfully. Proceed to the next module.")
      })
    }, ignoreNULL = FALSE)
    
    observeEvent(input$load_example, {
      example_path <- system.file("examples", "Meristic-Morphometric-Categorical.csv", package = "GroupStruct2")
      req(file.exists(example_path))
      
      df_raw <- tryCatch({
        readr::read_csv(example_path, show_col_types = FALSE)
      }, error = function(e) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste("Error loading example data: ", e$message))
        })
        return(NULL)
      })
      
      if (is.null(df_raw) || ncol(df_raw) < 2) {
        msg <- "Error: Example data must have at least two columns (Group/OTU and at least one trait)."
        showNotification(msg, type = "error", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", msg)
        })
        processed_combined_data_r(NULL)
        return()
      }
      
      group_col_name <- names(df_raw)[1]
      
      if (all(is.na(df_raw[[group_col_name]]) | df_raw[[group_col_name]] == "")) {
        msg <- paste0("Error: The first column ('", group_col_name, "', Group/OTU) cannot be empty or contain only missing values.")
        showNotification(msg, type = "error", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", msg)
        })
        processed_combined_data_r(NULL)
        return()
      }
      
      df_raw[[group_col_name]] <- as.factor(df_raw[[group_col_name]])
      
      trait_cols <- names(df_raw)[-1]
      categorical_cols_found <- c()
      
      if (length(trait_cols) > 0) {
        for (col_name in trait_cols) {
          if (any(is.na(df_raw[[col_name]]))) {
            showNotification(paste0("Warning: Trait column '", col_name, "' contains missing values (NA). These will need to be handled in downstream modules like Allometric Correction or MFA."), type = "warning", duration = 8)
          }
          
          if (!is.numeric(df_raw[[col_name]])) {
            df_raw[[col_name]] <- as.factor(df_raw[[col_name]])
            categorical_cols_found <- c(categorical_cols_found, col_name)
          }
        }
      }
      
      processed_combined_data_r(list(
        data = df_raw,
        group_col = group_col_name,
        categorical_cols = categorical_cols_found
      ))
      
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success", "Example dataset loaded successfully.")
      })
    })
    
    
    observeEvent(input$detect_outliers, {
      req(processed_combined_data_r())
      df <- processed_combined_data_r()$data
      group_col <- processed_combined_data_r()$group_col
      method <- "iqr"  # hardcoded
      
      numeric_traits <- df[, sapply(df, is.numeric), drop = FALSE]
      flagged <- list()
      skipped_otus <- c()
      
      for (trait in names(numeric_traits)) {
        vals <- numeric_traits[[trait]]
        
        flagged[[trait]] <- unlist(lapply(split(seq_len(nrow(df)), df[[group_col]]), function(rows) {
          x <- vals[rows]
          if (length(x) < 4) {
            skipped_otus <<- unique(c(skipped_otus, as.character(df[[group_col]][rows[1]])))
            return(integer(0))
          }
          q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
          iqr <- q[2] - q[1]
          lower <- q[1] - 1.5 * iqr
          upper <- q[2] + 1.5 * iqr
          is_outlier <- x < lower | x > upper
          rows[is_outlier]
        }))
      }
      
      report <- ""
      for (trait in names(flagged)) {
        inds <- flagged[[trait]]
        if (length(inds) > 0) {
          report <- paste0(report, "\nTrait: ", trait, "\n")
          report <- paste0(report, "Row(s): ", paste(inds, collapse = ", "), "\n")
        }
      }
      
      any_flagged <- any(vapply(flagged, length, FUN.VALUE = integer(1)) > 0)
      
      if (!any_flagged) {
        report <- "No outliers detected with selected method."
      } else {
        report <- paste(report, "\n\nConsider re-inspecting your data.", sep = "")
      }
      
      if (length(skipped_otus) > 0) {
        report <- paste0(report,
                         "\n\nNote: The following OTUs had fewer than 4 samples and were skipped:\n",
                         paste(skipped_otus, collapse = ", "))
      }
      
      output$outlier_report <- renderText({ report })
    })
    
    output$data_preview <- renderDT({
      req(processed_combined_data_r()$data)
      datatable(processed_combined_data_r()$data,
                options = list(
                  pageLength = 10,
                  lengthMenu = c(10, 25, 50, 100),
                  scrollX = TRUE,
                  dom = 'tip'
                ))
    })
    
    return(processed_combined_data_r)
  })
}
