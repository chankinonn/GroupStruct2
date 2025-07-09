
mod_data_ui_meristic <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Meristic Data"),
    p("The first column should be Group/OTU names (e.g., species or population). Other traits should be in the 2nd column onwards"),
    p(strong("Missing values and singletons are not allowed.")),
    p("A preview of the data will be shown as soon as it is uploaded."),
    fileInput(ns("file"), "Upload file (.csv, .tsv, or .txt)", accept = c(".csv", ".tsv", ".txt")),
    actionButton(ns("load_example"), "Load Example Meristic Dataset"),
    uiOutput(ns("upload_status_message")),
    br(),
    p("Note on the example dataset: This example dataset contains 11 meristic traits from four species of lizard."),
    p("Additional details on this dataset can be found at: Grismer et al. (2022). Phylogenetic and multivariate analyses of Gekko smithii Gray, 1842 recover a new species from Peninsular Malaysia and support the resurrection of G. albomaculatus (Giebel, 1861) from Sumatra. Vertebrate Zoology, 72, 47–80. https://doi.org/10.3897/vz.72.e77702)"), 
    hr(),
    h4("Outlier Detection"),
    p("The Boxplot IQR method is used to detect values exceeding 1.5×IQR within each OTU. Useful when comparing across species/populations with heterogeneous distributions. Requires ≥4 samples per group to work well."),
    p(strong("Outliers will be flagged but NOT REMOVED. It is up to the user to determine what to do with them."), style = "color: red;"),
    
    actionButton(ns("detect_outliers"), "Detect Outliers"),
    verbatimTextOutput(ns("outlier_report")),
    hr(),
    h4("Data Preview"),
    DTOutput(ns("preview")),
    hr(),
  )
}


mod_data_server_meristic <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)
    
    # Initialize upload status message as empty UI
    output$upload_status_message <- renderUI({ NULL })
    
    observeEvent(input$file, {
      req(input$file)
      
      file_path <- input$file$datapath
      
      df <- tryCatch({
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") {
          read.csv(file_path, stringsAsFactors = FALSE)
        } else if (ext %in% c("tsv", "txt")) {
          read.delim(file_path, stringsAsFactors = FALSE)
        } else {
          output$upload_status_message <- renderUI({
            tags$div(class = "alert alert-danger",
                     "Error: Unsupported file type. Please upload a .csv, .tsv, or .txt file.")
          })
          return(NULL)
        }
      }, error = function(e) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste0("Error reading file: ", e$message))
        })
        return(NULL)
      })
      
      req(df)
      
      if (ncol(df) < 2) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger",
                   "Error: Meristic data must have at least two columns (OTU names + at least one trait).")
        })
        data(NULL)
        return()
      }
      
      df[[1]] <- as.character(df[[1]])
      df[[1]] <- factor(df[[1]])
      
      trait_cols_to_check <- df[, 2:ncol(df), drop = FALSE]
      all_valid <- TRUE
      
      for (i in seq_along(trait_cols_to_check)) {
        col_name <- names(trait_cols_to_check)[i]
        col_values <- trait_cols_to_check[[i]]
        
        if (any(is.na(col_values))) {
          output$upload_status_message <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: Trait column '", col_name,
                            "' contains missing values (NA). Missing values are not allowed for meristic data."))
          })
          all_valid <- FALSE
          break
        }
        
        if (!is.numeric(col_values)) {
          output$upload_status_message <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: Trait column '", col_name,
                            "' is not numeric. All trait columns must be numeric for meristic data."))
          })
          all_valid <- FALSE
          break
        }
      }
      
      if (!all_valid) {
        data(NULL)
        return()
      }
      
      # Passed all checks
      data(df)
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success",
                 "File uploaded and validated successfully. Proceed to the next module.")
      })
    })
    
    # Load Example Data 
    observeEvent(input$load_example, {
      example_path <- system.file("examples", "Meristic-only.csv", package = "GroupStruct2") 
      req(file.exists(example_path))
      
      df <- tryCatch({
        read.csv(example_path, stringsAsFactors = FALSE)
      }, error = function(e) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste0("Error loading example data: ", e$message))
        })
        return(NULL)
      })
      
      req(df)
      
      if (ncol(df) < 2) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger",
                   "Error: Meristic data must have at least two columns (OTU names + at least one trait).")
        })
        data(NULL)
        return()
      }
      
      df[[1]] <- as.character(df[[1]])
      df[[1]] <- factor(df[[1]])
      
      trait_cols_to_check <- df[, 2:ncol(df), drop = FALSE]
      all_valid <- TRUE
      
      for (i in seq_along(trait_cols_to_check)) {
        col_name <- names(trait_cols_to_check)[i]
        col_values <- trait_cols_to_check[[i]]
        
        if (any(is.na(col_values))) {
          output$upload_status_message <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: Trait column '", col_name,
                            "' contains missing values (NA). Missing values are not allowed for meristic data."))
          })
          all_valid <- FALSE
          break
        }
        
        if (!is.numeric(col_values)) {
          output$upload_status_message <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: Trait column '", col_name,
                            "' is not numeric. All trait columns must be numeric for meristic data."))
          })
          all_valid <- FALSE
          break
        }
      }
      
      if (!all_valid) {
        data(NULL)
        return()
      }
      
      data(df)
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success",
                 "Example dataset loaded successfully.")
      })
    })
    
    observeEvent(input$detect_outliers, {
      req(data())
      df <- data()
      otu_col <- df[[1]]
      trait_data <- df[, 2:ncol(df), drop = FALSE]
      method <- "iqr"
      
      flagged <- list()
      skipped_otus <- c()
      
      for (trait in names(trait_data)) {
        vals <- trait_data[[trait]]
        
        if (method == "iqr") {
          flagged[[trait]] <- unlist(lapply(split(seq_len(nrow(df)), otu_col), function(rows) {
            x <- vals[rows]
            if (length(x) < 4) {
              skipped_otus <<- unique(c(skipped_otus, as.character(otu_col[rows[1]])))
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
    
    output$preview <- renderDT({
      req(data())
      datatable(
        data(),
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          dom = 'tip'
        )
      )
    })
    
    output$summary_text <- renderPrint({
      req(data())
      df <- data()
      n_otus <- length(unique(df[[1]]))
      sample_size_per_otu <- table(df[[1]])
      n_traits <- ncol(df) - 1
      
      cat("Number of OTUs:", n_otus, "\n\n")
      cat("Sample size per OTU:\n")
      print(sample_size_per_otu)
      cat("\nNumber of Traits:", n_traits, "\n")
    })
    
    return(data)
  })
}
