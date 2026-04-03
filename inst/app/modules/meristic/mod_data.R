
mod_data_ui_meristic <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Meristic Data"),
    hr(),
    
    # Informational note about supported formats — no user action required
    tags$div(
      style = "background-color: #d1ecf1; border-left: 5px solid #17a2b8; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Supported File Formats"),
      p("Two column layouts are accepted. The application will detect the format automatically."),
      tags$ul(
        tags$li(strong("Without specimen IDs:"), "Column 1 = OTU/group name, Column 2 onward = traits.",
                "Specimen IDs will be assigned automatically as sequential integers (1, 2, 3, ...)."),
        tags$li(strong("With specimen IDs:"), "Column 1 = Specimen ID (must be unique per row), Column 2 = OTU/group name, Column 3 onward = traits.")
      )
    ),
    
    # Upload
    tags$div(
      style = "background-color: #e9ecef; border-left: 5px solid #6c757d; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Upload Your Data"),
      p("A preview of the data will be shown as soon as it is uploaded."),
      p(strong("Missing values and singletons are not allowed.")),
      fileInput(ns("file"), "Upload file (.csv, .tsv, or .txt)", accept = c(".csv", ".tsv", ".txt")),
      actionButton(ns("load_example"), "Load Example Meristic Dataset"),
      uiOutput(ns("upload_status_message"))
    ),
    
    br(),
    p(strong("The example datasets included in this package are for practice purposes only and are not meant to inform taxonomic changes.")),
    p("Note on the example dataset: This example dataset contains 11 meristic traits from four species of lizard."),
    p("Additional details on this dataset can be found at: Grismer et al. (2022). Phylogenetic and multivariate analyses of Gekko smithii Gray, 1842 recover a new species from Peninsular Malaysia and support the resurrection of G. albomaculatus (Giebel, 1861) from Sumatra. Vertebrate Zoology, 72, 47\u201380. https://doi.org/10.3897/vz.72.e77702)"),
    hr(),
    h4("Outlier Detection"),
    p("The Boxplot Interquartile Range (IQR) method flags values that fall beyond a user-defined multiplier of the IQR within each OTU. A multiplier of 3.0 is recommended for morphological data. Requires ≥4 samples per group to work well."),
    sliderInput(ns("iqr_multiplier"), "IQR Multiplier:", min = 1.5, max = 5.0, value = 3.0, step = 0.5, width = "250px"),
    p(strong("Outliers will be flagged but NOT REMOVED. It is up to the user to determine what to do with them."), style = "color: red;"),
    actionButton(ns("detect_outliers"), "Detect Outliers", icon = icon("play"), class = "btn-primary"),
    br(),
    verbatimTextOutput(ns("outlier_report")),
    hr(),
    h4("Data Preview"),
    DTOutput(ns("preview")),
    hr()
  )
}


mod_data_server_meristic <- function(id) {
  moduleServer(id, function(input, output, session) {
    data         <- reactiveVal(NULL)  # OTU + traits only (analysis-ready)
    specimen_ids <- reactiveVal(NULL)  # character vector or NULL
    full_data    <- reactiveVal(NULL)  # full data including ID col, for display only
    
    output$upload_status_message <- renderUI({ NULL })
    
    # -------------------------------------------------------------------------
    # Internal helper: validate and parse a raw data frame
    # Returns a list: list(ok, data, specimen_ids, full_data, message)
    # -------------------------------------------------------------------------
    parse_meristic_df <- function(df) {
      
      # Auto-detect format: all-unique col 1 values mean specimen IDs are present.
      # An OTU column with all unique values would imply all singletons, which is
      # already disallowed, so this detection is unambiguous.
      col1_vals <- trimws(as.character(df[[1]]))
      has_id    <- !anyDuplicated(col1_vals)
      
      min_cols  <- if (has_id) 3L else 2L
      id_label  <- if (has_id) " (specimen ID + OTU + at least one trait)" else " (OTU + at least one trait)"
      
      if (ncol(df) < min_cols) {
        return(list(ok = FALSE,
                    message = paste0("Error: Meristic data must have at least ", min_cols,
                                     " columns", id_label, ".")))
      }
      
      # Extract or auto-generate specimen IDs
      # When present: use the values from col 1 (uniqueness already confirmed above)
      # When absent:  assign sequential integers so IDs are always available downstream
      if (has_id) {
        ids_out <- col1_vals
        df      <- df[, -1, drop = FALSE]  # drop ID col; OTU is now col 1
      } else {
        ids_out <- as.character(seq_len(nrow(df)))
      }
      
      # Standardise OTU column (always col 1 after possible strip above)
      df[[1]] <- factor(trimws(as.character(df[[1]])))
      
      # Validate trait columns (col 2 onward)
      trait_cols <- df[, 2:ncol(df), drop = FALSE]
      
      for (i in seq_along(trait_cols)) {
        col_name <- names(trait_cols)[i]
        col_vals <- trait_cols[[i]]
        
        if (any(is.na(col_vals))) {
          return(list(ok = FALSE,
                      message = paste0("Error: Trait column '", col_name,
                                       "' contains missing values (NA). Missing values are not allowed.")))
        }
        if (!is.numeric(col_vals)) {
          return(list(ok = FALSE,
                      message = paste0("Error: Trait column '", col_name,
                                       "' is not numeric. All trait columns must be numeric for meristic data.")))
        }
      }
      
      # Reconstruct full display data — SpecimenID column always present
      display_df <- cbind(data.frame(SpecimenID = ids_out, stringsAsFactors = FALSE), df)
      
      list(ok           = TRUE,
           data         = df,
           specimen_ids = ids_out,
           full_data    = display_df,
           message      = NULL)
    }
    
    # -------------------------------------------------------------------------
    # File upload
    # -------------------------------------------------------------------------
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
      
      result <- parse_meristic_df(df)
      
      if (!result$ok) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", result$message)
        })
        data(NULL)
        specimen_ids(NULL)
        full_data(NULL)
        return()
      }
      
      data(result$data)
      specimen_ids(result$specimen_ids)
      full_data(result$full_data)
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success",
                 "File uploaded and validated successfully. Proceed to the next module.")
      })
    })
    
    # -------------------------------------------------------------------------
    # Load example dataset (no specimen IDs; format selection is ignored)
    # -------------------------------------------------------------------------
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
      
      # Example files never carry specimen IDs
      result <- parse_meristic_df(df)
      
      if (!result$ok) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", result$message)
        })
        data(NULL)
        specimen_ids(NULL)
        full_data(NULL)
        return()
      }
      
      data(result$data)
      specimen_ids(result$specimen_ids)
      full_data(result$full_data)
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success",
                 "Example dataset loaded successfully.")
      })
    })
    
    # -------------------------------------------------------------------------
    # Outlier detection
    # -------------------------------------------------------------------------
    observeEvent(input$detect_outliers, {
      req(data())
      df       <- data()
      otu_col  <- df[[1]]
      trait_data <- df[, 2:ncol(df), drop = FALSE]
      ids      <- specimen_ids()
      
      flagged      <- list()
      skipped_otus <- c()
      
      for (trait in names(trait_data)) {
        vals <- trait_data[[trait]]
        
        flagged[[trait]] <- unlist(lapply(
          split(seq_len(nrow(df)), otu_col),
          function(rows) {
            x <- vals[rows]
            if (length(x) < 4) {
              skipped_otus <<- unique(c(skipped_otus, as.character(otu_col[rows[1]])))
              return(integer(0))
            }
            q   <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
            iqr <- q[2] - q[1]
            rows[x < (q[1] - input$iqr_multiplier * iqr) | x > (q[2] + input$iqr_multiplier * iqr)]
          }
        ))
      }
      
      report <- ""
      for (trait in names(flagged)) {
        inds <- flagged[[trait]]
        if (length(inds) > 0) {
          report <- paste0(report, "\nTrait: ", trait, "\n")
          report <- paste0(report, "Specimen ID(s): ", paste(ids[inds], collapse = ", "), "\n")
        }
      }
      
      any_flagged <- any(vapply(flagged, length, FUN.VALUE = integer(1)) > 0)
      
      if (!any_flagged) {
        report <- "No outliers detected with selected method."
      } else {
        report <- paste0(report, "\n\nConsider re-inspecting your data.")
      }
      
      if (length(skipped_otus) > 0) {
        report <- paste0(report,
                         "\n\nNote: The following OTUs had fewer than 4 samples and were skipped:\n",
                         paste(skipped_otus, collapse = ", "))
      }
      
      output$outlier_report <- renderText({ report })
    })
    
    # -------------------------------------------------------------------------
    # Data preview (shows full data including specimen ID column if present)
    # -------------------------------------------------------------------------
    output$preview <- renderDT({
      req(full_data())
      datatable(
        full_data(),
        options = list(
          pageLength  = 10,
          lengthMenu  = c(10, 25, 50, 100),
          scrollX     = TRUE,
          dom         = "tip"
        )
      )
    })
    
    output$summary_text <- renderPrint({
      req(data())
      df <- data()
      n_otus              <- length(unique(df[[1]]))
      sample_size_per_otu <- table(df[[1]])
      n_traits            <- ncol(df) - 1
      
      cat("Number of OTUs:", n_otus, "\n\n")
      cat("Sample size per OTU:\n")
      print(sample_size_per_otu)
      cat("\nNumber of Traits:", n_traits, "\n")
    })
    
    # Return both reactives so downstream modules can access specimen IDs
    return(list(data = data, specimen_ids = specimen_ids))
  })
}
