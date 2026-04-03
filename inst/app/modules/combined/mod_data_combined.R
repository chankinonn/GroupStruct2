
mod_data_combined_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Mixed Data (Meristic, Morphological, Categorical)"),
    hr(),
    
    tags$div(
      style = "background-color: #d1ecf1; border-left: 5px solid #17a2b8; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Supported File Formats"),
      p("Two column layouts are accepted. The application will detect the format automatically."),
      tags$ul(
        tags$li(strong("Without specimen IDs:"), "Column 1 = OTU/group name, Column 2 onward = traits.",
                "Sequential integers will be assigned as specimen IDs automatically."),
        tags$li(strong("With specimen IDs:"), "Column 1 = Specimen ID (must be unique per row), Column 2 = OTU/group name, Column 3 onward = traits.")
      )
    ),
    
    tags$div(
      style = "background-color: #e9ecef; border-left: 5px solid #6c757d; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Upload Your Data"),
      p("A preview of the data will be shown as soon as it is uploaded."),
      p(strong("Missing values and singletons are not allowed for numeric traits.")),
      fileInput(ns("file_upload"), "Upload file (.csv, .tsv, or .txt)", accept = c(".csv", ".tsv", ".txt")),
      actionButton(ns("load_example_1"), "Load Example 1: Meristic + Morphometric Only"),
      actionButton(ns("load_example_2"), "Load Example 2: Meristic + Morphometric + Categorical Dataset"),
      uiOutput(ns("upload_status_message"))
    ),
    
    br(),
    p(strong("The example datasets included in this package are for practice purposes only and are not meant to inform taxonomic changes.")),
    p("Note on the example dataset: The second column (SVL) represents body size. Columns 3 (HH) to 16 (TW) are morphometric traits and should be size corrected. Columns 17 (SL) to 27 (TL4) are meristic traits and should not be size corrected. Columns 28 (iris color) to 32 (drk on body) are categorical traits."),
    p("Additional details on this dataset can be found at: Grismer et al. (2022). Phylogenetic and multivariate analyses of Gekko smithii Gray, 1842 recover a new species from Peninsular Malaysia and support the resurrection of G. albomaculatus (Giebel, 1861) from Sumatra. Vertebrate Zoology, 72, 47\u201380. https://doi.org/10.3897/vz.72.e77702)"),
    hr(),
    h4("Outlier Detection"),
    p("The Boxplot Interquartile Range (IQR) method is used to detect values exceeding 1.5\u00d7IQR within each OTU. Requires \u22654 samples per group to work well."),
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
    specimen_ids_r             <- reactiveVal(NULL)
    full_display_data_r        <- reactiveVal(NULL)
    
    # -------------------------------------------------------------------------
    # Internal helper: parse a raw data frame into the combined data structure
    # Auto-detects specimen ID column by uniqueness of column 1
    # -------------------------------------------------------------------------
    parse_combined_df <- function(df, success_msg) {
      
      if (ncol(df) < 2) {
        return(list(ok = FALSE,
                    message = "Error: Data must have at least two columns (Group/OTU and at least one trait)."))
      }
      
      # Auto-detect specimen IDs
      col1_vals <- trimws(as.character(df[[1]]))
      has_id    <- !anyDuplicated(col1_vals)
      
      if (has_id) {
        ids_out <- col1_vals
        df      <- df[, -1, drop = FALSE]
      } else {
        ids_out <- as.character(seq_len(nrow(df)))
      }
      
      if (ncol(df) < 2) {
        return(list(ok = FALSE,
                    message = "Error: After removing the specimen ID column, data must have at least two columns (Group/OTU and at least one trait)."))
      }
      
      group_col_name <- names(df)[1]
      
      if (all(is.na(df[[group_col_name]]) | df[[group_col_name]] == "")) {
        return(list(ok = FALSE,
                    message = paste0("Error: The OTU/group column ('", group_col_name, "') cannot be empty or contain only missing values.")))
      }
      
      df[[group_col_name]] <- as.factor(trimws(as.character(df[[group_col_name]])))
      
      # Validate and coerce trait columns
      trait_cols <- names(df)[-1]
      categorical_cols_found <- c()
      
      for (col_name in trait_cols) {
        if (any(is.na(df[[col_name]]))) {
          showNotification(
            paste0("Warning: Trait column '", col_name, "' contains missing values (NA). These will need to be handled in downstream modules."),
            type = "warning", duration = 8)
        }
        if (!is.numeric(df[[col_name]])) {
          df[[col_name]] <- as.factor(df[[col_name]])
          categorical_cols_found <- c(categorical_cols_found, col_name)
        }
      }
      
      data_list  <- list(data = df, group_col = group_col_name, categorical_cols = categorical_cols_found)
      display_df <- cbind(data.frame(SpecimenID = ids_out, stringsAsFactors = FALSE), df)
      
      list(ok           = TRUE,
           data_list    = data_list,
           specimen_ids = ids_out,
           full_data    = display_df,
           message      = success_msg)
    }
    
    # -------------------------------------------------------------------------
    # Apply parse result to reactiveVals and update status message
    # -------------------------------------------------------------------------
    apply_result <- function(result) {
      if (!result$ok) {
        showNotification(result$message, type = "error", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", result$message)
        })
        processed_combined_data_r(NULL)
        specimen_ids_r(NULL)
        full_display_data_r(NULL)
        return()
      }
      processed_combined_data_r(result$data_list)
      specimen_ids_r(result$specimen_ids)
      full_display_data_r(result$full_data)
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success", result$message)
      })
    }
    
    # -------------------------------------------------------------------------
    # File upload
    # -------------------------------------------------------------------------
    observeEvent(input$file_upload, {
      req(input$file_upload)
      
      df_raw <- tryCatch({
        ext <- tolower(tools::file_ext(input$file_upload$name))
        if (ext == "csv") {
          readr::read_csv(input$file_upload$datapath, show_col_types = FALSE)
        } else if (ext %in% c("tsv", "txt")) {
          readr::read_tsv(input$file_upload$datapath, show_col_types = FALSE)
        } else {
          stop("Unsupported file type. Please upload a .csv, .tsv, or .txt file.")
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error", duration = 8)
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", paste("File reading error:", e$message))
        })
        return(NULL)
      })
      
      if (is.null(df_raw)) return()
      apply_result(parse_combined_df(as.data.frame(df_raw),
                                     "File uploaded and validated successfully. Proceed to the next module."))
    }, ignoreNULL = FALSE)
    
    # -------------------------------------------------------------------------
    # Example loaders
    # -------------------------------------------------------------------------
    load_example <- function(filename, msg) {
      file_path <- system.file("examples", filename, package = "GroupStruct2")
      if (!file.exists(file_path)) {
        showNotification(paste("Example file not found:", filename), type = "error")
        return()
      }
      df_raw <- tryCatch(
        as.data.frame(readr::read_csv(file_path, show_col_types = FALSE)),
        error = function(e) {
          showNotification(paste("Error loading example:", e$message), type = "error")
          return(NULL)
        })
      if (!is.null(df_raw)) apply_result(parse_combined_df(df_raw, msg))
    }
    
    observeEvent(input$load_example_1, {
      load_example("Meristic-Morphometric.csv", "Example 1 loaded: Meristic + Morphometric dataset.")
    })
    observeEvent(input$load_example_2, {
      load_example("Meristic-Morphometric-Categorical.csv", "Example 2 loaded: Meristic + Morphometric + Categorical dataset.")
    })
    
    # -------------------------------------------------------------------------
    # Outlier detection
    # -------------------------------------------------------------------------
    observeEvent(input$detect_outliers, {
      req(processed_combined_data_r())
      df        <- processed_combined_data_r()$data
      group_col <- processed_combined_data_r()$group_col
      ids       <- specimen_ids_r()
      
      numeric_traits <- df[, sapply(df, is.numeric), drop = FALSE]
      flagged        <- list()
      skipped_otus   <- c()
      
      for (trait in names(numeric_traits)) {
        vals <- numeric_traits[[trait]]
        flagged[[trait]] <- unlist(lapply(
          split(seq_len(nrow(df)), df[[group_col]]),
          function(rows) {
            x <- vals[rows]
            if (length(x) < 4) {
              skipped_otus <<- unique(c(skipped_otus, as.character(df[[group_col]][rows[1]])))
              return(integer(0))
            }
            q   <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
            iqr <- q[2] - q[1]
            rows[x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr)]
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
      
      if (!any(vapply(flagged, length, FUN.VALUE = integer(1)) > 0)) {
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
    # Preview — shows full data including SpecimenID column
    # -------------------------------------------------------------------------
    output$data_preview <- renderDT({
      req(full_display_data_r())
      datatable(full_display_data_r(),
                options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100),
                               scrollX = TRUE, dom = "tip"))
    })
    
    return(list(data_list = processed_combined_data_r, specimen_ids = specimen_ids_r))
  })
}
