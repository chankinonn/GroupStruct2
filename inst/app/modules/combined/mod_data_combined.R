
mod_data_combined_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Mixed Data (Meristic, Morphological, Categorical)"),
    hr(),
    
    tags$div(
      style = "background-color: #d1ecf1; border-left: 5px solid #17a2b8; padding: 12px; margin-bottom: 20px;",
      p(style = "margin: 0;",
        strong("File format:"),
        "Upload a CSV, TSV, or TXT file with one row per specimen.",
        "After uploading, select which column contains your OTU/group labels and",
        "(optionally) which column contains specimen identifiers such as catalog numbers.",
        "All remaining columns are treated as traits.")
    ),
    
    tags$div(
      style = "background-color: #e9ecef; border-left: 5px solid #6c757d; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Upload Your Data"),
      p("A preview of the data will be shown as soon as it is uploaded."),
      p(strong("Missing values and singletons are not allowed for numeric traits.")),
      fileInput(ns("file_upload"), "Upload file (.csv, .tsv, or .txt)", accept = c(".csv", ".tsv", ".txt")),
      actionButton(ns("load_example_1"), "Load Example 1: Meristic + Morphometric Only"),
      actionButton(ns("load_example_2"), "Load Example 2: Meristic + Morphometric + Categorical Dataset"),
      br(), br(),
      uiOutput(ns("column_selector_ui")),
      uiOutput(ns("upload_status_message"))
    ),
    
    br(),
    p(strong("The example datasets included in this package are for practice purposes only and are not meant to inform taxonomic changes.")),
    p("Note on the example dataset: The second column (SVL) represents body size. Columns 3 (HH) to 16 (TW) are morphometric traits and should be size corrected. Columns 17 (SL) to 27 (TL4) are meristic traits and should not be size corrected. Columns 28 (iris color) to 32 (drk on body) are categorical traits."),
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
    raw_df_r                   <- reactiveVal(NULL)   # holds the unprocessed uploaded df
    
    # -------------------------------------------------------------------------
    # Internal helper: parse with explicit column choices
    # group_col: column name for OTU/group (required)
    # id_col:    column name for specimen ID (NULL = none, auto-assign integers)
    # -------------------------------------------------------------------------
    parse_combined_df <- function(df, group_col, id_col = NULL, success_msg) {
      
      if (ncol(df) < 2) {
        return(list(ok = FALSE,
                    message = "Error: Data must have at least two columns."))
      }
      
      # Extract specimen IDs
      if (!is.null(id_col) && id_col %in% names(df)) {
        ids_out <- trimws(as.character(df[[id_col]]))
        df      <- df[, !names(df) %in% id_col, drop = FALSE]
      } else {
        ids_out <- as.character(seq_len(nrow(df)))
      }
      
      if (!group_col %in% names(df)) {
        return(list(ok = FALSE,
                    message = paste0("Error: OTU column '", group_col, "' not found after removing ID column.")))
      }
      
      if (ncol(df) < 2) {
        return(list(ok = FALSE,
                    message = "Error: After removing the specimen ID column, at least one trait column is required."))
      }
      
      if (all(is.na(df[[group_col]]) | trimws(as.character(df[[group_col]])) == "")) {
        return(list(ok = FALSE,
                    message = paste0("Error: OTU column '", group_col, "' is empty or contains only missing values.")))
      }
      
      df[[group_col]] <- as.factor(trimws(as.character(df[[group_col]])))
      
      trait_cols             <- setdiff(names(df), group_col)
      categorical_cols_found <- c()
      
      for (col_name in trait_cols) {
        if (any(is.na(df[[col_name]]))) {
          showNotification(
            paste0("Warning: '", col_name, "' contains missing values. Handle in downstream modules."),
            type = "warning", duration = 8)
        }
        if (!is.numeric(df[[col_name]])) {
          df[[col_name]] <- as.factor(df[[col_name]])
          categorical_cols_found <- c(categorical_cols_found, col_name)
        }
      }
      
      # Re-order so group_col is first
      df <- df[, c(group_col, trait_cols), drop = FALSE]
      
      data_list  <- list(data = df, group_col = group_col, categorical_cols = categorical_cols_found)
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
    # Column selector UI — appears once a raw df is loaded
    # -------------------------------------------------------------------------
    output$column_selector_ui <- renderUI({
      df <- raw_df_r()
      if (is.null(df)) return(NULL)
      
      col_names <- names(df)
      
      # Guess sensible defaults: OTU = first col with repeated values; ID = first all-unique col
      guess_otu <- col_names[which(vapply(col_names, function(cn) {
        anyDuplicated(trimws(as.character(df[[cn]]))) > 0
      }, logical(1)))[1]]
      if (is.na(guess_otu)) guess_otu <- col_names[1]
      
      guess_id_candidates <- col_names[vapply(col_names, function(cn) {
        !anyDuplicated(trimws(as.character(df[[cn]])))
      }, logical(1))]
      guess_id <- if (length(guess_id_candidates) > 0) guess_id_candidates[1] else "None"
      
      tagList(
        fluidRow(
          column(4,
                 selectInput(ns("col_otu"), "OTU / Group column:",
                             choices  = col_names,
                             selected = guess_otu,
                             width    = "100%")
          ),
          column(4,
                 selectInput(ns("col_id"), "Specimen ID column (optional):",
                             choices  = c("None", col_names),
                             selected = guess_id,
                             width    = "100%")
          ),
          column(4,
                 br(),
                 actionButton(ns("apply_cols"), "Apply",
                              icon  = icon("check"),
                              class = "btn-primary",
                              style = "margin-top: 4px;")
          )
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Apply column selection
    # -------------------------------------------------------------------------
    observeEvent(input$apply_cols, {
      df <- raw_df_r()
      req(df, input$col_otu)
      id_col <- if (!is.null(input$col_id) && input$col_id != "None") input$col_id else NULL
      if (!is.null(id_col) && id_col == input$col_otu) {
        showNotification("OTU column and Specimen ID column cannot be the same.", type = "error")
        return()
      }
      result <- parse_combined_df(df, group_col = input$col_otu, id_col = id_col,
                                  success_msg  = "Data loaded successfully. Proceed to the next module.")
      apply_result(result)
    })
    
    # -------------------------------------------------------------------------
    # File upload — stores raw df, then waits for column selection
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
      raw_df_r(as.data.frame(df_raw))
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-info",
                 "File read. Select the OTU and (optionally) Specimen ID columns above, then click Apply.")
      })
    }, ignoreNULL = FALSE)
    
    # -------------------------------------------------------------------------
    # Example loaders — auto-apply using guessed columns
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
      if (is.null(df_raw)) return()
      raw_df_r(df_raw)
      # For examples, auto-detect: first repeated col = OTU, first all-unique non-OTU col = ID
      col_names <- names(df_raw)
      otu_col <- col_names[which(vapply(col_names, function(cn) {
        anyDuplicated(trimws(as.character(df_raw[[cn]]))) > 0
      }, logical(1)))[1]]
      if (is.na(otu_col)) otu_col <- col_names[1]
      result <- parse_combined_df(df_raw, group_col = otu_col, id_col = NULL, success_msg = msg)
      apply_result(result)
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
