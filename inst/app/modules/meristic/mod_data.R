
mod_data_ui_meristic <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Meristic Data"),
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
    
    # Upload
    tags$div(
      style = "background-color: #e9ecef; border-left: 5px solid #6c757d; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Upload Your Data"),
      p("A preview of the data will be shown as soon as it is uploaded."),
      p(strong("Missing values and singletons are not allowed.")),
      fileInput(ns("file"), "Upload file (.csv, .tsv, or .txt)", accept = c(".csv", ".tsv", ".txt")),
      actionButton(ns("load_example"), "Load Example Meristic Dataset"),
      br(), br(),
      uiOutput(ns("column_selector_ui")),
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
    ns <- session$ns
    data         <- reactiveVal(NULL)
    specimen_ids <- reactiveVal(NULL)
    full_data    <- reactiveVal(NULL)
    raw_df_r     <- reactiveVal(NULL)
    
    output$upload_status_message <- renderUI({ NULL })
    
    # -------------------------------------------------------------------------
    # Internal helper: validate and parse with explicit column choices
    # -------------------------------------------------------------------------
    parse_meristic_df <- function(df, group_col, id_col = NULL) {
      
      if (!group_col %in% names(df))
        return(list(ok = FALSE, message = paste0("Error: OTU column '", group_col, "' not found.")))
      
      # Extract or auto-generate specimen IDs
      if (!is.null(id_col) && id_col %in% names(df)) {
        ids_out <- trimws(as.character(df[[id_col]]))
        df      <- df[, !names(df) %in% id_col, drop = FALSE]
      } else {
        ids_out <- as.character(seq_len(nrow(df)))
      }
      
      if (ncol(df) < 2)
        return(list(ok = FALSE, message = "Error: At least one trait column is required after removing the ID column."))
      
      if (all(is.na(df[[group_col]]) | trimws(as.character(df[[group_col]])) == ""))
        return(list(ok = FALSE, message = paste0("Error: OTU column '", group_col, "' is empty.")))
      
      df[[group_col]] <- factor(trimws(as.character(df[[group_col]])))
      trait_cols      <- setdiff(names(df), group_col)
      
      for (col_name in trait_cols) {
        col_vals <- df[[col_name]]
        if (any(is.na(col_vals)))
          return(list(ok = FALSE, message = paste0("Error: Trait column '", col_name,
                                                   "' contains missing values. Missing values are not allowed.")))
        if (!is.numeric(col_vals))
          return(list(ok = FALSE, message = paste0("Error: Trait column '", col_name,
                                                   "' is not numeric. All trait columns must be numeric for meristic data.")))
      }
      
      # Re-order so OTU is first
      df         <- df[, c(group_col, trait_cols), drop = FALSE]
      display_df <- cbind(data.frame(SpecimenID = ids_out, stringsAsFactors = FALSE), df)
      
      list(ok = TRUE, data = df, specimen_ids = ids_out, full_data = display_df, message = NULL)
    }
    
    # -------------------------------------------------------------------------
    # Apply a successful parse result
    # -------------------------------------------------------------------------
    apply_result <- function(result, success_msg) {
      if (!result$ok) {
        output$upload_status_message <- renderUI(tags$div(class = "alert alert-danger", result$message))
        data(NULL); specimen_ids(NULL); full_data(NULL)
        return()
      }
      data(result$data); specimen_ids(result$specimen_ids); full_data(result$full_data)
      output$upload_status_message <- renderUI(tags$div(class = "alert alert-success", success_msg))
    }
    
    # -------------------------------------------------------------------------
    # Column selector UI — appears once a raw df is loaded
    # -------------------------------------------------------------------------
    output$column_selector_ui <- renderUI({
      df <- raw_df_r()
      if (is.null(df)) return(NULL)
      col_names <- names(df)
      
      guess_otu <- col_names[which(vapply(col_names, function(cn)
        anyDuplicated(trimws(as.character(df[[cn]]))) > 0, logical(1)))[1]]
      if (is.na(guess_otu)) guess_otu <- col_names[1]
      
      unique_cols <- col_names[vapply(col_names, function(cn)
        !anyDuplicated(trimws(as.character(df[[cn]]))), logical(1))]
      guess_id <- if (length(unique_cols) > 0) unique_cols[1] else "None"
      
      tagList(
        fluidRow(
          column(4, selectInput(ns("col_otu"), "OTU / Group column:",
                                choices = col_names, selected = guess_otu, width = "100%")),
          column(4, selectInput(ns("col_id"),  "Specimen ID column (optional):",
                                choices = c("None", col_names), selected = guess_id, width = "100%")),
          column(4, br(), actionButton(ns("apply_cols"), "Apply",
                                       icon = icon("check"), class = "btn-primary",
                                       style = "margin-top: 4px;"))
        )
      )
    })
    
    observeEvent(input$apply_cols, {
      df <- raw_df_r(); req(df, input$col_otu)
      id_col <- if (!is.null(input$col_id) && input$col_id != "None") input$col_id else NULL
      if (!is.null(id_col) && id_col == input$col_otu) {
        showNotification("OTU and Specimen ID columns cannot be the same.", type = "error"); return()
      }
      apply_result(parse_meristic_df(df, group_col = input$col_otu, id_col = id_col),
                   "Data loaded successfully. Proceed to the next module.")
    })
    
    # -------------------------------------------------------------------------
    # File upload — stores raw df, waits for column selection
    # -------------------------------------------------------------------------
    observeEvent(input$file, {
      req(input$file)
      df <- tryCatch({
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") read.csv(input$file$datapath, stringsAsFactors = FALSE)
        else if (ext %in% c("tsv", "txt")) read.delim(input$file$datapath, stringsAsFactors = FALSE)
        else stop("Unsupported file type. Please upload a .csv, .tsv, or .txt file.")
      }, error = function(e) {
        output$upload_status_message <- renderUI(
          tags$div(class = "alert alert-danger", paste0("Error reading file: ", e$message)))
        return(NULL)
      })
      if (is.null(df)) return()
      raw_df_r(df)
      output$upload_status_message <- renderUI(
        tags$div(class = "alert alert-info",
                 "File read. Select the OTU and (optionally) Specimen ID columns above, then click Apply."))
    })
    
    # -------------------------------------------------------------------------
    # Load example dataset — auto-apply with guessed OTU column
    # -------------------------------------------------------------------------
    observeEvent(input$load_example, {
      example_path <- system.file("examples", "Meristic-only.csv", package = "GroupStruct2")
      req(file.exists(example_path))
      df <- tryCatch(read.csv(example_path, stringsAsFactors = FALSE), error = function(e) {
        output$upload_status_message <- renderUI(
          tags$div(class = "alert alert-danger", paste0("Error loading example: ", e$message)))
        return(NULL)
      })
      if (is.null(df)) return()
      raw_df_r(df)
      col_names <- names(df)
      otu_col <- col_names[which(vapply(col_names, function(cn)
        anyDuplicated(trimws(as.character(df[[cn]]))) > 0, logical(1)))[1]]
      if (is.na(otu_col)) otu_col <- col_names[1]
      apply_result(parse_meristic_df(df, group_col = otu_col),
                   "Example dataset loaded successfully.")
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
