
mod_data_ui_morphometric <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Morphometric Data"),
    hr(),
    
    tags$div(
      style = "background-color: #d1ecf1; border-left: 5px solid #17a2b8; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "How to Format Your File"),
      p("The application automatically detects whether your file includes specimen identifiers (e.g. museum catalog numbers) based on a simple rule:"),
      tags$div(
        style = "background-color: #ffffff; border: 1px solid #bee5eb; border-radius: 4px; padding: 10px; margin: 8px 0;",
        p(style = "margin: 0;",
          strong("Detection rule:"), " if every value in Column 1 is unique, it is treated as a Specimen ID column.",
          " If Column 1 contains repeated values (i.e. multiple specimens share the same label), it is treated as the OTU/group column.")
      ),
      fluidRow(
        column(6,
               p(strong("Without specimen IDs")),
               p(em("Column 1 values repeat — detected as OTU/group.")),
               tags$table(
                 class = "table table-bordered table-condensed",
                 style = "font-size: 0.85em; background: white;",
                 tags$thead(tags$tr(
                   tags$th("Species"), tags$th("Trait1"), tags$th("Trait2")
                 )),
                 tags$tbody(
                   tags$tr(tags$td("Gekko_smithii"), tags$td("14"), tags$td("22.1")),
                   tags$tr(tags$td("Gekko_smithii"), tags$td("13"), tags$td("21.4")),
                   tags$tr(tags$td("Gekko_albomaculatus"), tags$td("16"), tags$td("25.3")),
                   tags$tr(tags$td("Gekko_albomaculatus"), tags$td("15"), tags$td("24.8"))
                 )
               ),
               tags$div(
                 style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-top: 6px; font-size: 0.85em;",
                 tags$p(style = "margin: 0;",
                        strong("Note:"), " No specimen IDs detected in this format.",
                        " Sequential integers (1, 2, 3, ...) will be automatically assigned as specimen IDs for outlier reporting and interactive plot labels.")
               )
        ),
        column(6,
               p(strong("With specimen IDs")),
               p(em("Column 1 values are all unique — detected as Specimen ID.")),
               tags$table(
                 class = "table table-bordered table-condensed",
                 style = "font-size: 0.85em; background: white;",
                 tags$thead(tags$tr(
                   tags$th("CatalogNo"), tags$th("Species"), tags$th("Trait1"), tags$th("Trait2")
                 )),
                 tags$tbody(
                   tags$tr(tags$td("LSUHC 13451"), tags$td("Gekko_smithii"), tags$td("14"), tags$td("22.1")),
                   tags$tr(tags$td("LSUHC 13452"), tags$td("Gekko_smithii"), tags$td("13"), tags$td("21.4")),
                   tags$tr(tags$td("ZRC 2.7891"), tags$td("Gekko_albomaculatus"), tags$td("16"), tags$td("25.3")),
                   tags$tr(tags$td("ZRC 2.7892"), tags$td("Gekko_albomaculatus"), tags$td("15"), tags$td("24.8"))
                 )
               )
        )
      ),
      p(style = "margin-bottom: 0;",
        em("Column headers can be named anything. The detection is based entirely on whether the values in Column 1 repeat."))
    ),
    
    tags$div(
      style = "background-color: #e9ecef; border-left: 5px solid #6c757d; padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Upload Your Data"),
      p("A preview of the data will be shown as soon as it is uploaded."),
      p(strong("Missing values and singletons are not allowed.")),
      fileInput(ns("file"), "Upload file (.csv, .tsv, or .txt)", accept = c(".csv", ".tsv", ".txt")),
      actionButton(ns("load_example"), "Load Example Morphometric Dataset"),
      uiOutput(ns("upload_status_message"))
    ),
    
    br(),
    p(strong("The example datasets included in this package are for practice purposes only and are not meant to inform taxonomic changes.")),
    p("Note on the example dataset: This example dataset contains 15 morphometric traits from four species of lizards."),
    p("Additional details on this dataset can be found at: Grismer et al. (2022). Phylogenetic and multivariate analyses of ",
      em("Gekko smithii"), " Gray, 1842 recover a new species from Peninsular Malaysia and support the resurrection of ",
      em("G. albomaculatus"), " (Giebel, 1861) from Sumatra. ",
      em("Vertebrate Zoology"), ", 72, 47-80. ",
      tags$a(href = "https://doi.org/10.3897/vz.72.e77702",
             "https://doi.org/10.3897/vz.72.e77702", target = "_blank")),
    
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


mod_data_server_morphometric <- function(id) {
  moduleServer(id, function(input, output, session) {
    data         <- reactiveVal(NULL)
    specimen_ids <- reactiveVal(NULL)
    full_data    <- reactiveVal(NULL)
    
    output$upload_status_message <- renderUI({ NULL })
    
    # -------------------------------------------------------------------------
    # Internal helper: validate and parse a raw data frame
    # -------------------------------------------------------------------------
    parse_morphometric_df <- function(df) {
      col1_vals <- trimws(as.character(df[[1]]))
      has_id    <- !anyDuplicated(col1_vals)
      
      min_cols  <- if (has_id) 3L else 2L
      id_label  <- if (has_id) " (specimen ID + OTU + at least one trait)" else " (OTU + at least one trait)"
      
      if (ncol(df) < min_cols) {
        return(list(ok = FALSE,
                    message = paste0("Error: Morphometric data must have at least ", min_cols,
                                     " columns", id_label, ".")))
      }
      
      if (has_id) {
        ids_out <- col1_vals
        df      <- df[, -1, drop = FALSE]
      } else {
        ids_out <- as.character(seq_len(nrow(df)))
      }
      
      df[[1]] <- factor(trimws(as.character(df[[1]])))
      
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
                                       "' is not numeric. All trait columns must be numeric for morphometric data.")))
        }
      }
      
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
      
      df <- tryCatch({
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") {
          read.csv(input$file$datapath, stringsAsFactors = FALSE)
        } else if (ext %in% c("tsv", "txt")) {
          read.delim(input$file$datapath, stringsAsFactors = FALSE)
        } else {
          output$upload_status_message <- renderUI({
            tags$div(class = "alert alert-danger",
                     "Error: Unsupported file type. Please upload a .csv, .tsv, or .txt file.")
          })
          return(NULL)
        }
      }, error = function(e) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", paste0("Error reading file: ", e$message))
        })
        return(NULL)
      })
      
      req(df)
      result <- parse_morphometric_df(df)
      
      if (!result$ok) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", result$message)
        })
        data(NULL); specimen_ids(NULL); full_data(NULL)
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
    # Load example dataset
    # -------------------------------------------------------------------------
    observeEvent(input$load_example, {
      example_path <- system.file("examples", "Morphometric-only.csv", package = "GroupStruct2")
      req(file.exists(example_path))
      
      df <- tryCatch({
        read.csv(example_path, stringsAsFactors = FALSE)
      }, error = function(e) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", paste0("Error loading example data: ", e$message))
        })
        return(NULL)
      })
      
      req(df)
      result <- parse_morphometric_df(df)
      
      if (!result$ok) {
        output$upload_status_message <- renderUI({
          tags$div(class = "alert alert-danger", result$message)
        })
        data(NULL); specimen_ids(NULL); full_data(NULL)
        return()
      }
      
      data(result$data)
      specimen_ids(result$specimen_ids)
      full_data(result$full_data)
      output$upload_status_message <- renderUI({
        tags$div(class = "alert alert-success", "Example dataset loaded successfully.")
      })
    })
    
    # -------------------------------------------------------------------------
    # Outlier detection
    # -------------------------------------------------------------------------
    observeEvent(input$detect_outliers, {
      req(data())
      df         <- data()
      otu_col    <- df[[1]]
      trait_data <- df[, 2:ncol(df), drop = FALSE]
      ids        <- specimen_ids()
      
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
    # Data preview (shows full data including specimen ID column)
    # -------------------------------------------------------------------------
    output$preview <- renderDT({
      req(full_data())
      datatable(full_data(),
                options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100),
                               scrollX = TRUE, dom = "tip"))
    })
    
    output$summary_text <- renderPrint({
      req(data())
      df <- data()
      cat("Number of OTUs:", length(unique(df[[1]])), "\n\n")
      cat("Sample size per OTU:\n")
      print(table(df[[1]]))
      cat("\nNumber of Traits:", ncol(df) - 1, "\n")
      cat("\nNote: This summary is based on the raw, unadjusted data.\n")
    })
    
    return(list(data = data, specimen_ids = specimen_ids))
  })
}
