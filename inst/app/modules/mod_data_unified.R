mod_data_unified_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Input Data"),
    hr(),
    
    tags$div(
      style = "background-color: #f8f9fa; border-left: 5px solid #6c757d;
               border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 20px;",
      tags$p(style = "font-weight: bold; margin: 0 0 8px 0;", "Data Format Requirements"),
      tags$ul(style = "margin: 0;",
              tags$li("Specimens in rows, variables/traits in columns."),
              tags$li("A", strong("grouping / OTU column"), "is mandatory; this is typically the species or population label."),
              tags$li("A", strong("specimen ID column"), "(e.g., catalog numbers) is optional. If present, each specimen identifier must be unique.",
                      "If absent, sequential integers are assigned automatically."),
              tags$li("Columns that should not be analyzed (e.g., geographic coordinates) can be uploaded",
                      "and excluded using the", strong("Exclude columns"), "option in Step 2 below."),
              tags$li(strong("Missing values are not allowed"), "for any trait column."),
              tags$li("A minimum of", strong("2 specimens per group"), "is required (no singletons).")
      ),
      tags$p(style = "margin: 8px 0 0 0; font-size: 0.95em; color: #6c757d;",
             "For details about the included example datasets, visit the", strong("Example Datasets"), "tab on the home page.")
    ),
    
    # Upload
    tags$div(
      style = "background-color: #e9ecef; border-left: 5px solid #6c757d;
               padding: 15px; margin-bottom: 20px;",
      h4(style = "margin-top: 0;", "Upload File"),
      fileInput(ns("file_upload"),
                "Choose file (.csv, .tsv, or .txt):",
                accept = c(".csv", ".tsv", ".txt")),
      hr(),
      p(style = "margin-bottom: 12px;", strong("Or load an example dataset:")),
      
      # Gekko Lizards group
      tags$div(
        style = "margin-bottom: 15px;",
        tags$p(style = "margin: 0 0 8px 0; font-weight: 500; color: #495057;",
               "Gekko Lizards"),
        actionButton(ns("load_example_gekko_meristic"),
                     "Meristic",     class = "btn-default btn-sm"),
        actionButton(ns("load_example_gekko_morphometric"),
                     "Morphometric", class = "btn-default btn-sm"),
        actionButton(ns("load_example_gekko_mixed1"),
                     "Mixed (Meristic + Morphometric)",              class = "btn-default btn-sm"),
        actionButton(ns("load_example_gekko_mixed2"),
                     "Mixed (Meristic + Morphometric + Categorical)", class = "btn-default btn-sm")
      ),
      
      # Australian Sand Dragons group
      tags$div(
        style = "margin-bottom: 15px;",
        tags$p(style = "margin: 0 0 8px 0; font-weight: 500; color: #495057;",
               "Australian Sand Dragons (Ctenophorus)"),
        actionButton(ns("load_example_ctenophorus_mixed"),
                     "Mixed (Morphometric + Meristic + Categorical + Environmental)", class = "btn-default btn-sm")
      ),
      
      br(),
      uiOutput(ns("example_info_ui"))
    ),
    
    # Configuration (shown after upload)
    uiOutput(ns("config_ui")),
    
    # Detection banner + preview (shown after Apply)
    uiOutput(ns("detection_result_ui")),
    
    hr(),
    uiOutput(ns("data_preview_ui")),
    
    uiOutput(ns("outlier_ui"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_data_unified_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_df_r <- reactiveVal(NULL)
    
    # Returned to app_server.R
    detected_type_r  <- reactiveVal("")
    meristic_data_rv <- reactiveVal(NULL)
    meristic_ids_rv  <- reactiveVal(NULL)
    morpho_data_rv   <- reactiveVal(NULL)
    morpho_ids_rv    <- reactiveVal(NULL)
    combined_data_rv <- reactiveVal(NULL)
    combined_ids_rv  <- reactiveVal(NULL)
    full_display_rv  <- reactiveVal(NULL)
    
    # ── File reading ──────────────────────────────────────────────────────────
    
    read_df <- function(path, name) {
      ext <- tolower(tools::file_ext(name))
      tryCatch({
        if (ext == "csv")
          read.csv(path, stringsAsFactors = FALSE)
        else if (ext %in% c("tsv", "txt"))
          read.delim(path, stringsAsFactors = FALSE)
        else
          stop("Unsupported file type. Please upload a .csv, .tsv, or .txt file.")
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message),
                         type = "error", duration = 8)
        NULL
      })
    }
    
    reset_outputs <- function() {
      detected_type_r("")
      meristic_data_rv(NULL); meristic_ids_rv(NULL)
      morpho_data_rv(NULL);   morpho_ids_rv(NULL)
      combined_data_rv(NULL); combined_ids_rv(NULL)
      full_display_rv(NULL)
    }
    
    observeEvent(input$file_upload, {
      req(input$file_upload)
      df <- read_df(input$file_upload$datapath, input$file_upload$name)
      if (!is.null(df)) { raw_df_r(df); reset_outputs() }
    })
    
    active_example_r <- reactiveVal(NULL)
    
    load_example_file <- function(filename, example_key) {
      path <- system.file("examples", filename, package = "GroupStruct2")
      if (!file.exists(path)) {
        showNotification(paste("Example file not found:", filename),
                         type = "error"); return()
      }
      df <- tryCatch(
        read.csv(path, stringsAsFactors = FALSE),
        error = function(e) {
          showNotification(paste("Error loading example:", e$message),
                           type = "error"); NULL
        })
      if (!is.null(df)) {
        raw_df_r(df); reset_outputs()
        active_example_r(example_key)
      }
    }
    
    observeEvent(input$load_example_gekko_meristic,
                 load_example_file("Meristic-only.csv", "gekko_meristic"))
    observeEvent(input$load_example_gekko_morphometric,
                 load_example_file("Morphometric-only.csv", "gekko_morphometric"))
    observeEvent(input$load_example_gekko_mixed1,
                 load_example_file("Meristic-Morphometric.csv", "gekko_mixed1"))
    observeEvent(input$load_example_gekko_mixed2,
                 load_example_file("Meristic-Morphometric-Categorical.csv", "gekko_mixed2"))
    observeEvent(input$load_example_ctenophorus_mixed,
                 load_example_file("Ctenophorus.csv", "ctenophorus_mixed"))
    
    # Clear info box when user uploads their own file
    observeEvent(input$file_upload, { active_example_r(NULL) })
    
    # ── Example info boxes ────────────────────────────────────────────────────
    output$example_info_ui <- renderUI({
      key <- active_example_r()
      if (is.null(key)) return(NULL)
      
      info <- list(
        gekko_meristic = list(
          colour = "#d4edda", border = "#28a745",
          title  = "Gekko Lizards - Meristic Example Dataset Loaded",
          text   = tagList(
            p(em("OTU = Species; Specimen ID = None; Data type = Meristic")),
            tags$ul(
              tags$li(strong("Meristic (counts):"),
                      "SL, IS, IL, FS, CS, MB, PVT, LRT, VS, TL1, TL4")
            ),
            tags$div(
              style = "font-size: 0.85em; color: #555; margin-top: 6px;",
              p(style = "margin: 0;",
                "For variable definitions see: Grismer et al. (2022).",
                em("Vertebrate Zoology"), ", 72, 47\u201380.",
                tags$a("https://doi.org/10.3897/vz.72.e77702",
                       href = "https://doi.org/10.3897/vz.72.e77702",
                       target = "_blank"))
            )
          )
        ),
        gekko_morphometric = list(
          colour = "#d1ecf1", border = "#17a2b8",
          title  = "Gekko Lizards - Morphometric Example Dataset Loaded",
          text   = tagList(
            p(em("OTU = Species; Specimen ID = None; Data type = Morphometric")),
            tags$ul(
              tags$li(strong("Morphometric (continuous measurements):"),
                      "SVL, HH, HL, HW, IN, IO, TD, EE, NE, SE, OD, FL, CL, AG, TW")
            ),
            tags$div(
              style = "font-size: 0.85em; color: #555; margin-top: 6px;",
              p(style = "margin: 0;",
                "For variable definitions see: Grismer et al. (2022).",
                em("Vertebrate Zoology"), ", 72, 47\u201380.",
                tags$a("https://doi.org/10.3897/vz.72.e77702",
                       href = "https://doi.org/10.3897/vz.72.e77702",
                       target = "_blank"))
            )
          )
        ),
        gekko_mixed1 = list(
          colour = "#fff3cd", border = "#856404",
          title  = "Gekko Lizards - Mixed Example Dataset Loaded (Meristic + Morphometric)",
          text   = tagList(
            p(em("OTU = Species; Specimen ID = None; Data type = Mixed")),
            tags$ul(
              tags$li(strong("Morphometric (continuous measurements):"),
                      "SVL, HH, HL, HW, IN, IO, TD, EE, NE, SE, OD, FL, CL, AG, TW"),
              tags$li(strong("Meristic (counts):"),
                      "SL, IS, IL, FS, CS, MB, PVT, LRT, VS, TL1, TL4")
            ),
            tags$div(
              style = "font-size: 0.85em; color: #555; margin-top: 6px;",
              p(style = "margin: 0;",
                "For variable definitions see: Grismer et al. (2022).",
                em("Vertebrate Zoology"), ", 72, 47\u201380.",
                tags$a("https://doi.org/10.3897/vz.72.e77702",
                       href   = "https://doi.org/10.3897/vz.72.e77702",
                       target = "_blank"))
            )
          )
        ),
        gekko_mixed2 = list(
          colour = "#fff3cd", border = "#856404",
          title  = "Gekko Lizards - Mixed Example Dataset Loaded (Meristic + Morphometric + Categorical)",
          text   = tagList(
            p(em("OTU = Species; Specimen ID = None; Data type = Mixed")),
            tags$ul(
              tags$li(strong("Morphometric (continuous measurements):"),
                      "SVL, HH, HL, HW, IN, IO, TD, EE, NE, SE, OD, FL, CL, AG, TW"),
              tags$li(strong("Meristic (counts):"),
                      "SL, IS, IL, FS, CS, MB, PVT, LRT, VS, TL1, TL4"),
              tags$li(strong("Categorical:"),
                      "iris color, thin nuchal band, dark nuc band-eye, wht ocelli, drk on body")
            ),
            tags$div(
              style = "font-size: 0.85em; color: #555; margin-top: 6px;",
              p(style = "margin: 0;",
                "For variable definitions see: Grismer et al. (2022).",
                em("Vertebrate Zoology"), ", 72, 47\u201380.",
                tags$a("https://doi.org/10.3897/vz.72.e77702",
                       href   = "https://doi.org/10.3897/vz.72.e77702",
                       target = "_blank"))
            )
          )
        ),
        ctenophorus_mixed = list(
          colour = "#d4f0d1", border = "#28a745",
          title  = "Australian Sand Dragons (Ctenophorus) - Mixed Example Dataset Loaded",
          text   = tagList(
            p(em("OTU = Species; Specimen ID = None; Data type = Mixed (Morphometric + Meristic + Categorical + Environmental)")),
            tags$ul(
              tags$li(strong("Morphometric:"), "SVL, AG, ArmL, FingerL, LegL, ToeL, TailLength"),
              tags$li(strong("Meristic:"), "Lamel4thF, Lamel4thT, Pores"),
              tags$li(strong("Categorical:"), "CPE, CPPatt, TM, BGCol, PVL, PVSpots, FPVis, PoreArr"),
              tags$li(strong("Environmental:"), "19 WorldClim bioclimatic layers (BIO_1-19), digital elevation (RESAMPELEV), soil (Aclay, Athick, AKs, A_AWHC), vegetation (mNDVI, sdNDVI, TREE, mLAI, sdLAI)")
            ),
            tags$div(
              style = "font-size: 0.85em; color: #555; margin-top: 6px;",
              p(style = "margin: 0;",
                "Primary reference: Edwards, D. L., & Hutchinson, M. N. (2023).",
                em("Journal of Herpetology"), ", 57(2), 176\u2013196.",
                tags$a("https://doi.org/10.1670/22-021",
                       href = "https://doi.org/10.1670/22-021",
                       target = "_blank"))
            )
          )
        )
      )[[key]]
      
      tags$div(
        style = paste0("background-color:", info$colour,
                       "; border-left: 4px solid ", info$border,
                       "; padding: 12px; margin-top: 2px; border-radius: 3px;"),
        h5(style = "margin-top: 0;", info$title),
        info$text
      )
    })
    
    # ── Configuration UI ──────────────────────────────────────────────────────
    
    output$config_ui <- renderUI({
      df <- raw_df_r()
      if (is.null(df)) return(NULL)
      
      col_names <- names(df)
      
      # Guess OTU column: first non-numeric col with repeated values
      guess_otu <- {
        candidates <- Filter(function(cn) {
          vals <- df[[cn]]
          is_char <- !is.numeric(vals)
          has_dup <- anyDuplicated(trimws(as.character(vals))) > 0
          is_char && has_dup
        }, col_names)
        if (length(candidates) > 0) candidates[1] else col_names[1]
      }
      
      # Guess ID column: first non-numeric col with all unique values
      guess_id <- {
        candidates <- Filter(function(cn) {
          if (cn == guess_otu) return(FALSE)
          vals <- df[[cn]]
          !is.numeric(vals) && !anyDuplicated(trimws(as.character(vals)))
        }, col_names)
        if (length(candidates) > 0) candidates[1] else "none"
      }
      
      tagList(
        hr(),
        
        # ── Step 1 ───────────────────────────────────────────────────────────
        h4("Step 1: Select Data Type"),
        tags$div(
          style = "display: flex; gap: 10px; margin-bottom: 14px;",
          lapply(
            list(
              list(val  = "meristic",
                   icon = "\U0001F522",
                   name = "Meristic",
                   desc = "Count / integer traits (e.g. scale rows, fin rays)"),
              list(val  = "morphometric",
                   icon = "\U0001F4CF",
                   name = "Morphometric",
                   desc = "Continuous measurements (e.g. head length, jaw width)"),
              list(val  = "combined",
                   icon = "\U0001F522\U0001F4CF",
                   name = "Mixed",
                   desc = "Any combination: meristic, morphometric, categorical, environmental")
            ),
            function(opt) {
              selected <- !is.null(input$data_type_choice) &&
                !identical(input$data_type_choice, "") &&
                input$data_type_choice == opt$val
              tags$div(
                style = paste0(
                  "flex: 1; cursor: pointer; border-radius: 6px; padding: 14px 12px;",
                  " text-align: center; user-select: none;",
                  " border: 2px solid ", if (selected) "#0d6efd;" else "#ced4da;",
                  " background: ",       if (selected) "#e8f0fe;" else "white;",
                  " box-shadow: ",       if (selected) "0 0 0 3px rgba(13,110,253,.2);" else "none;"
                ),
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                  ns("data_type_choice"), opt$val
                ),
                tags$div(style = "font-size: 1.6em; margin-bottom: 4px;", opt$icon),
                tags$div(
                  style = paste0(
                    "font-size: 1.05em; font-weight: bold; margin-bottom: 5px;",
                    " color: ", if (selected) "#0d6efd;" else "#212529;"
                  ),
                  opt$name
                ),
                tags$div(
                  style = "font-size: 0.82em; color: #6c757d; line-height: 1.3;",
                  opt$desc
                )
              )
            }
          )
        ),
        hr(),
        
        # ── Step 2 ───────────────────────────────────────────────────────────
        h4("Step 2: Identify Key Columns"),
        p("If no Specimen ID column is present, sequential integers will be",
          "assigned automatically for outlier reporting and plot labels."),
        fluidRow(
          column(4,
                 selectInput(ns("otu_col"),
                             "OTU / Group column:",
                             choices  = col_names,
                             selected = guess_otu,
                             width    = "100%")
          ),
          column(4,
                 selectInput(ns("id_col"),
                             "Specimen ID column (optional):",
                             choices  = c("None" = "none", col_names),
                             selected = guess_id,
                             width    = "100%")
          ),
          column(4,
                 tags$label("Exclude columns (optional):"),
                 uiOutput(ns("exclude_ui"))
          )
        ),
        
        br(),
        actionButton(ns("apply_config"), "Load Data",
                     icon = icon("check"), class = "btn-primary"),
        uiOutput(ns("apply_status_ui")),
        uiOutput(ns("loading_ui"))
      )
    })
    
    # Exclude selectize — updates when OTU/ID dropdowns change
    output$exclude_ui <- renderUI({
      df <- raw_df_r(); req(df)
      otu_col <- input$otu_col %||% ""
      id_col  <- input$id_col  %||% "none"
      special <- c(otu_col, if (id_col != "none") id_col)
      trait_cols <- setdiff(names(df), special)
      if (length(trait_cols) == 0) return(NULL)
      selectizeInput(
        ns("exclude_cols"),
        label   = NULL,
        choices = trait_cols,
        selected = NULL,
        multiple = TRUE,
        options  = list(placeholder = "Click to select columns to exclude\u2026",
                        plugins     = list("remove_button"))
      )
    })
    
    # ── Apply ─────────────────────────────────────────────────────────────────
    
    observeEvent(input$apply_config, {
      df <- raw_df_r(); req(df)
      
      # Validate first (before spinner — fast, no need to show spinner for errors)
      err <- NULL
      if (is.null(input$otu_col) || input$otu_col == "")
        err <- "Please select an OTU / Group column."
      if (is.null(err) && (is.null(input$data_type_choice) ||
                           input$data_type_choice == ""))
        err <- "Please select a data type (Meristic, Morphometric, or Mixed)."
      if (is.null(err) && !is.null(input$id_col) &&
          input$id_col != "none" && input$id_col == input$otu_col)
        err <- "OTU / Group and Specimen ID columns cannot be the same."
      
      if (!is.null(err)) {
        output$apply_status_ui <- renderUI(
          tags$div(class = "alert alert-danger",
                   style = "margin-top: 8px;", err))
        return()
      }
      
      output$loading_ui <- renderUI(
        tags$div(
          style = "background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px;
                   padding: 14px 18px; margin-top: 10px; display: flex; align-items: center; gap: 12px;",
          tags$span(class = "fa fa-spinner fa-spin", style = "font-size: 1.4em; color: #0c5460;"),
          tags$span(style = "color: #0c5460; font-weight: bold;", "Loading data...")
        )
      )
      on.exit(output$loading_ui <- renderUI(NULL))
      
      otu_col  <- input$otu_col
      id_col   <- if (!is.null(input$id_col) &&
                      input$id_col != "none") input$id_col else NULL
      excl     <- input$exclude_cols %||% character(0)
      dtype    <- input$data_type_choice
      
      # Specimen IDs — keep original column name; use ".RowID" only as internal fallback
      id_col_name <- if (!is.null(id_col)) id_col else ".RowID"
      ids_out <- if (!is.null(id_col))
        trimws(as.character(df[[id_col]]))
      else
        as.character(seq_len(nrow(df)))
      
      # Drop excluded + special cols to get trait columns
      special    <- c(otu_col, id_col, excl)
      trait_cols <- setdiff(names(df), special)
      
      if (length(trait_cols) == 0) {
        output$apply_status_ui <- renderUI(
          tags$div(class = "alert alert-danger",
                   style = "margin-top: 8px;",
                   "No trait columns remain after removing OTU, ID, and excluded columns."))
        return()
      }
      
      # Validate data type selection against actual column types
      trait_df_check <- df[, trait_cols, drop = FALSE]
      non_numeric_cols <- names(trait_df_check)[vapply(trait_df_check, function(x) {
        !is.numeric(x) && suppressWarnings(any(is.na(as.numeric(as.character(x)))))
      }, logical(1))]
      
      if (dtype %in% c("meristic", "morphometric") && length(non_numeric_cols) > 0) {
        output$apply_status_ui <- renderUI(
          tags$div(class = "alert alert-danger", style = "margin-top: 8px;",
                   strong("Data type mismatch: "),
                   tags$b(if (dtype == "meristic") "Meristic" else "Morphometric"),
                   " requires all trait columns to be numeric, but the following column(s) contain non-numeric values: ",
                   tags$b(paste(non_numeric_cols, collapse = ", ")), ". ",
                   "Either exclude these columns or select ", tags$b("Mixed"), " as the data type."))
        return()
      }
      
      # Collect any soft warnings to display statically after loading
      warnings_ui <- list()
      
      # Warn if Meristic selected but data contains decimal values
      if (dtype == "meristic") {
        numeric_cols <- names(trait_df_check)[sapply(trait_df_check, is.numeric)]
        has_decimals <- any(vapply(trait_df_check[numeric_cols], function(x)
          any(x != floor(x), na.rm = TRUE), logical(1)))
        if (has_decimals)
          warnings_ui <- c(warnings_ui, list(tags$div(
            class = "alert alert-warning", style = "margin-top: 8px;",
            icon("exclamation-triangle"), strong(" Note: "),
            "Meristic is selected but some trait columns contain decimal values. ",
            "Meristic data typically consists of integer counts. ",
            "If your data contains continuous measurements, consider selecting ",
            tags$b("Morphometric"), " instead.")))
      }
      
      # Warn if Morphometric selected but data contains a mix of integer and decimal columns
      if (dtype == "morphometric") {
        numeric_cols <- names(trait_df_check)[sapply(trait_df_check, is.numeric)]
        int_cols <- numeric_cols[vapply(trait_df_check[numeric_cols],
                                        function(x) all(x == floor(x), na.rm = TRUE), logical(1))]
        decimal_cols <- setdiff(numeric_cols, int_cols)
        if (length(int_cols) > 0 && length(decimal_cols) > 0)
          warnings_ui <- c(warnings_ui, list(tags$div(
            class = "alert alert-warning", style = "margin-top: 8px;",
            icon("exclamation-triangle"), strong(" Note: "),
            "Morphometric is selected but the following column(s) appear to contain integer counts: ",
            tags$b(paste(int_cols, collapse = ", ")), ". ",
            "If these are meristic characters mixed with morphometric measurements, ",
            "consider selecting ", tags$b("Mixed"), " instead.")))
      }
      
      # Warn if Mixed selected but all columns are the same numeric type
      if (dtype == "combined" && length(non_numeric_cols) == 0) {
        num_cols  <- names(trait_df_check)[sapply(trait_df_check, is.numeric)]
        all_int   <- all(vapply(trait_df_check[num_cols], function(x)
          all(x == floor(x), na.rm = TRUE), logical(1)))
        all_decim <- all(vapply(trait_df_check[num_cols], function(x)
          any(x != floor(x), na.rm = TRUE), logical(1)))
        if (all_int)
          warnings_ui <- c(warnings_ui, list(tags$div(
            class = "alert alert-warning", style = "margin-top: 8px;",
            icon("exclamation-triangle"), strong(" Note: "),
            "Mixed is selected but all trait columns appear to be integer counts. ",
            "If your data is purely count-based, consider selecting ", tags$b("Meristic"), " instead.")))
        else if (all_decim)
          warnings_ui <- c(warnings_ui, list(tags$div(
            class = "alert alert-warning", style = "margin-top: 8px;",
            icon("exclamation-triangle"), strong(" Note: "),
            "Mixed is selected but all trait columns appear to be continuous measurements. ",
            "If your data is purely morphometric, consider selecting ", tags$b("Morphometric"), " instead.")))
      }
      
      # Build trait data frame
      otu_vals <- factor(trimws(as.character(df[[otu_col]])))
      trait_df <- df[, trait_cols, drop = FALSE]
      
      # Coerce numerics
      for (cn in trait_cols) {
        if (!is.numeric(trait_df[[cn]])) {
          coerced <- suppressWarnings(as.numeric(as.character(trait_df[[cn]])))
          if (!any(is.na(coerced)))
            trait_df[[cn]] <- coerced
          else
            trait_df[[cn]] <- as.factor(trait_df[[cn]])
        }
      }
      
      out_df <- cbind(
        data.frame(setNames(list(otu_vals), otu_col), stringsAsFactors = FALSE),
        trait_df)
      
      display_df <- cbind(
        data.frame(setNames(list(ids_out), id_col_name), stringsAsFactors = FALSE),
        out_df)
      
      # Route
      if (dtype == "meristic") {
        meristic_data_rv(out_df); meristic_ids_rv(ids_out)
      } else if (dtype == "morphometric") {
        morpho_data_rv(out_df); morpho_ids_rv(ids_out)
      } else {
        cat_cols <- names(trait_df)[sapply(trait_df, is.factor)]
        combined_data_rv(list(
          data             = out_df,
          group_col        = otu_col,
          categorical_cols = cat_cols
        ))
        combined_ids_rv(ids_out)
      }
      
      full_display_rv(display_df)
      detected_type_r(dtype)
      
      output$apply_status_ui <- renderUI(
        if (length(warnings_ui) > 0)
          tagList(do.call(tagList, warnings_ui))
        else
          NULL
      )
    })
    
    # Detection banner — removed (redundant with sidebar indicator)
    output$detection_result_ui <- renderUI({ NULL })
    
    # ── Outlier detection ─────────────────────────────────────────────────────
    
    output$outlier_ui <- renderUI({
      if (is.null(full_display_rv())) return(NULL)
      tagList(
        hr(),
        h4("Outlier Detection (Optional)"),
        p("The Boxplot IQR method flags values that fall beyond a user-defined",
          "multiplier of the IQR within each OTU.",
          "A multiplier of 3.0 is recommended for morphological data.",
          "Requires \u22654 samples per group."),
        p(strong("Outliers are flagged but NOT removed.",
                 "It is up to the user to decide what to do with them."),
          style = "color: red;"),
        sliderInput(ns("iqr_multiplier"), "IQR Multiplier:",
                    min = 1.5, max = 5.0, value = 3.0, step = 0.5, width = "250px"),
        actionButton(ns("detect_outliers"), "Detect Outliers",
                     icon = icon("search"), class = "btn-primary"),
        br(), br(),
        verbatimTextOutput(ns("outlier_report"))
      )
    })
    
    observeEvent(input$detect_outliers, {
      df  <- full_display_rv(); req(df)
      id_col_name <- names(df)[1]   # first col is always the ID col (original name preserved)
      ids <- df[[id_col_name]]
      
      # Get numeric trait columns only (exclude ID and OTU col)
      otu_col <- input$otu_col %||% names(df)[2]
      skip    <- c(id_col_name, otu_col)
      num_df  <- df[, setdiff(names(df), skip), drop = FALSE]
      num_df  <- num_df[, sapply(num_df, is.numeric), drop = FALSE]
      
      if (ncol(num_df) == 0) {
        output$outlier_report <- renderText("No numeric trait columns found.")
        return()
      }
      
      otu_vals    <- df[[otu_col]]
      skipped_otus <- c()
      report      <- ""
      
      for (trait in names(num_df)) {
        vals   <- num_df[[trait]]
        flagged <- unlist(lapply(
          split(seq_len(nrow(df)), otu_vals),
          function(rows) {
            x <- vals[rows]
            if (length(x) < 4) {
              skipped_otus <<- unique(c(skipped_otus,
                                        as.character(otu_vals[rows[1]])))
              return(integer(0))
            }
            q   <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
            iqr <- q[2] - q[1]
            rows[x < (q[1] - input$iqr_multiplier * iqr) |
                   x > (q[2] + input$iqr_multiplier * iqr)]
          }
        ))
        if (length(flagged) > 0) {
          report <- paste0(report,
                           "\nTrait: ", trait, "\n",
                           "Specimen ID(s): ",
                           paste(ids[flagged], collapse = ", "), "\n")
        }
      }
      
      if (nchar(report) == 0)
        report <- "No outliers detected with the selected IQR multiplier."
      else
        report <- paste0(report, "\nConsider re-inspecting flagged specimens.")
      
      if (length(skipped_otus) > 0)
        report <- paste0(report,
                         "\n\nNote: OTUs with fewer than 4 specimens were skipped:\n",
                         paste(skipped_otus, collapse = ", "))
      
      output$outlier_report <- renderText(report)
    })
    
    # ── Data preview ──────────────────────────────────────────────────────────
    
    output$data_preview_ui <- renderUI({
      if (is.null(full_display_rv())) return(NULL)
      tagList(h4("Data Preview"), DT::dataTableOutput(ns("preview_table")))
    })
    
    output$preview_table <- DT::renderDataTable({
      req(full_display_rv())
      DT::datatable(
        full_display_rv(),
        class   = "display nowrap",
        options = list(pageLength = 10, scrollX = TRUE,
                       lengthMenu = c(10, 25, 50), dom = "tip"))
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      detected_type_r = detected_type_r,
      meristic_data_r = meristic_data_rv,
      meristic_ids_r  = meristic_ids_rv,
      morpho_data_r   = morpho_data_rv,
      morpho_ids_r    = morpho_ids_rv,
      combined_data_r = combined_data_rv,
      combined_ids_r  = combined_ids_rv
    ))
  })
}
