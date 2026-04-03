mod_allometry_ui_combined <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Allometric Correction and Log Transformation"),
    hr(),
    p("This module allows you to perform allometric size correction on the morphometric traits within your combined dataset."),
    p(strong("Important:"), "The first column must be Group/OTU names (e.g., species or population). Avoid special characters in trait names."),
    p("Each Group/OTU must be represented by more than two individuals (to calculate mean) and missing data is not allowed. This adjustment should also be performed separately on different sexes to account for possible sexual dimorphism."),
    p(strong("Note: Allometric correction should only performed on morphometric data and not on meristic, categorical, or other non-morphemtric data."),style="color:red"),
    p(strong("Multispecies:"), "Use this option if the OTUs in your dataset represent DIFFERENT SPECIES i.e., different geographic populations/localities have been pooled under a single OTU/species name (pooled populations). Traits are adjusted according to an OTU-specific standard size and OTU-specific regression slope. The caveat is that the OTU-specific regression slope is calculated from pooled populations, which assumes that variation among populations of the same species is homogenous and can be represented by a common regression slope. If there is substantial variation among populations of the same species, such that different populations have significantly different regression slopes, use the multipopulation option. The multispecies option is also useful when population sample size is small."),
    p(strong("Multipopulation:"), "Use this option if the OTUs in your dataset represent different populations of ONE SPECIES. If you use this option, each uploaded dataset comprises ONE SPECIES. If more than one species are to be analyzed, do them separately. The multipopulation option is used when populations within a single species vary significantly, such that they have different slopes. Because each OTU represents a population, a separate slope is calculated for each population. However, a single grand mean for body size is calculated across all populations. CAVEAT: calculating accurate population-specific slopes require large sample sizes. Low population sample sizes will skew the slope and affect the correction. In this case, consider the multispecies option that pools populations to boost sample size."),    p("If you use this function, please cite the original paper: Chan, K. O., & Grismer, L. L. (2022). GroupStruct: An R package for allometric size correction. Zootaxa, 5124(4), 471–482."), 
    hr(),
    uiOutput(ns("body_size_selector_ui")),
    p(strong("Important:"), "Make sure you select the variable that represents body size (e.g., snout-vent-length). It is crucial to select the correct variable because the allometric function scales morphometric traits to this body-size variable. Selecting a variable that is not representative of overall body size will cause severe errors"),
    hr(),
    h4("Select Morphometric Traits for Correction"),
    p("Select which of your numeric traits are morphometric and should be corrected for body-size variation. Do not select meristic, categorical, or other non-morphometric traits."),
    uiOutput(ns("morphometric_traits_selector")),
    br(),
    radioButtons(ns("correction_type"), "Select Correction Type:",
                 choiceNames = list(
                   "Multispecies",
                   "Multipopulation",
                   "No correction (raw data)",
                   "No correction (log10-transformed)"),
                 choiceValues = list(
                   "species",
                   "population1",
                   "none",
                   "log10_only"
                 ),
                 selected = "species",
                 inline = TRUE
    ),
    hr(),
    actionButton(ns("run_correction"), "Run Allometric Correction", icon = icon("play"), class = "btn-primary"),
    hr(),
    br(),
    h4("Combined Data Preview (Size-corrected Morphometric + All Other Traits)"),
    DTOutput(ns("adjusted_data_preview")),
    br(),
    h4("Note on Adjusted Data:"),
    p("The 'Body-size' column (your original 2nd column) in the adjusted data is now its log10-transformed value."),
    p("Other trait columns that you selected as morphometric now contain their size-corrected and log-transformed values which are suitable for further analyses where size effects need to be removed."),
    p("Non-morphometric traits are unchanged."),
    br(),
    downloadButton(ns("download_adjusted_data_csv"), "Download Size-adjusted Combined Data"),
    hr()
  )
}

mod_allometry_server_combined <- function(id, raw_combined_data_r, specimen_ids_r = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Disable trait selection when "none" is selected
    observeEvent(input$correction_type, {
      if (input$correction_type == "none") {
        updateCheckboxGroupInput(session, "morphometric_traits_to_correct", selected = character(0))
      }
    })
    
    adjusted_data_output_r   <- reactiveVal(NULL)
    specimen_ids_adjusted_r  <- reactiveVal(NULL)
    
    # UI for body size column
    output$body_size_selector_ui <- renderUI({
      req(raw_combined_data_r())
      df <- raw_combined_data_r()$data
      group_col <- raw_combined_data_r()$group_col
      
      candidate_cols <- setdiff(names(df), group_col)
      numeric_vars <- candidate_cols[sapply(df[, candidate_cols, drop = FALSE], is.numeric)]
      
      if (length(numeric_vars) == 0) {
        return(p("No numeric variables available for body size selection."))
      }
      
      radioButtons(
        inputId = ns("body_size_col"),
        label = "Select Body Size Variable:",
        choices = numeric_vars,
        selected = numeric_vars[1],
        inline = TRUE
      )
    })
    
    # Morphometric trait selector (excluding selected body size)
    output$morphometric_traits_selector <- renderUI({
      req(raw_combined_data_r(), input$body_size_col)
      df <- raw_combined_data_r()$data
      group_col <- raw_combined_data_r()$group_col
      
      trait_cols <- setdiff(names(df), c(group_col, input$body_size_col))
      numeric_traits <- trait_cols[sapply(df[, trait_cols, drop = FALSE], is.numeric)]
      
      if (length(numeric_traits) == 0) {
        return(p("No numeric traits available for allometric correction."))
      }
      
      tagList(
        fluidRow(
          column(12,
                 HTML("<label><strong>Choose morphometric traits (select multiple if needed):</strong></label>"),
                 checkboxGroupInput(ns("morphometric_traits_to_correct"), NULL,
                                    choices = numeric_traits,
                                    selected = NULL,
                                    inline = TRUE)
          )
        )
      )
    })
    
    # Correction function
    allom_modified <- function(data_subset, type, group_col_name, body_size_col_name, trait_col_names) {
      if (type == "none" || length(trait_col_names) == 0) return(data_subset)
      
      # If log10 transformation only (no allometric correction)
      if (type == "log10_only") {
        adjusted_df_output <- data_subset
        
        # Ensure body-size column and trait columns are positive
        if (any(data_subset[[body_size_col_name]] <= 0, na.rm = TRUE)) {
          stop("Error: Body-size column must contain only positive values for log transformation.")
        }
        if (any(data_subset[, trait_col_names] <= 0, na.rm = TRUE)) {
          stop("Error: Trait columns must contain only positive values for log transformation.")
        }
        
        # Log-transform body-size column and all trait columns
        adjusted_df_output[[body_size_col_name]] <- log10(data_subset[[body_size_col_name]])
        for (trait in trait_col_names) {
          adjusted_df_output[[trait]] <- log10(data_subset[[trait]])
        }
        
        return(adjusted_df_output)
      }
      
      adjusted_df_output <- data_subset
      adjusted_df_output[[body_size_col_name]] <- log10(data_subset[[body_size_col_name]])
      
      # Fix tidyselect warning here:
      df_log_internal <- data_subset %>%
        mutate(across(all_of(c(body_size_col_name, trait_col_names)), log10)) %>%
        filter(complete.cases(.))
      
      if (nrow(df_log_internal) == 0) stop("No complete cases available after log transform.")
      
      # Singleton check (only relevant for correction types requiring regression)
      if (type %in% c("species", "population1")) {
        for (otu in unique(df_log_internal[[group_col_name]])) {
          if (sum(df_log_internal[[group_col_name]] == otu) < 2) {
            stop(paste0("Error: OTU '", otu, "' has less than 2 individuals. Each OTU must have more than one individual for allometric adjustment."))
          }
        }
      }
      
      if (type == "species") {
        for (otu in unique(df_log_internal[[group_col_name]])) {
          sub_log <- df_log_internal %>% filter(!!sym(group_col_name) == otu)
          z_mean <- mean(data_subset[[body_size_col_name]][data_subset[[group_col_name]] == otu], na.rm = TRUE)
          log_z_mean <- log10(z_mean)
          
          for (trait in trait_col_names) {
            beta <- coef(lm(as.formula(paste0("`", trait, "` ~ `", body_size_col_name, "`")), data = sub_log))[2]
            
            idx <- which(data_subset[[group_col_name]] == otu)
            adjusted_vals <- log10(data_subset[[trait]][idx]) - beta * (log10(data_subset[[body_size_col_name]][idx]) - log_z_mean)
            adjusted_df_output[idx, trait] <- adjusted_vals
          }
        }
      } else if (type == "population1") {
        for (trait in trait_col_names) {
          beta <- coef(lm(as.formula(paste0("log10(`", trait, "`) ~ log10(`", body_size_col_name, "`)")), data = data_subset))[2]
          
          for (otu in unique(data_subset[[group_col_name]])) {
            idx <- which(data_subset[[group_col_name]] == otu)
            z_mean <- mean(data_subset[[body_size_col_name]], na.rm = TRUE)
            log_z_mean <- log10(z_mean)
            adjusted_vals <- log10(data_subset[[trait]][idx]) - beta * (log10(data_subset[[body_size_col_name]][idx]) - log_z_mean)
            adjusted_df_output[idx, trait] <- adjusted_vals
          }
        }
      }
      
      return(adjusted_df_output)
    }
    
    # Apply correction ONLY when button is clicked
    observeEvent(input$run_correction, {
      req(raw_combined_data_r(), input$correction_type, input$body_size_col)
      
      withProgress(message = 'Running allometric correction...', value = 0, {
        
        df_raw_full <- raw_combined_data_r()$data
        group_col_name <- raw_combined_data_r()$group_col
        body_size_col_name <- input$body_size_col
        selected_morph_traits <- input$morphometric_traits_to_correct
        
        # Validation
        if (ncol(df_raw_full) < 3) {
          showNotification("Error: Data must have at least 3 columns (OTU, Body-size, and at least one trait) for allometric correction.", type = "error")
          adjusted_data_output_r(NULL)
          return()
        }
        
        if (input$correction_type == "none") {
          adjusted_data_output_r(df_raw_full)
          specimen_ids_adjusted_r(
            if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) specimen_ids_r() else
              as.character(seq_len(nrow(df_raw_full)))
          )
          showNotification("No correction applied. Raw data returned.", type = "message")
          return()
        }
        
        if (length(selected_morph_traits) == 0 && input$correction_type != "none") {
          showNotification("Please select at least one morphometric trait for correction.", type = "warning")
          return()
        }
        
        incProgress(0.3, detail = "Validating data...")
        
        non_numeric <- selected_morph_traits[!sapply(df_raw_full[, selected_morph_traits, drop = FALSE], is.numeric)]
        if (length(non_numeric) > 0) {
          showNotification("Allometric correction cannot be performed on categorical or meristic data. Please select morphometric data only.", type = "error")
          return()
        }
        
        incProgress(0.3, detail = "Applying correction...")
        
        columns_to_select <- c(group_col_name, body_size_col_name, selected_morph_traits)
        df_subset <- df_raw_full %>% dplyr::select(all_of(columns_to_select))
        
        adjusted_df <- tryCatch({
          allom_modified(
            data_subset = df_subset,
            type = input$correction_type,
            group_col_name = group_col_name,
            body_size_col_name = body_size_col_name,
            trait_col_names = selected_morph_traits
          )
        }, error = function(e) {
          showNotification(paste("Correction failed:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(adjusted_df)) {
          adjusted_data_output_r(NULL)
          return()
        }
        
        incProgress(0.3, detail = "Finalizing...")
        
        # Merge corrected columns back into full dataset
        df_raw_full[[body_size_col_name]] <- adjusted_df[[body_size_col_name]]
        for (trait in selected_morph_traits) {
          df_raw_full[[trait]] <- adjusted_df[[trait]]
        }
        
        adjusted_data_output_r(df_raw_full)
        # Row order is preserved (no sorting in combined allometry), so IDs align directly
        specimen_ids_adjusted_r(
          if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) specimen_ids_r() else
            as.character(seq_len(nrow(df_raw_full)))
        )
        
        if (!is.null(df_raw_full)) {
          showNotification("Allometric correction completed successfully!", type = "message")
        }
      })
    })
    
    # Display preview
    output$adjusted_data_preview <- renderDT({
      data_to_display <- adjusted_data_output_r()
      if (is.null(data_to_display)) {
        return(datatable(
          data.frame("Status" = "No data available. Click 'Run Allometric Correction' to process."),
          options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE)))
      }
      ids <- specimen_ids_adjusted_r()
      display_df <- if (!is.null(ids) && length(ids) == nrow(data_to_display)) {
        cbind(data.frame(SpecimenID = ids, stringsAsFactors = FALSE), data_to_display)
      } else {
        data_to_display
      }
      return(datatable(display_df, options = list(pageLength = 10, scrollX = TRUE, lengthMenu = c(10, 25, 50, 100))))
    })
    
    # Download
    output$download_adjusted_data_csv <- downloadHandler(
      filename = function() {
        paste0("combined_adjusted_data_", input$correction_type, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(adjusted_data_output_r())
        df   <- adjusted_data_output_r()
        ids  <- specimen_ids_adjusted_r()
        download_df <- if (!is.null(ids) && length(ids) == nrow(df)) {
          cbind(data.frame(SpecimenID = ids, stringsAsFactors = FALSE), df)
        } else {
          df
        }
        write.csv(download_df, file, row.names = FALSE)
      }
    )
    
    return(list(data = adjusted_data_output_r, specimen_ids = specimen_ids_adjusted_r))
  })
}