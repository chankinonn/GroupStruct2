mod_allometry_ui_morphometric <- function(id) { 
  ns <- NS(id)
  
  tagList(
    h3("Allometric Correction and Log Transformation"),
    hr(),
    p(strong("Important:"), "The first column must be Group/OTU names (e.g., species or population). Avoid special characters in trait names."),
    p("Each Group/OTU must be represented by more than two individuals (to calculate mean) and missing data is not allowed. This adjustment should also be performed separately on different sexes to account for possible sexual dimorphism."),
    p(strong("Multispecies:"), "Use this option if the OTUs in your dataset represent DIFFERENT SPECIES i.e., different geographic populations/localities have been pooled under a single OTU/species name (pooled populations). Traits are adjusted according to an OTU-specific standard size and OTU-specific regression slope. The caveat is that the OTU-specific regression slope is calculated from pooled populations, which assumes that variation among populations of the same species is homogenous and can be represented by a common regression slope. If there is substantial variation among populations of the same species, such that different populations have significantly different regression slopes, use the multipopulation option. The multispecies option is also useful when population sample size is small."),
    p(strong("Multipopulation:"), "Use this option if the OTUs in your dataset represent different populations of ONE SPECIES. If you use this option, each uploaded dataset comprises ONE SPECIES. If more than one species are to be analyzed, do them separately. The multipopulation option is used when populations within a single species vary significantly, such that they have different slopes. Because each OTU represents a population, a separate slope is calculated for each population. However, a single grand mean for body size is calculated across all populations. CAVEAT: calculating accurate population-specific slopes require large sample sizes. Low population sample sizes will skew the slope and affect the correction. In this case, consider the multispecies option that pools populations to boost sample size."),    
    p("If you use this function, please cite the original paper: Chan, K. O., & Grismer, L. L. (2022). GroupStruct: An R package for allometric size correction. Zootaxa, 5124(4), 471–482."), # Removed hyperlink
    hr(),
    radioButtons(ns("correction_type"), "Select Correction Method:",
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
                 inline = TRUE), 
    hr(),
    uiOutput(ns("body_size_selector_ui")),
    p(strong("Important:"), "Make sure you select the variable that represents body size (e.g., snout-vent-length)"),
    hr(),
    actionButton(ns("run_correction"), "Run Allometric Correction", icon = icon("play")),  
    hr(),
    br(),
    h4("Size-Corrected Data Preview"),
    DTOutput(ns("adjusted_data_preview")),
    br(),
    h4("Note on Adjusted Data:"),
    p("The 'Body-size' column (your original 2nd column) in the adjusted data is now its log10-transformed value."),
    p("Other trait columns contain their size-corrected and log10-transformed values, suitable for downdstream analyses where size effects need to be removed."),
    br(),
    downloadButton(ns("download_adjusted_data_csv"), "Download Size-adjusted Data"),
    hr()
  )
}

mod_allometry_server_morphometric <- function(id, raw_data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    adjusted_data_r <- reactiveVal(NULL) # Reactive value to store the adjusted data
    
    # Updated allom_modified to accept dynamic body_size_col_name
    allom_modified <- function(data, type, body_size_col_name = colnames(data)[2]) {
      original_col_names <- colnames(data)
      otu_col_name <- original_col_names[1]
      # Use selected body size column
      trait_col_names <- setdiff(original_col_names, c(otu_col_name, body_size_col_name))
      
      data_ordered <- data[order(data[[otu_col_name]]), ]
      
      # If no correction, return raw data directly
      if (type == "none") {
        return(data_ordered)
      }
      
      # If log10 transformation only (no allometric correction)
      if (type == "log10_only") {
        adjusted_df_output <- data_ordered
        
        # Ensure body-size column and trait columns are positive
        if (any(data_ordered[[body_size_col_name]] <= 0)) {
          stop("Error: Body-size column must contain only positive values for log transformation.")
        }
        if (any(data_ordered[, trait_col_names] <= 0)) {
          stop("Error: Trait columns must contain only positive values for log transformation.")
        }
        
        # Log-transform body-size column and all trait columns
        adjusted_df_output[[body_size_col_name]] <- log10(data_ordered[[body_size_col_name]])
        for (trait_name in trait_col_names) {
          adjusted_df_output[[trait_name]] <- log10(data_ordered[[trait_name]])
        }
        
        return(adjusted_df_output)
      }
      
      # Checks for minimum individuals per OTU (only relevant for correction types)
      if (type %in% c("species", "population1", "population2")) {
        for (i in unique(data_ordered[[otu_col_name]])) {
          if (sum(data_ordered[[otu_col_name]] == i) < 2) {
            stop(paste0("Error: For type '", type, "', OTU '", i, "' has less than 2 individuals. Each OTU must have more than one individual for allometric adjustment."))
          }
        }
      }
      
      # Ensure body-size column and trait columns are positive
      if (any(data_ordered[[body_size_col_name]] <= 0)) {
        stop("Error: Body-size column must contain only positive values for log transformation.")
      }
      if (any(data_ordered[, trait_col_names] <= 0)) {
        stop("Error: Trait columns must contain only positive values for log transformation.")
      }
      
      # Create copy of data to store adjusted values
      adjusted_df_output <- data_ordered
      
      # Log-transform body-size column in place for corrected data output (only if correction is applied)
      if (type != "none") {
        adjusted_df_output[[body_size_col_name]] <- log10(data_ordered[[body_size_col_name]])
      }
      
      # Log-transform all traits for internal calculation
      df_log_internal <- data_ordered %>%
        dplyr::mutate(dplyr::across(c(all_of(body_size_col_name), all_of(trait_col_names)), log10))
      
      if (type == "species") {
        # Multi-species (per-OTU slopes, per-OTU reference mean on original scale)
        for (current_otu in unique(data_ordered[[otu_col_name]])) {
          species_subsets_indices <- which(data_ordered[[otu_col_name]] == current_otu)
          species_subsets_log <- df_log_internal[species_subsets_indices, ]
          
          log_y_subset <- species_subsets_log[[body_size_col_name]]
          z_mean <- mean(data_ordered[[body_size_col_name]][species_subsets_indices])
          log_z_mean <- log10(z_mean)
          
          for (trait_name in trait_col_names) {
            log_x_subset <- species_subsets_log[[trait_name]]
            
            if (length(log_y_subset) > 1 && sd(log_y_subset) > 0 && length(unique(log_x_subset)) > 1) {
              lm_model <- lm(log_x_subset ~ log_y_subset)
              beta <- as.numeric(lm_model$coefficients[2])
            } else {
              beta <- 0
              warning(paste("Cannot calculate slope for trait", trait_name, "in group", current_otu, ". Assuming beta = 0 for adjustment."))
            }
            
            adjusted_df_output[species_subsets_indices, trait_name] <- log_x_subset - beta * (log_y_subset - log_z_mean)
          }
        }
        
      } else if (type == "population1") {
        # Multi-population (per-OTU slopes, single grand geometric-mean reference)
        log_z_mean <- mean(log10(data_ordered[[body_size_col_name]]), na.rm = TRUE)  # grand geometric mean on log10 scale
        
        for (current_otu in unique(data_ordered[[otu_col_name]])) {
          species_subsets_indices <- which(data_ordered[[otu_col_name]] == current_otu)
          species_subsets_log <- df_log_internal[species_subsets_indices, ]
          
          log_y_subset <- species_subsets_log[[body_size_col_name]]
          
          for (trait_name in trait_col_names) {
            log_x_subset <- species_subsets_log[[trait_name]]
            
            if (length(log_y_subset) > 1 && sd(log_y_subset) > 0 && length(unique(log_x_subset)) > 1) {
              lm_model <- lm(log_x_subset ~ log_y_subset)  # OTU-specific slope
              beta <- as.numeric(lm_model$coefficients[2])
            } else {
              beta <- 0
              warning(paste("Cannot calculate slope for trait", trait_name, "in group", current_otu, 
                            ". Assuming beta = 0 for adjustment."))
            }
            
            # Thorpe adjustment on log10 scale with common grand geometric mean
            adjusted_df_output[species_subsets_indices, trait_name] <- 
              log_x_subset - beta * (log_y_subset - log_z_mean)
          }
        }
        
      } else if (type == "population2") {
        y_full <- data_ordered[[body_size_col_name]]
        log_y_full <- log10(y_full)
        z_mean_full <- mean(y_full)
        log_z_mean_full <- log10(z_mean_full)
        
        for (trait_name in trait_col_names) {
          x_full <- data_ordered[[trait_name]]
          log_x_full <- log10(x_full)
          
          lm_model <- lm(log_x_full ~ log_y_full)
          beta <- as.numeric(lm_model$coefficients[2])
          
          adjusted_df_output[[trait_name]] <- log_x_full - beta * (log_y_full - log_z_mean_full)
        }
      }
      
      return(adjusted_df_output)
    }
    
    # Dynamic UI for body size selector
    output$body_size_selector_ui <- renderUI({
      df <- raw_data_r()
      req(df)
      
      cols <- colnames(df)
      if (length(cols) < 2) {
        return(NULL)
      }
      choices <- cols[-1] # exclude first column (OTU)
      
      radioButtons(session$ns("body_size_col"), "Select Body Size Variable:",
                   choices = choices,
                   selected = choices[1], inline = TRUE)
    })
    
    # CHANGE: Run correction only when button is clicked
    observeEvent(input$run_correction, {
      req(raw_data_r(), input$correction_type, input$body_size_col)
      
      withProgress(message = 'Running allometric correction...', value = 0, {
        
        df_raw <- raw_data_r()
        
        if (ncol(df_raw) < 3) {
          showNotification("Error: Data must have at least 3 columns (OTU, Body-size, and at least one trait) for allometric correction.", type = "error")
          adjusted_data_r(NULL)
          return()
        }
        
        incProgress(0.5, detail = "Applying correction...")
        
        adjusted_df <- tryCatch({
          allom_modified(data = df_raw,
                         type = input$correction_type,
                         body_size_col_name = input$body_size_col)
        }, error = function(e) {
          showNotification(paste("Correction failed:", e$message), type = "error")
          return(NULL)
        })
        
        incProgress(0.5, detail = "Finalizing...")
        
        adjusted_data_r(adjusted_df)
        
        if (!is.null(adjusted_df)) {
          showNotification("Allometric correction completed successfully!", type = "message")
        }
      })
    })
    
    # Display adjusted data preview
    output$adjusted_data_preview <- renderDT({
      req(adjusted_data_r())
      DT::datatable(
        adjusted_data_r(),
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          dom = 'tip'
        )
      )
    })
    
    # Download adjusted data
    output$download_adjusted_data_csv <- downloadHandler(
      filename = function() {
        paste0("morphometric_adjusted_data_", input$correction_type, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(adjusted_data_r())
        write.csv(adjusted_data_r(), file, row.names = FALSE)
      }
    )
    
    return(adjusted_data_r)
  })
}
