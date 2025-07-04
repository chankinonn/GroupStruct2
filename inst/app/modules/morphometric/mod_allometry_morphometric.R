
mod_allometry_ui_morphometric <- function(id) { 
  ns <- NS(id)
  
  tagList(
    h3("Allometric Correction and Log Transformation"),
    hr(),
    p(strong("Important:"), "The first column must be Group/OTU names (e.g., species or population). Avoid special characters in trait names."),
    p("Each Group/OTU must be represented by more than two individuals (to calculate mean) and missing data is not allowed. This adjustment should also be performed separately on different sexes to account for possible sexual dimorphism."),
    p("Use the multispecies option if your dataset includes more than one species or putative species. Use the multipopulation option if your dataset includes more than one population from only ONE species."),
    p("If you use this function, please cite the original paper: Chan, K. O., & Grismer, L. L. (2022). GroupStruct: An R package for allometric size correction. Zootaxa, 5124(4), 471–482."), # Removed hyperlink
    hr(),
    radioButtons(ns("correction_type"), "Select Correction Method:",
                 choiceNames = list(
                   "Multispecies",
                   "Multipopulation",
                   HTML("No correction (raw data) <span style='color:red; font-weight:bold; font-style:italic;'>*Not recommended</span>")
                 ),
                 choiceValues = list(
                   "species",
                   "population1",
                   "none"
                 ),
                 selected = "species",
                 inline = TRUE), 
    hr(),
    uiOutput(ns("body_size_selector_ui")),
    p(strong("Important:"), "Make sure you select the variable that represents body size (e.g., snout-vent-length)"),
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
        # Multi-species (species-specific body-size mean)
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
        # Multi-population (common within-group pooling)
        common_betas <- list()
        for (trait_name in trait_col_names) {
          formula_full <- as.formula(paste0("`", trait_name, "` ~ `", body_size_col_name, "`"))
          if (nrow(df_log_internal) > 1 && sd(df_log_internal[[body_size_col_name]]) > 0 && length(unique(df_log_internal[[trait_name]])) > 1) {
            lm_model_full <- lm(formula_full, data = df_log_internal)
            common_betas[[trait_name]] <- as.numeric(lm_model_full$coefficients[body_size_col_name])
          } else {
            common_betas[[trait_name]] <- 0
            warning(paste("Cannot calculate common slope for trait", trait_name, ". Assuming beta = 0 for adjustment."))
          }
        }
        
        for (current_otu in unique(data_ordered[[otu_col_name]])) {
          species_subsets_indices <- which(data_ordered[[otu_col_name]] == current_otu)
          species_subsets_log <- df_log_internal[species_subsets_indices, ]
          
          log_y_subset <- species_subsets_log[[body_size_col_name]]
          z_mean <- mean(data_ordered[[body_size_col_name]][species_subsets_indices])
          log_z_mean <- log10(z_mean)
          
          for (trait_name in trait_col_names) {
            log_x_subset <- species_subsets_log[[trait_name]]
            beta <- common_betas[[trait_name]]
            
            adjusted_df_output[species_subsets_indices, trait_name] <- log_x_subset - beta * (log_y_subset - log_z_mean)
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
    
    # Observe inputs and run correction
    observe({
      req(raw_data_r(), input$correction_type, input$body_size_col)
      
      df_raw <- raw_data_r()
      
      if (ncol(df_raw) < 3) {
        showNotification("Error: Data must have at least 3 columns (OTU, Body-size, and at least one trait) for allometric correction.", type = "error")
        adjusted_data_r(NULL)
        return()
      }
      
      adjusted_df <- tryCatch({
        allom_modified(data = df_raw,
                       type = input$correction_type,
                       body_size_col_name = input$body_size_col)
      }, error = function(e) {
        showNotification(paste("Correction failed:", e$message), type = "error")
        return(NULL)
      })
      
      adjusted_data_r(adjusted_df)
      
      if (!is.null(adjusted_df)) {
        showNotification("Correction completed successfully!", type = "default")
      }
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
