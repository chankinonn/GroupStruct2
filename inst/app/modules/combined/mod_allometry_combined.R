
get_color_scale <- function(palette_choices) {
  if (is.null(palette_choices)) {
    return(scales::hue_pal())
  } else if (is.function(palette_choices)) {
    return(palette_choices)
  } else if (is.character(palette_choices) && length(palette_choices) > 1) {
    return(ggplot2::scale_color_manual(values = palette_choices))
  } else {
    return(scales::hue_pal())
  }
}

mod_allometry_ui_combined <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Allometric Correction and Log Transformation"),
    hr(),
    p("This module allows you to perform allometric size correction on the morphometric traits within your combined dataset."),
    p(strong("Important:"), "The first column must be Group/OTU names (e.g., species or population). Avoid special characters in trait names."),
    p("Each Group/OTU must be represented by more than two individuals (to calculate mean) and missing data is not allowed. This adjustment should also be performed separately on different sexes to account for possible sexual dimorphism."),
    p(strong("Note: Allometric correction should only performed on morphometric data and not on meristic, categorical, or other non-morphemtric data."),style="color:red"),
    p("Use the multispecies option if your dataset includes more than one species. Use the multipopulation option if your dataset includes more than one population from only ONE species."),
    p("If you use this function, please cite the original paper: Chan, K. O., & Grismer, L. L. (2022). GroupStruct: An R package for allometric size correction. Zootaxa, 5124(4), 471–482."), 
    
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
                   HTML("No correction (raw data) <span style='color:red; font-weight:bold; font-style:italic;'>*Not recommended</span>")
                 ),
                 choiceValues = list(
                   "species",
                   "population1",
                   "none"
                 ),
                 selected = "species",
                 inline = TRUE
    ),
    
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

mod_allometry_server_combined <- function(id, raw_combined_data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$correction_type, {
      if (input$correction_type == "none") {
        updateCheckboxGroupInput(session, "morphometric_traits_to_correct", selected = character(0))
      }
    })
    
    adjusted_data_output_r <- reactiveVal(NULL)
    
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
      
      adjusted_df_output <- data_subset
      adjusted_df_output[[body_size_col_name]] <- log10(data_subset[[body_size_col_name]])
      
      df_log_internal <- data_subset %>%
        mutate(across(c(body_size_col_name, all_of(trait_col_names)), log10)) %>%
        filter(complete.cases(.))
      
      if (nrow(df_log_internal) == 0) stop("No complete cases available after log transform.")
      
      if (type == "species") {
        for (otu in unique(df_log_internal[[group_col_name]])) {
          sub_log <- df_log_internal %>% filter(!!sym(group_col_name) == otu)
          z_mean <- mean(data_subset[[body_size_col_name]][data_subset[[group_col_name]] == otu], na.rm = TRUE)
          log_z_mean <- log10(z_mean)
          
          for (trait in trait_col_names) {
            beta <- if (nrow(sub_log) > 1) {
              coef(lm(as.formula(paste0("`", trait, "` ~ `", body_size_col_name, "`")), data = sub_log))[2]
            } else {
              0
            }
            
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
    
    # Apply correction
    observe({
      req(raw_combined_data_r(), input$correction_type, input$morphometric_traits_to_correct, input$body_size_col)
      
      df_raw_full <- raw_combined_data_r()$data
      group_col_name <- raw_combined_data_r()$group_col
      body_size_col_name <- input$body_size_col
      selected_morph_traits <- input$morphometric_traits_to_correct
      
      if (input$correction_type == "none") {
        adjusted_data_output_r(df_raw_full)
        showNotification("No correction applied. Raw data returned.", type = "message")
        return()
      }
      
      non_numeric <- selected_morph_traits[!sapply(df_raw_full[, selected_morph_traits, drop = FALSE], is.numeric)]
      if (length(non_numeric) > 0) {
        showNotification("Allometric correction cannot be performed on categorical or meristic data. Please select morphometric data only.", type = "error")
        return()
      }
      
      columns_to_select <- c(group_col_name, body_size_col_name, selected_morph_traits)
      df_subset <- df_raw_full %>% dplyr::select(all_of(columns_to_select))
      
      tryCatch({
        adjusted_df <- allom_modified(
          data_subset = df_subset,
          type = input$correction_type,
          group_col_name = group_col_name,
          body_size_col_name = body_size_col_name,
          trait_col_names = selected_morph_traits
        )
        
        df_raw_full[[body_size_col_name]] <- adjusted_df[[body_size_col_name]]
        for (trait in selected_morph_traits) {
          df_raw_full[[trait]] <- adjusted_df[[trait]]
        }
        
        adjusted_data_output_r(df_raw_full)
        showNotification("Allometric Correction completed successfully! Data updated.", type = "default")
      }, error = function(e) {
        showNotification(paste("Allometric Correction failed:", e$message), type = "error")
        adjusted_data_output_r(NULL)
      })
    })
    
    # Display preview
    output$adjusted_data_preview <- renderDT({
      data_to_display <- adjusted_data_output_r()
      if (is.null(data_to_display)) {
        return(datatable(data.frame("Status" = "."), options = list(dom = 't', paging = FALSE, searching = FALSE, info = FALSE)))
      } else {
        return(datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE)))
      }
    })
    
    # Download
    output$download_adjusted_data_csv <- downloadHandler(
      filename = function() {
        paste0("combined_adjusted_data_", input$correction_type, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(adjusted_data_output_r())
        write.csv(adjusted_data_output_r(), file, row.names = FALSE)
      }
    )
    
    return(adjusted_data_output_r)
  })
}
