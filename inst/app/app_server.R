#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

source("global.R", local = TRUE)

app_server <- function(input, output, session) {
  current_module <- reactiveVal("home")
  
  observeEvent(input$go_data, current_module("data"))
  observeEvent(input$go_summary, current_module("summary"))
  observeEvent(input$go_allometry, current_module("allometry"))
  observeEvent(input$go_stats, current_module("stats"))
  observeEvent(input$go_visual, current_module("visual"))
  observeEvent(input$go_mfa, current_module("mfa"))
  observeEvent(input$go_species_delim, current_module("species_delim"))
  
  # Render example datasets for landing page
  output$example_meristic <- renderTable({
    example_meristic
  }, striped = TRUE, hover = TRUE)
  
  output$example_morphometric <- renderTable({
    df <- example_morphometric
    df$Snout_vent_Length <- sprintf("%.1f", df$Snout_vent_Length)
    df$Head_Length <- sprintf("%.1f", df$Head_Length)
    df$Snout_Length <- sprintf("%.1f", df$Snout_Length)
    df
  }, striped = TRUE, hover = TRUE)
  
  output$example_mixed <- renderTable({
    df <- example_mixed
    df$Midbody_Scale <- sprintf("%.0f", df$Midbody_Scale)
    df$Ventral_Scale <- sprintf("%.0f", df$Ventral_Scale)
    df$Snout_vent_Length <- sprintf("%.1f", df$Snout_vent_Length)
    df$Head_Length <- sprintf("%.1f", df$Head_Length)
    df
  }, striped = TRUE, hover = TRUE)
  
  
  # Reactive values to hold outputs of data input modules
  # Meristic Data
  meristic_data_output <- reactiveVal(NULL)
  
  # Reactive helper to access the actual reactive from meristic_data_output
  meristic_data_r <- reactive({ req(meristic_data_output())() })
  
  # Morphometric Data
  morphometric_data_output <- reactiveVal(NULL)
  
  # Reactive helper to access the actual reactive from morphometric_data_output
  morphometric_data_r <- reactive({
    req(morphometric_data_output())
    morphometric_data_output()()
  })
  
  # Reactive for morphometric allometry module's output
  allometry_module_output <- reactiveVal(NULL)
  adjusted_data_r <- reactive({
    req(allometry_module_output())
    allometry_module_output()()
  })
  
  # Combined Data
  data_combined_output <- reactiveVal(NULL)
  combined_data_list_r <- reactive({ req(data_combined_output())() })
  combined_data_df_r <- reactive({ req(combined_data_list_r()$data) })
  combined_data_group_col_r <- reactive({ req(combined_data_list_r()$group_col) })
  
  # Explicitly pass the group column name to visual module
  combined_group_col_name_r <- reactive({
    req(combined_data_list_r()$group_col)
    combined_data_list_r()$group_col
  })
  
  allometry_combined_output <- reactiveVal(NULL) # This reactiveVal holds the *reactive* from mod_allometry_server_combined
  
  mfa_combined_module_output <- reactiveVal(NULL)
  
  # Extract and *call* the individual reactives from the MFA module's output list
  mfa_results_for_plot_r <- reactive({
    req(mfa_combined_module_output()) 
    mfa_combined_module_output()$mfa_results_r() 
  })
  
  trait_group_df_for_plot_r <- reactive({
    
    req(mfa_combined_module_output()) 
    mfa_combined_module_output()$trait_group_df_r() 
  })
  
  # Extract hypothesis data from MFA module
  mfa_hypothesis_data_for_plot_r <- reactive({
    req(mfa_combined_module_output())
    if (!is.null(mfa_combined_module_output()$mfa_hypothesis_data_r)) {
      mfa_combined_module_output()$mfa_hypothesis_data_r()
    } else {
      NULL
    }
  })
  
  # Preparea the final data frame to be sent to the visualization module.
  # Prioritize allometry-adjusted data if available and valid, otherwise falls back to raw combined data.
  combined_data_with_adjusted_morphometrics_r <- reactive({
    message("Preparing combined data for visualization...")
    
    # Try adjusted data (module returns a reactive that yields a data.frame)
    if (!is.null(allometry_combined_output())) {
      adjusted_df <- tryCatch(
        allometry_combined_output()(),   # <-- returns a data.frame
        error = function(e) NULL
      )
      if (is.data.frame(adjusted_df)) {
        message("Using allometry-adjusted data")
        return(adjusted_df)
      }
    }
    
    # Fallback to raw combined data
    req(combined_data_df_r())
    message("Using raw combined data")
    combined_data_df_r()
  })
  
  # Reactive values for plot customization
  manual_colors_r <- reactiveVal(NULL) #  OTU-based coloring
  mfa_type_colors_r <- reactiveVal(NULL) #  MFA Variable Type coloring
  
  # Reactive to hold the currently active dataset for color initialization
  active_raw_dataset_for_colors_r <- reactive({
    if (input$data_type == "meristic") {
      meristic_data_r()
    } else if (input$data_type == "morphometric") {
      morphometric_data_r()
    } else if (input$data_type == "combined") {
      combined_data_list_r()$data
    } else {
      NULL
    }
  })
  
  # Manual colors
  observeEvent(input$`manual_color_modal-save_otu_colors`, {
    updateCheckboxInput(session, "use_manual_colors", value = TRUE)
    
    df <- active_raw_dataset_for_colors_r()
    req(df)
    
    current_otus <- unique(df[[1]])
    
    new_colors <- setNames(
      vapply(current_otus, function(otu) {
        input[[paste0("manual_color_modal-otu_color_", otu)]] %||% "#000000"
      }, character(1)),
      current_otus
    )
    
    manual_colors_r(new_colors)
    removeModal()
  })
  
  # Initialize manual colors for OTUs when active_raw_dataset_for_colors_r changes
  observeEvent(active_raw_dataset_for_colors_r(), {
    df <- active_raw_dataset_for_colors_r()
    if (!is.null(df) && nrow(df) > 0) {
      otus <- unique(df[[1]])
      default_colors <- scales::hue_pal()(length(otus))
      names(default_colors) <- otus
      manual_colors_r(default_colors)
    } else {
      manual_colors_r(NULL) # Clear colors if no data
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # Initialize MFA Type colors dynamically based on actual group names
  observeEvent(trait_group_df_for_plot_r(), { # Trigger when trait_group_df_for_plot_r changes
    trait_df <- trait_group_df_for_plot_r()
    if (!is.null(trait_df) && nrow(trait_df) > 0) {
      # Get the actual unique group names from trait definitions 
      actual_mfa_types <- unique(trait_df$Type)
      
      # Generate a set of default colors
      default_colors_for_actual_types <- scales::hue_pal()(length(actual_mfa_types))
      names(default_colors_for_actual_types) <- actual_mfa_types
      
      # Get the current state of colors
      current_mfa_colors <- mfa_type_colors_r()
      
      if (is.null(current_mfa_colors) || length(current_mfa_colors) == 0) {
        # If no colors are set yet, use the newly generated defaults
        mfa_type_colors_r(default_colors_for_actual_types)
      } else {
        # Start with new defaults, then overlay any existing user-set colors
        new_mfa_colors_map <- default_colors_for_actual_types
        for (type_name in names(current_mfa_colors)) {
          if (type_name %in% actual_mfa_types) {
            new_mfa_colors_map[type_name] <- current_mfa_colors[type_name]
          }
        }
        mfa_type_colors_r(new_mfa_colors_map)
      }
    } else {
      # If no trait groups are defined yet, or data is empty, set to NULL or a basic default
      mfa_type_colors_r(NULL)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE) # Run on init and when trait_df is updated
  
  
  # Home landing page
  observeEvent(input$reset_data_type, {
    updateSelectInput(session, "data_type", selected = "")
  })
  
  # Initialize data module servers based on data_type selection
  
  observeEvent(input$data_type, {
    if (input$data_type == "meristic") {
      if (is.null(meristic_data_output())) {
        meristic_data_output(mod_data_server_meristic("data_ui_1_meristic"))
      }
    } else if (input$data_type == "morphometric") {
      if (is.null(morphometric_data_output())) {
        morphometric_data_output(mod_data_server_morphometric("data_ui_1_morphometric"))
      }
    } else if (input$data_type == "combined") {
      if (is.null(data_combined_output())) {
        data_combined_output(mod_data_combined_server("data_ui_1_combined"))
      }
    }
    
    # Reset to landing/home page every time data_type is changed
    current_module("home")
    
  }, ignoreInit = FALSE)
  
  
  # Meristic module servers
  # Modules will only be initialized and re-run when meristic_data_r() has data
  observe({
    req(input$data_type == "meristic")
    req(meristic_data_r())
    
    mod_summary_server_meristic("summary_ui_1_meristic", meristic_data_r)
    mod_inferential_server_meristic("inferential_ui_1_meristic", meristic_data_r)
    
    mod_visual_server_meristic(
      "visual_ui_1_meristic",
      dataset = meristic_data_r,
      plot_palette = reactive(input[["visual_ui_1_meristic-plot_palette"]]),
      plot_axis_text_size = reactive(input[["visual_ui_1_meristic-plot_axis_text_size"]]),
      plot_axis_label_size = reactive(input[["visual_ui_1_meristic-plot_axis_label_size"]]),
      plot_x_angle = reactive(input[["visual_ui_1_meristic-plot_x_angle"]]),
      plot_facet_size = reactive(input[["visual_ui_1_meristic-plot_facet_size"]]),
      legend_text_size = reactive(input[["visual_ui_1_meristic-legend_text_size"]]),
      legend_title_size = reactive(input[["visual_ui_1_meristic-legend_title_size"]]),
      manual_colors_r = manual_colors_r
    )
  })
  
  # Morphometric module servers
  
  # Capture the results from species delimitation module
  species_delim_output <- reactiveVal(NULL)
  
  observe({
    req(input$data_type == "morphometric")
    req(morphometric_data_r()) # Ensure raw data is loaded
    
    mod_summary_server_morphometric("summary_ui_1_morphometric", morphometric_data_r)
    
    # Initialize allometry, which will return a reactive with adjusted data
    if (is.null(allometry_module_output())) { # Prevent re-running if module is already assigned
      allometry_module_output(mod_allometry_server_morphometric("allometry_ui_1_morphometric", morphometric_data_r))
    }
    
    # Determine which data to pass to inferential stats, visual, AND species delimitation
    stats_visual_data_source_r <- reactive({
      # Add species_delim to the list of modules that use adjusted data
      if (current_module() %in% c("stats", "visual", "species_delim") && !is.null(adjusted_data_r())) {
        # Check if adjusted_data_r() actually has data and is a data.frame
        if (is.data.frame(adjusted_data_r()) && nrow(adjusted_data_r()) > 0) {
          adjusted_data_r()
        } else {
          morphometric_data_r()
        }
      } else {
        morphometric_data_r()
      }
    })
    
    mod_inferential_server_morphometric("inferential_ui_1_morphometric", stats_visual_data_source_r)
    
    # Remove the if (is.null(...)) check to allow re-initialization when data changes
    # This will make it reactive to allometry corrections
    species_delim_output(mod_species_delim_server("species_delim_ui_1", stats_visual_data_source_r))
    
    mod_visual_server_morphometric(
      "visual_ui_1_morphometric",
      dataset = stats_visual_data_source_r,
      plot_palette = reactive(input[["visual_ui_1_morphometric-plot_palette"]]),
      plot_axis_text_size = reactive(input[["visual_ui_1_morphometric-plot_axis_text_size"]]),
      plot_axis_label_size = reactive(input[["visual_ui_1_morphometric-plot_axis_label_size"]]),
      plot_x_angle = reactive(input[["visual_ui_1_morphometric-plot_x_angle"]]),
      plot_facet_size = reactive(input[["visual_ui_1_morphometric-plot_facet_size"]]),
      legend_text_size = reactive(input[["visual_ui_1_morphometric-legend_text_size"]]),
      legend_title_size = reactive(input[["visual_ui_1_morphometric-legend_title_size"]]),
      manual_colors_r = manual_colors_r,
      species_delim_results_r = reactive({
        if (!is.null(species_delim_output())) {
          species_delim_output()()
        } else {
          NULL
        }
      }),
      
      boruta_results_r = reactive({
        if (!is.null(species_delim_output())) {
          results <- species_delim_output()()
          if (!is.null(results$boruta)) {
            results$boruta  # Changed from results$boruta_results to results$boruta
          } else {
            NULL
          }
        } else {
          NULL
        }
      })
    )
  })
  
  # Combined module servers
  observe({
    req(input$data_type == "combined")
    req(combined_data_list_r()) # Ensure raw data is loaded
    
    mod_summary_server_combined("summary_ui_1_combined", combined_data_list_r)
    
    #mod_inferential_server_combined("inferential_ui_1_combined", combined_data_list_r)
    
    if (is.null(allometry_combined_output())) {
      allometry_combined_output(mod_allometry_server_combined("allometry_ui_1_combined", combined_data_list_r))
    }
    
    if (is.null(mfa_combined_module_output())) {
      mfa_combined_module_output(
        mod_mfa_server("mfa_ui_1_combined", combined_data_list_r, allometry_combined_output())
      )
    }
    
    mod_visual_server_combined(
      "visual_ui_1_combined",
      dataset_r = combined_data_with_adjusted_morphometrics_r,
      group_col_name_r = combined_group_col_name_r,
      mfa_results_r = mfa_results_for_plot_r,
      trait_group_df_r = trait_group_df_for_plot_r,
      plot_palette = reactive(input[["visual_ui_1_combined-plot_palette"]]),
      plot_axis_text_size = reactive(input[["visual_ui_1_combined-plot_axis_text_size"]]),
      plot_axis_label_size = reactive(input[["visual_ui_1_combined-plot_axis_label_size"]]),
      plot_x_angle = reactive(input[["visual_ui_1_combined-plot_x_angle"]]),
      plot_facet_size = reactive(input[["visual_ui_1_combined-plot_facet_size"]]),
      legend_text_size = reactive(input[["visual_ui_1_combined-legend_text_size"]]),
      legend_title_size = reactive(input[["visual_ui_1_combined-legend_title_size"]]),
      mfa_point_size = reactive(input[["visual_ui_1_combined-mfa_point_size"]]),
      mfa_point_shape = reactive(input[["visual_ui_1_combined-mfa_point_shape"]]),
      mfa_ellipse = reactive(input[["visual_ui_1_combined-mfa_ellipse"]]),
      mfa_ellipse_alpha = reactive(input[["visual_ui_1_combined-mfa_ellipse_alpha"]]),
      manual_colors_r = manual_colors_r,
      mfa_type_colors_r = mfa_type_colors_r,
      mfa_hypothesis_data_r = mfa_hypothesis_data_for_plot_r  # ADD THIS LINE
    )
    
  })
  
  
  # Observer to manage active button styling
  observe({
    all_button_ids <- c("go_data", "go_summary", "go_allometry", "go_mfa", "go_visual", "go_stats","go_species_delim")
    active_mod <- current_module()
    
    for (btn_id in all_button_ids) {
      if (!is.null(input[[btn_id]])) {
        if (btn_id == paste0("go_", active_mod)) {
          shinyjs::addClass(id = btn_id, class = "module-active")
        } else {
          shinyjs::removeClass(id = btn_id, class = "module-active")
        }
      }
    }
  })
  
  # Conditional UI for Allometric Correction button
  output$allometry_button_ui <- renderUI({
    req(input$data_type)
    if (input$data_type %in% c("morphometric", "combined")) {
      actionButton("go_allometry", "Allometric Correction", width = "100%")
    } else {
      NULL
    }
  })
  
  
  observe({
    req(input$data_type)
    
    ns_visual_target <- switch(input$data_type,
                               "meristic" = NS("visual_ui_1_meristic"),
                               "morphometric" = NS("visual_ui_1_morphometric"),
                               "combined" = NS("visual_ui_1_combined")
    )
    
    palette_val <- input[[ns_visual_target("plot_palette")]]
    button_div_id <- ns_visual_target("manual_colors_button_div")
    
    if (!is.null(palette_val) && palette_val == "manual") {
      shinyjs::show(id = button_div_id)
    } else {
      shinyjs::hide(id = button_div_id)
    }
  })
  
  output$stats_button_ui <- renderUI({
    req(input$data_type)
    if (input$data_type %in% c("meristic", "morphometric")) {
      actionButton("go_stats", "Inferential Statistics", width = "100%")
    } else {
      NULL # Hide for combined data type
    }
  })
  
  #output$stats_button_ui <- renderUI({
  #  req(input$data_type)
  # Show button for all data types
  #  actionButton("go_stats", "Inferential Statistics", width = "100%")
  #})
  
  # Conditional UI for Multiple Factor Analysis button
  output$mfa_button_ui <- renderUI({
    req(input$data_type)
    if (input$data_type == "combined") {
      actionButton("go_mfa", "Multiple Factor Analysis", width = "100%")
    } else {
      NULL
    }
  })
  
  # Conditional UI for Species Delimitation button (morphometric only)
  output$species_delim_button_ui <- renderUI({
    req(input$data_type)
    if (input$data_type == "morphometric") {
      actionButton("go_species_delim", "Morphometric Delimitation", width = "100%")
    } else {
      NULL
    }
  })
  
  # UI for module content
  output$module_ui <- renderUI({
    if (is.null(input$data_type) || input$data_type == "") {
      landing_page_ui()
    } else {
      switch(input$data_type,
             
             meristic = switch(current_module(),
                               home = mod_home_ui_meristic("home_ui_1_meristic"),
                               data = mod_data_ui_meristic("data_ui_1_meristic"),
                               summary = mod_summary_ui_meristic("summary_ui_1_meristic"),
                               stats = mod_inferential_ui_meristic("inferential_ui_1_meristic"),
                               visual = mod_visual_ui_meristic("visual_ui_1_meristic")),
             
             morphometric = switch(current_module(),
                                   home = mod_home_ui_morphometric("home_ui_1_morphometric"),
                                   data = mod_data_ui_morphometric("data_ui_1_morphometric"),
                                   summary = mod_summary_ui_morphometric("summary_ui_1_morphometric"),
                                   allometry = mod_allometry_ui_morphometric("allometry_ui_1_morphometric"),
                                   stats = mod_inferential_ui_morphometric("inferential_ui_1_morphometric"),
                                   species_delim = mod_species_delim_ui("species_delim_ui_1"), 
                                   visual = mod_visual_ui_morphometric("visual_ui_1_morphometric")),
             
             combined = switch(current_module(),
                               home = mod_home_ui_combined("home_ui_1_combined"),
                               data = mod_data_combined_ui("data_ui_1_combined"),
                               summary = mod_summary_ui_combined("summary_ui_1_combined"),
                               allometry = mod_allometry_ui_combined("allometry_ui_1_combined"),
                               stats = mod_inferential_ui_combined("stats_ui_1_combined"),
                               mfa = mod_mfa_ui("mfa_ui_1_combined"),
                               visual = mod_visual_ui_combined("visual_ui_1_combined"))
      )
    }
  })
  
  # Dynamic UI for customization based on active visual tab
  output$visual_customization_ui <- renderUI({
    req(input$go_visual > 0, current_module() == "visual")
    
    ns_visual_target <- if (input$data_type == "meristic") {
      NS("visual_ui_1_meristic")
    } else if (input$data_type == "morphometric") {
      NS("visual_ui_1_morphometric")
    } else if (input$data_type == "combined") {
      NS("visual_ui_1_combined")
    } else {
      return(NULL)
    }
    
    tab <- input[[ns_visual_target("visual_tab")]]
    if (is.null(tab)) return(NULL)
    
    brewer_info <- RColorBrewer::brewer.pal.info
    brewer_qual <- rownames(brewer_info[brewer_info$category == "qual", ])
    brewer_seq  <- rownames(brewer_info[brewer_info$category == "seq", ])
    brewer_div  <- rownames(brewer_info[brewer_info$category == "div", ])
    
    wellPanel(
      id = ns_visual_target("plot_settings_panel"),
      #h4("Plot Settings"),
      #hr(),
      # Only show plot_palette for non-MFA var contribs if combined
      # Or for all plots if not combined
      if (!(input$data_type == "combined" && tab == "MFA: Variable Contributions")) {
        tagList(
          
          selectInput(ns_visual_target("plot_theme"), "Select Plot Theme:",
                      choices = c(
                        "Classic" = "theme_classic",
                        "Minimal" = "theme_minimal",
                        "Light" = "theme_light",
                        "Dark" = "theme_dark",
                        "Void" = "theme_void",
                        "Grey" = "theme_grey",
                        "BW" = "theme_bw"
                      ),
                      selected = isolate(input[[ns_visual_target("plot_theme")]]) %||% "theme_classic"
          ),
          
          selectInput(ns_visual_target("plot_palette"), "Select Color Palette:",
                      choices = list(
                        "Viridis Palettes" = setNames(
                          paste0("viridis:", c("viridis", "magma", "plasma", "inferno", "cividis", "turbo", "rocket", "mako")),
                          str_to_title(c("viridis", "magma", "plasma", "inferno", "cividis", "turbo", "rocket", "mako"))
                        ),
                        "RColorBrewer (Qualitative)" = setNames(paste0("brewer:", brewer_qual), brewer_qual),
                        "RColorBrewer (Sequential)"  = setNames(paste0("brewer:", brewer_seq), brewer_seq),
                        "RColorBrewer (Diverging)"   = setNames(paste0("brewer:", brewer_div), brewer_div),
                        "Colorblind" = c(
                          "Colorblind (Set2)" = "colorblind:Set2",
                          "Colorblind (Dark2)" = "colorblind:Dark2",
                          "Colorblind (Wong)" = "ggthemes:colorblind",
                          "Manual (Custom)" = "manual"
                        )
                      ),
                      selected = isolate(input[[ns_visual_target("plot_palette")]]) %||% "viridis:viridis"
          ),
          
          div(
            id = ns_visual_target("manual_colors_button_div"),
            style = "margin-bottom: 15px; display: none;",
            actionButton(ns_visual_target("open_manual_colors_modal"), "Manual Colors", class = "btn-primary"),
          )
        )
      },
      
      numericInput(ns_visual_target("plot_axis_text_size"), "Axis Text Size:", value = 10, min = 6, max = 20),
      numericInput(ns_visual_target("plot_axis_label_size"), "Axis Label Size:", value = 12, min = 8, max = 24),
      
      numericInput(ns_visual_target("legend_text_size"), "Legend Text Size:", value = 10, min = 6, max = 20),
      numericInput(ns_visual_target("legend_title_size"), "Legend Title Size:", value = 12, min = 8, max = 24),
      
      if (input$data_type %in% c("meristic", "morphometric")) {
        tagList(
          numericInput(ns_visual_target("plot_facet_size"), "Facet Label Size:", value = 14, min = 8, max = 24),
          sliderInput(ns_visual_target("plot_x_angle"), "X-axis Label Angle:", min = 0, max = 90, value = 0),
        )
      } else if (input$data_type == "combined") {
        conditionalPanel(
          condition = paste0("input['", ns_visual_target("visual_tab"), "'] == 'Boxplot' || input['", ns_visual_target("visual_tab"), "'] == 'Violin Plot' || input['", ns_visual_target("visual_tab"), "'] == 'Categorical Bar Plots'"),
          numericInput(ns_visual_target("plot_facet_size"), "Facet Label Size:", value = 14, min = 8, max = 24),
          sliderInput(ns_visual_target("plot_x_angle"), "X-axis Label Angle:", min = 0, max = 90, value = 0)
        )
      }
    )
  })
  
  
  # Toggle manual colors button visibility for OTU colors
  observe({
    req(input$data_type, current_module() == "visual")
    
    # Get the correct namespace based on data type
    ns_visual_target <- switch(input$data_type,
                               "meristic" = NS("visual_ui_1_meristic"),
                               "morphometric" = NS("visual_ui_1_morphometric"),
                               "combined" = NS("visual_ui_1_combined")
    )
    
    # Get the current palette
    palette_val <- input[[ns_visual_target("plot_palette")]]
    
    # For combined data, watch the tab changes
    if (input$data_type == "combined") {
      # This makes the observer react to tab changes
      current_tab <- input[[ns_visual_target("visual_tab")]]
      req(current_tab)
    }
    
    # Only show if palette is "manual" and not NA/NULL
    show_button <- !is.null(palette_val) && palette_val == "manual"
    
    # Toggle visibility
    shinyjs::toggle(
      id = ns_visual_target("manual_colors_button_div"),
      condition = show_button
    )
  })
  
  # Enable/disable X-axis angle and facet size based on tab and data type
  observe({
    ns_visual_target <- if (input$data_type == "meristic") {
      NS("visual_ui_1_meristic")
    } else if (input$data_type == "morphometric") {
      NS("visual_ui_1_morphometric")
    } else if (input$data_type == "combined") {
      NS("visual_ui_1_combined")
    } else {
      return(NULL)
    }
    
    current_tab <- input[[ns_visual_target("visual_tab")]]
    if (is.null(current_tab)) return(NULL)
    
    x_angle_id <- ns_visual_target("plot_x_angle")
    facet_size_id <- ns_visual_target("plot_facet_size")
    
    if (input$data_type %in% c("meristic", "morphometric")) {
      tryCatch({ shinyjs::enable(x_angle_id) }, error = function(e){})
      tryCatch({ shinyjs::enable(facet_size_id) }, error = function(e){})
    } else if (input$data_type == "combined") {
      if (current_tab %in% c("Boxplot", "Violin Plot", "Categorical Bar Plots")) {
        tryCatch({ shinyjs::enable(x_angle_id) }, error = function(e){})
        tryCatch({ shinyjs::enable(facet_size_id) }, error = function(e){})
      } else {
        tryCatch({ shinyjs::disable(x_angle_id) }, error = function(e){})
        tryCatch({ shinyjs::disable(facet_size_id) }, error = function(e){})
      }
    }
  })
  
  # Manual color observers for OTU colors
  observeEvent(input[[if (input$data_type == "meristic") NS("visual_ui_1_meristic")("open_manual_colors_modal")
                      else if (input$data_type == "morphometric") NS("visual_ui_1_morphometric")("open_manual_colors_modal")
                      else NS("visual_ui_1_combined")("open_manual_colors_modal")]], {
                        df <- active_raw_dataset_for_colors_r()
                        req(df)
                        current_otus <- unique(df[[1]])
                        current_manual_colors <- manual_colors_r()
                        
                        if (is.null(current_manual_colors) || length(current_manual_colors) == 0) {
                          current_manual_colors <- setNames(scales::hue_pal()(length(current_otus)), current_otus)
                        } else {
                          missing_otus <- setdiff(current_otus, names(current_manual_colors))
                          if (length(missing_otus) > 0) {
                            new_colors <- setNames(scales::hue_pal()(length(missing_otus)), missing_otus)
                            current_manual_colors <- c(current_manual_colors, new_colors)
                          }
                          current_manual_colors <- current_manual_colors[current_otus] # Preserve order
                        }
                        
                        
                        color_pickers <- lapply(current_otus, function(otu) {
                          colourInput(
                            inputId = NS("manual_color_modal")(paste0("otu_color_", otu)),
                            label = paste0("Color for ", otu),
                            value = current_manual_colors[otu]
                          )
                        })
                        
                        
                        showModal(modalDialog(
                          title = "Set Manual Colors for OTUs",
                          do.call(tagList, color_pickers),
                          footer = tagList(
                            actionButton(NS("manual_color_modal")("save_otu_colors"), "Save Colors", class = "btn-success"), # Renamed ID
                            modalButton("Cancel")
                          ),
                          size = "l",
                          easyClose = TRUE
                        ))
                      })
  
  
  observeEvent(input$plot_palette, {
    if (!is.null(input$plot_palette) && !startsWith(input$plot_palette, "manual")) {
      updateCheckboxInput(session, "use_manual_colors", value = FALSE)
    }
  })
  
  # Manual color observers for MFA Variable Type colors
  observeEvent(input[[NS("visual_ui_1_combined")("open_mfa_type_colors_modal")]], {
    # Dynamically define the types for MFA Variable Contributions plot from actual data
    req(trait_group_df_for_plot_r())
    mfa_types <- unique(trait_group_df_for_plot_r()$Type)
    if ("Unknown" %in% levels(trait_group_df_for_plot_r()$Type) || any(is.na(trait_group_df_for_plot_r()$Type))) {
      mfa_types <- unique(c(mfa_types, "Unknown"))
    }
    
    current_mfa_type_colors <- mfa_type_colors_r()
    
    # Ensure all current types have a color, fall back to a default if missing
    if (is.null(current_mfa_type_colors) || length(current_mfa_type_colors) == 0) {
      current_mfa_type_colors <- setNames(scales::hue_pal()(length(mfa_types)), mfa_types)
    } else {
      existing_colors <- current_mfa_type_colors
      current_mfa_type_colors <- setNames(scales::hue_pal()(length(mfa_types)), mfa_types) # Start with defaults
      current_mfa_type_colors[names(existing_colors)] <- existing_colors # Overlay existing custom colors
    }
    
    # Create color pickers for each actual MFA type
    color_pickers_mfa_types <- lapply(mfa_types, function(type) {
      colourInput(
        inputId = NS("mfa_type_color_modal")(paste0("color_mfa_type_", type)),
        label = paste0("Color for '", type, "' Variables"),
        value = current_mfa_type_colors[type] %||% "#CCCCCC" # Fallback if specific type is missing a color
      )
    })
    
    showModal(modalDialog(
      title = "Set Manual Colors for MFA Variable Types",
      do.call(tagList, color_pickers_mfa_types),
      footer = tagList(
        actionButton(NS("mfa_type_color_modal")("save_mfa_type_colors"), "Save Colors", class = "btn-success"),
        modalButton("Cancel")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input[["mfa_type_color_modal-save_mfa_type_colors"]], {
    req(trait_group_df_for_plot_r()) # Ensure data is available to get types
    mfa_types <- unique(trait_group_df_for_plot_r()$Type)
    if ("Unknown" %in% levels(trait_group_df_for_plot_r()$Type) || any(is.na(trait_group_df_for_plot_r()$Type))) {
      mfa_types <- unique(c(mfa_types, "Unknown"))
    }
    
    # Collect the new colors from the input fields
    new_mfa_colors <- sapply(mfa_types, function(type) {
      input[[NS("mfa_type_color_modal")(paste0("color_mfa_type_", type))]]
    }, USE.NAMES = TRUE)
    
    # Update the reactive value with the new colors
    mfa_type_colors_r(new_mfa_colors)
    removeModal()
  })
  
}
