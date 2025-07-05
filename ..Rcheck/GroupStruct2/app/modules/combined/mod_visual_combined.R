
mod_visual_ui_combined <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Data Visualization"),
    hr(),
    tabsetPanel(id = ns("visual_tab"),
                tabPanel("Scatterplot",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("plot_scatter"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("plot_scatter_height"), "Plot Height (px)", value = 500, min = 200, step = 50),
                                  numericInput(ns("plot_scatter_width"), "Plot Width (px)", value = 700, min = 200, step = 50),
                                  hr(),
                                  uiOutput(ns("scatter_controls")),
                                  checkboxInput(ns("scatter_show_labels"), "Show Individual Labels", value = FALSE),
                                  checkboxInput(ns("scatter_show_lm"), "Show Regression Line", value = TRUE),
                                  checkboxInput(ns("scatter_show_lm_se"), "Show Confidence Interval", value = TRUE),
                                  hr(),
                                  downloadButton(ns("download_scatter_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_scatter_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("Boxplot",
                         fluidRow( 
                           column(9, 
                                  plotOutput(ns("plot_box"))
                           ),
                           column(3, 
                                  br(),
                                  numericInput(ns("plot_box_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  numericInput(ns("plot_box_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '150px'),
                                  hr(),
                                  uiOutput(ns("box_variable_selector")),
                                  uiOutput(ns("box_group_selector")),
                                  hr(),
                                  downloadButton(ns("download_box_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_box_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("Violin Plot",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("plot_violin"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("plot_violin_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  numericInput(ns("plot_violin_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '150px'),
                                  hr(),
                                  uiOutput(ns("violin_variable_selector")),
                                  uiOutput(ns("violin_group_selector")),
                                  hr(),
                                  downloadButton(ns("download_violin_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_violin_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("MFA: Eigenvalues",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("mfa_eigen_plot"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("mfa_eigen_plot_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '100%'), # Added width = '100%'
                                  numericInput(ns("mfa_eigen_plot_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '100%'), # ADDED PLOT WIDTH
                                  hr(),
                                  downloadButton(ns("download_mfa_eigen_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_mfa_eigen_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("MFA: Individuals",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("mfa_individuals_plot"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("mfa_individuals_plot_height"), "Plot Height (px)", value = 500, min = 300, step = 50, width = '150px'), 
                                  numericInput(ns("mfa_individuals_plot_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px'), 
                                  hr(),
                                  numericInput(ns("mfa_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                  checkboxInput(ns("mfa_outline_points"), "Outline Points", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("mfa_outline_points")),
                                    sliderInput(ns("mfa_point_stroke"), "Point Outline Width", min = 0, max = 5, value = 1, step = 1, width = '150px')
                                  ),
                                  checkboxInput(ns("mfa_centroids"), "Group Centroids", value = FALSE),
                                  checkboxInput(ns("mfa_ellipse"), "95% Confidence Ellipses", value = FALSE),
                                  checkboxInput(ns("mfa_convex_hull"), "Convex Hulls", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("mfa_outline_shapes")),
                                    sliderInput(ns("mfa_outline_stroke"), "Ellipse/Hull Outline Width", min = 0, max = 5, value = 1, step = 1, width = '150px')
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s'] || input['%s']", ns("mfa_ellipse"), ns("mfa_convex_hull")),
                                    checkboxInput(ns("mfa_outline_shapes"), "Outline Ellipse or Hull", value = FALSE)
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s'] || input['%s']", ns("mfa_ellipse"), ns("mfa_convex_hull")),
                                    sliderInput(ns("mfa_ellipse_alpha"), "Ellipse/Hull Fill Transparency", min = 0, max = 1, value = 0.4, step = 0.05, width = '150px')
                                  ),
                                  downloadButton(ns("download_mfa_individuals_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_mfa_individuals_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("MFA: Group Contributions",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("mfa_group_contrib_hist"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("mfa_group_contrib_hist_height"), "Plot Height (px)", value = 800, min = 400, step = 50, width = '100%'),
                                  numericInput(ns("mfa_group_contrib_hist_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '100%'),
                                  hr(),
                                  h5("MFA Group Contributions Settings"),
                                  actionButton(ns("open_mfa_type_colors_modal"), "Set Manual Colors", class = "btn-primary"),
                                  br(), hr(),
                                  downloadButton(ns("download_mfa_group_contrib_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_mfa_group_contrib_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("MFA: Variable Contributions",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("mfa_var_contrib_hist"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("mfa_var_contrib_hist_height"), "Plot Height (px)", value = 800, min = 400, step = 50, width = '100%'),
                                  numericInput(ns("mfa_var_contrib_hist_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '100%'),
                                  hr(),
                                  downloadButton(ns("download_mfa_var_contrib_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_mfa_var_contrib_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                )
    )
  )
}


mod_visual_server_combined <- function(id, dataset_r,
                                       mfa_results_r,
                                       trait_group_df_r,
                                       group_col_name_r,
                                       plot_palette, plot_axis_text_size,
                                       plot_axis_label_size, plot_x_angle, plot_facet_size,
                                       legend_text_size, legend_title_size,
                                       mfa_point_size, mfa_point_shape, mfa_ellipse,
                                       mfa_ellipse_alpha,
                                       manual_colors_r, 
                                       mfa_type_colors_r 
) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get manual colors from the passed reactive (for OTUs)
    get_manual_colors <- reactive({
      req(manual_colors_r()) # Require the manual_colors_r to be ready
      manual_colors_r()
    })
    
    # Separate color control for Variable Contributions plot
    get_mfa_var_type_colors <- reactive({
      req(mfa_type_colors_r())  # Temporarily still using same source
      mfa_type_colors_r()
    })
    
    # Get MFA Type colors from the passed reactive
    get_mfa_type_colors <- reactive({
      req(mfa_type_colors_r())
      mfa_type_colors_r()
    })
    
    ##Scatterplot
    output$scatter_controls <- renderUI({
      req(dataset_r(), group_col_name_r())
      df <- dataset_r()
      group_col <- group_col_name_r()
      
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      x_choices <- setdiff(numeric_vars, group_col)
      
      tagList(
        # X Variable Dropdown 
        selectInput(ns("scatter_xvar"), "X Variable", choices = x_choices, selected = x_choices[1]),
        
        # Y Variable Dropdown 
        selectInput(ns("scatter_yvar"), "Y Variable", choices = x_choices, selected = x_choices[2]),
        
        # Groups to Plot Checkboxes
        checkboxGroupInput(ns("scatter_group_filter"), "Select Groups to Plot:",
                           choices = unique(df[[group_col]]), selected = unique(df[[group_col]]))
      )
    })
    
    # Add variable selection for box and violin plots
    output$box_variable_selector <- renderUI({
      req(dataset_r())
      # Get only numeric columns from the dataset, excluding the group column
      numeric_cols <- names(dataset_r())[sapply(dataset_r(), is.numeric)]
      group_var <- group_col_name_r()
      traits_to_offer <- setdiff(numeric_cols, group_var)
      
      checkboxGroupInput(ns("selected_box_traits"), "Select Traits to Plot:",
                         choices = traits_to_offer, selected = traits_to_offer)
    })
    
    # Group filter for boxplot
    output$box_group_selector <- renderUI({
      req(dataset_r(), group_col_name_r())
      groups <- unique(dataset_r()[[group_col_name_r()]])
      checkboxGroupInput(ns("selected_box_groups"), "Select Groups to Plot:",
                         choices = groups, selected = groups)
    })
    
    output$violin_variable_selector <- renderUI({
      req(dataset_r())
      # Get only numeric columns from the dataset, excluding the group column
      numeric_cols <- names(dataset_r())[sapply(dataset_r(), is.numeric)]
      group_var <- group_col_name_r()
      traits_to_offer <- setdiff(numeric_cols, group_var)
      
      checkboxGroupInput(ns("selected_violin_traits"), "Select Traits to Plot:",
                         choices = traits_to_offer, selected = traits_to_offer)
    })
    
    # Group filter for violin plot
    output$violin_group_selector <- renderUI({
      req(dataset_r(), group_col_name_r())
      groups <- unique(dataset_r()[[group_col_name_r()]])
      checkboxGroupInput(ns("selected_violin_groups"), "Select Groups to Plot:",
                         choices = groups, selected = groups)
    })
    
    
    ## ggplot themes
    get_ggplot_theme <- function(theme_name) {
      switch(theme_name,
             "theme_classic" = ggplot2::theme_classic(),
             "theme_minimal" = ggplot2::theme_minimal(),
             "theme_light"   = ggplot2::theme_light(),
             "theme_dark"    = ggplot2::theme_dark(),
             "theme_void"    = ggplot2::theme_void(),
             "theme_bw"      = ggplot2::theme_bw(),
             "theme_grey"    = ggplot2::theme_grey(),
             ggplot2::theme_classic() # fallback
      )
    }
    
    # Choose fill scale for OTU-based plots
    get_fill_scale_otu <- function(palette_name) {
      if (palette_name == "manual") {
        req(get_manual_colors())
        return(ggplot2::scale_fill_manual(values = get_manual_colors()))
      }
      
      if (startsWith(palette_name, "viridis:")) {
        pal <- sub("viridis:", "", palette_name)
        return(ggplot2::scale_fill_viridis_d(option = pal))
      }
      
      if (startsWith(palette_name, "brewer:")) {
        pal <- sub("brewer:", "", palette_name)
        return(ggplot2::scale_fill_brewer(palette = pal))
      }
      
      if (startsWith(palette_name, "colorblind:")) {
        pal <- sub("colorblind:", "", palette_name)
        return(ggplot2::scale_fill_brewer(palette = pal))
      }
      
      if (startsWith(palette_name, "ggthemes:")) {
        pal <- sub("ggthemes:", "", palette_name)
        if (pal == "colorblind") {
          return(ggthemes::scale_fill_colorblind())
        }
      }
      
      # fallback
      return(ggplot2::scale_fill_discrete())
    }
    
    
    # Choose color scale for OTU-based plots (for PCA, etc.)
    get_color_scale_otu <- function(palette_name) {
      if (palette_name == "manual") {
        req(get_manual_colors())
        return(ggplot2::scale_color_manual(values = get_manual_colors()))
      }
      
      if (startsWith(palette_name, "viridis:")) {
        pal <- sub("viridis:", "", palette_name)
        return(ggplot2::scale_color_viridis_d(option = pal))
      }
      
      if (startsWith(palette_name, "brewer:")) {
        pal <- sub("brewer:", "", palette_name)
        return(ggplot2::scale_color_brewer(palette = pal))
      }
      
      if (startsWith(palette_name, "colorblind:")) {
        pal <- sub("colorblind:", "", palette_name)
        return(ggplot2::scale_color_brewer(palette = pal))
      }
      
      if (startsWith(palette_name, "ggthemes:")) {
        pal <- sub("ggthemes:", "", palette_name)
        if (pal == "colorblind") {
          return(ggthemes::scale_color_colorblind())
        }
      }
      
      # fallback
      return(ggplot2::scale_color_discrete())
    }
  
    # Theme generator (always classic, adjusted for x-axis label angle)
    get_custom_theme <- reactive({
      axis_text_size_val <- plot_axis_text_size()
      axis_label_size_val <- plot_axis_label_size()
      x_angle_val <- plot_x_angle()
      facet_size_val <- plot_facet_size()
      legend_text_size_val <- legend_text_size()
      legend_title_size_val <- legend_title_size()
      
      theme_choice <- input$plot_theme
      
      # Adjust vjust and hjust for x-axis text based on angle
      x_vjust <- 0.5
      x_hjust <- 0.5
      if (x_angle_val == 45) {
        x_hjust <- 1
        x_vjust <- 1
      } else if (x_angle_val == 90) {
        x_hjust <- 1
        x_vjust <- 0.5
      }
      
      theme_base <- get_ggplot_theme(theme_choice)
      
      theme_elements <- list(
        axis.text.x = ggplot2::element_text(size = axis_text_size_val, angle = as.numeric(x_angle_val),
                                            vjust = x_vjust, hjust = x_hjust),
        axis.text.y = ggplot2::element_text(size = axis_text_size_val), # Y-axis text not angled
        axis.title = ggplot2::element_text(size = axis_label_size_val),
        strip.text = ggplot2::element_text(size = facet_size_val),
        
        legend.position = "right",
        legend.text = ggplot2::element_text(size = legend_text_size_val),
        legend.title = ggplot2::element_text(size = legend_title_size_val, face = "bold")
      )
      
      theme_base + do.call(ggplot2::theme, theme_elements)
    })
    
    # Reactive to check if common plot inputs (excluding palette if specific to MFA types) are ready
    common_plot_inputs_ready <- reactive({
      req(plot_axis_text_size(), plot_axis_label_size(),
          plot_x_angle(), plot_facet_size(),
          legend_text_size(), legend_title_size(),input$plot_theme)
      TRUE
    })
    
    
    # Scatterplot
    plot_scatter_obj <- reactive({
      req(dataset_r(), input$scatter_xvar, input$scatter_yvar, input$scatter_group_filter, group_col_name_r())
      df <- dataset_r()
      group_col <- group_col_name_r()
      
      df <- df[df[[group_col]] %in% input$scatter_group_filter, ]
      
      p <- ggplot(df, aes_string(x = input$scatter_xvar, y = input$scatter_yvar, color = group_col)) +
        geom_point(size = 3, alpha = 0.8)
      
      if (isTRUE(input$scatter_show_lm)) {
        p <- p + geom_smooth(method = "lm", se = isTRUE(input$scatter_show_lm_se), linetype = "solid")
      }
      
      if (isTRUE(input$scatter_show_labels)) {
        p <- p + geom_text(aes(label = rownames(df)), hjust = 1.1, vjust = 1.1, size = 3, check_overlap = TRUE)
      }
      
      p +
        get_color_scale_otu(plot_palette()) +
        get_custom_theme()
    })
    
    # Reactive plot objects (Boxplot, Violin)
    plot_box_obj <- reactive({
      req(dataset_r(), common_plot_inputs_ready(), group_col_name_r())
      req(input$selected_box_traits) # Ensure traits are selected
      
      # If plot_palette is 'manual', ensure get_manual_colors() is ready
      if (plot_palette() == "manual") {
        req(get_manual_colors())
      }
      
      df <- dataset_r()
      group_var_name <- group_col_name_r()
      if (!is.null(input$selected_box_groups)) {
        df <- df[df[[group_var_name]] %in% input$selected_box_groups, ]
      }
      
      validate(
        need(is.data.frame(df), "Data is not a data frame."),
        need(ncol(df) > 1, "Data contains only one column."),
        need(group_var_name %in% names(df),
             paste("Group column", group_var_name, "not found in data."))
      )
      
      # Filter to only selected numeric columns (excluding the group column)
      selected_traits <- input$selected_box_traits
      numeric_cols_in_data <- names(df)[sapply(df, is.numeric)]
      traits_to_plot <- intersect(selected_traits, numeric_cols_in_data)
      traits_to_plot <- setdiff(traits_to_plot, group_var_name)
      
      validate(
        need(length(traits_to_plot) > 0,
             "Please select at least one numeric trait to plot from the list.")
      )
      
      df[[group_var_name]] <- as.factor(df[[group_var_name]])
      df_long <- df %>%
        dplyr::select(all_of(c(group_var_name, traits_to_plot))) %>%
        pivot_longer(-all_of(group_var_name), names_to = "Trait", values_to = "Value")
      
      ggplot(df_long, aes_string(x = group_var_name, y = "Value", fill = group_var_name)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.7) +
        facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale_otu(plot_palette()) + # Use OTU-specific fill scale
        get_custom_theme() +
        labs(fill = str_to_title(group_var_name)) # Dynamic legend title
    })
    
    plot_violin_obj <- reactive({
      req(dataset_r(), common_plot_inputs_ready(), group_col_name_r())
      req(input$selected_violin_traits) # Ensure traits are selected
      
      # If plot_palette is 'manual', ensure get_manual_colors() is ready
      if (plot_palette() == "manual") {
        req(get_manual_colors())
      }
      
      df <- dataset_r()
      group_var_name <- group_col_name_r()
      if (!is.null(input$selected_violin_groups)) {
        df <- df[df[[group_var_name]] %in% input$selected_violin_groups, ]
      }
      
      validate(
        need(is.data.frame(df), "Data is not a data frame."),
        need(ncol(df) > 1, "Data contains only one column."),
        need(group_var_name %in% names(df),
             paste("Group column", group_var_name, "not found in data."))
      )
      
      # Filter to only selected numeric columns (excluding the group column)
      selected_traits <- input$selected_violin_traits
      numeric_cols_in_data <- names(df)[sapply(df, is.numeric)]
      traits_to_plot <- intersect(selected_traits, numeric_cols_in_data)
      traits_to_plot <- setdiff(traits_to_plot, group_var_name)
      
      validate(
        need(length(traits_to_plot) > 0,
             "Please select at least one numeric trait to plot from the list.")
      )
      
      df[[group_var_name]] <- as.factor(df[[group_var_name]])
      df_long <- df %>%
        dplyr::select(all_of(c(group_var_name, traits_to_plot))) %>%
        pivot_longer(-all_of(group_var_name), names_to = "Trait", values_to = "Value")
      
      ggplot(df_long, aes_string(x = group_var_name, y = "Value", fill = group_var_name)) +
        geom_violin(width = 0.9, alpha = 0.6, color = NA) +
        facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale_otu(plot_palette()) + # Use OTU-specific fill scale
        get_custom_theme() +
        labs(fill = str_to_title(group_var_name)) # Dynamic legend title
    })
    
    # MFA Plot Objects
    mfa_eigen_plot_obj <- reactive({
      req(mfa_results_r())
      mfa_res <- mfa_results_r() # Extract the value from the reactive
      
      validate(
        need(!is.null(mfa_res), "MFA results are not available."),
        need(inherits(mfa_res, "MFA"), "Invalid MFA results object.")
      )
      
      fviz_eig(mfa_res) +
        get_custom_theme() +
        labs(title = "Eigenvalue Scree Plot") # Add a title
    })
    
    mfa_group_contrib_hist_obj <- reactive({
      req(mfa_results_r(), trait_group_df_r(), common_plot_inputs_ready(), get_mfa_type_colors())
      
      mfa_res <- mfa_results_r()
      group_contrib <- mfa_res$group$contrib
      if (is.null(group_contrib)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No group contributions available.") + theme_void())
      }
      
      trait_to_type <- trait_group_df_r() %>% dplyr::distinct(Type) %>%
        dplyr::mutate(Group = Type)
      
      dims <- c("Dim.1", "Dim.2")
      plots <- lapply(dims, function(dim_name) {
        df <- data.frame(Group = rownames(group_contrib),
                         Contribution = group_contrib[, dim_name])
        
        df <- df %>%
          left_join(trait_to_type, by = "Group") %>%
          arrange(Contribution) %>%
          mutate(Group = factor(Group, levels = Group),
                 Type = ifelse(is.na(Type), "Other", Type))
        
        ggplot(df, aes(x = Group, y = Contribution, fill = Type)) +
          geom_col(width = 0.7) +
          geom_hline(yintercept = 100 / nrow(df), linetype = "dashed", color = "red", linewidth = 0.8) +
          coord_flip() +
          labs(title = paste("Group Contributions to", dim_name), x = "Group", y = "Contribution (%)") +
          scale_fill_manual(values = get_mfa_type_colors(), drop = FALSE) +
          get_custom_theme() +
          theme(legend.position = "bottom")
      })
      
      ggpubr::ggarrange(plotlist = plots, ncol = 2, common.legend = TRUE, legend = "bottom")
    })
    
    mfa_var_contrib_hist_obj <- reactive({

      req(mfa_results_r())
      req(trait_group_df_r())
      req(common_plot_inputs_ready())
      req(get_mfa_var_type_colors())
      
      mfa_res <- mfa_results_r()
      current_mfa_type_colors <- get_mfa_var_type_colors()
      
      trait_to_type <- trait_group_df_r() %>% mutate(Variable = as.character(Variable))
      
      all_dim_data <- list()
      dims <- c("Dim.1", "Dim.2")
      
      for (dim_name in dims) {
        message(paste0("Processing dimension: ", dim_name))
        
        quanti_contrib_dim <- NULL
        if (!is.null(mfa_res$quanti.var$contrib) && dim_name %in% colnames(mfa_res$quanti.var$contrib)) {
          quanti_contrib_dim <- as.data.frame(mfa_res$quanti.var$contrib) %>%
            dplyr::select(!!sym(dim_name)) %>%
            tibble::rownames_to_column("Variable") %>%
            mutate(Variable_Cleaned = stringr::str_remove(Variable, " \\(.*\\)"),
                   Variable_Plot = Variable,
                   Type = "Unknown")
        }
        
        quali_contrib_dim <- NULL
        if (!is.null(mfa_res$quali.var$contrib) && dim_name %in% colnames(mfa_res$quali.var$contrib)) {
          quali_contrib_data_temp <- as.data.frame(mfa_res$quali.var$contrib) %>%
            dplyr::select(!!sym(dim_name)) %>%
            tibble::rownames_to_column("Variable_Raw_Name") %>%
            mutate(
              Variable_Cleaned = NA_character_,
              Variable_Plot = NA_character_,
              Type = "Unknown"
            )
          
          original_mfa_data <- mfa_res$call$X
          
          for (i in 1:nrow(quali_contrib_data_temp)) {
            raw_name <- quali_contrib_data_temp$Variable_Raw_Name[i]
            
            if (is.null(original_mfa_data)) {
              if (stringr::str_detect(raw_name, "_")) {
                quali_contrib_data_temp$Variable_Cleaned[i] <- stringr::str_split_fixed(raw_name, "_", 2)[1]
                quali_contrib_data_temp$Variable_Plot[i] <- raw_name
              } else if (stringr::str_detect(raw_name, "\\.")) {
                quali_contrib_data_temp$Variable_Cleaned[i] <- stringr::str_split_fixed(raw_name, "\\.", 2)[1]
                quali_contrib_data_temp$Variable_Plot[i] <- raw_name
              } else {
                quali_contrib_data_temp$Variable_Cleaned[i] <- raw_name
                quali_contrib_data_temp$Variable_Plot[i] <- raw_name
              }
              next
            }
            
            split_by_underscore <- stringr::str_split_fixed(raw_name, "_", 2)
            if (ncol(split_by_underscore) == 2 &&
                split_by_underscore[1,1] %in% names(original_mfa_data) &&
                split_by_underscore[1,2] %in% levels(factor(original_mfa_data[[split_by_underscore[1,1]]]))) {
              quali_contrib_data_temp$Variable_Cleaned[i] <- split_by_underscore[1,1]
              quali_contrib_data_temp$Variable_Plot[i] <- paste0(split_by_underscore[1,1], ": ", split_by_underscore[1,2])
              next
            }
            
            split_by_dot <- stringr::str_split_fixed(raw_name, "\\.", 2)
            if (ncol(split_by_dot) == 2 &&
                split_by_dot[1,1] %in% names(original_mfa_data) &&
                split_by_dot[1,2] %in% levels(factor(original_mfa_data[[split_by_dot[1,1]]]))) {
              quali_contrib_data_temp$Variable_Cleaned[i] <- split_by_dot[1,1]
              quali_contrib_data_temp$Variable_Plot[i] <- paste0(split_by_dot[1,1], ": ", split_by_dot[1,2])
              next
            }
            
            found_original_var <- FALSE
            for (col_name in names(original_mfa_data)) {
              if ((is.factor(original_mfa_data[[col_name]]) || is.character(original_mfa_data[[col_name]])) &&
                  raw_name %in% levels(factor(original_mfa_data[[col_name]]))) {
                quali_contrib_data_temp$Variable_Cleaned[i] <- col_name
                quali_contrib_data_temp$Variable_Plot[i] <- paste0(col_name, ": ", raw_name)
                found_original_var <- TRUE
                break
              }
            }
            
            if (!found_original_var) {
              quali_contrib_data_temp$Variable_Cleaned[i] <- raw_name
              quali_contrib_data_temp$Variable_Plot[i] <- raw_name
            }
          }
          
          quali_contrib_dim <- quali_contrib_data_temp %>%
            dplyr::rename(Variable = Variable_Raw_Name)
        }
        
        current_dim_data <- dplyr::bind_rows(quanti_contrib_dim, quali_contrib_dim)
        if (is.null(current_dim_data) || nrow(current_dim_data) == 0) next
        
        current_dim_data <- current_dim_data %>%
          dplyr::left_join(trait_to_type, by = c("Variable_Cleaned" = "Variable")) %>%
          mutate(Type = ifelse(is.na(Type.y), Type.x, Type.y)) %>%
          dplyr::select(-Type.x, -Type.y) %>%
          dplyr::rename(Contribution = !!sym(dim_name)) %>%
          dplyr::filter(!is.na(Type))
        
        if (nrow(current_dim_data) == 0) next
        
        ordered_data <- current_dim_data %>%
          dplyr::arrange(Contribution) %>%
          mutate(Variable_Plot = factor(Variable_Plot, levels = unique(Variable_Plot)),
                 Type = factor(Type, levels = unique(trait_to_type$Type)))
        
        all_dim_data[[dim_name]] <- ordered_data
      }
      
      if (length(all_dim_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No valid variable contributions found for any dimension to plot.") +
                 theme_void())
      }
      
      plot_list <- purrr::map(names(all_dim_data), function(dim) {
        dim_data <- all_dim_data[[dim]]
        avg_contrib_threshold <- 100 / nrow(dim_data)
        
        ggplot(dim_data, aes(x = Variable_Plot, y = Contribution, fill = Type)) +
          geom_col(width = 0.7) +
          geom_hline(yintercept = avg_contrib_threshold, linetype = "dashed", color = "red", linewidth = 0.8) +
          labs(x = "Variables", y = paste("Contribution to", dim, "(%)"),
               title = paste("Variable Contributions to", dim)) +
          coord_flip() +
          scale_fill_manual(values = get_mfa_var_type_colors(), name = "Trait Type",
                            na.value = "grey50", drop = FALSE) +
          get_custom_theme() +
          theme(legend.position = "bottom")
      })
      
      if (length(plot_list) == 2) {
        final_plot <- ggpubr::ggarrange(plotlist = plot_list, ncol = 2, common.legend = TRUE, legend = "bottom")
      } else {
        final_plot <- plot_list[[1]]
      }
      
      return(final_plot)
    })
    
    
    # MFA Individuals plot object
    mfa_individuals_plot_obj <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r()) # Ensure dataset_r is also required
      
      mfa_res <- mfa_results_r()
      
      validate(
        need(!is.null(mfa_res), "MFA results are not available. Please ensure MFA is run successfully."),
        need("ind" %in% names(mfa_res), "MFA results object missing 'ind' component."),
        need("coord" %in% names(mfa_res$ind), "MFA results 'ind' component missing 'coord'."),
        need(nrow(mfa_res$ind$coord) > 0, "MFA individuals coordinates are empty. No individuals to plot."),
        need(ncol(mfa_res$ind$coord) >= 2, "MFA individuals coordinates have less than 2 dimensions (Dim.1, Dim.2).")
      )
      
      req(common_plot_inputs_ready())
      req(input$mfa_point_size) # Use input$mfa_point_size directly
      # mfa_point_shape() is not used directly for the default point, as it's either 21 or 19.
      
      if (isTRUE(input$mfa_ellipse)) { # Use input$mfa_ellipse
        req(input$mfa_ellipse_alpha) # Use input$mfa_ellipse_alpha
      }
      
      # Get the original data to extract the group column
      df_original <- dataset_r() # This is the dataset (raw or adjusted) that was passed to visual module
      group_col_name <- group_col_name_r() # USE THE PASSED GROUP NAME
      
      validate(
        need(group_col_name %in% names(df_original), paste0("Group column '", group_col_name, "' not found in the dataset for MFA individuals plot."))
      )
      
      # Extract individual coordinates and merge with original group labels
      mfa_ind_coord <- as.data.frame(mfa_res$ind$coord)
      
      # Ensure row names are preserved for joining
      mfa_ind_coord <- mfa_ind_coord %>%
        tibble::rownames_to_column(var = "Individual_ID")
      
      # Prepare original data for joining, ensuring group column is a factor
      original_data_for_join <- df_original %>%
        tibble::rownames_to_column(var = "Individual_ID") %>%
        dplyr::select(Individual_ID, Group = !!sym(group_col_name)) %>%
        dplyr::mutate(Group = as.factor(Group))
      
      mfa_ind_coord <- dplyr::left_join(mfa_ind_coord, original_data_for_join, by = "Individual_ID") %>%
        tibble::column_to_rownames(var = "Individual_ID") # Convert back if desired
      
      
      # Validate join result
      validate(
        need(!any(is.na(mfa_ind_coord$Group)), "Group information missing after joining MFA coordinates with original data. Check row names/IDs or the group column name."),
        need(nlevels(mfa_ind_coord$Group) >= 1, "No valid groups found for individuals plot.")
      )
      
      p <- ggplot(mfa_ind_coord, aes(x = Dim.1, y = Dim.2))
      
      if (isTRUE(input$mfa_outline_points)) {
        p <- p + geom_point(
          aes(fill = Group),
          shape = 21,
          size = input$mfa_point_size,
          stroke = input$mfa_point_stroke,
          color = "black"
        ) + get_fill_scale_otu(plot_palette())
      } else {
        p <- p + geom_point(
          aes(color = Group),
          shape = 19,
          size = input$mfa_point_size
        ) + get_color_scale_otu(plot_palette())
      }
      
      # ELLIPSE
      if (isTRUE(input$mfa_ellipse)) {
        if (isTRUE(input$mfa_outline_shapes)) {
          p <- p + stat_ellipse(
            aes(group = Group, fill = Group, color = Group),
            type = "norm", level = 0.95, geom = "polygon",
            alpha = input$mfa_ellipse_alpha, 
            size = input$mfa_outline_stroke,
            show.legend = FALSE
          ) + get_fill_scale_otu(plot_palette()) + get_color_scale_otu(plot_palette())
        } else {
          p <- p + stat_ellipse(
            aes(group = Group, fill = Group),
            type = "norm", level = 0.95, geom = "polygon",
            alpha = input$mfa_ellipse_alpha, color = NA,
            show.legend = FALSE
          ) + get_fill_scale_otu(plot_palette())
        }
      }
      
      # HULL
      if (isTRUE(input$mfa_convex_hull)) {
        hull_df <- mfa_ind_coord %>%
          dplyr::group_by(Group) %>%
          dplyr::filter(n() >= 3) %>%
          dplyr::slice(chull(Dim.1, Dim.2)) %>%
          dplyr::ungroup()
        
        if (isTRUE(input$mfa_outline_shapes)) {
          p <- p + geom_polygon(
            data = hull_df,
            aes(x = Dim.1, y = Dim.2, group = Group, fill = Group, color = Group),
            alpha = input$mfa_ellipse_alpha,
            size = input$mfa_outline_stroke,
            inherit.aes = FALSE, show.legend = FALSE
          ) + get_fill_scale_otu(plot_palette()) + get_color_scale_otu(plot_palette())
        } else {
          p <- p + geom_polygon(
            data = hull_df,
            aes(x = Dim.1, y = Dim.2, group = Group, fill = Group),
            color = NA,
            alpha = input$mfa_ellipse_alpha,
            inherit.aes = FALSE, show.legend = FALSE
          ) + get_fill_scale_otu(plot_palette())
        }
      }
      
      # CENTROIDS
      if (isTRUE(input$mfa_centroids)) {
        centroids <- mfa_ind_coord %>%
          group_by(Group) %>%
          summarize(Dim.1 = mean(Dim.1), Dim.2 = mean(Dim.2), .groups = "drop")
        
        p <- p + geom_point(
          data = centroids,
          aes(x = Dim.1, y = Dim.2),
          shape = 8, size = 4, color = "black", fill = "white", stroke = 1,
          inherit.aes = FALSE, show.legend = FALSE
        )
      }
      
      # FINAL LABELS + THEME + GUIDES
      p <- p +
        get_custom_theme() +
        labs(
          x = paste0("Dim 1 (", round(mfa_res$eig[1, 2], 2), "%)"),
          y = paste0("Dim 2 (", round(mfa_res$eig[2, 2], 2), "%)"),
          fill = str_to_title(group_col_name),
          color = str_to_title(group_col_name)
        )
      
      # Guides — conditionally toggle which legend is used
      if (isTRUE(input$mfa_outline_points)) {
        p <- p + guides(
          fill = guide_legend(override.aes = list(
            size = input$mfa_point_size, shape = 21, color = "black"
          )),
          color = "none"
        )
      } else {
        p <- p + guides(
          color = guide_legend(override.aes = list(
            size = input$mfa_point_size, shape = 19
          )),
          fill = "none"
        )
      }
      
      return(p)
    })
    
    # Render plots with dynamic height and width
    output$plot_scatter <- renderPlot({
      plot_scatter_obj()
    }, height = function() input$plot_scatter_height,
    width = function() input$plot_scatter_width)
    
    output$plot_box <- renderPlot(
      { plot_box_obj() },
      height = function() input$plot_box_height,
      width = function() input$plot_box_width 
    )
    output$plot_violin <- renderPlot(
      { plot_violin_obj() },
      height = function() input$plot_violin_height,
      width = function() input$plot_violin_width 
    )
    output$mfa_eigen_plot <- renderPlot(
      { mfa_eigen_plot_obj() },
      height = function() input$mfa_eigen_plot_height,
      width = function() input$mfa_eigen_plot_width 
    )
    output$mfa_group_contrib_hist <- renderPlot({
      mfa_group_contrib_hist_obj()
    }, height = function() input$mfa_group_contrib_hist_height,
    width = function() input$mfa_group_contrib_hist_width)
    
    output$mfa_individuals_plot <- renderPlot(
      { mfa_individuals_plot_obj() },
      height = function() input$mfa_individuals_plot_height,
      width = function() input$mfa_individuals_plot_width 
    )
    
    # Cache MFA Variable Contributions Plot Reactive
    mfa_var_contrib_hist_reactive <- reactive({
      req(mfa_results_r())
      req(trait_group_df_r())
      req(common_plot_inputs_ready())
      req(get_mfa_var_type_colors())
      mfa_var_contrib_hist_obj(
        mfa_results_r = mfa_results_r,
        trait_group_df_r = trait_group_df_r,
        get_mfa_type_colors = get_mfa_type_colors,
        common_plot_inputs_ready = common_plot_inputs_ready
      )()
    })
    
    
    output$mfa_var_contrib_hist <- renderPlot({
      mfa_var_contrib_hist_obj()
    }, 
    height = function() input$mfa_var_contrib_hist_height,
    width = function() input$mfa_var_contrib_hist_width)

    DEFAULT_DOWNLOAD_WIDTH <- 10
    DEFAULT_DOWNLOAD_HEIGHT <- 8 # This will mostly be overridden by user input
    
    # The download function for all plots
    create_download_handler <- function(plot_obj_reactive, filename_prefix, type = "pdf", height_input_id, width_input_id) { # Added width_input_id
      downloadHandler(
        filename = function() { paste0(filename_prefix, "_", Sys.Date(), ".", type) },
        content = function(file) {
          # Get user-specified height and width from the numericInputs
          # Use ns() because the input ID is namespaced within the module
          plot_height_val_px <- input[[height_input_id]]
          plot_width_val_px <- input[[width_input_id]] 
          
          plot_height_val_in <- if (!is.null(plot_height_val_px) && plot_height_val_px > 0) {
            plot_height_val_px / 96
          } else {
            DEFAULT_DOWNLOAD_HEIGHT # Fallback if input is null or zero
          }
          
          plot_width_val_in <- if (!is.null(plot_width_val_px) && plot_width_val_px > 0) {
            plot_width_val_px / 96
          } else {
            DEFAULT_DOWNLOAD_WIDTH
          }
          
          tryCatch({
            if (type == "pdf") {
              ggplot2::ggsave(file, plot = plot_obj_reactive(), device = "pdf",
                              width = plot_width_val_in, height = plot_height_val_in, units = "in")
            } else if (type == "jpeg") {
              ggplot2::ggsave(file, plot = plot_obj_reactive(), device = "jpeg",
                              width = plot_width_val_in, height = plot_height_val_in, units = "in", dpi = 300)
            }
          }, error = function(e) {
            message("Error saving ", type, " for ", filename_prefix, ": ", e$message)
            stop(e)
          })
        }
      )
    }
    
    # Assign download handlers, passing the corresponding height AND width input IDs
    output$download_scatter_pdf <- create_download_handler(plot_scatter_obj, "scatterplot_combined", "pdf", "plot_scatter_height", "plot_scatter_width")
    output$download_scatter_jpeg <- create_download_handler(plot_scatter_obj, "scatterplot_combined", "jpeg", "plot_scatter_height", "plot_scatter_width")
    
    output$download_box_pdf <- create_download_handler(plot_box_obj, "boxplot_combined", "pdf", "plot_box_height", "plot_box_width")
    output$download_box_jpeg <- create_download_handler(plot_box_obj, "boxplot_combined", "jpeg", "plot_box_height", "plot_box_width")
    output$download_violin_pdf <- create_download_handler(plot_violin_obj, "violinplot_combined", "pdf", "plot_violin_height", "plot_violin_width")
    output$download_violin_jpeg <- create_download_handler(plot_violin_obj, "violinplot_combined", "jpeg", "plot_violin_height", "plot_violin_width")
    
    output$download_mfa_eigen_pdf <- create_download_handler(mfa_eigen_plot_obj, "mfa_eigenvalues", "pdf", "mfa_eigen_plot_height", "mfa_eigen_plot_width")
    output$download_mfa_eigen_jpeg <- create_download_handler(mfa_eigen_plot_obj, "mfa_eigenvalues", "jpeg", "mfa_eigen_plot_height", "mfa_eigen_plot_width")
    output$download_mfa_individuals_pdf <- create_download_handler(mfa_individuals_plot_obj, "mfa_individuals", "pdf", "mfa_individuals_plot_height", "mfa_individuals_plot_width")
    output$download_mfa_individuals_jpeg <- create_download_handler(mfa_individuals_plot_obj, "mfa_individuals", "jpeg", "mfa_individuals_plot_height", "mfa_individuals_plot_width")
    output$download_mfa_var_contrib_pdf <- create_download_handler(mfa_var_contrib_hist_obj, "mfa_var_contributions", "pdf", "mfa_var_contrib_hist_height", "mfa_var_contrib_hist_width")
    output$download_mfa_var_contrib_jpeg <- create_download_handler(mfa_var_contrib_hist_obj, "mfa_var_contributions", "jpeg", "mfa_var_contrib_hist_height", "mfa_var_contrib_hist_width")
    
    output$download_mfa_group_contrib_pdf <- create_download_handler(mfa_group_contrib_hist_obj, "mfa_group_contributions", "pdf", "mfa_group_contrib_hist_height", "mfa_group_contrib_hist_width")
    output$download_mfa_group_contrib_jpeg <- create_download_handler(mfa_group_contrib_hist_obj, "mfa_group_contributions", "jpeg", "mfa_group_contrib_hist_height", "mfa_group_contrib_hist_width")

  }) 
} 
