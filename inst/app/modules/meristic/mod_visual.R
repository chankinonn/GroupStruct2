
mod_visual_ui_meristic <- function(id) {
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
                                  numericInput(ns("plot_scatter_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  numericInput(ns("plot_scatter_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '150px'),
                                  uiOutput(ns("scatter_controls")),
                                  hr(),
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
                                  hr(),
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
                                  checkboxInput(ns("violin_outline"), "Outline Violin", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s'] == true", ns("violin_outline")),
                                    sliderInput(ns("violin_point_stroke"), "Point Outline Width",
                                                min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                  ),
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
                
                tabPanel("PCA",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("plot_pca"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("plot_pca_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  numericInput(ns("plot_pca_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px'),
                                  hr(),
                                  numericInput(ns("pca_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                  checkboxInput(ns("pca_outline_points"), "Outline Points", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_outline_points")),
                                    sliderInput(ns("pca_point_stroke"), "Point Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                  ),
                                  checkboxInput(ns("pca_centroids"), "Group Centroids", value = FALSE),
                                  checkboxInput(ns("pca_ellipse"), "95% Confidence Ellipses", value = FALSE),
                                  checkboxInput(ns("pca_convex"), "Convex Hulls", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s'] || input['%s']", ns("pca_ellipse"), ns("pca_convex")),
                                    checkboxInput(ns("pca_outline"), "Outline Ellipses/Hulls", value = FALSE)
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_outline")),
                                    sliderInput(ns("pca_outline_stroke"), "Ellipse/Hull Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s'] || input['%s']", ns("pca_ellipse"), ns("pca_convex")),
                                    sliderInput(ns("pca_alpha_ellipse"), "Ellipse/Hull Fill Transparency", min = 0, max = 1, value = 0.3, step = 0.05, width = '150px')
                                  ),
                                  hr(),
                                  downloadButton(ns("download_pca_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_pca_jpeg"), "Download JPEG"),
                                  br(),
                                  downloadButton(ns("download_pca_summary"), "Download PCA Summary"),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("DAPC",
                         fluidRow(
                           column(9,
                                  plotOutput(ns("plot_dapc"))
                           ),
                           column(3,
                                  br(),
                                  numericInput(ns("plot_dapc_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  numericInput(ns("plot_dapc_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px'),
                                  hr(),
                                  uiOutput(ns("n_pca_dapc_ui")),
                                  sliderInput(ns("n_da_dapc"), "Number of Discriminant Axes (n.da):", min = 1, max = 5, value = 2, step = 1),
                                  numericInput(ns("dapc_point_size"), "Point Size:", value = 3, min = 1, max = 10,width = '150px'),
                                  checkboxInput(ns("dapc_outline_points"), "Outline Points", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("dapc_outline_points")),
                                    sliderInput(ns("dapc_point_stroke"), "Point Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                  ),
                                  checkboxInput(ns("dapc_centroids"), "Group Centroids", value = FALSE),
                                  checkboxInput(ns("dapc_ellipse"), "67% Confidence Ellipses (following adegenet)", value = FALSE),
                                  checkboxInput(ns("dapc_convex"), "Convex Hulls", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s'] || input['%s']", ns("dapc_ellipse"), ns("dapc_convex")),
                                    checkboxInput(ns("dapc_outline"), "Outline Ellipses/Hulls", value = FALSE)
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("dapc_outline")),
                                    sliderInput(ns("dapc_outline_stroke"), "Ellipse/Hull Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                  ),                                  
                                  conditionalPanel(
                                    condition = sprintf("input['%s'] || input['%s']", ns("dapc_ellipse"), ns("dapc_convex")),
                                    sliderInput(ns("dapc_alpha_ellipse"), "Ellipse/Hull Fill Transparency", min = 0, max = 1, value = 0.3, step = 0.05, width = '150px')
                                  ),                                  
                                  hr(),
                                  downloadButton(ns("download_dapc_pdf"), "Download PDF"),
                                  br(),
                                  downloadButton(ns("download_dapc_jpeg"), "Download JPEG"),
                                  hr()
                           )
                         )
                )
    )
  )
}

mod_visual_server_meristic <- function(id, dataset,
                                       plot_palette, plot_axis_text_size,
                                       plot_axis_label_size, plot_x_angle, plot_facet_size,
                                       legend_text_size, legend_title_size,
                                       manual_colors_r) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    output$scatter_controls <- renderUI({
      req(dataset())
      df <- dataset()
      traits <- names(df)[-1]
      groups <- unique(df[[1]])
      
      tagList(
        selectInput(ns("scatter_xvar"), "X Variable", choices = traits, selected = traits[1], width = '150px'),
        selectInput(ns("scatter_yvar"), "Y Variable", choices = traits, selected = traits[2], width = '150px'),
        checkboxGroupInput(ns("scatter_group_filter"), "Filter by Group", choices = groups, selected = groups)
      )
    })
    
    # Dynamically set max for n.pca based on number of traits
    output$n_pca_dapc_ui <- renderUI({
      req(dataset())
      n_traits <- ncol(dataset()) - 1
      sliderInput(ns("n_pca_dapc"), "Number of PCA Components (n.pca). Download the PCA summary table from the PCA tab in Inferential Statstics and retain the number of PCs that explain 80-90% of total variance:",
                  min = 1, max = n_traits, value = min(5, n_traits), step = 1)
    })
    
    # Add variable selection for box and violin plots
    output$box_variable_selector <- renderUI({
      req(dataset())
      traits <- names(dataset())[-1]
      checkboxGroupInput(ns("selected_box_traits"), "Select Traits to Plot:", 
                         choices = traits, selected = traits)
    })
    
    output$box_group_selector <- renderUI({
      req(dataset())
      groups <- unique(dataset()[[1]])
      checkboxGroupInput(ns("selected_box_groups"), "Select Groups to Plot:", 
                         choices = groups, selected = groups)
    })
    
    
    output$violin_variable_selector <- renderUI({
      req(dataset())
      traits <- names(dataset())[-1]
      checkboxGroupInput(ns("selected_violin_traits"), "Select Traits to Plot:", 
                         choices = traits, selected = traits)
    })
    
    output$violin_group_selector <- renderUI({
      req(dataset())
      groups <- unique(dataset()[[1]])
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

    # Get manual colors from the passed reactive
    get_manual_colors <- function() {
      req(manual_colors_r())
      manual_colors_r()
    }
    
    # Choose fill scale
    get_fill_scale <- function(palette) {
      if (is.null(palette) || length(palette) == 0) {
        return(ggplot2::scale_fill_viridis_d())
      }
      
      if (palette == "manual") {
        req(manual_colors_r())
        return(ggplot2::scale_fill_manual(values = manual_colors_r()))
      }
      
      if (startsWith(palette, "viridis:")) {
        option <- strsplit(palette, ":")[[1]][2]
        return(ggplot2::scale_fill_viridis_d(option = option))
      }
      
      if (startsWith(palette, "brewer:")) {
        brewer_pal <- strsplit(palette, ":")[[1]][2]
        return(ggplot2::scale_fill_brewer(palette = brewer_pal))
      }
      
      if (palette == "ggthemes:colorblind") {
        return(ggplot2::scale_fill_manual(values = ggthemes::colorblind_pal()(8)))
      }
      
      if (startsWith(palette, "colorblind:")) {
        brewer_pal <- strsplit(palette, ":")[[1]][2]
        return(ggplot2::scale_fill_brewer(palette = brewer_pal))
      }
      
      return(ggplot2::scale_fill_viridis_d())  # fallback
    }
    
    # Choose color scale
    get_color_scale <- function(palette) {
      if (is.null(palette) || length(palette) == 0) {
        return(ggplot2::scale_color_viridis_d())
      }
      
      if (palette == "manual") {
        req(manual_colors_r())
        return(ggplot2::scale_color_manual(values = manual_colors_r()))
      }
      
      if (startsWith(palette, "viridis:")) {
        option <- strsplit(palette, ":")[[1]][2]
        return(ggplot2::scale_color_viridis_d(option = option))
      }
      
      if (startsWith(palette, "brewer:")) {
        brewer_pal <- strsplit(palette, ":")[[1]][2]
        return(ggplot2::scale_color_brewer(palette = brewer_pal))
      }
      
      if (palette == "ggthemes:colorblind") {
        return(ggplot2::scale_color_manual(values = ggthemes::colorblind_pal()(8)))
      }
      
      if (startsWith(palette, "colorblind:")) {
        brewer_pal <- strsplit(palette, ":")[[1]][2]
        return(ggplot2::scale_color_brewer(palette = brewer_pal))
      }
      
      return(ggplot2::scale_color_viridis_d())  # fallback
    }

    
    # Theme generator (always classic, adjusted for x-axis label angle)
    get_custom_theme <- function(axis_text_size, axis_label_size, x_angle, facet_size,
                                 legend_text_size, legend_title_size, theme_choice = "theme_classic") {
      
      # Adjust vjust and hjust for x-axis text based on angle
      x_vjust <- 0.5
      x_hjust <- 0.5
      if (x_angle == 45) {
        x_hjust <- 1
        x_vjust <- 1
      } else if (x_angle == 90) {
        x_hjust <- 1
        x_vjust <- 0.5
      }
      
      theme_base <- get_ggplot_theme(theme_choice)
      
      theme_elements <- list(
        axis.text.x = ggplot2::element_text(size = axis_text_size, angle = as.numeric(x_angle),
                                            vjust = x_vjust, hjust = x_hjust),
        axis.text.y = ggplot2::element_text(size = axis_text_size), # Y-axis text not angled
        axis.title = ggplot2::element_text(size = axis_label_size),
        strip.text = ggplot2::element_text(size = facet_size),
        legend.position = "right",
        legend.text = ggplot2::element_text(size = legend_text_size()),
        legend.title = ggplot2::element_text(size = legend_title_size(), face = "bold")
      )
      
      theme_base + do.call(ggplot2::theme, theme_elements)
    }
    
    # Reactive to check if all common plot inputs are ready
    common_plot_inputs_ready <- reactive({
      req(plot_palette(), plot_axis_text_size(), plot_axis_label_size(),
          plot_x_angle(), plot_facet_size(),
          legend_text_size(), legend_title_size())
      
      # If manual palette is selected, ensure manual_colors_r is also ready
      if (plot_palette() == "manual") {
        req(manual_colors_r())
      }
      TRUE
    })
    
    # Reactive plot objects
    plot_scatter_obj <- reactive({
      req(dataset(), input$scatter_xvar, input$scatter_yvar, input$scatter_group_filter)
      
      df <- dataset()
      group_col <- names(df)[1]
      df <- df[df[[group_col]] %in% input$scatter_group_filter, ]
      
      p <- ggplot(df, aes_string(x = input$scatter_xvar, y = input$scatter_yvar, color = group_col)) +
        geom_point(size = 3, alpha = 0.8)
      
      if (input$scatter_show_lm) {
        p <- p + geom_smooth(method = "lm", 
                             se = isTRUE(input$scatter_show_lm_se), 
                             linetype = "solid")
      }
      
      if (input$scatter_show_labels) {
        p <- p + geom_text(aes(label = rownames(df)), 
                           hjust = 1.1, vjust = 1.1, 
                           size = 3, check_overlap = TRUE)
      }
      
      p + get_color_scale(plot_palette()) +
        get_custom_theme(plot_axis_text_size(), plot_axis_label_size(), 
                         plot_x_angle(), plot_facet_size(), 
                         legend_text_size(), legend_title_size(),input$plot_theme)
    })
    
    
    plot_box_obj <- reactive({
      req(dataset(), common_plot_inputs_ready(), input$selected_box_traits)
      df <- dataset()
      if (!is.null(input$selected_box_groups)) {
        df <- df[df[[1]] %in% input$selected_box_groups, ]
      }
      traits_to_plot <- input$selected_box_traits
      req(length(traits_to_plot) > 0)
      
      df_long <- tidyr::pivot_longer(df, cols = all_of(traits_to_plot), names_to = "Trait", values_to = "Value")
      
      ggplot2::ggplot(df_long, ggplot2::aes_string(x = names(df)[1], y = "Value", fill = names(df)[1])) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
        ggplot2::facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale(plot_palette()) + # Removed prefix
        get_custom_theme(plot_axis_text_size(), plot_axis_label_size(),
                         plot_x_angle(), plot_facet_size(),
                         legend_text_size(), legend_title_size(),input$plot_theme)
    })
    
    plot_violin_obj <- reactive({
      req(dataset(), common_plot_inputs_ready(), input$selected_violin_traits)
      df <- dataset()
      
      if (!is.null(input$selected_violin_groups)) {
        df <- df[df[[1]] %in% input$selected_violin_groups, ]
      }
      
      traits_to_plot <- input$selected_violin_traits
      req(length(traits_to_plot) > 0)
      
      df_long <- tidyr::pivot_longer(df, cols = all_of(traits_to_plot), names_to = "Trait", values_to = "Value")
      
      violin_outline_color <- if (isTRUE(input$violin_outline)) "black" else NA
      violin_outline_size  <- if (isTRUE(input$violin_outline)) input$violin_point_stroke else 0
      
      ggplot2::ggplot(df_long, ggplot2::aes_string(x = names(df)[1], y = "Value", fill = names(df)[1])) +
        ggplot2::geom_violin(width = 0.7, alpha = 0.6, color = violin_outline_color, size = violin_outline_size) +
        ggplot2::facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale(plot_palette()) +
        get_custom_theme(plot_axis_text_size(), plot_axis_label_size(),
                         plot_x_angle(), plot_facet_size(),
                         legend_text_size(), legend_title_size(),
                         input$plot_theme)
    })
    
    # Reactive for PCA results
    pca_results_r <- reactive({
      df <- dataset()
      # Ensure data_mat is numeric and has no NA values for PCA
      data_mat_numeric <- as.data.frame(lapply(df[, -1], as.numeric))
      complete_rows <- complete.cases(data_mat_numeric)
      
      if (sum(complete_rows) < 2 || ncol(data_mat_numeric) < 2) {
        return(NULL) # Return NULL if data is insufficient
      }
      
      prcomp(data_mat_numeric[complete_rows, ], center = TRUE, scale. = TRUE)
    })
    
    plot_pca_obj <- reactive({
      req(dataset(), common_plot_inputs_ready())
      req(input$pca_point_size)
      
      df <- dataset()
      otu_col <- names(df)[1]
      data_mat <- df[, -1]
      complete_rows <- complete.cases(data_mat)
      
      if (sum(complete_rows) < 2 || ncol(data_mat) < 2) {
        return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5,
                                                     label = "Not enough data for PCA. Need at least 2 complete rows and 2 numeric variables."))
      }
      
      pca <- prcomp(data_mat[complete_rows, ], center = TRUE, scale. = TRUE)
      pca_df <- as.data.frame(pca$x)
      pca_df$Group <- df[[otu_col]][complete_rows]
      
      # Calculate % variance explained
      var_explained <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 1)
      pc1_label <- paste0("PC1 (", var_explained[1], "%)")
      pc2_label <- paste0("PC2 (", var_explained[2], "%)")
      
      # Base plot
      p <- ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2)) +
        ggplot2::xlab(pc1_label) +
        ggplot2::ylab(pc2_label)
      
      if (isTRUE(input$pca_outline_points)) {
        p <- p + ggplot2::geom_point(
          aes(fill = Group),
          shape = 21,
          size = input$pca_point_size,
          color = "black",  
          stroke = input$pca_point_stroke
        )
      } else {
        p <- p + ggplot2::geom_point(
          aes(color = Group),
          size = input$pca_point_size,
          shape = 19
        )
      }
      
      if (isTRUE(input$pca_ellipse)) {
        if (isTRUE(input$pca_outline)) {
          p <- p + ggplot2::stat_ellipse(
            aes(group = Group, fill = Group, color = Group),
            type = "norm",
            geom = "polygon",
            alpha = input$pca_alpha_ellipse,
            size = input$pca_outline_stroke,
            show.legend = FALSE
          )
        } else {
          p <- p + ggplot2::stat_ellipse(
            aes(group = Group, fill = Group),
            color = NA,
            type = "norm",
            geom = "polygon",
            alpha = input$pca_alpha_ellipse,
            show.legend = FALSE
          )
        }
      }
      
      if (isTRUE(input$pca_convex)) {
        hull_df <- dplyr::bind_rows(lapply(split(pca_df, pca_df$Group), function(df) {
          df[chull(df$PC1, df$PC2), ]
        }), .id = "Group")
        
        if (isTRUE(input$pca_outline)) {
          p <- p + ggplot2::geom_polygon(
            data = hull_df,
            aes(x = PC1, y = PC2, group = Group, fill = Group, color = Group),
            alpha = input$pca_alpha_ellipse,
            size = input$pca_outline_stroke,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
        } else {
          p <- p + ggplot2::geom_polygon(
            data = hull_df,
            aes(x = PC1, y = PC2, group = Group, fill = Group),
            color = NA,
            alpha = input$pca_alpha_ellipse,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
        }
      }
      
      if (isTRUE(input$pca_centroids)) {
        centroids <- pca_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
        
        p <- p + ggplot2::geom_point(data = centroids,
                                     aes(x = PC1, y = PC2),
                                     shape = 8, size = 4, color = "black", fill = "white", stroke = 1,
                                     inherit.aes = FALSE)
      }
      
      if (isTRUE(input$pca_outline_points)) {
        p <- p + get_fill_scale(plot_palette())
        p <- p + get_color_scale(plot_palette()) + ggplot2::guides(color = "none")
      } else {
        p <- p + get_color_scale(plot_palette())
        p <- p + get_fill_scale(plot_palette()) + ggplot2::guides(fill = "none")
      }
      p + get_custom_theme(plot_axis_text_size(), plot_axis_label_size(), 0, plot_facet_size(),
                           legend_text_size(), legend_title_size(),input$plot_theme)
      
    })
    
    
    plot_dapc_obj <- reactive({
      req(dataset())
      req(input$n_pca_dapc, input$n_da_dapc, input$dapc_point_size,
          common_plot_inputs_ready())
      
      df <- dataset()
      otu_col <- names(df)[1]
      data_mat <- df[, -1]
      complete_rows <- complete.cases(data_mat)
      
      if (sum(complete_rows) < 2 || ncol(data_mat) < 2 || dplyr::n_distinct(df[[otu_col]]) < 2) {
        return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5,
                                                     label = "Not enough data or groups for DAPC. Need at least 2 complete rows, 2 numeric variables, and 2 groups."))
      }
      
      data_for_dapc <- as.data.frame(data_mat[complete_rows, ])
      group_for_dapc <- as.factor(df[[otu_col]][complete_rows])
      
      dapc_res <- tryCatch({
        adegenet::dapc(data_for_dapc, group_for_dapc,
                       n.pca = input$n_pca_dapc, n.da = input$n_da_dapc)
      }, error = function(e) {
        warning("DAPC error: ", e$message)
        NULL
      })
      
      if (is.null(dapc_res)) {
        return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5,
                                                     label = "DAPC could not be performed. Check data and parameters."))
      }
      
      if (ncol(dapc_res$ind.coord) < 2) {
        return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5,
                                                     label = "DAPC did not produce enough discriminant axes (LD1, LD2). Try reducing n.da or increasing groups."))
      }
      
      dapc_df <- as.data.frame(dapc_res$ind.coord)
      dapc_df$Group <- dapc_res$grp
      
      # Calculate % variance explained for LD1 and LD2
      eig <- dapc_res$eig
      eig_percent <- round(100 * eig / sum(eig), 1)
      ld1_label <- paste0("LD1 (", eig_percent[1], "%)")
      ld2_label <- paste0("LD2 (", eig_percent[2], "%)")
      
      p <- ggplot2::ggplot(dapc_df, ggplot2::aes(x = LD1, y = LD2)) +
        ggplot2::xlab(ld1_label) +
        ggplot2::ylab(ld2_label)
      
      if (isTRUE(input$dapc_outline_points)) {
        p <- p + ggplot2::geom_point(
          aes(fill = Group),
          shape = 21,
          size = input$dapc_point_size,
          color = "black",
          stroke = input$dapc_point_stroke
        ) +
          get_fill_scale(plot_palette())
      } else {
        p <- p + ggplot2::geom_point(
          aes(fill = Group, color = Group),
          shape = 21,
          size = input$dapc_point_size
        ) +
          get_fill_scale(plot_palette()) +
          get_color_scale(plot_palette())
      }
      
      if (isTRUE(input$dapc_ellipse)) {
        if (isTRUE(input$dapc_outline)) {
          p <- p + ggplot2::stat_ellipse(
            aes(group = Group, fill = Group, color = Group),
            type = "norm",
            level = 0.67,
            geom = "polygon",
            alpha = input$dapc_alpha_ellipse,
            size = input$dapc_outline_stroke,
            show.legend = FALSE
          )
        } else {
          p <- p + ggplot2::stat_ellipse(
            aes(group = Group, fill = Group),
            color = NA,
            type = "norm",
            level = 0.67,
            geom = "polygon",
            alpha = input$dapc_alpha_ellipse,
            show.legend = FALSE
          )
        }
      }
      
      if (isTRUE(input$dapc_convex)) {
        hull_df <- dplyr::bind_rows(lapply(split(dapc_df, dapc_df$Group), function(df) {
          df[chull(df$LD1, df$LD2), ]
        }), .id = "Group")
        
        if (isTRUE(input$dapc_outline)) {
          p <- p + ggplot2::geom_polygon(
            data = hull_df,
            aes(x = LD1, y = LD2, group = Group, fill = Group, color = Group),
            alpha = input$dapc_alpha_ellipse,
            size = input$dapc_outline_stroke,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
        } else {
          p <- p + ggplot2::geom_polygon(
            data = hull_df,
            aes(x = LD1, y = LD2, group = Group, fill = Group),
            color = NA,
            alpha = input$dapc_alpha_ellipse,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
        }
      }
      
      if (isTRUE(input$dapc_centroids)) {
        centroids <- dapc_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(LD1 = mean(LD1), LD2 = mean(LD2), .groups = "drop")
        
        p <- p + ggplot2::geom_point(
          data = centroids,
          aes(x = LD1, y = LD2),
          shape = 8,
          size = 4,
          color = "black",
          fill = "white",
          stroke = 1,
          inherit.aes = FALSE
        )
      }
      
      p +
        get_color_scale(plot_palette()) +
        get_fill_scale(plot_palette()) +
        get_custom_theme(
          plot_axis_text_size(),
          plot_axis_label_size(),
          0,
          plot_facet_size(),
          legend_text_size(),
          legend_title_size(),
          input$plot_theme
        )
      
    })
    
    
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
    
    output$plot_pca <- renderPlot(
      { plot_pca_obj() },
      height = function() input$plot_pca_height,
      width = function() input$plot_pca_width 
    )
    output$plot_dapc <- renderPlot(
      { plot_dapc_obj() },
      height = function() input$plot_dapc_height,
      width = function() input$plot_dapc_width 
    )
    
    # Default download dimensions (in inches)
    DEFAULT_DOWNLOAD_WIDTH <- 10
    DEFAULT_DOWNLOAD_HEIGHT <- 8 # This will mostly be overridden by user input now
    
    # The download function for all plots
    create_download_handler <- function(plot_obj_reactive, filename_prefix, type = "pdf", height_input_id, width_input_id) { # Added width_input_id
      downloadHandler(
        filename = function() { paste0(filename_prefix, "_", Sys.Date(), ".", type) },
        content = function(file) {
          # Get user-specified height and width from the numericInputs
          # Use ns() because the input ID is namespaced within the module
          plot_height_val_px <- input[[height_input_id]]
          plot_width_val_px <- input[[width_input_id]] # GET WIDTH INPUT
          
          # Convert pixels to inches for ggsave (assuming 96 dpi for web display)
          plot_height_val_in <- if (!is.null(plot_height_val_px) && plot_height_val_px > 0) {
            plot_height_val_px / 96
          } else {
            DEFAULT_DOWNLOAD_HEIGHT # Fallback if input is null or zero
          }
          
          plot_width_val_in <- if (!is.null(plot_width_val_px) && plot_width_val_px > 0) {
            plot_width_val_px / 96
          } else {
            # Fallback to a default or current rendered width if input is null or zero
            # For download, it's better to explicitly use the input or a fixed default
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
    output$download_scatter_pdf <- create_download_handler(plot_scatter_obj, "scatterplot_meristic", "pdf", "plot_scatter_height", "plot_scatter_width")
    output$download_scatter_jpeg <- create_download_handler(plot_scatter_obj, "scatterplot_meristic", "jpeg", "plot_scatter_height", "plot_scatter_width")
    
    output$download_box_pdf <- create_download_handler(plot_box_obj, "boxplot_meristic", "pdf", "plot_box_height", "plot_box_width")
    output$download_box_jpeg <- create_download_handler(plot_box_obj, "boxplot_meristic", "jpeg", "plot_box_height", "plot_box_width")
    output$download_violin_pdf <- create_download_handler(plot_violin_obj, "violinplot_meristic", "pdf", "plot_violin_height", "plot_violin_width")
    output$download_violin_jpeg <- create_download_handler(plot_violin_obj, "violinplot_meristic", "jpeg", "plot_violin_height", "plot_violin_width")
    
    output$download_pca_pdf <- create_download_handler(plot_pca_obj, "pca_meristic", "pdf", "plot_pca_height", "plot_pca_width")
    output$download_pca_jpeg <- create_download_handler(plot_pca_obj, "pca_meristic", "jpeg", "plot_pca_height", "plot_pca_width")
    
    output$download_dapc_pdf <- create_download_handler(plot_dapc_obj, "dapc_meristic", "pdf", "plot_dapc_height", "plot_dapc_width")
    output$download_dapc_jpeg <- create_download_handler(plot_dapc_obj, "dapc_meristic", "jpeg", "plot_dapc_height", "plot_dapc_width")
  })
}
