
mod_visual_ui_combined <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Data Visualization"),
    hr(),
    tags$style(HTML(paste0(
      "#", ns("visual_tab"), " > li > a {",
      "  background: none; font-weight: bold;",
      "}",
      "#", ns("visual_tab"), " > li.active > a,",
      "#", ns("visual_tab"), " > li.active > a:hover,",
      "#", ns("visual_tab"), " > li.active > a:focus {",
      "  background-color: #e8f4fc; font-weight: bold;",
      "  border-top: 3px solid #337ab7;",
      "}",
      "#", ns("visual_subtabs_exploratory"), " > li > a,",
      "#", ns("visual_subtabs_mfa"), " > li > a,",
      "#", ns("mfa_bedda_vis_subtabs"), " > li > a {",
      "  background: none; white-space: nowrap;",
      "  border-bottom: 2px solid transparent;",
      "}",
      "#", ns("visual_subtabs_exploratory"), " > li.active > a,",
      "#", ns("visual_subtabs_exploratory"), " > li.active > a:hover,",
      "#", ns("visual_subtabs_exploratory"), " > li.active > a:focus,",
      "#", ns("visual_subtabs_mfa"), " > li.active > a,",
      "#", ns("visual_subtabs_mfa"), " > li.active > a:hover,",
      "#", ns("visual_subtabs_mfa"), " > li.active > a:focus,",
      "#", ns("mfa_bedda_vis_subtabs"), " > li.active > a,",
      "#", ns("mfa_bedda_vis_subtabs"), " > li.active > a:hover,",
      "#", ns("mfa_bedda_vis_subtabs"), " > li.active > a:focus {",
      "  background: none; font-weight: bold; white-space: nowrap;",
      "  border-bottom: 2px solid #337ab7;"
    ))),
    tabsetPanel(id = ns("visual_tab"),
                tabPanel("Exploratory Plots",
                         tabsetPanel(id = ns("visual_subtabs_exploratory"),
                                     tabPanel("Scatterplot",
                                              fluidRow(
                                                column(9,
                                                       uiOutput(ns("scatter_plot_ui"))
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       tags$div(
                                                         style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
                                                         checkboxInput(ns("scatter_interactive"), tags$strong("\U0001f5b1 Interactive Mode"), value = FALSE)
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("scatter_interactive")),
                                                         tags$div(
                                                           style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
                                                           tags$p(style = "margin: 0;", "Hover over points to see specimen IDs. Outline points, individual labels, and theme selection are not available in interactive mode.")
                                                         )
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("scatter_interactive")),
                                                         numericInput(ns("plot_scatter_height"), "Plot Height (px)", value = 500, min = 200, step = 50)
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("scatter_interactive")),
                                                         numericInput(ns("plot_scatter_width"), "Plot Width (px)", value = 600, min = 200, step = 50)
                                                       ),
                                                       hr(),
                                                       uiOutput(ns("scatter_controls")),
                                                       hr(),
                                                       numericInput(ns("scatter_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("scatter_interactive")),
                                                         checkboxInput(ns("scatter_outline_points"), "Outline Points", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("scatter_outline_points")),
                                                           sliderInput(ns("scatter_point_stroke"), "Point Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                         )
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("scatter_interactive")),
                                                         checkboxInput(ns("scatter_show_labels"), "Show Individual Labels", value = FALSE)
                                                       ),
                                                       checkboxInput(ns("scatter_show_lm"), "Show Regression Line", value = TRUE),
                                                       checkboxInput(ns("scatter_show_lm_se"), "Show Confidence Interval", value = TRUE),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("scatter_interactive")),
                                                         downloadButton(ns("download_scatter_pdf"), "Download PDF"),
                                                         br(),
                                                         downloadButton(ns("download_scatter_jpeg"), "Download JPEG")
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("scatter_interactive")),
                                                         p(em("Use the camera icon in the plot toolbar to download."))
                                                       ),
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
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       numericInput(ns("plot_box_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                                       numericInput(ns("plot_box_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px'),
                                                       hr(),
                                                       sliderInput(ns("box_outline_stroke"), "Boxplot Outline Width",
                                                                   min = 0, max = 2, value = 0.5, step = 0.1, width = '150px'),
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
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       numericInput(ns("plot_violin_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                                       numericInput(ns("plot_violin_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px'),
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
                                     
                         )
                ),
                
                tabPanel("MFA",
                         tabsetPanel(id = ns("visual_subtabs_mfa"),
                                     tabPanel("Eigenvalues",
                                              fluidRow(
                                                column(9,
                                                       plotOutput(ns("mfa_eigen_plot"))
                                                ),
                                                column(3,
                                                       br(),
                                                       numericInput(ns("mfa_eigen_plot_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '100%'),
                                                       numericInput(ns("mfa_eigen_plot_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '100%'),
                                                       hr(),
                                                       downloadButton(ns("download_mfa_eigen_pdf"), "Download PDF"),
                                                       br(),
                                                       downloadButton(ns("download_mfa_eigen_jpeg"), "Download JPEG"),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Individuals",
                                              fluidRow(
                                                column(9,
                                                       uiOutput(ns("mfa_individuals_plot_ui"))
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       tags$div(
                                                         style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
                                                         checkboxInput(ns("mfa_interactive"), tags$strong("\U0001f5b1 Interactive Mode"), value = FALSE)
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_interactive")),
                                                         tags$div(
                                                           style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
                                                           tags$p(style = "margin: 0;", "Hover over points to see specimen IDs. Biplot arrows, qualitative categories, outline points, spider plot, MST, centroid distances, and theme selection are not available in interactive mode.")
                                                         )
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_interactive")),
                                                         numericInput(ns("mfa_individuals_plot_height"), "Plot Height (px)", value = 500, min = 300, step = 50, width = '150px')
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_interactive")),
                                                         numericInput(ns("mfa_individuals_plot_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px')
                                                       ),
                                                       hr(),
                                                       h5("MFA Dimension Selection"),
                                                       uiOutput(ns("mfa_x_axis_selector")),
                                                       uiOutput(ns("mfa_y_axis_selector")),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_interactive")),
                                                         h5("Biplot Options"),
                                                         checkboxInput(ns("mfa_biplot_quanti"), "Show Quantitative Variables (Arrows)", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("mfa_biplot_quanti")),
                                                           checkboxInput(ns("mfa_quanti_labels"), "Show Quantitative Labels", value = TRUE),
                                                           colourInput(ns("mfa_arrow_color"), "Arrow Color:", value = "#8B0000", showColour = "background"),
                                                           sliderInput(ns("mfa_arrow_size"), "Arrow Width:", min = 0.5, max = 3, value = 1, step = 0.1, width = '150px'),
                                                           sliderInput(ns("mfa_arrow_alpha"), "Arrow Transparency:", min = 0.1, max = 1, value = 0.7, step = 0.1, width = '150px'),
                                                           numericInput(ns("mfa_arrow_label_size"), "Arrow Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px')
                                                         ),
                                                         checkboxInput(ns("mfa_biplot_quali"), "Show Qualitative Categories (Points)", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("mfa_biplot_quali")),
                                                           checkboxInput(ns("mfa_quali_labels"), "Show Category Labels", value = TRUE),
                                                           colourInput(ns("mfa_quali_color"), "Category Color:", value = "#0066CC", showColour = "background"),
                                                           numericInput(ns("mfa_quali_size"), "Category Point Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px'),
                                                           numericInput(ns("mfa_quali_label_size"), "Category Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px')
                                                         ),
                                                       ),  # end biplot conditionalPanel
                                                       hr(),
                                                       h5("Points & Groups"),
                                                       numericInput(ns("mfa_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_interactive")),
                                                         checkboxInput(ns("mfa_outline_points"), "Outline Points", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("mfa_outline_points")),
                                                           sliderInput(ns("mfa_point_stroke"), "Point Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                         )
                                                       ),
                                                       checkboxInput(ns("mfa_centroids"), "Group Centroids", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_centroids")),
                                                         numericInput(ns("mfa_centroid_size"), "Centroid Size:", value = 4, min = 1, max = 15, width = '150px'),
                                                         colourInput(ns("mfa_centroid_color"), "Centroid Color:", value = "#000000", showColour = "background")
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_interactive")),
                                                         checkboxInput(ns("mfa_spider"), "Show Spider Plot", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("mfa_spider")),
                                                           sliderInput(ns("mfa_spider_alpha"), "Spider Line Transparency:", min = 0.1, max = 1, value = 0.4, step = 0.1, width = '150px'),
                                                           sliderInput(ns("mfa_spider_width"), "Spider Line Width:", min = 0.1, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                         ),
                                                         checkboxInput(ns("mfa_centroid_distances"), "Show Centroid Distances", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("mfa_centroid_distances")),
                                                           colourInput(ns("mfa_centroid_dist_color"), "Distance Line Color:", value = "#444444", showColour = "background"),
                                                           sliderInput(ns("mfa_centroid_dist_width"), "Distance Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                                           sliderInput(ns("mfa_centroid_dist_alpha"), "Distance Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                                           numericInput(ns("mfa_centroid_dist_label_size"), "Distance Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px'),
                                                           helpText("Euclidean distances between centroids in 2D MFA space. Not equivalent to PERMANOVA distances.")
                                                         ),
                                                         checkboxInput(ns("mfa_mst"), "Show Minimum Spanning Tree", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("mfa_mst")),
                                                           colourInput(ns("mfa_mst_color"), "MST Line Color:", value = "#222222", showColour = "background"),
                                                           sliderInput(ns("mfa_mst_alpha"), "MST Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                                           sliderInput(ns("mfa_mst_width"), "MST Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                                           checkboxInput(ns("mfa_mst_labels"), "Show MST Edge Distances", value = FALSE),
                                                           conditionalPanel(
                                                             condition = sprintf("input['%s']", ns("mfa_mst_labels")),
                                                             numericInput(ns("mfa_mst_label_size"), "MST Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px')
                                                           )
                                                         )
                                                       ),
                                                       checkboxInput(ns("mfa_ellipse"), "95% Confidence Ellipses", value = FALSE),
                                                       checkboxInput(ns("mfa_convex_hull"), "Convex Hulls", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_outline_shapes")),
                                                         sliderInput(ns("mfa_outline_stroke"), "Ellipse/Hull Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s'] || input['%s']", ns("mfa_ellipse"), ns("mfa_convex_hull")),
                                                         checkboxInput(ns("mfa_outline_shapes"), "Outline Ellipse or Hull", value = FALSE)
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s'] || input['%s']", ns("mfa_ellipse"), ns("mfa_convex_hull")),
                                                         sliderInput(ns("mfa_ellipse_alpha"), "Ellipse/Hull Fill Transparency", min = 0, max = 1, value = 0.4, step = 0.05, width = '150px')
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_interactive")),
                                                         downloadButton(ns("download_mfa_individuals_pdf"), "Download PDF"),
                                                         br(),
                                                         downloadButton(ns("download_mfa_individuals_jpeg"), "Download JPEG")
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_interactive")),
                                                         p(em("Use the camera icon in the plot toolbar to download."))
                                                       ),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Individuals (3D)",
                                              fluidRow(
                                                column(9,
                                                       plotly::plotlyOutput(ns("mfa_individuals_3d_plot"), height = "580px")
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       h5("Dimension Selection"),
                                                       uiOutput(ns("mfa_3d_x_selector")),
                                                       uiOutput(ns("mfa_3d_y_selector")),
                                                       uiOutput(ns("mfa_3d_z_selector")),
                                                       hr(),
                                                       numericInput(ns("mfa_3d_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                                       sliderInput(ns("mfa_3d_point_alpha"), "Point Opacity:", min = 0.1, max = 1, value = 0.8, step = 0.05, width = '150px'),
                                                       hr(),
                                                       h5("Centroids"),
                                                       checkboxInput(ns("mfa_3d_centroids"), "Show Group Centroids", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_3d_centroids")),
                                                         numericInput(ns("mfa_3d_centroid_size"), "Centroid Size:", value = 4, min = 2, max = 20, width = '150px'),
                                                         colourInput(ns("mfa_3d_centroid_color"), "Centroid Color:", value = "#000000", showColour = "background")
                                                       ),
                                                       hr(),
                                                       h5("Convex Hulls"),
                                                       checkboxInput(ns("mfa_3d_hull"), "Show Convex Hulls", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_3d_hull")),
                                                         sliderInput(ns("mfa_3d_hull_alpha"), "Hull Opacity:", min = 0.05, max = 0.5, value = 0.15, step = 0.05, width = '150px')
                                                       ),
                                                       hr(),
                                                       h5("Spider Plot"),
                                                       checkboxInput(ns("mfa_3d_spider"), "Show Spider Lines", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_3d_spider")),
                                                         sliderInput(ns("mfa_3d_spider_alpha"), "Spider Line Opacity:", min = 0.1, max = 1, value = 0.4, step = 0.05, width = '150px'),
                                                         sliderInput(ns("mfa_3d_spider_width"), "Spider Line Width:", min = 1, max = 6, value = 2, step = 1, width = '150px')
                                                       ),
                                                       hr(),
                                                       p(em("Use the camera icon in the plot toolbar to download."))
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Group Contributions",
                                              fluidRow(
                                                column(9,
                                                       plotOutput(ns("mfa_group_contrib_hist"))
                                                ),
                                                column(3,
                                                       br(),
                                                       numericInput(ns("mfa_group_contrib_hist_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '100%'),
                                                       numericInput(ns("mfa_group_contrib_hist_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '100%'),
                                                       hr(),
                                                       uiOutput(ns("mfa_group_contrib_dim_selector")),
                                                       checkboxInput(ns("mfa_group_contrib_show_refline"), "Show reference line", value = TRUE),
                                                       p(style = "font-size: 0.85em; color: #555;",
                                                         "The red dashed reference line marks the expected contribution if all groups contributed equally (100 / number of groups).",
                                                         "Bars exceeding this threshold contribute more than average to the selected dimension."),
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
                                     
                                     tabPanel("Variable Contributions",
                                              fluidRow(
                                                column(9,
                                                       plotOutput(ns("mfa_var_contrib_hist"))
                                                ),
                                                column(3,
                                                       br(),
                                                       numericInput(ns("mfa_var_contrib_hist_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '100%'),
                                                       numericInput(ns("mfa_var_contrib_hist_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '100%'),
                                                       hr(),
                                                       uiOutput(ns("mfa_var_contrib_dim_selector")),
                                                       checkboxInput(ns("mfa_var_contrib_show_refline"), "Show reference line", value = TRUE),
                                                       p(style = "font-size: 0.85em; color: #555;",
                                                         "The red dashed reference line marks the expected contribution if all variables contributed equally (100 / number of variables).",
                                                         "Bars exceeding this threshold are the primary drivers of the selected dimension."),
                                                       hr(),
                                                       downloadButton(ns("download_mfa_var_contrib_pdf"), "Download PDF"),
                                                       br(),
                                                       downloadButton(ns("download_mfa_var_contrib_jpeg"), "Download JPEG"),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     ## ================================================================
                                     ## DAMF TAB
                                     ## ================================================================
                                     tabPanel("DAMF (Discriminant Analysis of Multiple Factors)",
                                              fluidRow(
                                                column(9,
                                                       uiOutput(ns("damfa_plot_ui"))
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       tags$div(
                                                         style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
                                                         checkboxInput(ns("damfa_interactive"), tags$strong("\U0001f5b1 Interactive Mode"), value = FALSE)
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("damfa_interactive")),
                                                         tags$div(
                                                           style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
                                                           tags$p(style = "margin: 0;", "Hover over points to see specimen IDs. Outline points, spider plot, MST, centroid distances, and theme selection are not available in interactive mode.")
                                                         )
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("damfa_interactive")),
                                                         numericInput(ns("plot_damfa_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px')
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("damfa_interactive")),
                                                         numericInput(ns("plot_damfa_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px')
                                                       ),
                                                       hr(),
                                                       uiOutput(ns("n_mfa_damfa_ui")),
                                                       numericInput(ns("damfa_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("damfa_interactive")),
                                                         checkboxInput(ns("damfa_outline_points"), "Outline Points", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("damfa_outline_points")),
                                                           sliderInput(ns("damfa_point_stroke"), "Point Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                         )
                                                       ),
                                                       checkboxInput(ns("damfa_centroids"), "Group Centroids", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("damfa_centroids")),
                                                         numericInput(ns("damfa_centroid_size"), "Centroid Size:", value = 4, min = 1, max = 15, width = '150px'),
                                                         colourInput(ns("damfa_centroid_color"), "Centroid Color:", value = "#000000", showColour = "background")
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("damfa_interactive")),
                                                         checkboxInput(ns("damfa_spider"), "Show Spider Plot", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("damfa_spider")),
                                                           sliderInput(ns("damfa_spider_alpha"), "Spider Line Transparency:", min = 0.1, max = 1, value = 0.4, step = 0.1, width = '150px'),
                                                           sliderInput(ns("damfa_spider_width"), "Spider Line Width:", min = 0.1, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                         ),
                                                         checkboxInput(ns("damfa_centroid_distances"), "Show Centroid Distances", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("damfa_centroid_distances")),
                                                           colourInput(ns("damfa_centroid_dist_color"), "Distance Line Color:", value = "#444444", showColour = "background"),
                                                           sliderInput(ns("damfa_centroid_dist_width"), "Distance Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                                           sliderInput(ns("damfa_centroid_dist_alpha"), "Distance Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                                           numericInput(ns("damfa_centroid_dist_label_size"), "Distance Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px'),
                                                           helpText("Euclidean distances in 2D DAMF space. Not equivalent to PERMANOVA distances.")
                                                         ),
                                                         checkboxInput(ns("damfa_mst"), "Show Minimum Spanning Tree", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("damfa_mst")),
                                                           colourInput(ns("damfa_mst_color"), "MST Line Color:", value = "#222222", showColour = "background"),
                                                           sliderInput(ns("damfa_mst_alpha"), "MST Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                                           sliderInput(ns("damfa_mst_width"), "MST Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                                           checkboxInput(ns("damfa_mst_labels"), "Show MST Edge Distances", value = FALSE),
                                                           conditionalPanel(
                                                             condition = sprintf("input['%s']", ns("damfa_mst_labels")),
                                                             numericInput(ns("damfa_mst_label_size"), "MST Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px')
                                                           )
                                                         )
                                                       ),
                                                       checkboxInput(ns("damfa_ellipse"), "67% Confidence Ellipses (following adegenet)", value = FALSE),
                                                       checkboxInput(ns("damfa_convex"), "Convex Hulls", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s'] || input['%s']", ns("damfa_ellipse"), ns("damfa_convex")),
                                                         checkboxInput(ns("damfa_outline_shapes"), "Outline Ellipses/Hulls", value = FALSE),
                                                         conditionalPanel(
                                                           condition = sprintf("input['%s']", ns("damfa_outline_shapes")),
                                                           sliderInput(ns("damfa_outline_stroke"), "Ellipse/Hull Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                         ),
                                                         sliderInput(ns("damfa_alpha_ellipse"), "Ellipse/Hull Fill Transparency", min = 0, max = 1, value = 0.3, step = 0.05, width = '150px')
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("damfa_interactive")),
                                                         downloadButton(ns("download_damfa_pdf"), "Download PDF"),
                                                         br(),
                                                         downloadButton(ns("download_damfa_jpeg"), "Download JPEG")
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("damfa_interactive")),
                                                         p(em("Use the camera icon in the plot toolbar to download."))
                                                       ),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("DAMF (3D)",
                                              fluidRow(
                                                column(9,
                                                       plotly::plotlyOutput(ns("plot_damfa_3d"), height = "580px")
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       uiOutput(ns("damfa_3d_status_ui")),
                                                       hr(),
                                                       numericInput(ns("damfa_3d_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                                       sliderInput(ns("damfa_3d_point_alpha"), "Point Opacity:", min = 0.1, max = 1, value = 0.8, step = 0.05, width = '150px'),
                                                       hr(),
                                                       h5("Centroids"),
                                                       checkboxInput(ns("damfa_3d_centroids"), "Show Group Centroids", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("damfa_3d_centroids")),
                                                         numericInput(ns("damfa_3d_centroid_size"), "Centroid Size:", value = 4, min = 2, max = 20, width = '150px'),
                                                         colourInput(ns("damfa_3d_centroid_color"), "Centroid Color:", value = "#000000", showColour = "background")
                                                       ),
                                                       hr(),
                                                       h5("Convex Hulls"),
                                                       checkboxInput(ns("damfa_3d_hull"), "Show Convex Hulls", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("damfa_3d_hull")),
                                                         sliderInput(ns("damfa_3d_hull_alpha"), "Hull Opacity:", min = 0.05, max = 0.5, value = 0.15, step = 0.05, width = '150px')
                                                       ),
                                                       hr(),
                                                       h5("Spider Plot"),
                                                       checkboxInput(ns("damfa_3d_spider"), "Show Spider Lines", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("damfa_3d_spider")),
                                                         sliderInput(ns("damfa_3d_spider_alpha"), "Spider Line Opacity:", min = 0.1, max = 1, value = 0.4, step = 0.05, width = '150px'),
                                                         sliderInput(ns("damfa_3d_spider_width"), "Spider Line Width:", min = 1, max = 6, value = 2, step = 1, width = '150px')
                                                       ),
                                                       hr(),
                                                       p(em("Use the camera icon in the plot toolbar to download."))
                                                )
                                              )
                                     ),
                                     
                                     ## ================================================================
                                     ## MFA DELIMITATION VISUALIZATIONS TAB
                                     ## ================================================================
                         )
                ),
                
                tabPanel("MFA Delimitation",
                         tabsetPanel(id = ns("mfa_bedda_vis_subtabs"),
                                     
                                     tabPanel("Unsupervised Clustering (Model Comparison)",
                                              fluidRow(
                                                column(9,
                                                       plotOutput(ns("plot_mfa_bic"))
                                                ),
                                                column(3,
                                                       br(),
                                                       numericInput(ns("plot_mfa_bic_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                                       numericInput(ns("plot_mfa_bic_width"),  "Plot Width (px)",  value = 600, min = 200, step = 50, width = '150px'),
                                                       hr(),
                                                       downloadButton(ns("download_mfa_bic_pdf"),  "Download PDF"),
                                                       br(),
                                                       downloadButton(ns("download_mfa_bic_jpeg"), "Download JPEG"),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Unsupervised Clustering",
                                              fluidRow(
                                                column(9,
                                                       tags$div(
                                                         style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 10px 0 5px 0; font-size: 0.9em;",
                                                         tags$p(style = "margin: 0;",
                                                                icon("info-circle"),
                                                                strong(" Note:"),
                                                                "GMM cluster assignments overlaid on the MFA Individuals plot. Colors = clusters; shapes = original OTUs.")
                                                       ),
                                                       uiOutput(ns("mfa_clusters_plot_ui"))
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       tags$div(
                                                         style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
                                                         checkboxInput(ns("mfa_clusters_interactive"), tags$strong("\U0001f5b1 Interactive Mode"), value = FALSE)
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_clusters_interactive")),
                                                         tags$div(
                                                           style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
                                                           tags$p(style = "margin: 0;", "Hover over points to see specimen IDs, OTU, and cluster assignment. Outline points and ellipse/hull outlines are not available in interactive mode.")
                                                         )
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_clusters_interactive")),
                                                         numericInput(ns("plot_mfa_clusters_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px')
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_clusters_interactive")),
                                                         numericInput(ns("plot_mfa_clusters_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px')
                                                       ),
                                                       hr(),
                                                       uiOutput(ns("mfa_cluster_model_selector")),
                                                       uiOutput(ns("mfa_cluster_x_selector")),
                                                       uiOutput(ns("mfa_cluster_y_selector")),
                                                       hr(),
                                                       numericInput(ns("mfa_clusters_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                                       checkboxInput(ns("mfa_clusters_centroids"), "Cluster Centroids", value = FALSE),
                                                       checkboxInput(ns("mfa_clusters_ellipse"), "Cluster Ellipses", value = FALSE),
                                                       checkboxInput(ns("mfa_clusters_convex"), "Convex Hulls", value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s'] || input['%s']", ns("mfa_clusters_ellipse"), ns("mfa_clusters_convex")),
                                                         conditionalPanel(
                                                           condition = sprintf("!input['%s']", ns("mfa_clusters_interactive")),
                                                           checkboxInput(ns("mfa_clusters_outline_shapes"), "Outline Ellipses/Hulls", value = FALSE),
                                                           conditionalPanel(
                                                             condition = sprintf("input['%s']", ns("mfa_clusters_outline_shapes")),
                                                             sliderInput(ns("mfa_clusters_outline_stroke"), "Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                                           )
                                                         ),
                                                         sliderInput(ns("mfa_clusters_alpha_ellipse"), "Transparency", min = 0, max = 1, value = 0.3, step = 0.05, width = '150px')
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_clusters_interactive")),
                                                         downloadButton(ns("download_mfa_clusters_pdf"),  "Download PDF"),
                                                         br(),
                                                         downloadButton(ns("download_mfa_clusters_jpeg"), "Download JPEG")
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_clusters_interactive")),
                                                         p(em("Use the camera icon in the plot toolbar to download."))
                                                       ),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     ## ---- Unsupervised Clustering (OTU Correspondence) ----
                                     tabPanel("Unsupervised Clustering (OTU Correspondence)",
                                              fluidRow(
                                                column(9,
                                                       tags$div(
                                                         style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 10px 0 5px 0; font-size: 0.9em;",
                                                         tags$p(style = "margin: 0;",
                                                                icon("info-circle"),
                                                                strong(" Note:"),
                                                                "Proportion of each OTU\u2019s specimens assigned to each unsupervised cluster.",
                                                                "Darker blue = higher proportion; yellow = lower proportion.",
                                                                "Each row (OTU) sums to 1. Reflects the selected model in the Unsupervised Clustering tab.")
                                                       ),
                                                       uiOutput(ns("mfa_post_heatmap_ui"))
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       numericInput(ns("plot_mfa_post_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                                       numericInput(ns("plot_mfa_post_width"),  "Plot Width (px)",  value = 600, min = 200, step = 50, width = '150px'),
                                                       hr(),
                                                       sliderInput(ns("mfa_post_text_size"), "Cell Label Size:", min = 2, max = 8, value = 4, step = 0.5, width = '150px'),
                                                       checkboxInput(ns("mfa_post_show_zeros"), "Show 0.00 cells", value = FALSE),
                                                       hr(),
                                                       downloadButton(ns("download_mfa_post_pdf"),  "Download PDF"),
                                                       br(),
                                                       downloadButton(ns("download_mfa_post_jpeg"), "Download JPEG"),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Topology-aware Hypothesis Testing",
                                              fluidRow(
                                                column(9,
                                                       style = "padding-top: 0;",
                                                       tags$div(
                                                         style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 0 0 5px 0; font-size: 0.9em;",
                                                         tags$p(style = "margin: 0;",
                                                                icon("info-circle"),
                                                                strong(" Note:"),
                                                                "Reference tree with top-ranked phylogenetic hypotheses shown as colored rectangles.",
                                                                "Each column = one hypothesis (BIC-ranked left to right).",
                                                                "Grey = not lumped with any other taxon; colors = lumped clades.")
                                                       ),
                                                       tags$div(style = "margin-top: 0;",
                                                                uiOutput(ns("phylo_tree_plot_ui"))
                                                       )
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-top: 10px; padding-right: 15px;",
                                                       numericInput(ns("plot_phylo_tree_height"), "Plot Height (px)", value = 500, min = 300, step = 50, width = '150px'),
                                                       numericInput(ns("plot_phylo_tree_width"),  "Plot Width (px)",  value = 600, min = 300, step = 50, width = '150px'),
                                                       hr(),
                                                       numericInput(ns("phylo_hyp_n_top"),
                                                                    "Hypotheses to show:",
                                                                    value = 3, min = 1, max = 20, step = 1, width = '150px'),
                                                       
                                                       sliderInput(ns("phylo_tip_label_size"),
                                                                   "Tip label size:",
                                                                   min = 2, max = 6, value = 3, step = 0.5, width = '150px'),
                                                       sliderInput(ns("phylo_rect_alpha"),
                                                                   "Rectangle opacity:",
                                                                   min = 0.3, max = 1, value = 0.80, step = 0.05, width = '150px'),
                                                       hr(),
                                                       downloadButton(ns("download_phylo_tree_pdf"),  "Download PDF"),
                                                       br(),
                                                       downloadButton(ns("download_phylo_tree_jpeg"), "Download JPEG"),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     ## ---- Hypothesis Testing scatter tab (new) ----
                                     tabPanel("User-specified Hypothesis Testing",
                                              fluidRow(
                                                column(9,
                                                       tags$div(
                                                         style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 10px 0 5px 0; font-size: 0.9em;",
                                                         tags$p(style = "margin: 0;",
                                                                icon("info-circle"),
                                                                strong(" Note:"),
                                                                "MFA Individuals plot colored by the selected user-defined hypothesis.",
                                                                "Upload your hypothesis CSV in the MFA Delimitation 2192 User-specified Hypothesis Testing tab first.",
                                                                "Colors = hypothesis groups; shapes = original OTUs.")
                                                       ),
                                                       uiOutput(ns("mfa_hyp_plot_ui"))
                                                ),
                                                column(3,
                                                       style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                                       br(),
                                                       tags$div(
                                                         style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
                                                         checkboxInput(ns("mfa_hyp_interactive"), tags$strong("\U0001f5b1 Interactive Mode"), value = FALSE)
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_hyp_interactive")),
                                                         tags$div(
                                                           style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
                                                           tags$p(style = "margin: 0;", "Hover over points to see specimen ID, OTU, and hypothesis group.")
                                                         )
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_hyp_interactive")),
                                                         numericInput(ns("plot_mfa_hyp_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px')
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_hyp_interactive")),
                                                         numericInput(ns("plot_mfa_hyp_width"),  "Plot Width (px)",  value = 600, min = 200, step = 50, width = '150px')
                                                       ),
                                                       hr(),
                                                       uiOutput(ns("mfa_hyp_col_selector")),
                                                       uiOutput(ns("mfa_hyp_x_selector")),
                                                       uiOutput(ns("mfa_hyp_y_selector")),
                                                       hr(),
                                                       numericInput(ns("mfa_hyp_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                                       checkboxInput(ns("mfa_hyp_centroids"), "Cluster Centroids", value = FALSE),
                                                       checkboxInput(ns("mfa_hyp_ellipse"), "Group Ellipses", value = FALSE),
                                                       checkboxInput(ns("mfa_hyp_convex"),  "Convex Hulls",   value = FALSE),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s'] || input['%s']", ns("mfa_hyp_ellipse"), ns("mfa_hyp_convex")),
                                                         sliderInput(ns("mfa_hyp_alpha_shape"), "Transparency", min = 0, max = 1, value = 0.3, step = 0.05, width = '150px')
                                                       ),
                                                       hr(),
                                                       conditionalPanel(
                                                         condition = sprintf("!input['%s']", ns("mfa_hyp_interactive")),
                                                         downloadButton(ns("download_mfa_hyp_pdf"),  "Download PDF"),
                                                         br(),
                                                         downloadButton(ns("download_mfa_hyp_jpeg"), "Download JPEG")
                                                       ),
                                                       conditionalPanel(
                                                         condition = sprintf("input['%s']", ns("mfa_hyp_interactive")),
                                                         p(em("Use the camera icon in the plot toolbar to download."))
                                                       ),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Diagnostic Characters (Ridge Plot)",
                                              fluidRow(
                                                column(9,
                                                       plotOutput(ns("plot_mfa_boruta_ridge"))
                                                ),
                                                column(3,
                                                       br(),
                                                       numericInput(ns("plot_mfa_boruta_ridge_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                                       numericInput(ns("plot_mfa_boruta_ridge_width"),  "Plot Width (px)",  value = 600, min = 200, step = 50, width = '150px'),
                                                       hr(),
                                                       numericInput(ns("mfa_boruta_ridge_scale"), "Ridge Scale:", value = 4, min = 1, max = 10, step = 0.5, width = '150px'),
                                                       sliderInput(ns("mfa_boruta_ridge_alpha"), "Fill Transparency:", min = 0, max = 1, value = 0.5, step = 0.05, width = '150px'),
                                                       hr(),
                                                       downloadButton(ns("download_mfa_boruta_ridge_pdf"),  "Download PDF"),
                                                       br(),
                                                       downloadButton(ns("download_mfa_boruta_ridge_jpeg"), "Download JPEG"),
                                                       hr()
                                                )
                                              )
                                     ),
                                     
                                     tabPanel("Diagnostic Characters (Box Plot)",
                                              fluidRow(
                                                column(9,
                                                       plotOutput(ns("plot_mfa_boruta_box"))
                                                ),
                                                column(3,
                                                       br(),
                                                       numericInput(ns("plot_mfa_boruta_box_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                                       numericInput(ns("plot_mfa_boruta_box_width"),  "Plot Width (px)",  value = 600, min = 200, step = 50, width = '150px'),
                                                       hr(),
                                                       downloadButton(ns("download_mfa_boruta_box_pdf"),  "Download PDF"),
                                                       br(),
                                                       downloadButton(ns("download_mfa_boruta_box_jpeg"), "Download JPEG"),
                                                       hr()
                                                )
                                              )
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
                                       plot_palette, plot_theme,
                                       plot_axis_text_size,
                                       plot_axis_label_size, plot_x_angle, plot_facet_size,
                                       legend_text_size, legend_title_size,
                                       mfa_point_size, mfa_point_shape, mfa_ellipse,
                                       mfa_ellipse_alpha,
                                       manual_colors_r,
                                       mfa_type_colors_r,
                                       specimen_ids_r = NULL,
                                       mfa_bedda_unsup_results_r  = reactive(NULL),
                                       mfa_bedda_boruta_results_r = reactive(NULL),
                                       mfa_bedda_sup_models_r     = reactive(NULL),
                                       mfa_hyp_data_r             = reactive(NULL)
) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get manual colors from the passed reactive (for OTUs)
    get_manual_colors <- reactive({
      req(manual_colors_r())
      manual_colors_r()
    })
    
    # Separate color control for Variable Contributions plot
    get_mfa_var_type_colors <- reactive({
      req(mfa_type_colors_r())
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
        selectInput(ns("scatter_xvar"), "X Variable", choices = x_choices, selected = x_choices[1]),
        selectInput(ns("scatter_yvar"), "Y Variable", choices = x_choices, selected = x_choices[2]),
        checkboxGroupInput(ns("scatter_group_filter"), "Select Groups to Plot:",
                           choices = unique(df[[group_col]]), selected = unique(df[[group_col]]))
      )
    })
    
    # Add variable selection for box and violin plots
    output$box_variable_selector <- renderUI({
      req(dataset_r())
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
    
    # MFA axis selectors
    output$mfa_x_axis_selector <- renderUI({
      req(mfa_results_r())
      mfa_res <- mfa_results_r()
      
      validate(
        need(!is.null(mfa_res$ind$coord), "MFA coordinates not available")
      )
      
      # Get number of dimensions
      n_dims <- ncol(mfa_res$ind$coord)
      dim_choices <- paste0("Dim.", 1:n_dims)
      
      selectInput(ns("mfa_x_axis"), "X-axis:", 
                  choices = dim_choices, 
                  selected = "Dim.1",
                  width = '150px')
    })
    
    output$mfa_y_axis_selector <- renderUI({
      req(mfa_results_r())
      mfa_res <- mfa_results_r()
      
      validate(
        need(!is.null(mfa_res$ind$coord), "MFA coordinates not available")
      )
      
      # Get number of dimensions
      n_dims <- ncol(mfa_res$ind$coord)
      dim_choices <- paste0("Dim.", 1:n_dims)
      
      selectInput(ns("mfa_y_axis"), "Y-axis:", 
                  choices = dim_choices, 
                  selected = "Dim.2",
                  width = '150px')
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
    
    get_plotly_theme_layout <- function(theme_name) {
      base <- list(
        paper_bgcolor = "white", plot_bgcolor = "white",
        font = list(color = "black"),
        xaxis = list(showgrid = FALSE, showline = TRUE, zeroline = FALSE,
                     linecolor = "black", gridcolor = "#e5e5e5"),
        yaxis = list(showgrid = FALSE, showline = TRUE, zeroline = FALSE,
                     linecolor = "black", gridcolor = "#e5e5e5")
      )
      switch(theme_name,
             "theme_classic" = base,
             "theme_minimal" = modifyList(base, list(
               xaxis = list(showgrid = TRUE, showline = FALSE, zeroline = FALSE, gridcolor = "#e5e5e5"),
               yaxis = list(showgrid = TRUE, showline = FALSE, zeroline = FALSE, gridcolor = "#e5e5e5"))),
             "theme_light" = modifyList(base, list(
               plot_bgcolor = "#f8f8f8",
               xaxis = list(showgrid = TRUE, showline = TRUE, zeroline = FALSE, linecolor = "#cccccc", gridcolor = "white"),
               yaxis = list(showgrid = TRUE, showline = TRUE, zeroline = FALSE, linecolor = "#cccccc", gridcolor = "white"))),
             "theme_dark" = modifyList(base, list(
               paper_bgcolor = "#2d2d2d", plot_bgcolor = "#2d2d2d",
               font = list(color = "white"),
               xaxis = list(showgrid = TRUE, showline = FALSE, zeroline = FALSE, gridcolor = "#555555", tickfont = list(color = "white")),
               yaxis = list(showgrid = TRUE, showline = FALSE, zeroline = FALSE, gridcolor = "#555555", tickfont = list(color = "white")))),
             "theme_void" = modifyList(base, list(
               xaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
               yaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""))),
             "theme_grey" = modifyList(base, list(
               plot_bgcolor = "#ebebeb",
               xaxis = list(showgrid = TRUE, showline = FALSE, zeroline = FALSE, gridcolor = "white"),
               yaxis = list(showgrid = TRUE, showline = FALSE, zeroline = FALSE, gridcolor = "white"))),
             "theme_bw" = modifyList(base, list(
               xaxis = list(showgrid = TRUE, showline = TRUE, zeroline = FALSE, linecolor = "black", gridcolor = "#cccccc"),
               yaxis = list(showgrid = TRUE, showline = TRUE, zeroline = FALSE, linecolor = "black", gridcolor = "#cccccc"))),
             base
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
      
      return(ggplot2::scale_fill_discrete())
    }
    
    # Choose color scale for OTU-based plots
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
      
      return(ggplot2::scale_color_discrete())
    }
    
    # Theme generator
    get_custom_theme <- reactive({
      axis_text_size_val <- plot_axis_text_size()
      axis_label_size_val <- plot_axis_label_size()
      x_angle_val <- plot_x_angle()
      facet_size_val <- plot_facet_size()
      legend_text_size_val <- legend_text_size()
      legend_title_size_val <- legend_title_size()
      
      theme_choice <- plot_theme() %||% "theme_classic"
      
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
        axis.text.y = ggplot2::element_text(size = axis_text_size_val),
        axis.title = ggplot2::element_text(size = axis_label_size_val),
        strip.text = ggplot2::element_text(size = facet_size_val),
        legend.position = "right",
        legend.text = ggplot2::element_text(size = legend_text_size_val),
        legend.title = ggplot2::element_text(size = legend_title_size_val, face = "bold")
      )
      
      theme_base + do.call(ggplot2::theme, theme_elements)
    })
    
    # Reactive to check if common plot inputs are ready
    common_plot_inputs_ready <- reactive({
      req(plot_axis_text_size(), plot_axis_label_size(),
          plot_x_angle(), plot_facet_size(),
          legend_text_size(), legend_title_size(), plot_theme())
      TRUE
    })
    
    # Scatterplot
    plot_scatter_obj <- reactive({
      req(dataset_r(), input$scatter_xvar, input$scatter_yvar, input$scatter_group_filter, group_col_name_r())
      req(input$scatter_point_size)
      
      df        <- dataset_r()
      group_col <- group_col_name_r()
      keep_rows <- df[[group_col]] %in% input$scatter_group_filter
      df        <- df[keep_rows, ]
      ids <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[keep_rows]
      } else {
        as.character(seq_len(nrow(df)))
      }
      df$.label <- ids
      
      x_var_sym <- rlang::sym(input$scatter_xvar)
      y_var_sym <- rlang::sym(input$scatter_yvar)
      group_col_sym <- rlang::sym(group_col)
      
      p <- ggplot(df, aes(x = !!x_var_sym, y = !!y_var_sym, color = !!group_col_sym))
      
      if (isTRUE(input$scatter_outline_points)) {
        p <- p + geom_point(
          aes(fill = !!group_col_sym),
          shape = 21,
          size = input$scatter_point_size,
          color = "black",
          stroke = input$scatter_point_stroke,
          alpha = 0.8
        )
      } else {
        p <- p + geom_point(
          size = input$scatter_point_size,
          alpha = 0.8
        )
      }
      
      if (isTRUE(input$scatter_show_lm)) {
        p <- p + geom_smooth(method = "lm", se = isTRUE(input$scatter_show_lm_se), linetype = "solid")
      }
      
      if (isTRUE(input$scatter_show_labels)) {
        p <- p + geom_text(aes(label = .data[[".label"]]), hjust = 1.1, vjust = 1.1, size = 3, check_overlap = TRUE)
      }
      
      if (isTRUE(input$scatter_outline_points)) {
        p <- p + get_fill_scale_otu(plot_palette())
        p <- p + get_color_scale_otu(plot_palette()) + guides(color = "none")
      } else {
        p <- p + get_color_scale_otu(plot_palette())
      }
      
      p +
        get_custom_theme() +
        labs(color = str_to_title(group_col), fill = str_to_title(group_col))
    })
    
    # Boxplot
    plot_box_obj <- reactive({
      req(dataset_r(), common_plot_inputs_ready(), group_col_name_r())
      req(input$selected_box_traits)
      
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
      
      ggplot(df_long, aes(x = .data[[group_var_name]], y = .data[["Value"]], fill = .data[[group_var_name]])) +
        geom_boxplot(outlier.shape = NA, 
                     alpha = 0.7,
                     color = "black",
                     linewidth = input$box_outline_stroke) +
        facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale_otu(plot_palette()) +
        get_custom_theme() +
        labs(fill = str_to_title(group_var_name))
    })
    
    # Violin plot
    plot_violin_obj <- reactive({
      req(dataset_r(), common_plot_inputs_ready(), group_col_name_r())
      req(input$selected_violin_traits)
      
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
      
      violin_outline_color <- if (isTRUE(input$violin_outline)) "black" else NA
      violin_outline_width  <- if (isTRUE(input$violin_outline)) input$violin_point_stroke else 0
      
      ggplot(df_long, aes(x = .data[[group_var_name]], y = .data[["Value"]], fill = .data[[group_var_name]])) +
        geom_violin(width = 0.9, alpha = 0.6, color = violin_outline_color, linewidth = violin_outline_width) +
        facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale_otu(plot_palette()) +
        get_custom_theme() +
        labs(fill = str_to_title(group_var_name))
    })
    
    # MFA Plot Objects
    mfa_eigen_plot_obj <- reactive({
      req(mfa_results_r())
      mfa_res <- mfa_results_r()
      
      validate(
        need(!is.null(mfa_res), "MFA results are not available."),
        need(inherits(mfa_res, "MFA"), "Invalid MFA results object.")
      )
      
      fviz_eig(mfa_res) +
        get_custom_theme() +
        labs(title = "Eigenvalue Scree Plot")
    })
    
    # Dimension selector helpers — build choices with % variance from MFA eigenvalues
    mfa_dim_choices <- reactive({
      req(mfa_results_r())
      eig <- mfa_results_r()$eig
      pct <- round(eig[, 2], 1)   # % variance per dimension (column 2 of $eig)
      dims <- paste0("Dim.", seq_len(nrow(eig)))
      setNames(dims, paste0("Dim ", seq_len(nrow(eig)), " (", pct, "%)"))
    })
    
    output$mfa_group_contrib_dim_selector <- renderUI({
      req(mfa_dim_choices())
      selectInput(ns("mfa_group_contrib_dim"), "Dimension:",
                  choices = mfa_dim_choices(), selected = "Dim.1", width = "100%")
    })
    
    output$mfa_var_contrib_dim_selector <- renderUI({
      req(mfa_dim_choices())
      selectInput(ns("mfa_var_contrib_dim"), "Dimension:",
                  choices = mfa_dim_choices(), selected = "Dim.1", width = "100%")
    })
    
    mfa_group_contrib_hist_obj <- reactive({
      req(mfa_results_r(), trait_group_df_r(), common_plot_inputs_ready(), get_mfa_type_colors())
      req(input$mfa_group_contrib_dim)
      
      mfa_res      <- mfa_results_r()
      group_contrib <- mfa_res$group$contrib
      if (is.null(group_contrib)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No group contributions available.") + theme_void())
      }
      
      dim_name <- input$mfa_group_contrib_dim
      if (!dim_name %in% colnames(group_contrib)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                   label = paste(dim_name, "not available.")) + theme_void())
      }
      
      # % variance for selected dimension
      dim_idx  <- as.integer(sub("Dim\\.", "", dim_name))
      pct_var  <- round(mfa_res$eig[dim_idx, 2], 1)
      dim_label <- paste0("Dim ", dim_idx, " (", pct_var, "% variance)")
      
      trait_to_type <- trait_group_df_r() %>% dplyr::distinct(Type) %>% dplyr::mutate(Group = Type)
      
      df <- data.frame(Group = rownames(group_contrib),
                       Contribution = group_contrib[, dim_name]) %>%
        dplyr::left_join(trait_to_type, by = "Group") %>%
        dplyr::arrange(Contribution) %>%
        dplyr::mutate(Group = factor(Group, levels = Group),
                      Type  = ifelse(is.na(Type), "Other", Type))
      
      equal_contrib <- 100 / nrow(df)
      
      ggplot(df, aes(x = Group, y = Contribution, fill = Type)) +
        geom_col(width = 0.7) +
        { if (isTRUE(input$mfa_group_contrib_show_refline))
          geom_hline(yintercept = equal_contrib, linetype = "dashed",
                     color = "red", linewidth = 0.8) } +
        coord_flip() +
        labs(title = paste("Group Contributions to", dim_label),
             x = "Group", y = "Contribution (%)", fill = "Group type") +
        scale_fill_manual(values = get_mfa_type_colors(), drop = FALSE) +
        get_custom_theme() +
        theme(legend.position = "bottom")
    })
    
    mfa_var_contrib_hist_obj <- reactive({
      req(mfa_results_r(), trait_group_df_r(), common_plot_inputs_ready(), get_mfa_var_type_colors())
      req(input$mfa_var_contrib_dim)
      
      mfa_res               <- mfa_results_r()
      current_mfa_type_colors <- get_mfa_var_type_colors()
      trait_to_type         <- trait_group_df_r() %>% dplyr::mutate(Variable = as.character(Variable))
      dim_name              <- input$mfa_var_contrib_dim
      
      dim_idx   <- as.integer(sub("Dim\\.", "", dim_name))
      pct_var   <- round(mfa_res$eig[dim_idx, 2], 1)
      dim_label <- paste0("Dim ", dim_idx, " (", pct_var, "% variance)")
      
      quanti_contrib_dim <- NULL
      if (!is.null(mfa_res$quanti.var$contrib) && dim_name %in% colnames(mfa_res$quanti.var$contrib)) {
        quanti_contrib_dim <- as.data.frame(mfa_res$quanti.var$contrib) %>%
          dplyr::select(!!sym(dim_name)) %>%
          tibble::rownames_to_column("Variable") %>%
          dplyr::mutate(Variable_Cleaned = stringr::str_remove(Variable, " \\(.*\\)"),
                        Variable_Plot = Variable,
                        Type = "Unknown")
      }
      
      quali_contrib_dim <- NULL
      if (!is.null(mfa_res$quali.var$contrib) && dim_name %in% colnames(mfa_res$quali.var$contrib)) {
        quali_contrib_data_temp <- as.data.frame(mfa_res$quali.var$contrib) %>%
          dplyr::select(!!sym(dim_name)) %>%
          tibble::rownames_to_column("Variable_Raw_Name") %>%
          dplyr::mutate(Variable_Cleaned = NA_character_, Variable_Plot = NA_character_, Type = "Unknown")
        
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
        
        quali_contrib_dim <- quali_contrib_data_temp %>% dplyr::rename(Variable = Variable_Raw_Name)
      }
      
      current_dim_data <- dplyr::bind_rows(quanti_contrib_dim, quali_contrib_dim)
      if (is.null(current_dim_data) || nrow(current_dim_data) == 0) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                   label = paste("No variable contributions available for", dim_label)) + theme_void())
      }
      
      current_dim_data <- current_dim_data %>%
        dplyr::left_join(trait_to_type, by = c("Variable_Cleaned" = "Variable")) %>%
        dplyr::mutate(Type = ifelse(is.na(Type.y), Type.x, Type.y)) %>%
        dplyr::select(-Type.x, -Type.y) %>%
        dplyr::rename(Contribution = !!sym(dim_name)) %>%
        dplyr::filter(!is.na(Type))
      
      if (nrow(current_dim_data) == 0) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                   label = "No valid variable contributions found.") + theme_void())
      }
      
      ordered_data <- current_dim_data %>%
        dplyr::arrange(Contribution) %>%
        dplyr::mutate(Variable_Plot = factor(Variable_Plot, levels = unique(Variable_Plot)),
                      Type = factor(Type, levels = unique(trait_to_type$Type)))
      
      equal_contrib <- 100 / nrow(ordered_data)
      
      ggplot(ordered_data, aes(x = Variable_Plot, y = Contribution, fill = Type)) +
        geom_col(width = 0.7) +
        { if (isTRUE(input$mfa_var_contrib_show_refline))
          geom_hline(yintercept = equal_contrib, linetype = "dashed",
                     color = "red", linewidth = 0.8) } +
        labs(x = "Variable", y = paste0("Contribution to ", dim_label, " (%)"),
             title = paste("Variable Contributions to", dim_label)) +
        coord_flip() +
        scale_fill_manual(values = get_mfa_var_type_colors(), name = "Group",
                          na.value = "grey50", drop = FALSE) +
        get_custom_theme() +
        theme(legend.position = "bottom")
    })
    
    # MFA Individuals plot object with biplot functionality
    mfa_individuals_plot_obj <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r())
      req(input$mfa_x_axis, input$mfa_y_axis)
      
      mfa_res <- mfa_results_r()
      
      validate(
        need(!is.null(mfa_res), "MFA results are not available."),
        need("ind" %in% names(mfa_res), "MFA results object missing 'ind' component."),
        need("coord" %in% names(mfa_res$ind), "MFA results 'ind' component missing 'coord'."),
        need(nrow(mfa_res$ind$coord) > 0, "MFA individuals coordinates are empty."),
        need(ncol(mfa_res$ind$coord) >= 2, "MFA has less than 2 dimensions.")
      )
      
      req(common_plot_inputs_ready())
      req(input$mfa_point_size)
      
      if (isTRUE(input$mfa_ellipse)) {
        req(input$mfa_ellipse_alpha)
      }
      
      # Get the original data
      df_original <- dataset_r()
      group_col_name <- group_col_name_r()
      
      validate(
        need(group_col_name %in% names(df_original), 
             paste0("Group column '", group_col_name, "' not found in dataset."))
      )
      
      # Get selected dimensions
      dim_x <- input$mfa_x_axis
      dim_y <- input$mfa_y_axis
      
      # Extract dimension numbers
      dim_x_num <- as.numeric(gsub("Dim\\.", "", dim_x))
      dim_y_num <- as.numeric(gsub("Dim\\.", "", dim_y))
      
      # Extract individual coordinates
      mfa_ind_coord <- as.data.frame(mfa_res$ind$coord)
      mfa_ind_coord <- mfa_ind_coord %>%
        tibble::rownames_to_column(var = "Individual_ID")
      
      # Prepare original data for joining
      original_data_for_join <- df_original %>%
        tibble::rownames_to_column(var = "Individual_ID") %>%
        dplyr::select(Individual_ID, Group = !!sym(group_col_name)) %>%
        dplyr::mutate(Group = as.factor(Group))
      
      mfa_ind_coord <- dplyr::left_join(mfa_ind_coord, original_data_for_join, by = "Individual_ID") %>%
        tibble::column_to_rownames(var = "Individual_ID")
      
      validate(
        need(!any(is.na(mfa_ind_coord$Group)), "Group information missing after joining."),
        need(nlevels(mfa_ind_coord$Group) >= 1, "No valid groups found.")
      )
      
      # Base plot with selected dimensions
      p <- ggplot(mfa_ind_coord, aes(x = .data[[dim_x]], y = .data[[dim_y]]))
      
      # Add biplot for QUANTITATIVE variables (arrows) if enabled
      if (isTRUE(input$mfa_biplot_quanti)) {
        if (!is.null(mfa_res$quanti.var$coord)) {
          loadings <- as.data.frame(mfa_res$quanti.var$coord[, c(dim_x_num, dim_y_num)])
          colnames(loadings) <- c("Dim_x", "Dim_y")
          loadings$variable <- rownames(loadings)
          
          # Scale arrows to fit nicely
          data_range_x <- max(abs(range(mfa_ind_coord[[dim_x]])))
          data_range_y <- max(abs(range(mfa_ind_coord[[dim_y]])))
          
          scale_factor <- min(data_range_x / max(abs(loadings$Dim_x)), 
                              data_range_y / max(abs(loadings$Dim_y))) * 0.7
          
          loadings$Dim_x_scaled <- loadings$Dim_x * scale_factor
          loadings$Dim_y_scaled <- loadings$Dim_y * scale_factor
          
          # Add arrows
          p <- p + ggplot2::geom_segment(
            data = loadings,
            aes(x = 0, y = 0, xend = Dim_x_scaled, yend = Dim_y_scaled),
            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed"),
            color = input$mfa_arrow_color,
            linewidth = input$mfa_arrow_size * 0.5,
            alpha = input$mfa_arrow_alpha,
            inherit.aes = FALSE
          )
          
          # Add variable labels if enabled
          if (isTRUE(input$mfa_quanti_labels)) {
            p <- p + ggplot2::geom_text(
              data = loadings,
              aes(x = Dim_x_scaled * 1.1, y = Dim_y_scaled * 1.1, label = variable),
              color = input$mfa_arrow_color,
              size = input$mfa_arrow_label_size,
              hjust = ifelse(loadings$Dim_x_scaled > 0, 0, 1),
              vjust = ifelse(loadings$Dim_y_scaled > 0, 0, 1),
              inherit.aes = FALSE
            )
          }
        }
      }
      
      # Add biplot for QUALITATIVE variables (category points) if enabled
      if (isTRUE(input$mfa_biplot_quali)) {
        if (!is.null(mfa_res$quali.var$coord)) {
          quali_coords <- as.data.frame(mfa_res$quali.var$coord[, c(dim_x_num, dim_y_num)])
          colnames(quali_coords) <- c("Dim_x", "Dim_y")
          quali_coords$category <- rownames(quali_coords)
          
          # Scale qualitative category coordinates similarly to quantitative
          data_range_x <- max(abs(range(mfa_ind_coord[[dim_x]])))
          data_range_y <- max(abs(range(mfa_ind_coord[[dim_y]])))
          
          # Qualitative coordinates are often on a different scale, adjust accordingly
          quali_range_x <- max(abs(range(quali_coords$Dim_x)))
          quali_range_y <- max(abs(range(quali_coords$Dim_y)))
          
          scale_factor_quali <- min(data_range_x / quali_range_x, 
                                    data_range_y / quali_range_y) * 0.6
          
          quali_coords$Dim_x_scaled <- quali_coords$Dim_x * scale_factor_quali
          quali_coords$Dim_y_scaled <- quali_coords$Dim_y * scale_factor_quali
          
          # Add category points
          p <- p + ggplot2::geom_point(
            data = quali_coords,
            aes(x = Dim_x_scaled, y = Dim_y_scaled),
            color = input$mfa_quali_color,
            size = input$mfa_quali_size,
            shape = 17,  # Triangle for qualitative categories
            inherit.aes = FALSE
          )
          
          # Add category labels if enabled
          if (isTRUE(input$mfa_quali_labels)) {
            p <- p + ggplot2::geom_text(
              data = quali_coords,
              aes(x = Dim_x_scaled, y = Dim_y_scaled, label = category),
              color = input$mfa_quali_color,
              size = input$mfa_quali_label_size,
              hjust = -0.2,
              vjust = -0.5,
              inherit.aes = FALSE
            )
          }
        }
      }
      
      # Add points
      if (isTRUE(input$mfa_outline_points)) {
        p <- p + geom_point(
          aes(fill = Group),
          shape = 21,
          size = input$mfa_point_size,
          stroke = input$mfa_point_stroke,
          color = "black"
        )
      } else {
        p <- p + geom_point(
          aes(color = Group),
          shape = 19,
          size = input$mfa_point_size
        )
      }
      
      # Add ellipses
      if (isTRUE(input$mfa_ellipse)) {
        if (isTRUE(input$mfa_outline_shapes)) {
          p <- p + stat_ellipse(
            aes(group = Group, fill = Group, color = Group),
            level = 0.95, geom = "polygon",
            alpha = input$mfa_ellipse_alpha, 
            linewidth = input$mfa_outline_stroke,
            show.legend = FALSE
          )
        } else {
          p <- p + stat_ellipse(
            aes(group = Group, fill = Group),
            level = 0.95, geom = "polygon",
            alpha = input$mfa_ellipse_alpha, color = NA,
            show.legend = FALSE
          )
        }
      }
      
      # Add convex hulls
      if (isTRUE(input$mfa_convex_hull)) {
        hull_df <- mfa_ind_coord %>%
          dplyr::group_by(Group) %>%
          dplyr::filter(n() >= 3) %>%
          dplyr::slice(chull(.data[[dim_x]], .data[[dim_y]])) %>%
          dplyr::ungroup()
        
        if (isTRUE(input$mfa_outline_shapes)) {
          p <- p + geom_polygon(
            data = hull_df,
            aes(x = .data[[dim_x]], y = .data[[dim_y]], group = Group, fill = Group, color = Group),
            alpha = input$mfa_ellipse_alpha,
            linewidth = input$mfa_outline_stroke,
            inherit.aes = FALSE, show.legend = FALSE
          )
        } else {
          p <- p + geom_polygon(
            data = hull_df,
            aes(x = .data[[dim_x]], y = .data[[dim_y]], group = Group, fill = Group),
            color = NA,
            alpha = input$mfa_ellipse_alpha,
            inherit.aes = FALSE, show.legend = FALSE
          )
        }
      }
      
      # Add centroids
      # Always compute centroids — needed by spider, MST, and centroid distances
      # even when centroid points are not shown
      centroids <- mfa_ind_coord %>%
        dplyr::group_by(Group) %>%
        dplyr::summarize(
          x_cent = mean(.data[[dim_x]]),
          y_cent = mean(.data[[dim_y]]),
          .groups = "drop"
        )
      
      # Spider plot
      if (isTRUE(input$mfa_spider)) {
        spider_df <- mfa_ind_coord %>%
          dplyr::left_join(centroids, by = "Group")
        
        p <- p + ggplot2::geom_segment(
          data = spider_df,
          aes(
            x     = .data[[dim_x]],
            y     = .data[[dim_y]],
            xend  = x_cent,
            yend  = y_cent,
            color = Group
          ),
          alpha       = input$mfa_spider_alpha,
          linewidth   = input$mfa_spider_width,
          inherit.aes = FALSE,
          show.legend = FALSE
        ) +
          get_color_scale_otu(plot_palette())
      }
      
      # MST
      if (isTRUE(input$mfa_mst) && nrow(centroids) >= 2) {
        cent_mat <- as.matrix(dist(centroids[, c("x_cent", "y_cent")]))
        rownames(cent_mat) <- centroids$Group
        colnames(cent_mat) <- centroids$Group
        
        mst_obj <- ape::mst(cent_mat)
        mst_idx <- which(mst_obj == 1, arr.ind = TRUE)
        mst_idx <- mst_idx[mst_idx[, 1] < mst_idx[, 2], , drop = FALSE]
        
        mst_df <- data.frame(
          x_start  = centroids$x_cent[mst_idx[, 1]],
          y_start  = centroids$y_cent[mst_idx[, 1]],
          x_end    = centroids$x_cent[mst_idx[, 2]],
          y_end    = centroids$y_cent[mst_idx[, 2]],
          x_mid    = (centroids$x_cent[mst_idx[, 1]] + centroids$x_cent[mst_idx[, 2]]) / 2,
          y_mid    = (centroids$y_cent[mst_idx[, 1]] + centroids$y_cent[mst_idx[, 2]]) / 2,
          distance = round(sqrt(
            (centroids$x_cent[mst_idx[, 2]] - centroids$x_cent[mst_idx[, 1]])^2 +
              (centroids$y_cent[mst_idx[, 2]] - centroids$y_cent[mst_idx[, 1]])^2
          ), 3)
        )
        
        p <- p + ggplot2::geom_segment(
          data = mst_df,
          aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
          color       = input$mfa_mst_color,
          alpha       = input$mfa_mst_alpha,
          linewidth   = input$mfa_mst_width,
          inherit.aes = FALSE
        )
        
        if (isTRUE(input$mfa_mst_labels)) {
          p <- p + ggplot2::geom_label(
            data = mst_df,
            aes(x = x_mid, y = y_mid, label = distance),
            size          = input$mfa_mst_label_size,
            color         = "black",
            fill          = "white",
            label.padding = ggplot2::unit(0.15, "lines"),
            inherit.aes   = FALSE
          )
        }
      }
      
      # Centroid points — only shown when checkbox is on
      if (isTRUE(input$mfa_centroids)) {
        centroid_size <- if (!is.null(input$mfa_centroid_size)) input$mfa_centroid_size else 4
        p <- p + ggplot2::geom_point(
          data = centroids,
          aes(x = x_cent, y = y_cent),
          shape = 8, size = centroid_size,
          color = input$mfa_centroid_color,
          fill = "white", stroke = 1,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
      }
      
      # Pairwise centroid distances
      if (isTRUE(input$mfa_centroid_distances) && nrow(centroids) >= 2) {
        pairs <- combn(nrow(centroids), 2)
        dist_df <- data.frame(
          x_start  = centroids$x_cent[pairs[1, ]],
          y_start  = centroids$y_cent[pairs[1, ]],
          x_end    = centroids$x_cent[pairs[2, ]],
          y_end    = centroids$y_cent[pairs[2, ]],
          x_mid    = (centroids$x_cent[pairs[1, ]] + centroids$x_cent[pairs[2, ]]) / 2,
          y_mid    = (centroids$y_cent[pairs[1, ]] + centroids$y_cent[pairs[2, ]]) / 2,
          distance = round(sqrt(
            (centroids$x_cent[pairs[2, ]] - centroids$x_cent[pairs[1, ]])^2 +
              (centroids$y_cent[pairs[2, ]] - centroids$y_cent[pairs[1, ]])^2
          ), 3)
        )
        
        p <- p +
          ggplot2::geom_segment(
            data = dist_df,
            aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
            color       = input$mfa_centroid_dist_color,
            linewidth   = input$mfa_centroid_dist_width,
            alpha       = input$mfa_centroid_dist_alpha,
            inherit.aes = FALSE
          ) +
          ggplot2::geom_label(
            data = dist_df,
            aes(x = x_mid, y = y_mid, label = distance),
            size          = input$mfa_centroid_dist_label_size,
            color         = "black",
            fill          = "white",
            label.padding = ggplot2::unit(0.15, "lines"),
            inherit.aes   = FALSE
          )
      }
      
      # Apply color scales
      p <- p + get_fill_scale_otu(plot_palette()) + 
        get_color_scale_otu(plot_palette())
      
      # Labels and theme
      p <- p +
        get_custom_theme() +
        labs(
          x = paste0(dim_x, " (", round(mfa_res$eig[dim_x_num, 2], 2), "%)"),
          y = paste0(dim_y, " (", round(mfa_res$eig[dim_y_num, 2], 2), "%)"),
          fill = str_to_title(group_col_name),
          color = str_to_title(group_col_name)
        )
      
      # Guides
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
    
    # ---- Scatter: switch between static and interactive ----
    output$scatter_plot_ui <- renderUI({
      if (isTRUE(input$scatter_interactive)) {
        plotly::plotlyOutput(ns("plot_scatter_plotly"),
                             height = paste0(input$plot_scatter_height %||% 500, "px"))
      } else {
        plotOutput(ns("plot_scatter"),
                   height = paste0(input$plot_scatter_height %||% 500, "px"),
                   width  = paste0(input$plot_scatter_width  %||% 700, "px"))
      }
    })
    
    plot_scatter_plotly <- reactive({
      req(dataset_r(), input$scatter_xvar, input$scatter_yvar, input$scatter_group_filter, group_col_name_r())
      df        <- dataset_r()
      group_col <- group_col_name_r()
      keep_rows <- df[[group_col]] %in% input$scatter_group_filter
      df        <- df[keep_rows, ]
      ids <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[keep_rows]
      } else { as.character(seq_len(nrow(df))) }
      hover_txt <- paste0("ID: ", ids, "<br>Group: ", df[[group_col]],
                          "<br>", input$scatter_xvar, ": ", round(df[[input$scatter_xvar]], 3),
                          "<br>", input$scatter_yvar, ": ", round(df[[input$scatter_yvar]], 3))
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x = .data[[input$scatter_xvar]], y = .data[[input$scatter_yvar]],
        color = .data[[group_col]], text = hover_txt)) +
        ggplot2::geom_point(size = input$scatter_point_size %||% 3, alpha = 0.8) +
        get_color_scale_otu(plot_palette()) +
        ggplot2::labs(color = stringr::str_to_title(group_col))
      if (isTRUE(input$scatter_show_lm))
        p <- p + ggplot2::geom_smooth(ggplot2::aes(text = NULL),
                                      method = "lm", se = isTRUE(input$scatter_show_lm_se), linetype = "solid")
      tl <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick <- plot_axis_text_size() %||% 10; ax_label <- plot_axis_label_size() %||% 12
      leg_text <- legend_text_size() %||% 10;  leg_title <- legend_title_size() %||% 12
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)),
                       paper_bgcolor = tl$paper_bgcolor, plot_bgcolor = tl$plot_bgcolor, font = tl$font,
                       xaxis = modifyList(tl$xaxis, list(autorange = TRUE,
                                                         title = list(text = input$scatter_xvar, font = list(size = ax_label)), tickfont = list(size = ax_tick))),
                       yaxis = modifyList(tl$yaxis, list(autorange = TRUE,
                                                         title = list(text = input$scatter_yvar, font = list(size = ax_label)), tickfont = list(size = ax_tick))),
                       legend = list(font = list(size = leg_text), title = list(font = list(size = leg_title))))
    })
    output$plot_scatter_plotly <- plotly::renderPlotly({
      tryCatch(plot_scatter_plotly(), error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(text = paste("Error:", e$message), showarrow = FALSE))
    })
    
    # ---- MFA Individuals: switch between static and interactive ----
    output$mfa_individuals_plot_ui <- renderUI({
      if (isTRUE(input$mfa_interactive)) {
        plotly::plotlyOutput(ns("mfa_individuals_plot_plotly"),
                             height = paste0(input$mfa_individuals_plot_height %||% 500, "px"))
      } else {
        plotOutput(ns("mfa_individuals_plot"),
                   height = paste0(input$mfa_individuals_plot_height %||% 500, "px"),
                   width  = paste0(input$mfa_individuals_plot_width  %||% 600, "px"))
      }
    })
    
    mfa_individuals_plotly <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r(), input$mfa_x_axis, input$mfa_y_axis)
      mfa_res        <- mfa_results_r()
      df_original    <- dataset_r()
      group_col_name <- group_col_name_r()
      dim_x          <- input$mfa_x_axis
      dim_y          <- input$mfa_y_axis
      dim_x_num      <- as.numeric(gsub("Dim\\.", "", dim_x))
      dim_y_num      <- as.numeric(gsub("Dim\\.", "", dim_y))
      
      mfa_ind_coord <- as.data.frame(mfa_res$ind$coord) %>%
        tibble::rownames_to_column(var = "Row_Index")
      
      original_data_for_join <- df_original %>%
        tibble::rownames_to_column(var = "Row_Index") %>%
        dplyr::select(Row_Index, Group = !!rlang::sym(group_col_name)) %>%
        dplyr::mutate(Group = as.factor(Group))
      
      mfa_ind_coord <- dplyr::left_join(mfa_ind_coord, original_data_for_join, by = "Row_Index")
      
      # Inject specimen IDs positionally (MFA preserves row order)
      mfa_ind_coord$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()
      } else {
        as.character(seq_len(nrow(mfa_ind_coord)))
      }
      
      x_label <- paste0(dim_x, " (", round(mfa_res$eig[dim_x_num, 2], 2), "%)")
      y_label <- paste0(dim_y, " (", round(mfa_res$eig[dim_y_num, 2], 2), "%)")
      
      hover_txt <- paste0(
        "ID: ",     mfa_ind_coord$SpecimenID,
        "<br>Group: ", mfa_ind_coord$Group,
        "<br>",     dim_x, ": ", round(mfa_ind_coord[[dim_x]], 3),
        "<br>",     dim_y, ": ", round(mfa_ind_coord[[dim_y]], 3)
      )
      
      p <- ggplot2::ggplot(mfa_ind_coord, ggplot2::aes(
        x = .data[[dim_x]], y = .data[[dim_y]],
        color = Group, fill = Group, text = hover_txt)) +
        ggplot2::geom_point(size = input$mfa_point_size %||% 3, alpha = 0.8) +
        ggplot2::xlab(x_label) + ggplot2::ylab(y_label) +
        get_color_scale_otu(plot_palette()) + get_fill_scale_otu(plot_palette()) +
        ggplot2::guides(fill = "none") +
        ggplot2::labs(color = stringr::str_to_title(group_col_name))
      
      if (isTRUE(input$mfa_ellipse))
        p <- p + ggplot2::stat_ellipse(
          ggplot2::aes(group = Group, fill = Group, text = NULL),
          color = NA, level = 0.95, geom = "polygon",
          alpha = input$mfa_ellipse_alpha %||% 0.4, show.legend = FALSE)
      
      if (isTRUE(input$mfa_convex_hull)) {
        hull_df <- mfa_ind_coord %>%
          dplyr::group_by(Group) %>% dplyr::filter(dplyr::n() >= 3) %>%
          dplyr::slice(chull(.data[[dim_x]], .data[[dim_y]])) %>% dplyr::ungroup()
        p <- p + ggplot2::geom_polygon(data = hull_df,
                                       ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]], group = Group, fill = Group),
                                       color = NA, alpha = input$mfa_ellipse_alpha %||% 0.4, inherit.aes = FALSE, show.legend = FALSE)
      }
      
      if (isTRUE(input$mfa_centroids)) {
        centroids <- mfa_ind_coord %>% dplyr::group_by(Group) %>%
          dplyr::summarize(x_cent = mean(.data[[dim_x]]), y_cent = mean(.data[[dim_y]]), .groups = "drop")
        p <- p + ggplot2::geom_point(data = centroids, ggplot2::aes(x = x_cent, y = y_cent),
                                     shape = 8, size = input$mfa_centroid_size %||% 4,
                                     color = input$mfa_centroid_color %||% "#000000", inherit.aes = FALSE)
      }
      
      tl <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick <- plot_axis_text_size() %||% 10; ax_label_sz <- plot_axis_label_size() %||% 12
      leg_text <- legend_text_size() %||% 10;  leg_title_sz <- legend_title_size() %||% 12
      
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)),
                       paper_bgcolor = tl$paper_bgcolor, plot_bgcolor = tl$plot_bgcolor, font = tl$font,
                       xaxis = modifyList(tl$xaxis, list(autorange = TRUE,
                                                         title = list(text = x_label, font = list(size = ax_label_sz)), tickfont = list(size = ax_tick))),
                       yaxis = modifyList(tl$yaxis, list(autorange = TRUE,
                                                         title = list(text = y_label, font = list(size = ax_label_sz)), tickfont = list(size = ax_tick))),
                       legend = list(font = list(size = leg_text), title = list(font = list(size = leg_title_sz))))
    })
    output$mfa_individuals_plot_plotly <- plotly::renderPlotly({
      tryCatch(mfa_individuals_plotly(), error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(text = paste("Error:", e$message), showarrow = FALSE))
    })
    
    # ---- MFA Individuals 3D ----
    make_mfa_3d_dim_selector <- function(input_id, label, default_dim) {
      renderUI({
        req(mfa_results_r())
        n_dims     <- ncol(mfa_results_r()$ind$coord)
        dim_choices <- paste0("Dim.", seq_len(n_dims))
        selectInput(ns(input_id), label,
                    choices  = dim_choices,
                    selected = dim_choices[min(default_dim, n_dims)],
                    width    = "150px")
      })
    }
    output$mfa_3d_x_selector <- make_mfa_3d_dim_selector("mfa_3d_x", "X-axis:", 1)
    output$mfa_3d_y_selector <- make_mfa_3d_dim_selector("mfa_3d_y", "Y-axis:", 2)
    output$mfa_3d_z_selector <- make_mfa_3d_dim_selector("mfa_3d_z", "Z-axis:", 3)
    
    mfa_individuals_3d <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r(),
          input$mfa_3d_x, input$mfa_3d_y, input$mfa_3d_z)
      
      mfa_res        <- mfa_results_r()
      df_original    <- dataset_r()
      group_col_name <- group_col_name_r()
      dim_x          <- input$mfa_3d_x
      dim_y          <- input$mfa_3d_y
      dim_z          <- input$mfa_3d_z
      dim_x_num      <- as.numeric(gsub("Dim\\.", "", dim_x))
      dim_y_num      <- as.numeric(gsub("Dim\\.", "", dim_y))
      dim_z_num      <- as.numeric(gsub("Dim\\.", "", dim_z))
      
      req(ncol(mfa_res$ind$coord) >= 3)
      
      mfa_coords <- as.data.frame(mfa_res$ind$coord) %>%
        tibble::rownames_to_column(var = "Row_Index")
      
      original_groups <- df_original %>%
        tibble::rownames_to_column(var = "Row_Index") %>%
        dplyr::select(Row_Index, Group = !!rlang::sym(group_col_name)) %>%
        dplyr::mutate(Group = as.factor(Group))
      
      mfa_coords <- dplyr::left_join(mfa_coords, original_groups, by = "Row_Index")
      
      mfa_coords$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()
      } else {
        as.character(seq_len(nrow(mfa_coords)))
      }
      
      pct <- round(mfa_res$eig[, 2], 2)
      x_label <- paste0(dim_x, " (", pct[dim_x_num], "%)")
      y_label <- paste0(dim_y, " (", pct[dim_y_num], "%)")
      z_label <- paste0(dim_z, " (", pct[dim_z_num], "%)")
      
      groups   <- levels(factor(mfa_coords$Group))
      n_groups <- length(groups)
      color_map <- if (!is.null(plot_palette()) && plot_palette() == "manual" &&
                       !is.null(manual_colors_r())) {
        manual_colors_r()[as.character(groups)]
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "viridis:")) {
        setNames(viridis::viridis(n_groups, option = sub("viridis:", "", plot_palette())), groups)
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "brewer:")) {
        pal_name <- sub("brewer:", "", plot_palette())
        max_n    <- RColorBrewer::brewer.pal.info[pal_name, "maxcolors"]
        setNames(RColorBrewer::brewer.pal(max(3L, min(n_groups, max_n)), pal_name)[seq_len(n_groups)], groups)
      } else if (!is.null(plot_palette()) && plot_palette() == "ggthemes:colorblind") {
        setNames(ggthemes::colorblind_pal()(min(n_groups, 8L)), groups)
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "colorblind:")) {
        pal_name <- sub("colorblind:", "", plot_palette())
        setNames(RColorBrewer::brewer.pal(max(3L, min(n_groups, 8L)), pal_name)[seq_len(n_groups)], groups)
      } else {
        setNames(scales::hue_pal()(n_groups), groups)
      }
      
      hover_txt <- paste0(
        "ID: ",     mfa_coords$SpecimenID,
        "<br>Group: ", mfa_coords$Group,
        "<br>",     dim_x, ": ", round(mfa_coords[[dim_x]], 3),
        "<br>",     dim_y, ": ", round(mfa_coords[[dim_y]], 3),
        "<br>",     dim_z, ": ", round(mfa_coords[[dim_z]], 3)
      )
      
      plt <- plotly::plot_ly(
        data      = mfa_coords,
        x         = ~get(dim_x),
        y         = ~get(dim_y),
        z         = ~get(dim_z),
        color     = ~Group,
        colors    = color_map,
        type      = "scatter3d",
        mode      = "markers",
        marker    = list(size    = (input$mfa_3d_point_size %||% 3) * 2,
                         opacity = input$mfa_3d_point_alpha %||% 0.8),
        text      = hover_txt,
        hoverinfo = "text"
      )
      
      # Centroids
      if (isTRUE(input$mfa_3d_centroids)) {
        centroids_3d <- mfa_coords %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(cx = mean(.data[[dim_x]]), cy = mean(.data[[dim_y]]), cz = mean(.data[[dim_z]]), .groups = "drop")
        for (grp in centroids_3d$Group) {
          row <- centroids_3d[centroids_3d$Group == grp, ]
          col <- color_map[as.character(grp)]
          plt <- plt %>% plotly::add_trace(
            x = row$cx, y = row$cy, z = row$cz,
            type = "scatter3d", mode = "markers",
            marker = list(symbol = "diamond", size = (input$mfa_3d_centroid_size %||% 8) * 2,
                          color = input$mfa_3d_centroid_color %||% "#000000",
                          line  = list(color = col, width = 2)),
            name = paste0(grp, " (centroid)"), hoverinfo = "text",
            text = paste0("Centroid<br>Group: ", grp),
            showlegend = FALSE, inherit = FALSE)
        }
      }
      
      # Convex hulls
      if (isTRUE(input$mfa_3d_hull)) {
        for (grp in groups) {
          sub <- mfa_coords[mfa_coords$Group == grp, c(dim_x, dim_y, dim_z)]
          if (nrow(sub) >= 4) {
            col <- color_map[as.character(grp)]
            plt <- plt %>% plotly::add_mesh(
              x = sub[[dim_x]], y = sub[[dim_y]], z = sub[[dim_z]],
              alphahull  = 0,
              opacity    = input$mfa_3d_hull_alpha %||% 0.15,
              colorscale = list(c(0, col), c(1, col)),
              intensity  = rep(0, nrow(sub)),
              showscale  = FALSE,
              name       = paste0(grp, " (hull)"),
              showlegend = FALSE, hoverinfo = "skip", inherit = FALSE)
          }
        }
      }
      
      
      # Spider lines
      if (isTRUE(input$mfa_3d_spider)) {
        centroids_sp <- mfa_coords %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(cx = mean(.data[[dim_x]]), cy = mean(.data[[dim_y]]), cz = mean(.data[[dim_z]]), .groups = "drop")
        for (grp in groups) {
          pts <- mfa_coords[mfa_coords$Group == grp, ]
          cen <- centroids_sp[centroids_sp$Group == grp, ]
          col <- color_map[as.character(grp)]
          for (i in seq_len(nrow(pts))) {
            plt <- plt %>% plotly::add_trace(
              x = c(pts[[dim_x]][i], cen$cx),
              y = c(pts[[dim_y]][i], cen$cy),
              z = c(pts[[dim_z]][i], cen$cz),
              type = "scatter3d", mode = "lines",
              line = list(color = col,
                          width = input$mfa_3d_spider_width %||% 2),
              opacity    = input$mfa_3d_spider_alpha %||% 0.4,
              showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
            )
          }
        }
      }
      
      plt %>%
        plotly::layout(
          scene = list(
            aspectmode = "cube",
            xaxis = list(title = list(text = x_label, font = list(size = plot_axis_label_size() %||% 12)),
                         tickfont = list(size = plot_axis_text_size() %||% 10)),
            yaxis = list(title = list(text = y_label, font = list(size = plot_axis_label_size() %||% 12)),
                         tickfont = list(size = plot_axis_text_size() %||% 10)),
            zaxis = list(title = list(text = z_label, font = list(size = plot_axis_label_size() %||% 12)),
                         tickfont = list(size = plot_axis_text_size() %||% 10))
          ),
          legend = list(title = list(text = stringr::str_to_title(group_col_name),
                                     font = list(size = legend_title_size() %||% 12)),
                        font  = list(size = legend_text_size() %||% 10))
        )
    })
    
    output$mfa_individuals_3d_plot <- plotly::renderPlotly({
      tryCatch(mfa_individuals_3d() %>%
                 plotly::config(
                   toImageButtonOptions = list(
                     format   = "png",
                     filename = "mfa_3d",
                     width    = 1600,
                     height   = 1200,
                     scale    = 3
                   )
                 ),
               error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(
                   text      = paste("Need at least 3 MFA dimensions.", e$message),
                   showarrow = FALSE))
    })
    
    # Render plots with dynamic height and width
    output$plot_scatter <- renderPlot({
      req(!isTRUE(input$scatter_interactive))
      plot_scatter_obj()
    }, height = function() input$plot_scatter_height %||% 500,
    width = function() input$plot_scatter_width %||% 700)
    
    output$plot_box <- renderPlot(
      { plot_box_obj() },
      height = function() input$plot_box_height %||% 500,
      width = function() input$plot_box_width %||% 700 
    )
    output$plot_violin <- renderPlot(
      { plot_violin_obj() },
      height = function() input$plot_violin_height %||% 500,
      width = function() input$plot_violin_width %||% 700 
    )
    output$mfa_eigen_plot <- renderPlot(
      { mfa_eigen_plot_obj() },
      height = function() input$mfa_eigen_plot_height %||% 500,
      width = function() input$mfa_eigen_plot_width %||% 700 
    )
    output$mfa_group_contrib_hist <- renderPlot({
      mfa_group_contrib_hist_obj()
    }, height = function() input$mfa_group_contrib_hist_height %||% 500,
    width = function() input$mfa_group_contrib_hist_width %||% 700)
    
    output$mfa_individuals_plot <- renderPlot({
      req(!isTRUE(input$mfa_interactive))
      mfa_individuals_plot_obj()
    }, height = function() input$mfa_individuals_plot_height %||% 500,
    width = function() input$mfa_individuals_plot_width %||% 700)
    
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
    height = function() input$mfa_var_contrib_hist_height %||% 500,
    width = function() input$mfa_var_contrib_hist_width %||% 700)
    
    DEFAULT_DOWNLOAD_WIDTH <- 10
    DEFAULT_DOWNLOAD_HEIGHT <- 8
    
    # The download function for all plots
    create_download_handler <- function(plot_obj_reactive, filename_prefix, type = "pdf", height_input_id, width_input_id) {
      downloadHandler(
        filename = function() { paste0(filename_prefix, "_", Sys.Date(), ".", type) },
        content = function(file) {
          plot_height_val_px <- input[[height_input_id]]
          plot_width_val_px <- input[[width_input_id]] 
          
          plot_height_val_in <- if (!is.null(plot_height_val_px) && plot_height_val_px > 0) {
            plot_height_val_px / 96
          } else {
            DEFAULT_DOWNLOAD_HEIGHT
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
    
    # Assign download handlers
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
    
    ## ========================================================================
    ## DAMF: Discriminant Analysis of Multiple Factors
    ## ========================================================================
    
    # Dynamic slider: number of MFA dims fed into the DA step
    # Cumulative variance from MFA eigenvalues (for variance threshold method)
    damfa_mfa_variance_r <- reactive({
      req(mfa_results_r())
      eig_vals <- mfa_results_r()$eig[, 1]
      data.frame(
        dim        = seq_along(eig_vals),
        cumulative = cumsum(eig_vals) / sum(eig_vals)
      )
    })
    
    # Unified MFA dimension selector — two deterministic methods, no buttons needed
    damfa_selected_n_mfa <- reactive({
      req(mfa_results_r())
      n_dims_avail <- ncol(mfa_results_r()$ind$coord)
      method <- input$damfa_pc_method %||% "Variance threshold"
      
      if (method == "Manual") {
        return(max(2L, min(n_dims_avail,
                           as.integer(input$n_mfa_damfa_manual %||% min(10L, n_dims_avail)))))
      }
      # Variance threshold (default)
      mfa_var   <- damfa_mfa_variance_r()
      req(!is.null(mfa_var))
      threshold <- (input$damfa_var_threshold %||% 80) / 100
      idx       <- which(mfa_var$cumulative >= threshold)[1]
      idx       <- if (is.na(idx) || is.null(idx)) nrow(mfa_var) else idx
      max(2L, min(n_dims_avail, idx))
    })
    
    # Cached DAMF result — shared by static and plotly outputs
    damfa_results_r <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r())
      n_mfa <- tryCatch(damfa_selected_n_mfa(), error = function(e) NULL)
      if (is.null(n_mfa)) {
        return(structure(list(message = "Adjust the variance threshold or use Manual to set MFA dimensions."),
                         class = "damfa_waiting"))
      }
      mfa_res      <- mfa_results_r()
      mfa_scores   <- as.data.frame(mfa_res$ind$coord)
      group_labels <- as.factor(dataset_r()[[group_col_name_r()]])
      complete_rows <- complete.cases(mfa_scores)
      mfa_complete  <- mfa_scores[complete_rows, , drop = FALSE]
      grp_complete  <- group_labels[complete_rows]
      if (nrow(mfa_complete) < 2 || nlevels(grp_complete) < 2) return(NULL)
      n_da_use  <- min(nlevels(grp_complete) - 1L, 2L)
      mfa_for_da <- mfa_complete[, seq_len(n_mfa), drop = FALSE]
      tryCatch(
        adegenet::dapc(mfa_for_da, grp_complete, n.pca = n_mfa, n.da = n_da_use),
        error = function(e) structure(list(message = e$message), class = "damfa_error")
      )
    })
    
    output$n_mfa_damfa_ui <- renderUI({
      req(mfa_results_r())
      n_dims <- ncol(mfa_results_r()$ind$coord)
      tagList(
        tags$div(
          style = "background-color: #f8f9fa; border-left: 4px solid #6c757d; padding: 8px; margin-bottom: 8px; border-radius: 3px; font-size: 0.82em;",
          tags$p(style = "margin: 0 0 4px 0;", strong("Choosing MFA dimensions:")),
          tags$p(style = "margin: 0;",
                 tags$b("Too few"), " \u2014 discriminant signal is lost; groups appear artificially compressed.",
                 tags$br(),
                 tags$b("Too many"), " \u2014 noise enters the analysis; groups appear artificially separated.",
                 tags$br(),
                 tags$b("Recommendation:"), " start at 80% and verify the plot is stable at 70% and 90%."
          )
        ),
        selectInput(ns("damfa_pc_method"), "Retain MFA dimensions by:",
                    choices  = c("Variance threshold", "Manual"),
                    selected = "Variance threshold",
                    width    = "200px"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Variance threshold'", ns("damfa_pc_method")),
          sliderInput(ns("damfa_var_threshold"), "Cumulative variance threshold (%)",
                      min = 70, max = 100, value = 80, step = 5, width = "220px"),
          textOutput(ns("damfa_threshold_status"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Manual'", ns("damfa_pc_method")),
          sliderInput(ns("n_mfa_damfa_manual"), "Number of MFA dimensions",
                      min = 2, max = n_dims, value = min(10L, n_dims), step = 1, width = "220px")
        ),
        hr(),
        htmlOutput(ns("damfa_retained_summary"))
      )
    })
    
    output$damfa_threshold_status <- renderText({
      req(mfa_results_r())
      if ((input$damfa_pc_method %||% "Variance threshold") != "Variance threshold") return("")
      mfa_var   <- damfa_mfa_variance_r()
      req(!is.null(mfa_var))
      threshold <- (input$damfa_var_threshold %||% 80) / 100
      idx <- which(mfa_var$cumulative >= threshold)[1]
      idx <- if (is.na(idx) || is.null(idx)) nrow(mfa_var) else idx
      idx <- max(2L, min(ncol(mfa_results_r()$ind$coord), idx))
      paste0(round(threshold * 100), "% cumulative variance retains ", idx, " MFA dimensions.")
    })
    
    output$damfa_retained_summary <- renderUI({
      req(mfa_results_r(), dataset_r(), group_col_name_r())
      method   <- input$damfa_pc_method %||% "Variance threshold"
      retained <- tryCatch(damfa_selected_n_mfa(), error = function(e) NULL)
      HTML(paste0(
        "<strong>MFA dims retained:</strong> ", retained %||% "<em>unknown</em>", "<br>",
        "<strong>Selection method:</strong> ", method
      ))
    })
    
    # Core DAMF ggplot reactive (static)
    plot_damfa_obj <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r(), input$damfa_point_size)
      group_col <- group_col_name_r()
      
      dapc_res <- damfa_results_r()
      if (inherits(dapc_res, "damfa_waiting")) {
        return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = dapc_res$message))
      }
      if (inherits(dapc_res, "damfa_error") || is.null(dapc_res)) {
        return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5,
                                                     label = "DAMFA could not be performed. Check data and parameters."))
      }
      if (ncol(dapc_res$ind.coord) < 2) {
        return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5,
                                                     label = "DAMF produced only 1 discriminant axis. More groups are needed for a 2D plot."))
      }
      
      dapc_df <- as.data.frame(dapc_res$ind.coord)
      dapc_df$Group <- dapc_res$grp
      eig     <- dapc_res$eig
      eig_pct <- round(100 * eig / sum(eig), 1)
      ld1_lbl <- paste0("LD1 (", eig_pct[1], "%)")
      ld2_lbl <- paste0("LD2 (", eig_pct[2], "%)")
      
      p <- ggplot2::ggplot(dapc_df, ggplot2::aes(x = LD1, y = LD2)) +
        ggplot2::xlab(ld1_lbl) + ggplot2::ylab(ld2_lbl)
      
      # Points
      if (isTRUE(input$damfa_outline_points)) {
        p <- p + ggplot2::geom_point(ggplot2::aes(fill = Group), shape = 21,
                                     size = input$damfa_point_size, color = "black",
                                     stroke = input$damfa_point_stroke) +
          get_fill_scale_otu(plot_palette())
      } else {
        p <- p + ggplot2::geom_point(ggplot2::aes(fill = Group, color = Group), shape = 21,
                                     size = input$damfa_point_size) +
          get_fill_scale_otu(plot_palette()) + get_color_scale_otu(plot_palette())
      }
      
      # 67% ellipses (adegenet convention)
      if (isTRUE(input$damfa_ellipse)) {
        if (isTRUE(input$damfa_outline_shapes)) {
          p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = Group, fill = Group, color = Group),
                                         level = 0.67, geom = "polygon",
                                         alpha = input$damfa_alpha_ellipse,
                                         linewidth = input$damfa_outline_stroke, show.legend = FALSE)
        } else {
          p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = Group, fill = Group), color = NA,
                                         level = 0.67, geom = "polygon",
                                         alpha = input$damfa_alpha_ellipse, show.legend = FALSE)
        }
      }
      
      # Convex hulls
      if (isTRUE(input$damfa_convex)) {
        hull_df <- dplyr::bind_rows(lapply(split(dapc_df, dapc_df$Group), function(d) d[chull(d$LD1, d$LD2), ]))
        if (isTRUE(input$damfa_outline_shapes)) {
          p <- p + ggplot2::geom_polygon(data = hull_df,
                                         ggplot2::aes(x = LD1, y = LD2, group = Group, fill = Group, color = Group),
                                         alpha = input$damfa_alpha_ellipse, linewidth = input$damfa_outline_stroke,
                                         inherit.aes = FALSE, show.legend = FALSE)
        } else {
          p <- p + ggplot2::geom_polygon(data = hull_df,
                                         ggplot2::aes(x = LD1, y = LD2, group = Group, fill = Group),
                                         color = NA, alpha = input$damfa_alpha_ellipse,
                                         inherit.aes = FALSE, show.legend = FALSE)
        }
      }
      
      # Centroids
      # Always compute centroids — needed by spider, MST, and centroid distances
      centroids <- dapc_df %>%
        dplyr::group_by(Group) %>%
        dplyr::summarize(LD1 = mean(LD1), LD2 = mean(LD2), .groups = "drop")
      
      if (isTRUE(input$damfa_spider)) {
        spider_df <- dplyr::left_join(dapc_df, centroids, by = "Group", suffix = c("", "_cent"))
        p <- p + ggplot2::geom_segment(data = spider_df,
                                       ggplot2::aes(x = LD1, y = LD2, xend = LD1_cent, yend = LD2_cent, color = Group),
                                       alpha = input$damfa_spider_alpha, linewidth = input$damfa_spider_width,
                                       inherit.aes = FALSE, show.legend = FALSE) +
          get_color_scale_otu(plot_palette())
      }
      
      if (isTRUE(input$damfa_mst) && nrow(centroids) >= 2) {
        cent_mat <- as.matrix(dist(centroids[, c("LD1", "LD2")]))
        rownames(cent_mat) <- centroids$Group; colnames(cent_mat) <- centroids$Group
        mst_obj <- ape::mst(cent_mat)
        mst_idx <- which(mst_obj == 1, arr.ind = TRUE)
        mst_idx <- mst_idx[mst_idx[, 1] < mst_idx[, 2], , drop = FALSE]
        mst_df  <- data.frame(
          x_start = centroids$LD1[mst_idx[, 1]], y_start = centroids$LD2[mst_idx[, 1]],
          x_end   = centroids$LD1[mst_idx[, 2]], y_end   = centroids$LD2[mst_idx[, 2]],
          x_mid   = (centroids$LD1[mst_idx[, 1]] + centroids$LD1[mst_idx[, 2]]) / 2,
          y_mid   = (centroids$LD2[mst_idx[, 1]] + centroids$LD2[mst_idx[, 2]]) / 2,
          distance = round(sqrt((centroids$LD1[mst_idx[, 2]] - centroids$LD1[mst_idx[, 1]])^2 +
                                  (centroids$LD2[mst_idx[, 2]] - centroids$LD2[mst_idx[, 1]])^2), 3)
        )
        p <- p + ggplot2::geom_segment(data = mst_df,
                                       ggplot2::aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                                       color = input$damfa_mst_color, alpha = input$damfa_mst_alpha,
                                       linewidth = input$damfa_mst_width, inherit.aes = FALSE)
        if (isTRUE(input$damfa_mst_labels)) {
          p <- p + ggplot2::geom_label(data = mst_df, ggplot2::aes(x = x_mid, y = y_mid, label = distance),
                                       size = input$damfa_mst_label_size, color = "black", fill = "white",
                                       label.padding = ggplot2::unit(0.15, "lines"), inherit.aes = FALSE)
        }
      }
      
      if (isTRUE(input$damfa_centroids)) {
        p <- p + ggplot2::geom_point(data = centroids, ggplot2::aes(x = LD1, y = LD2),
                                     shape = 8, size = input$damfa_centroid_size %||% 4,
                                     color = input$damfa_centroid_color, fill = "white", stroke = 1,
                                     inherit.aes = FALSE, show.legend = FALSE)
      }
      
      if (isTRUE(input$damfa_centroid_distances) && nrow(centroids) >= 2) {
        pairs   <- combn(nrow(centroids), 2)
        dist_df <- data.frame(
          x_start  = centroids$LD1[pairs[1, ]], y_start  = centroids$LD2[pairs[1, ]],
          x_end    = centroids$LD1[pairs[2, ]], y_end    = centroids$LD2[pairs[2, ]],
          x_mid    = (centroids$LD1[pairs[1, ]] + centroids$LD1[pairs[2, ]]) / 2,
          y_mid    = (centroids$LD2[pairs[1, ]] + centroids$LD2[pairs[2, ]]) / 2,
          distance = round(sqrt((centroids$LD1[pairs[2, ]] - centroids$LD1[pairs[1, ]])^2 +
                                  (centroids$LD2[pairs[2, ]] - centroids$LD2[pairs[1, ]])^2), 3)
        )
        p <- p +
          ggplot2::geom_segment(data = dist_df,
                                ggplot2::aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                                color = input$damfa_centroid_dist_color, linewidth = input$damfa_centroid_dist_width,
                                alpha = input$damfa_centroid_dist_alpha, inherit.aes = FALSE) +
          ggplot2::geom_label(data = dist_df, ggplot2::aes(x = x_mid, y = y_mid, label = distance),
                              size = input$damfa_centroid_dist_label_size, color = "black", fill = "white",
                              label.padding = ggplot2::unit(0.15, "lines"), inherit.aes = FALSE)
      }
      
      p + get_fill_scale_otu(plot_palette()) + get_color_scale_otu(plot_palette()) +
        get_custom_theme() +
        ggplot2::labs(fill = stringr::str_to_title(group_col),
                      color = stringr::str_to_title(group_col))
    })
    
    # DAMFA plotly reactive (interactive)
    plot_damfa_plotly <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r(), input$damfa_point_size)
      
      dapc_res <- damfa_results_r()
      req(!inherits(dapc_res, "damfa_waiting"), !inherits(dapc_res, "damfa_error"),
          !is.null(dapc_res), ncol(dapc_res$ind.coord) >= 2)
      
      group_col     <- group_col_name_r()
      mfa_scores    <- as.data.frame(mfa_results_r()$ind$coord)
      complete_rows <- complete.cases(mfa_scores)
      
      dapc_df <- as.data.frame(dapc_res$ind.coord)
      dapc_df$Group <- dapc_res$grp
      dapc_df$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[complete_rows]
      } else { as.character(which(complete_rows)) }
      eig     <- dapc_res$eig; eig_pct <- round(100 * eig / sum(eig), 1)
      
      hover_txt <- paste0("ID: ", dapc_df$SpecimenID, "<br>Group: ", dapc_df$Group,
                          "<br>LD1: ", round(dapc_df$LD1, 3), "<br>LD2: ", round(dapc_df$LD2, 3))
      
      p <- ggplot2::ggplot(dapc_df, ggplot2::aes(x = LD1, y = LD2, color = Group, fill = Group, text = hover_txt)) +
        ggplot2::geom_point(size = input$damfa_point_size %||% 3, alpha = 0.8) +
        ggplot2::xlab(paste0("LD1 (", eig_pct[1], "%)")) +
        ggplot2::ylab(paste0("LD2 (", eig_pct[2], "%)")) +
        get_color_scale_otu(plot_palette()) + get_fill_scale_otu(plot_palette()) +
        ggplot2::guides(fill = "none") +
        ggplot2::labs(color = stringr::str_to_title(group_col))
      
      if (isTRUE(input$damfa_ellipse))
        p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = Group, fill = Group, text = NULL),
                                       color = NA, level = 0.67, geom = "polygon",
                                       alpha = input$damfa_alpha_ellipse %||% 0.3, show.legend = FALSE)
      if (isTRUE(input$damfa_centroids)) {
        centroids <- dapc_df %>% dplyr::group_by(Group) %>%
          dplyr::summarize(LD1 = mean(LD1), LD2 = mean(LD2), .groups = "drop")
        p <- p + ggplot2::geom_point(data = centroids, ggplot2::aes(x = LD1, y = LD2),
                                     shape = 8, size = input$damfa_centroid_size %||% 4,
                                     color = input$damfa_centroid_color %||% "#000000", inherit.aes = FALSE)
      }
      
      tl <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick <- plot_axis_text_size() %||% 10; ax_lbl <- plot_axis_label_size() %||% 12
      leg_txt  <- legend_text_size() %||% 10;  leg_ttl <- legend_title_size() %||% 12
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)),
                       paper_bgcolor = tl$paper_bgcolor, plot_bgcolor = tl$plot_bgcolor, font = tl$font,
                       xaxis = modifyList(tl$xaxis, list(title = list(font = list(size = ax_lbl)), tickfont = list(size = ax_tick))),
                       yaxis = modifyList(tl$yaxis, list(title = list(font = list(size = ax_lbl)), tickfont = list(size = ax_tick))),
                       legend = list(font = list(size = leg_txt), title = list(font = list(size = leg_ttl))))
    })
    
    output$damfa_plot_ui <- renderUI({
      if (isTRUE(input$damfa_interactive)) {
        plotly::plotlyOutput(ns("plot_damfa_plotly"),
                             height = paste0(input$plot_damfa_height %||% 500, "px"))
      } else {
        plotOutput(ns("plot_damfa"),
                   height = paste0(input$plot_damfa_height %||% 500, "px"),
                   width  = paste0(input$plot_damfa_width  %||% 600, "px"))
      }
    })
    
    output$plot_damfa <- renderPlot({
      req(!isTRUE(input$damfa_interactive))
      plot_damfa_obj()
    }, height = function() input$plot_damfa_height %||% 500,
    width = function() input$plot_damfa_width %||% 600)
    
    output$plot_damfa_plotly <- plotly::renderPlotly({
      tryCatch(plot_damfa_plotly(), error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(text = paste("Error:", e$message), showarrow = FALSE))
    })
    
    ## ========================================================================
    ## DAMFA (3D)
    ## ========================================================================
    
    output$damfa_3d_status_ui <- renderUI({
      req(mfa_results_r(), dataset_r(), group_col_name_r())
      n_groups <- dplyr::n_distinct(dataset_r()[[group_col_name_r()]])
      available_ld <- max(0L, n_groups - 1L)
      if (available_ld >= 3L) {
        tags$div(
          style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
          tags$p(style = "margin: 0;",
                 paste0("3D DAMF is available. LD1\u2013LD3 will be shown (",
                        available_ld, " discriminant axes available)."))
        )
      } else {
        tags$div(
          style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
          tags$p(style = "margin: 0;",
                 paste0("3D DAMF requires at least 4 groups. ",
                        "Current data allow ", available_ld, " discriminant axis/axes."))
        )
      }
    })
    
    # Separate cached 3D result (n.da = 3)
    damfa_results_3d_r <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r())
      n_mfa <- tryCatch(damfa_selected_n_mfa(), error = function(e) NULL)
      if (is.null(n_mfa)) {
        return(structure(list(message = "Adjust the variance threshold or use Manual to set MFA dimensions."),
                         class = "damfa_waiting"))
      }
      mfa_res      <- mfa_results_r()
      mfa_scores   <- as.data.frame(mfa_res$ind$coord)
      group_labels <- as.factor(dataset_r()[[group_col_name_r()]])
      complete_rows <- complete.cases(mfa_scores)
      mfa_complete  <- mfa_scores[complete_rows, , drop = FALSE]
      grp_complete  <- group_labels[complete_rows]
      if (nrow(mfa_complete) < 2 || nlevels(grp_complete) < 2) return(NULL)
      n_da_use  <- min(nlevels(grp_complete) - 1L, 3L)
      mfa_for_da <- mfa_complete[, seq_len(n_mfa), drop = FALSE]
      tryCatch(
        adegenet::dapc(mfa_for_da, grp_complete, n.pca = n_mfa, n.da = n_da_use),
        error = function(e) structure(list(message = e$message), class = "damfa_error")
      )
    })
    
    plot_damfa_3d <- reactive({
      req(mfa_results_r(), dataset_r(), group_col_name_r())
      n_groups <- dplyr::n_distinct(dataset_r()[[group_col_name_r()]])
      if (n_groups < 4L) {
        return(plotly::plot_ly() %>%
                 plotly::add_annotations(text = "3D DAMF requires at least 4 groups so that LD1\u2013LD3 are available.",
                                         showarrow = FALSE))
      }
      dapc_res <- damfa_results_3d_r()
      if (inherits(dapc_res, "damfa_waiting")) {
        return(plotly::plot_ly() %>% plotly::add_annotations(text = dapc_res$message, showarrow = FALSE))
      }
      if (inherits(dapc_res, "damfa_error") || is.null(dapc_res) || ncol(dapc_res$ind.coord) < 3L) {
        return(plotly::plot_ly() %>%
                 plotly::add_annotations(text = "DAMF did not produce enough discriminant axes for a 3D plot.",
                                         showarrow = FALSE))
      }
      
      mfa_scores    <- as.data.frame(mfa_results_r()$ind$coord)
      complete_rows <- complete.cases(mfa_scores)
      
      dapc_df <- as.data.frame(dapc_res$ind.coord)
      dapc_df$Group <- dapc_res$grp
      dapc_df$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[complete_rows]
      } else {
        as.character(which(complete_rows))
      }
      eig         <- dapc_res$eig
      eig_percent <- round(100 * eig / sum(eig), 1)
      groups      <- levels(factor(dapc_df$Group))
      n_groups_c  <- length(groups)
      
      color_map <- if (!is.null(plot_palette()) && plot_palette() == "manual" && !is.null(manual_colors_r())) {
        cols <- manual_colors_r(); cols[as.character(groups)]
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "viridis:")) {
        opt <- sub("viridis:", "", plot_palette())
        setNames(viridis::viridis(n_groups_c, option = opt), groups)
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "brewer:")) {
        pal_name <- sub("brewer:", "", plot_palette())
        max_n    <- RColorBrewer::brewer.pal.info[pal_name, "maxcolors"]
        setNames(RColorBrewer::brewer.pal(max(3L, min(n_groups_c, max_n)), pal_name)[seq_len(n_groups_c)], groups)
      } else if (!is.null(plot_palette()) && plot_palette() == "ggthemes:colorblind") {
        setNames(ggthemes::colorblind_pal()(min(n_groups_c, 8L)), groups)
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "colorblind:")) {
        pal_name <- sub("colorblind:", "", plot_palette())
        setNames(RColorBrewer::brewer.pal(max(3L, min(n_groups_c, 8L)), pal_name)[seq_len(n_groups_c)], groups)
      } else {
        setNames(scales::hue_pal()(n_groups_c), groups)
      }
      
      hover_txt <- paste0("ID: ", dapc_df$SpecimenID,
                          "<br>Group: ", dapc_df$Group,
                          "<br>LD1: ", round(dapc_df$LD1, 3),
                          "<br>LD2: ", round(dapc_df$LD2, 3),
                          "<br>LD3: ", round(dapc_df$LD3, 3))
      
      plt <- plotly::plot_ly(
        data      = dapc_df, x = ~LD1, y = ~LD2, z = ~LD3,
        color     = ~Group, colors = color_map,
        type      = "scatter3d", mode = "markers",
        marker    = list(size    = (input$damfa_3d_point_size %||% 3) * 2,
                         opacity = input$damfa_3d_point_alpha %||% 0.8),
        text      = hover_txt, hoverinfo = "text"
      )
      
      if (isTRUE(input$damfa_3d_centroids)) {
        centroids_3d <- dapc_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(cx = mean(LD1), cy = mean(LD2), cz = mean(LD3), .groups = "drop")
        for (grp in centroids_3d$Group) {
          row <- centroids_3d[centroids_3d$Group == grp, ]
          col <- color_map[as.character(grp)]
          plt <- plt %>% plotly::add_trace(
            x = row$cx, y = row$cy, z = row$cz,
            type = "scatter3d", mode = "markers",
            marker    = list(symbol = "diamond",
                             size   = (input$damfa_3d_centroid_size %||% 8) * 2,
                             color  = input$damfa_3d_centroid_color %||% "#000000",
                             line   = list(color = col, width = 2)),
            name      = paste0(grp, " (centroid)"),
            hoverinfo = "text",
            text      = paste0("Centroid<br>Group: ", grp),
            showlegend = FALSE, inherit = FALSE
          )
        }
      }
      
      if (isTRUE(input$damfa_3d_hull)) {
        for (grp in groups) {
          sub <- dapc_df[dapc_df$Group == grp, c("LD1", "LD2", "LD3")]
          if (nrow(sub) >= 4L) {
            col <- color_map[as.character(grp)]
            plt <- plt %>% plotly::add_mesh(
              x = sub$LD1, y = sub$LD2, z = sub$LD3,
              alphahull  = 0,
              opacity    = input$damfa_3d_hull_alpha %||% 0.15,
              colorscale = list(c(0, col), c(1, col)),
              intensity  = rep(0, nrow(sub)),
              showscale  = FALSE,
              name       = paste0(grp, " (hull)"),
              showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
            )
          }
        }
      }
      
      if (isTRUE(input$damfa_3d_spider)) {
        centroids_sp <- dapc_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(cx = mean(LD1), cy = mean(LD2), cz = mean(LD3), .groups = "drop")
        for (grp in groups) {
          pts <- dapc_df[dapc_df$Group == grp, ]
          cen <- centroids_sp[centroids_sp$Group == grp, ]
          col <- color_map[as.character(grp)]
          for (i in seq_len(nrow(pts))) {
            plt <- plt %>% plotly::add_trace(
              x = c(pts$LD1[i], cen$cx), y = c(pts$LD2[i], cen$cy), z = c(pts$LD3[i], cen$cz),
              type = "scatter3d", mode = "lines",
              line       = list(color = col, width = input$damfa_3d_spider_width %||% 2),
              opacity    = input$damfa_3d_spider_alpha %||% 0.4,
              showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
            )
          }
        }
      }
      
      plt %>% plotly::layout(
        scene = list(
          aspectmode = "cube",
          xaxis = list(title = list(text = paste0("LD1 (", eig_percent[1], "%)"),
                                    font = list(size = plot_axis_label_size() %||% 12)),
                       tickfont = list(size = plot_axis_text_size() %||% 10)),
          yaxis = list(title = list(text = paste0("LD2 (", eig_percent[2], "%)"),
                                    font = list(size = plot_axis_label_size() %||% 12)),
                       tickfont = list(size = plot_axis_text_size() %||% 10)),
          zaxis = list(title = list(text = paste0("LD3 (", eig_percent[3], "%)"),
                                    font = list(size = plot_axis_label_size() %||% 12)),
                       tickfont = list(size = plot_axis_text_size() %||% 10))
        ),
        legend = list(
          title = list(text = "Group", font = list(size = legend_title_size() %||% 12)),
          font  = list(size = legend_text_size() %||% 10)
        )
      )
    })
    
    output$plot_damfa_3d <- plotly::renderPlotly({
      tryCatch(
        plot_damfa_3d() %>% plotly::config(
          toImageButtonOptions = list(format = "png", filename = "damfa_3d",
                                      width = 1600, height = 1200, scale = 3)),
        error = function(e) plotly::plot_ly() %>%
          plotly::add_annotations(text = paste("Error:", e$message), showarrow = FALSE)
      )
    })
    
    ## ========================================================================
    ## MFA: BEDDA VISUALIZATIONS
    ## ========================================================================
    
    # ---- BIC model comparison plot ----
    plot_mfa_bic_obj <- reactive({
      req(mfa_bedda_unsup_results_r())
      results  <- mfa_bedda_unsup_results_r()
      data_mod <- results$model
      req(!is.null(data_mod$BIC))
      
      DF <- data.frame(data_mod$BIC[], G = seq_len(nrow(data_mod$BIC)))
      model_cols <- setdiff(names(DF), "G")
      DF <- tidyr::pivot_longer(DF, cols = dplyr::all_of(model_cols), names_to = "Model", values_to = "BIC")
      valid_model_names <- mclust::mclust.options("emModelNames")
      DF$Model <- factor(DF$Model, levels = valid_model_names[valid_model_names %in% unique(DF$Model)])
      
      ggplot2::ggplot(DF, ggplot2::aes(x = G, y = BIC, colour = Model, shape = Model)) +
        ggplot2::geom_point(size = 10) +
        ggplot2::geom_line() +
        ggplot2::scale_shape_manual(values = mclust::mclust.options("bicPlotSymbols")) +
        ggplot2::scale_color_manual(values = mclust::mclust.options("bicPlotColors")) +
        ggplot2::scale_x_continuous(breaks = unique(DF$G)) +
        ggplot2::labs(x = "Number of mixture components", y = "BIC") +
        ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2)) +
        get_custom_theme() +
        ggplot2::theme(legend.position = "bottom")
    })
    
    output$plot_mfa_bic <- renderPlot({
      validate(need(!is.null(mfa_bedda_unsup_results_r()),
                    "Run Unsupervised Clustering in the MFA Delimitation tab first."))
      plot_mfa_bic_obj()
    }, height = function() input$plot_mfa_bic_height %||% 500,
    width  = function() input$plot_mfa_bic_width  %||% 600)
    
    # ---- Model selector for clusters plot ----
    output$mfa_cluster_model_selector <- renderUI({
      req(mfa_bedda_unsup_results_r())
      top_models <- mfa_bedda_unsup_results_r()$top_models
      req(top_models)
      top_n   <- min(10, nrow(top_models))
      choices <- paste0("G=", top_models$G[seq_len(top_n)], ",", top_models$Model[seq_len(top_n)])
      names(choices) <- paste0("Rank ", top_models$Rank[seq_len(top_n)],
                               ": G=", top_models$G[seq_len(top_n)],
                               ", ",   top_models$Model[seq_len(top_n)],
                               " (\u0394BIC=", round(top_models$Delta_BIC[seq_len(top_n)], 1), ")")
      selectInput(ns("mfa_cluster_model_select"), "Select Model to Visualize:",
                  choices = choices, selected = choices[1], width = "100%")
    })
    
    # Dynamic axis selectors for cluster plot (reuse MFA dimensions)
    make_mfa_cluster_dim_selector <- function(input_id, label, default_dim) {
      renderUI({
        req(mfa_results_r())
        n_dims     <- ncol(mfa_results_r()$ind$coord)
        dim_choices <- paste0("Dim.", seq_len(n_dims))
        selectInput(ns(input_id), label,
                    choices  = dim_choices,
                    selected = dim_choices[min(default_dim, n_dims)],
                    width    = "150px")
      })
    }
    output$mfa_cluster_x_selector <- make_mfa_cluster_dim_selector("mfa_cluster_x_axis", "X-axis:", 1)
    output$mfa_cluster_y_selector <- make_mfa_cluster_dim_selector("mfa_cluster_y_axis", "Y-axis:", 2)
    
    # ---- MFA Clusters static plot ----
    plot_mfa_clusters_obj <- reactive({
      req(mfa_bedda_unsup_results_r(), mfa_results_r(), dataset_r(), group_col_name_r())
      req(input$mfa_clusters_point_size)
      
      results    <- mfa_bedda_unsup_results_r()
      mfa_res    <- mfa_results_r()
      group_col  <- group_col_name_r()
      
      dim_x <- input$mfa_cluster_x_axis %||% "Dim.1"
      dim_y <- input$mfa_cluster_y_axis %||% "Dim.2"
      dim_x_num <- as.numeric(gsub("Dim\\.", "", dim_x))
      dim_y_num <- as.numeric(gsub("Dim\\.", "", dim_y))
      
      # Resolve which model to use
      selected <- input$mfa_cluster_model_select
      if (!is.null(selected)) {
        parts    <- strsplit(selected, ",")[[1]]
        G_val    <- as.numeric(sub("G=", "", parts[1]))
        mod_name <- parts[2]
        data_mod <- tryCatch(
          mclust::Mclust(results$model$data, G = G_val, modelNames = mod_name),
          error = function(e) NULL
        )
        if (is.null(data_mod)) {
          data_mod <- results$model
          showNotification("Selected model failed; reverting to best model.", type = "warning", duration = 3)
        }
      } else {
        data_mod <- results$model
      }
      
      # Build data frame: MFA coords + Cluster + OTU
      mfa_df <- as.data.frame(mfa_res$ind$coord) %>%
        tibble::rownames_to_column("Row_Index")
      otu_df <- dataset_r() %>%
        tibble::rownames_to_column("Row_Index") %>%
        dplyr::select(Row_Index, OTU = !!rlang::sym(group_col))
      mfa_df <- dplyr::left_join(mfa_df, otu_df, by = "Row_Index")
      mfa_df$Cluster <- as.factor(data_mod$classification)
      
      p <- ggplot2::ggplot(mfa_df, ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]])) +
        ggplot2::xlab(paste0(dim_x, " (", round(mfa_res$eig[dim_x_num, 2], 2), "%)")) +
        ggplot2::ylab(paste0(dim_y, " (", round(mfa_res$eig[dim_y_num, 2], 2), "%)"))
      
      # Cluster ellipses
      if (isTRUE(input$mfa_clusters_ellipse)) {
        if (isTRUE(input$mfa_clusters_outline_shapes)) {
          p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = Cluster, fill = Cluster, color = Cluster),
                                         geom = "polygon",
                                         alpha = input$mfa_clusters_alpha_ellipse,
                                         linewidth = input$mfa_clusters_outline_stroke, show.legend = FALSE)
        } else {
          p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = Cluster, fill = Cluster), color = NA,
                                         geom = "polygon",
                                         alpha = input$mfa_clusters_alpha_ellipse, show.legend = FALSE)
        }
      }
      
      # Cluster convex hulls
      if (isTRUE(input$mfa_clusters_convex)) {
        hull_df <- mfa_df %>% dplyr::group_by(Cluster) %>% dplyr::filter(dplyr::n() >= 3) %>%
          dplyr::slice(chull(.data[[dim_x]], .data[[dim_y]])) %>% dplyr::ungroup()
        if (isTRUE(input$mfa_clusters_outline_shapes)) {
          p <- p + ggplot2::geom_polygon(data = hull_df,
                                         ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]],
                                                      group = Cluster, fill = Cluster, color = Cluster),
                                         alpha = input$mfa_clusters_alpha_ellipse,
                                         linewidth = input$mfa_clusters_outline_stroke,
                                         inherit.aes = FALSE, show.legend = FALSE)
        } else {
          p <- p + ggplot2::geom_polygon(data = hull_df,
                                         ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]],
                                                      group = Cluster, fill = Cluster),
                                         color = NA, alpha = input$mfa_clusters_alpha_ellipse,
                                         inherit.aes = FALSE, show.legend = FALSE)
        }
      }
      
      # Points: color = Cluster, shape = OTU
      n_otu   <- length(unique(mfa_df$OTU))
      all_shapes <- c(16, 17, 15, 18, 8, 7, 1, 2, 0, 5, 4, 3, 6, 9, 10, 11, 12, 13, 14, 19, 20)
      mfa_df$OTU <- as.factor(mfa_df$OTU)
      p <- p + ggplot2::geom_point(ggplot2::aes(color = Cluster, shape = OTU),
                                   size = input$mfa_clusters_point_size) +
        ggplot2::scale_shape_manual(values = all_shapes[seq_len(n_otu)])
      
      # Cluster centroids
      if (isTRUE(input$mfa_clusters_centroids)) {
        centroids <- mfa_df %>% dplyr::group_by(Cluster) %>%
          dplyr::summarize(x_cent = mean(.data[[dim_x]]), y_cent = mean(.data[[dim_y]]), .groups = "drop")
        p <- p + ggplot2::geom_point(data = centroids, ggplot2::aes(x = x_cent, y = y_cent),
                                     shape = 8, size = 4, color = "black", fill = "white", stroke = 1,
                                     inherit.aes = FALSE, show.legend = FALSE)
      }
      
      # Apply both fill and color scales unconditionally so ellipses, hulls,
      # and points all share the same palette regardless of outline mode.
      p + get_fill_scale_otu(plot_palette()) +
        get_color_scale_otu(plot_palette()) +
        get_custom_theme()
    })
    
    # ---- MFA Clusters interactive (plotly) ----
    mfa_clusters_plotly <- reactive({
      req(mfa_bedda_unsup_results_r(), mfa_results_r(), dataset_r(), group_col_name_r())
      
      results   <- mfa_bedda_unsup_results_r()
      mfa_res   <- mfa_results_r()
      group_col <- group_col_name_r()
      dim_x     <- input$mfa_cluster_x_axis %||% "Dim.1"
      dim_y     <- input$mfa_cluster_y_axis %||% "Dim.2"
      dim_x_num <- as.numeric(gsub("Dim\\.", "", dim_x))
      dim_y_num <- as.numeric(gsub("Dim\\.", "", dim_y))
      
      mfa_df <- as.data.frame(mfa_res$ind$coord) %>% tibble::rownames_to_column("Row_Index")
      otu_df <- dataset_r() %>% tibble::rownames_to_column("Row_Index") %>%
        dplyr::select(Row_Index, OTU = !!rlang::sym(group_col))
      mfa_df <- dplyr::left_join(mfa_df, otu_df, by = "Row_Index")
      mfa_df$Cluster <- as.factor(results$model$classification)
      mfa_df$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()
      } else { seq_len(nrow(mfa_df)) }
      
      x_lbl <- paste0(dim_x, " (", round(mfa_res$eig[dim_x_num, 2], 2), "%)")
      y_lbl <- paste0(dim_y, " (", round(mfa_res$eig[dim_y_num, 2], 2), "%)")
      hover_txt <- paste0("ID: ", mfa_df$SpecimenID, "<br>OTU: ", mfa_df$OTU,
                          "<br>Cluster: ", mfa_df$Cluster,
                          "<br>", dim_x, ": ", round(mfa_df[[dim_x]], 3),
                          "<br>", dim_y, ": ", round(mfa_df[[dim_y]], 3))
      
      p <- ggplot2::ggplot(mfa_df, ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]],
                                                color = Cluster, fill = Cluster, text = hover_txt)) +
        ggplot2::geom_point(size = input$mfa_clusters_point_size %||% 3, alpha = 0.8) +
        ggplot2::xlab(x_lbl) + ggplot2::ylab(y_lbl) +
        get_color_scale_otu(plot_palette()) + get_fill_scale_otu(plot_palette()) +
        ggplot2::guides(fill = "none")
      
      if (isTRUE(input$mfa_clusters_ellipse))
        p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = Cluster, fill = Cluster, text = NULL),
                                       color = NA, geom = "polygon",
                                       alpha = input$mfa_clusters_alpha_ellipse %||% 0.3, show.legend = FALSE)
      
      tl <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick <- plot_axis_text_size() %||% 10; ax_lbl_sz <- plot_axis_label_size() %||% 12
      leg_txt  <- legend_text_size() %||% 10;  leg_ttl   <- legend_title_size() %||% 12
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)),
                       paper_bgcolor = tl$paper_bgcolor, plot_bgcolor = tl$plot_bgcolor, font = tl$font,
                       xaxis = modifyList(tl$xaxis, list(title = list(text = x_lbl, font = list(size = ax_lbl_sz)), tickfont = list(size = ax_tick))),
                       yaxis = modifyList(tl$yaxis, list(title = list(text = y_lbl, font = list(size = ax_lbl_sz)), tickfont = list(size = ax_tick))),
                       legend = list(font = list(size = leg_txt), title = list(font = list(size = leg_ttl))))
    })
    
    output$mfa_clusters_plot_ui <- renderUI({
      if (isTRUE(input$mfa_clusters_interactive)) {
        plotly::plotlyOutput(ns("mfa_clusters_plotly_out"),
                             height = paste0(input$plot_mfa_clusters_height %||% 500, "px"))
      } else {
        plotOutput(ns("mfa_clusters_plot"),
                   height = paste0(input$plot_mfa_clusters_height %||% 500, "px"),
                   width  = paste0(input$plot_mfa_clusters_width  %||% 600, "px"))
      }
    })
    
    output$mfa_clusters_plot <- renderPlot({
      req(!isTRUE(input$mfa_clusters_interactive))
      validate(need(!is.null(mfa_bedda_unsup_results_r()),
                    "Run Unsupervised Clustering in the MFA Delimitation tab first."))
      plot_mfa_clusters_obj()
    }, height = function() input$plot_mfa_clusters_height %||% 500,
    width  = function() input$plot_mfa_clusters_width  %||% 600)
    
    output$mfa_clusters_plotly_out <- plotly::renderPlotly({
      tryCatch(mfa_clusters_plotly(), error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(text = paste("Error:", e$message), showarrow = FALSE))
    })
    
    # ---- DAMFA + BEDDA download handlers ----
    output$download_damfa_pdf  <- create_download_handler(plot_damfa_obj, "damfa",           "pdf",  "plot_damfa_height",        "plot_damfa_width")
    output$download_damfa_jpeg <- create_download_handler(plot_damfa_obj, "damfa",           "jpeg", "plot_damfa_height",        "plot_damfa_width")
    output$download_mfa_bic_pdf  <- create_download_handler(plot_mfa_bic_obj,      "mfa_bic_model_comparison", "pdf",  "plot_mfa_bic_height", "plot_mfa_bic_width")
    output$download_mfa_bic_jpeg <- create_download_handler(plot_mfa_bic_obj,      "mfa_bic_model_comparison", "jpeg", "plot_mfa_bic_height", "plot_mfa_bic_width")
    output$download_mfa_clusters_pdf  <- create_download_handler(plot_mfa_clusters_obj, "mfa_clusters", "pdf",  "plot_mfa_clusters_height", "plot_mfa_clusters_width")
    output$download_mfa_clusters_jpeg <- create_download_handler(plot_mfa_clusters_obj, "mfa_clusters", "jpeg", "plot_mfa_clusters_height", "plot_mfa_clusters_width")
    
    ## ========================================================================
    ## MFA BEDDA: BORUTA VISUALIZATION
    ## ========================================================================
    
    plot_mfa_boruta_ridge_obj <- reactive({
      req(mfa_bedda_boruta_results_r())
      boruta_data <- mfa_bedda_boruta_results_r()
      req(boruta_data$boruta_object)
      boruta_obj <- boruta_data$boruta_object
      
      final_decision <- tibble::tibble(
        var      = names(boruta_obj$finalDecision),
        decision = as.character(boruta_obj$finalDecision)
      ) %>% dplyr::mutate(ID = dplyr::row_number())
      
      boruta_obj$ImpHistory %>%
        tibble::as_tibble() %>%
        tidyr::gather(var, value) %>%
        dplyr::left_join(final_decision, by = "var") %>%
        dplyr::mutate(value = replace(value, value == "-Inf", NA)) %>%
        ggplot2::ggplot(ggplot2::aes(x = value, y = reorder(var, value, na.rm = TRUE), fill = decision)) +
        ggridges::geom_density_ridges(
          scale            = input$mfa_boruta_ridge_scale %||% 4,
          alpha            = input$mfa_boruta_ridge_alpha %||% 0.5,
          rel_min_height   = 0.005
        ) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        get_fill_scale_otu(plot_palette()) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::labs(x = "Importance", y = "Variable", fill = "Decision") +
        get_custom_theme()
    })
    
    plot_mfa_boruta_box_obj <- reactive({
      req(mfa_bedda_boruta_results_r())
      boruta_data <- mfa_bedda_boruta_results_r()
      req(boruta_data$boruta_object)
      boruta_obj <- boruta_data$boruta_object
      
      imp_history    <- boruta_obj$ImpHistory
      final_decision <- boruta_obj$finalDecision
      
      imp_df_long <- as.data.frame(imp_history) %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "Importance")
      
      imp_df_long$Decision <- factor(
        final_decision[imp_df_long$Variable],
        levels = c("Confirmed", "Tentative", "Rejected", "shadowMax", "shadowMean", "shadowMin")
      )
      imp_df_long$VarType <- ifelse(
        imp_df_long$Variable %in% c("shadowMax", "shadowMean", "shadowMin"), "Shadow", "Attribute"
      )
      
      var_order <- imp_df_long %>%
        dplyr::group_by(Variable) %>%
        dplyr::summarise(median_imp = median(Importance, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(median_imp) %>%
        dplyr::pull(Variable)
      imp_df_long$Variable <- factor(imp_df_long$Variable, levels = var_order)
      
      ggplot2::ggplot(imp_df_long, ggplot2::aes(x = Variable, y = Importance, fill = Decision)) +
        ggplot2::geom_boxplot(outlier.size = 1) +
        get_fill_scale_otu(plot_palette()) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "Variable", y = "Importance", fill = "Decision") +
        get_custom_theme()
    })
    
    output$plot_mfa_boruta_ridge <- renderPlot({
      validate(need(!is.null(mfa_bedda_boruta_results_r()),
                    "Run Diagnostic Characters (Machine Learning) in the MFA Delimitation tab first."))
      plot_mfa_boruta_ridge_obj()
    }, height = function() input$plot_mfa_boruta_ridge_height %||% 500,
    width  = function() input$plot_mfa_boruta_ridge_width  %||% 600)
    
    output$plot_mfa_boruta_box <- renderPlot({
      validate(need(!is.null(mfa_bedda_boruta_results_r()),
                    "Run Diagnostic Characters (Machine Learning) in the MFA Delimitation tab first."))
      plot_mfa_boruta_box_obj()
    }, height = function() input$plot_mfa_boruta_box_height %||% 500,
    width  = function() input$plot_mfa_boruta_box_width  %||% 600)
    
    output$download_mfa_boruta_ridge_pdf  <- create_download_handler(plot_mfa_boruta_ridge_obj, "mfa_boruta_ridge", "pdf",  "plot_mfa_boruta_ridge_height", "plot_mfa_boruta_ridge_width")
    output$download_mfa_boruta_ridge_jpeg <- create_download_handler(plot_mfa_boruta_ridge_obj, "mfa_boruta_ridge", "jpeg", "plot_mfa_boruta_ridge_height", "plot_mfa_boruta_ridge_width")
    output$download_mfa_boruta_box_pdf    <- create_download_handler(plot_mfa_boruta_box_obj,   "mfa_boruta_box",   "pdf",  "plot_mfa_boruta_box_height",   "plot_mfa_boruta_box_width")
    output$download_mfa_boruta_box_jpeg   <- create_download_handler(plot_mfa_boruta_box_obj,   "mfa_boruta_box",   "jpeg", "plot_mfa_boruta_box_height",   "plot_mfa_boruta_box_width")
    
    ## =========================================================================
    ## SUPERVISED CLUSTERING VISUALIZATION
    ## =========================================================================
    
    ## ---- Phylogenetic Hypothesis Testing: cladogram + hypothesis rectangles ----
    
    # Helper: compute x/y layout for all nodes in the tree
    compute_tree_layout <- function(tree, type = "cladogram") {
      tree   <- ape::ladderize(tree)
      n_tips <- length(tree$tip.label)
      n_all  <- n_tips + tree$Nnode
      root   <- n_tips + 1L
      
      # Build children lookup table
      children_of <- vector("list", n_all)
      for (i in seq_len(nrow(tree$edge))) {
        p <- tree$edge[i, 1L]; ch <- tree$edge[i, 2L]
        children_of[[p]] <- c(children_of[[p]], ch)
      }
      
      # x-coordinates
      x <- numeric(n_all)
      for (i in seq_len(nrow(tree$edge))) {
        p <- tree$edge[i, 1L]; ch <- tree$edge[i, 2L]
        bl <- if (type == "phylogram" && !is.null(tree$edge.length))
          tree$edge.length[i] else 1
        x[ch] <- x[p] + bl
      }
      if (type != "phylogram" || is.null(tree$edge.length))
        x[seq_len(n_tips)] <- max(x[seq_len(n_tips)])  # align tips
      
      # y-coordinates: iterative pre-order for tips, then post-order for internal nodes
      y <- numeric(n_all)
      tip_counter <- 0L
      stack <- root
      post_order <- integer(0)
      
      while (length(stack) > 0L) {
        nd <- stack[length(stack)]; stack <- stack[-length(stack)]
        post_order <- c(nd, post_order)
        ch <- children_of[[nd]]
        if (length(ch) == 0L) {
          tip_counter <- tip_counter + 1L
          y[nd] <- tip_counter
        } else {
          stack <- c(stack, rev(ch))
        }
      }
      # Post-order: assign internal node y = mean of children
      for (nd in post_order) {
        ch <- children_of[[nd]]
        if (length(ch) > 0L) y[nd] <- mean(y[ch])
      }
      
      tip_df <- data.frame(label = tree$tip.label,
                           x = x[seq_len(n_tips)], y = y[seq_len(n_tips)],
                           stringsAsFactors = FALSE)
      
      # Horizontal segments (branch lines at child y)
      h_segs <- data.frame(x    = x[tree$edge[, 1L]],
                           xend = x[tree$edge[, 2L]],
                           y    = y[tree$edge[, 2L]],
                           yend = y[tree$edge[, 2L]])
      
      # Vertical segments (node bars spanning children)
      v_segs <- do.call(rbind, lapply(unique(tree$edge[, 1L]), function(nd) {
        ch <- children_of[[nd]]
        data.frame(x = x[nd], xend = x[nd], y = min(y[ch]), yend = max(y[ch]))
      }))
      
      list(tip_df = tip_df, h_segs = h_segs, v_segs = v_segs,
           max_x = max(x), n_tips = n_tips, tree = tree)
    }
    
    # Helper: build the full cladogram + hypothesis rectangles ggplot
    build_phylo_hyp_plot <- function(sup_data, n_top, tree_type, tip_sz, rect_alpha) {
      tree         <- sup_data$tree_used
      hyp_df       <- sup_data$hyp_df
      ordered_keys <- sup_data$ordered_hyp_keys
      species_col  <- as.character(sup_data$species_col)
      summary_tbl  <- sup_data$summary_table
      
      if (is.null(tree) || is.null(hyp_df) || length(ordered_keys) == 0)
        return(NULL)
      
      layout  <- compute_tree_layout(tree, type = tree_type)
      tip_df  <- layout$tip_df
      max_x   <- layout$max_x
      n_tips  <- layout$n_tips
      
      n_top_actual <- min(n_top, length(ordered_keys))
      top_keys     <- head(ordered_keys, n_top_actual)
      
      # Map each tip label to its group for each hypothesis
      sp_to_group_list <- lapply(top_keys, function(key) {
        col_labels <- as.character(hyp_df[[key]])
        setNames(
          vapply(tip_df$label, function(sp) {
            idx <- which(species_col == sp)
            if (length(idx) == 0L) return(sp)
            unique(col_labels[idx])[1L]
          }, character(1L)),
          tip_df$label
        )
      })
      
      # Qualitative palette (up to 12 lumped groups per column)
      qual_pal <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                    "#A65628","#F781BF","#66C2A5","#FC8D62","#8DA0CB",
                    "#E78AC3","#FFD92F")
      
      # Column geometry
      label_width <- max(nchar(tip_df$label)) * max_x * 0.022
      col_w       <- max_x * 0.10
      col_gap     <- max_x * 0.015
      col_x_ctr   <- max_x + label_width + col_w / 2 +
        (seq_len(n_top_actual) - 1L) * (col_w + col_gap)
      
      # Build rectangle data
      rect_rows <- list()
      for (i in seq_len(n_top_actual)) {
        sp_grp   <- sp_to_group_list[[i]]
        grp_sizes <- table(sp_grp[tip_df$label])
        lumped   <- names(grp_sizes)[grp_sizes > 1L]
        colors   <- setNames(qual_pal[seq_along(lumped)], lumped)
        
        for (grp in unique(sp_grp)) {
          tips_in <- tip_df$label[sp_grp[tip_df$label] == grp]
          ys      <- tip_df$y[tip_df$label %in% tips_in]
          if (length(ys) == 0L) next
          rect_rows <- c(rect_rows, list(data.frame(
            xmin = col_x_ctr[i] - col_w / 2,
            xmax = col_x_ctr[i] + col_w / 2,
            ymin = min(ys) - 0.38,
            ymax = max(ys) + 0.38,
            fill = if (grp %in% lumped) colors[grp] else "#CCCCCC",
            stringsAsFactors = FALSE
          )))
        }
      }
      rect_df <- if (length(rect_rows) > 0L) dplyr::bind_rows(rect_rows) else data.frame()
      
      # Column headers: rank + K + ΔBIC
      k_col   <- if ("K" %in% names(summary_tbl)) "K" else NULL
      dbic_col <- grep("BIC", names(summary_tbl), value = TRUE)[2]  # ΔBIC column
      hyp_col <- "Hypothesis"
      
      k_vals   <- if (!is.null(k_col))
        summary_tbl[[k_col]][match(top_keys, summary_tbl[[hyp_col]])] else rep("?", n_top_actual)
      dbic_raw <- if (!is.null(dbic_col))
        summary_tbl[[dbic_col]][match(top_keys, summary_tbl[[hyp_col]])] else rep(NA, n_top_actual)
      dbic_vals <- round(as.numeric(dbic_raw), 1)
      
      header_df <- data.frame(
        x      = col_x_ctr,
        label  = paste0("K=", k_vals),
        label2 = paste0("\u0394=", dbic_vals),
        stringsAsFactors = FALSE
      )
      rank_df <- data.frame(
        x     = col_x_ctr,
        label = paste0("#", seq_len(n_top_actual)),
        stringsAsFactors = FALSE
      )
      
      x_max_plot <- max(col_x_ctr) + col_w / 2 + max_x * 0.01
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_segment(data = layout$v_segs,
                              ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                              color = "black", linewidth = 0.55) +
        ggplot2::geom_segment(data = layout$h_segs,
                              ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                              color = "black", linewidth = 0.55) +
        ggplot2::geom_text(data = tip_df,
                           ggplot2::aes(x = max_x + max_x * 0.025, y = y, label = label),
                           hjust = 0, size = tip_sz, fontface = "italic") +
        ggplot2::geom_rect(data = rect_df,
                           ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                        fill = fill),
                           color = "white", linewidth = 0.35, alpha = rect_alpha) +
        ggplot2::scale_fill_identity() +
        ggplot2::geom_text(data = rank_df,
                           ggplot2::aes(x = x, y = n_tips + 2.1, label = label),
                           size = 3.8, hjust = 0.5, fontface = "bold") +
        ggplot2::geom_text(data = header_df,
                           ggplot2::aes(x = x, y = n_tips + 1.6, label = label),
                           size = 3.4, hjust = 0.5) +
        ggplot2::geom_text(data = header_df,
                           ggplot2::aes(x = x, y = n_tips + 1.1, label = label2),
                           size = 3.2, hjust = 0.5, color = "#444444") +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0.5, 0.5))) +
        ggplot2::coord_cartesian(xlim = c(-max_x * 0.02, x_max_plot), clip = "off") +
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.margin = ggplot2::margin(t = 5, r = 15, b = 10, l = 15, unit = "pt")
        )
      
      # Scale bar for phylogram
      if (tree_type == "phylogram" && !is.null(tree$edge.length)) {
        sb_len <- signif(max_x / 5, 1)
        p <- p +
          ggplot2::annotate("segment", x = 0, xend = sb_len,
                            y = -0.5, yend = -0.5, linewidth = 0.5) +
          ggplot2::annotate("text", x = sb_len / 2, y = -0.9,
                            label = as.character(sb_len), size = 2.5)
      }
      p
    }
    
    # Reactive that builds the plot object
    plot_phylo_tree_obj <- reactive({
      req(mfa_bedda_sup_models_r())
      sup_data <- mfa_bedda_sup_models_r()
      validate(need(!is.null(sup_data$tree_used),
                    "Run Topology-aware Hypothesis Testing in the MFA Delimitation tab first."))
      build_phylo_hyp_plot(
        sup_data   = sup_data,
        n_top      = input$phylo_hyp_n_top    %||% 5L,
        tree_type  = input$phylo_tree_type     %||% "cladogram",
        tip_sz     = input$phylo_tip_label_size %||% 3,
        rect_alpha = input$phylo_rect_alpha    %||% 0.80
      )
    })
    
    output$phylo_tree_plot_ui <- renderUI({
      if (is.null(mfa_bedda_sup_models_r()) || is.null(mfa_bedda_sup_models_r()$tree_used))
        return(NULL)
      plotOutput(ns("plot_phylo_tree_hyp"),
                 height = paste0(input$plot_phylo_tree_height %||% 600, "px"),
                 width  = paste0(input$plot_phylo_tree_width  %||% 750, "px"))
    })
    
    output$plot_phylo_tree_hyp <- renderPlot({
      validate(need(!is.null(mfa_bedda_sup_models_r()) &&
                      !is.null(mfa_bedda_sup_models_r()$tree_used),
                    "Run Topology-aware Hypothesis Testing in the MFA Delimitation tab first."))
      plot_phylo_tree_obj()
    }, height = function() input$plot_phylo_tree_height %||% 600,
    width  = function() input$plot_phylo_tree_width  %||% 750)
    
    output$download_phylo_tree_pdf  <- create_download_handler(
      plot_phylo_tree_obj, "mfa_phylo_tree_hypotheses", "pdf",
      "plot_phylo_tree_height", "plot_phylo_tree_width")
    output$download_phylo_tree_jpeg <- create_download_handler(
      plot_phylo_tree_obj, "mfa_phylo_tree_hypotheses", "jpeg",
      "plot_phylo_tree_height", "plot_phylo_tree_width")
    
    ## ---- Hypothesis Testing scatter plot ----
    
    # Populate hypothesis column dropdown from uploaded hypothesis data
    output$mfa_hyp_col_selector <- renderUI({
      hyp_data <- mfa_hyp_data_r()
      if (is.null(hyp_data) || ncol(hyp_data) == 0)
        return(p(em("Upload a hypothesis file in MFA Delimitation 2192 User-specified Hypothesis Testing first.")))
      hyp_cols <- names(hyp_data)
      selectInput(ns("mfa_hyp_col_choice"), "Select Hypothesis Column:",
                  choices = hyp_cols, selected = hyp_cols[1])
    })
    
    output$mfa_hyp_x_selector <- renderUI({
      req(mfa_results_r())
      n_dims <- ncol(mfa_results_r()$ind$coord)
      selectInput(ns("mfa_hyp_x_dim"), "X Axis:",
                  choices = paste0("Dim.", seq_len(n_dims)), selected = "Dim.1", width = '150px')
    })
    output$mfa_hyp_y_selector <- renderUI({
      req(mfa_results_r())
      n_dims <- ncol(mfa_results_r()$ind$coord)
      selectInput(ns("mfa_hyp_y_dim"), "Y Axis:",
                  choices = paste0("Dim.", seq_len(n_dims)), selected = "Dim.2", width = '150px')
    })
    
    # Build the hypothesis testing scatter plot
    plot_mfa_hyp_obj <- reactive({
      req(mfa_hyp_data_r(), mfa_results_r(), dataset_r(), group_col_name_r(),
          input$mfa_hyp_col_choice, input$mfa_hyp_x_dim, input$mfa_hyp_y_dim)
      
      hyp_data  <- mfa_hyp_data_r()
      mfa_res   <- mfa_results_r()
      group_col <- group_col_name_r()
      hyp_col   <- input$mfa_hyp_col_choice
      dim_x     <- input$mfa_hyp_x_dim
      dim_y     <- input$mfa_hyp_y_dim
      dim_x_num <- as.numeric(gsub("Dim\\.", "", dim_x))
      dim_y_num <- as.numeric(gsub("Dim\\.", "", dim_y))
      
      validate(need(hyp_col %in% names(hyp_data),
                    paste("Hypothesis column", hyp_col, "not found.")))
      validate(need(nrow(hyp_data) == nrow(mfa_res$ind$coord),
                    "Hypothesis file row count does not match specimen count. Please reload."))
      
      mfa_df <- as.data.frame(mfa_res$ind$coord) %>%
        tibble::rownames_to_column("Row_Index")
      otu_df <- dataset_r() %>%
        tibble::rownames_to_column("Row_Index") %>%
        dplyr::select(Row_Index, OTU = !!rlang::sym(group_col))
      mfa_df <- dplyr::left_join(mfa_df, otu_df, by = "Row_Index")
      mfa_df$Group <- as.factor(hyp_data[[hyp_col]])
      mfa_df$OTU   <- as.factor(mfa_df$OTU)
      
      n_otu      <- length(unique(mfa_df$OTU))
      all_shapes <- c(16, 17, 15, 18, 8, 7, 1, 2, 0, 5, 4, 3, 6, 9, 10, 11, 12, 13, 14, 19, 20)
      
      p <- ggplot2::ggplot(mfa_df, ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]])) +
        ggplot2::xlab(paste0(dim_x, " (", round(mfa_res$eig[dim_x_num, 2], 2), "%)")) +
        ggplot2::ylab(paste0(dim_y, " (", round(mfa_res$eig[dim_y_num, 2], 2), "%)"))
      
      if (isTRUE(input$mfa_hyp_ellipse)) {
        p <- p + ggplot2::stat_ellipse(
          ggplot2::aes(group = Group, fill = Group), color = NA,
          geom = "polygon",
          alpha = input$mfa_hyp_alpha_shape %||% 0.3, show.legend = FALSE
        )
      }
      
      if (isTRUE(input$mfa_hyp_convex)) {
        hull_df <- mfa_df %>%
          dplyr::group_by(Group) %>%
          dplyr::filter(dplyr::n() >= 3) %>%
          dplyr::slice(chull(.data[[dim_x]], .data[[dim_y]])) %>%
          dplyr::ungroup()
        p <- p + ggplot2::geom_polygon(
          data = hull_df,
          ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]], group = Group, fill = Group),
          color = NA, alpha = input$mfa_hyp_alpha_shape %||% 0.3,
          inherit.aes = FALSE, show.legend = FALSE
        )
      }
      
      if (isTRUE(input$mfa_hyp_centroids)) {
        centroids <- mfa_df %>% dplyr::group_by(Group) %>%
          dplyr::summarize(x_cent = mean(.data[[dim_x]]), y_cent = mean(.data[[dim_y]]), .groups = "drop")
        p <- p + ggplot2::geom_point(data = centroids, ggplot2::aes(x = x_cent, y = y_cent),
                                     shape = 8, size = 4, color = "black", fill = "white", stroke = 1,
                                     inherit.aes = FALSE, show.legend = FALSE)
      }
      
      p <- p + ggplot2::geom_point(
        ggplot2::aes(color = Group, shape = OTU),
        size = input$mfa_hyp_point_size %||% 3
      ) +
        ggplot2::scale_shape_manual(values = all_shapes[seq_len(n_otu)])
      
      p +
        get_fill_scale_otu(plot_palette()) +
        get_color_scale_otu(plot_palette()) +
        ggplot2::labs(color = hyp_col, fill = hyp_col, shape = "OTU") +
        get_custom_theme()
    })
    
    output$mfa_hyp_plot_ui <- renderUI({
      if (isTRUE(input$mfa_hyp_interactive)) {
        plotly::plotlyOutput(ns("plot_mfa_hyp_interactive"),
                             height = paste0(input$plot_mfa_hyp_height %||% 500, "px"))
      } else {
        plotOutput(ns("plot_mfa_hyp_static"),
                   height = paste0(input$plot_mfa_hyp_height %||% 500, "px"),
                   width  = paste0(input$plot_mfa_hyp_width  %||% 600, "px"))
      }
    })
    
    output$plot_mfa_hyp_static <- renderPlot({
      validate(need(!is.null(mfa_hyp_data_r()),
                    "Upload a hypothesis file in the MFA Delimitation 2192 User-specified Hypothesis Testing tab first."))
      plot_mfa_hyp_obj()
    }, height = function() input$plot_mfa_hyp_height %||% 500,
    width  = function() input$plot_mfa_hyp_width  %||% 600)
    
    output$plot_mfa_hyp_interactive <- plotly::renderPlotly({
      validate(need(!is.null(mfa_hyp_data_r()),
                    "Upload a hypothesis file in the MFA Delimitation 2192 User-specified Hypothesis Testing tab first."))
      hyp_data  <- mfa_hyp_data_r()
      mfa_res   <- mfa_results_r()
      group_col <- group_col_name_r()
      hyp_col   <- input$mfa_hyp_col_choice %||% names(hyp_data)[1]
      dim_x     <- input$mfa_hyp_x_dim %||% "Dim.1"
      dim_y     <- input$mfa_hyp_y_dim %||% "Dim.2"
      dim_x_num <- as.numeric(gsub("Dim\\.", "", dim_x))
      dim_y_num <- as.numeric(gsub("Dim\\.", "", dim_y))
      
      mfa_df <- as.data.frame(mfa_res$ind$coord) %>% tibble::rownames_to_column("Row_Index")
      otu_df <- dataset_r() %>% tibble::rownames_to_column("Row_Index") %>%
        dplyr::select(Row_Index, OTU = !!rlang::sym(group_col))
      mfa_df <- dplyr::left_join(mfa_df, otu_df, by = "Row_Index")
      mfa_df$Group <- as.factor(hyp_data[[hyp_col]])
      mfa_df$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()
      } else { seq_len(nrow(mfa_df)) }
      
      x_lbl     <- paste0(dim_x, " (", round(mfa_res$eig[dim_x_num, 2], 2), "%)")
      y_lbl     <- paste0(dim_y, " (", round(mfa_res$eig[dim_y_num, 2], 2), "%)")
      hover_txt <- paste0("ID: ",     mfa_df$SpecimenID,
                          "<br>OTU: ",    mfa_df$OTU,
                          "<br>", hyp_col, ": ", mfa_df$Group,
                          "<br>", dim_x, ": ", round(mfa_df[[dim_x]], 3),
                          "<br>", dim_y, ": ", round(mfa_df[[dim_y]], 3))
      
      p <- ggplot2::ggplot(mfa_df, ggplot2::aes(x = .data[[dim_x]], y = .data[[dim_y]],
                                                color = Group, fill = Group, text = hover_txt)) +
        ggplot2::geom_point(size = input$mfa_hyp_point_size %||% 3, alpha = 0.8) +
        ggplot2::xlab(x_lbl) + ggplot2::ylab(y_lbl) +
        get_color_scale_otu(plot_palette()) +
        get_fill_scale_otu(plot_palette()) +
        ggplot2::labs(color = hyp_col, fill = hyp_col) +
        ggplot2::guides(fill = "none")
      
      if (isTRUE(input$mfa_hyp_ellipse))
        p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = Group, fill = Group, text = NULL),
                                       color = NA, geom = "polygon",
                                       alpha = input$mfa_hyp_alpha_shape %||% 0.3,
                                       show.legend = FALSE)
      
      tl      <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick <- plot_axis_text_size()  %||% 10
      ax_lbl  <- plot_axis_label_size() %||% 12
      leg_txt <- legend_text_size()     %||% 10
      leg_ttl <- legend_title_size()    %||% 12
      
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          hoverlabel    = list(bgcolor = "white", font = list(size = 12)),
          paper_bgcolor = tl$paper_bgcolor, plot_bgcolor = tl$plot_bgcolor, font = tl$font,
          xaxis  = modifyList(tl$xaxis, list(title = list(text = x_lbl,  font = list(size = ax_lbl)), tickfont = list(size = ax_tick))),
          yaxis  = modifyList(tl$yaxis, list(title = list(text = y_lbl,  font = list(size = ax_lbl)), tickfont = list(size = ax_tick))),
          legend = list(font = list(size = leg_txt), title = list(font = list(size = leg_ttl)))
        )
    })
    
    output$download_mfa_hyp_pdf  <- create_download_handler(plot_mfa_hyp_obj, "mfa_hypothesis_scatter", "pdf",  "plot_mfa_hyp_height", "plot_mfa_hyp_width")
    output$download_mfa_hyp_jpeg <- create_download_handler(plot_mfa_hyp_obj, "mfa_hypothesis_scatter", "jpeg", "plot_mfa_hyp_height", "plot_mfa_hyp_width")
    
    ## ---- Cluster-OTU Correspondence heatmap (driven by unsupervised results) ----
    
    # Build row-normalised proportion df from the selected unsupervised model
    mfa_cluster_corr_df <- reactive({
      req(mfa_bedda_unsup_results_r())
      results <- mfa_bedda_unsup_results_r()
      
      # Mirror the model selection used in the scatter plot
      selected <- input$mfa_cluster_model_select
      if (!is.null(selected) && nchar(selected) > 0) {
        parts    <- strsplit(selected, ",")[[1]]
        G_val    <- as.numeric(sub("G=", "", parts[1]))
        mod_name <- trimws(parts[2])
        data_mod <- tryCatch(
          mclust::Mclust(results$model$data, G = G_val, modelNames = mod_name),
          error = function(e) NULL
        )
        if (is.null(data_mod)) data_mod <- results$model
      } else {
        data_mod <- results$model
      }
      
      species_col  <- as.character(results$species_col)
      cluster_vec  <- as.character(data_mod$classification)
      all_otus     <- sort(unique(species_col))
      all_clusters <- sort(unique(cluster_vec),
                           method = "radix") # numeric sort e.g. 1,2,...,10
      
      tbl <- table(OTU = species_col, Cluster = cluster_vec)
      df  <- as.data.frame(tbl, stringsAsFactors = FALSE)
      names(df) <- c("OTU", "Cluster", "n")
      
      df %>%
        dplyr::group_by(OTU) %>%
        dplyr::mutate(Prop = n / sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          OTU     = factor(OTU,     levels = rev(all_otus)),
          Cluster = factor(Cluster, levels = all_clusters)
        )
    })
    
    plot_mfa_post_obj <- reactive({
      req(mfa_cluster_corr_df())
      prop_df   <- mfa_cluster_corr_df()
      text_size <- input$mfa_post_text_size %||% 4
      show_zero <- isTRUE(input$mfa_post_show_zeros)
      
      label_df <- dplyr::mutate(prop_df,
                                label = if (show_zero) sprintf("%.2f", Prop)
                                else           ifelse(Prop == 0, "", sprintf("%.2f", Prop))
      )
      
      ggplot2::ggplot(label_df, ggplot2::aes(x = Cluster, y = OTU, fill = Prop)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = label),
                           size = text_size, color = "black") +
        ggplot2::scale_fill_gradient(low = "#ffffcc", high = "#1a5276",
                                     limits = c(0, 1), name = "Proportion") +
        ggplot2::labs(x = "Cluster", y = "OTU") +
        get_custom_theme() +
        ggplot2::theme(
          axis.text.x     = ggplot2::element_text(angle = 0, hjust = 0.5),
          panel.grid      = ggplot2::element_blank(),
          panel.border    = ggplot2::element_blank(),
          axis.ticks      = ggplot2::element_blank(),
          legend.position = "right"
        )
    })
    
    output$mfa_post_heatmap_ui <- renderUI({
      plotOutput(ns("plot_mfa_post_heatmap"),
                 height = paste0(input$plot_mfa_post_height %||% 500, "px"),
                 width  = paste0(input$plot_mfa_post_width  %||% 700, "px"))
    })
    
    output$plot_mfa_post_heatmap <- renderPlot({
      validate(need(!is.null(mfa_bedda_unsup_results_r()),
                    "Run Unsupervised Clustering in the MFA Delimitation tab first."))
      plot_mfa_post_obj()
    }, height = function() input$plot_mfa_post_height %||% 500,
    width  = function() input$plot_mfa_post_width  %||% 700)
    
    output$download_mfa_post_pdf  <- create_download_handler(plot_mfa_post_obj, "mfa_cluster_otu_correspondence", "pdf",  "plot_mfa_post_height", "plot_mfa_post_width")
    output$download_mfa_post_jpeg <- create_download_handler(plot_mfa_post_obj, "mfa_cluster_otu_correspondence", "jpeg", "plot_mfa_post_height", "plot_mfa_post_width")
    
    output$download_mfa_post_csv <- downloadHandler(
      filename = function() paste0("mfa_cluster_otu_correspondence_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(mfa_cluster_corr_df())
        write.csv(mfa_cluster_corr_df(), file, row.names = FALSE)
      }
    )
    
  }) 
} 


