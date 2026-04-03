
mod_visual_ui_meristic <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Data Visualization"),
    hr(),
    tabsetPanel(id = ns("visual_tab"),
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
                                      tags$p(style = "margin: 0;", "Hover over points to see specimen IDs. Outline points, individual labels, and theme selection are not available in interactive mode. Use the camera icon in the toolbar to save the plot.")
                                    )
                                  ),
                                  hr(),
                                  numericInput(ns("plot_scatter_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  conditionalPanel(
                                    condition = sprintf("!input['%s']", ns("scatter_interactive")),
                                    numericInput(ns("plot_scatter_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '150px')
                                  ),
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
                                  numericInput(ns("plot_box_width"), "Plot Width (px)", value = 700, min = 200, step = 50, width = '150px'),
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
                                  style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
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
                                  uiOutput(ns("pca_plot_ui"))
                           ),
                           column(3,
                                  style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                  br(),
                                  tags$div(
                                    style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
                                    checkboxInput(ns("pca_interactive"), tags$strong("\U0001f5b1 Interactive Mode"), value = FALSE)
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_interactive")),
                                    tags$div(
                                      style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
                                      tags$p(style = "margin: 0;", "Hover over points to see specimen IDs. Outline points, biplot arrows, spider plot, MST, centroid distances, and theme selection are not available in interactive mode. Use the camera icon in the toolbar to save the plot.")
                                    )
                                  ),
                                  hr(),
                                  numericInput(ns("plot_pca_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  conditionalPanel(
                                    condition = sprintf("!input['%s']", ns("pca_interactive")),
                                    numericInput(ns("plot_pca_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px')
                                  ),
                                  hr(),
                                  h5("PC Axis Selection"),
                                  uiOutput(ns("pca_x_axis_selector")),
                                  uiOutput(ns("pca_y_axis_selector")),
                                  hr(),
                                  conditionalPanel(
                                    condition = sprintf("!input['%s']", ns("pca_interactive")),
                                    h5("Biplot Options"),
                                    checkboxInput(ns("pca_biplot"), "Show Variable Loadings (Biplot)", value = FALSE),
                                    conditionalPanel(
                                      condition = sprintf("input['%s']", ns("pca_biplot")),
                                      checkboxInput(ns("pca_biplot_labels"), "Show Variable Labels", value = TRUE),
                                      colourInput(ns("pca_arrow_color"), "Arrow Color:", value = "#8B0000", showColour = "background"),
                                      sliderInput(ns("pca_arrow_size"), "Arrow Width:", min = 0.5, max = 3, value = 1, step = 0.1, width = '150px'),
                                      sliderInput(ns("pca_arrow_alpha"), "Arrow Transparency:", min = 0.1, max = 1, value = 0.7, step = 0.1, width = '150px'),
                                      numericInput(ns("pca_label_size"), "Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px')
                                    ),
                                  ),  # end biplot conditionalPanel
                                  hr(),
                                  h5("Points & Groups"),
                                  numericInput(ns("pca_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                  conditionalPanel(
                                    condition = sprintf("!input['%s']", ns("pca_interactive")),
                                    checkboxInput(ns("pca_outline_points"), "Outline Points", value = FALSE),
                                    conditionalPanel(
                                      condition = sprintf("input['%s']", ns("pca_outline_points")),
                                      sliderInput(ns("pca_point_stroke"), "Point Outline Width", min = 0, max = 2, value = 0.5, step = 0.1, width = '150px')
                                    )
                                  ),
                                  checkboxInput(ns("pca_centroids"), "Group Centroids", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_centroids")),
                                    numericInput(ns("pca_centroid_size"), "Centroid Size:", value = 4, min = 1, max = 15, width = '150px'),
                                    colourInput(ns("pca_centroid_color"), "Centroid Color:", value = "#000000", showColour = "background"),
                                    
                                    conditionalPanel(
                                      condition = sprintf("!input['%s']", ns("pca_interactive")),
                                      checkboxInput(ns("pca_spider"), "Show Spider Plot", value = FALSE),
                                      conditionalPanel(
                                        condition = sprintf("input['%s']", ns("pca_spider")),
                                        sliderInput(ns("pca_spider_alpha"), "Spider Line Transparency:", min = 0.1, max = 1, value = 0.4, step = 0.1, width = '150px'),
                                        sliderInput(ns("pca_spider_width"), "Spider Line Width:", min = 0.1, max = 2, value = 0.5, step = 0.1, width = '150px')
                                      ),
                                      checkboxInput(ns("pca_centroid_distances"), "Show Centroid Distances", value = FALSE),
                                      conditionalPanel(
                                        condition = sprintf("input['%s']", ns("pca_centroid_distances")),
                                        colourInput(ns("pca_centroid_dist_color"), "Distance Line Color:", value = "#444444", showColour = "background"),
                                        sliderInput(ns("pca_centroid_dist_width"), "Distance Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                        sliderInput(ns("pca_centroid_dist_alpha"), "Distance Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                        numericInput(ns("pca_centroid_dist_label_size"), "Distance Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px'),
                                        helpText("Distances shown are Euclidean distances between group centroids in the 2D PCA space of the currently selected axes. These are not equivalent to PERMANOVA distances, which are computed from the full multivariate dataset across all variables. Use these values for visual interpretation only.")
                                      ),
                                      checkboxInput(ns("pca_mst"), "Show Minimum Spanning Tree", value = FALSE),
                                      conditionalPanel(
                                        condition = sprintf("input['%s']", ns("pca_mst")),
                                        colourInput(ns("pca_mst_color"), "MST Line Color:", value = "#222222", showColour = "background"),
                                        sliderInput(ns("pca_mst_alpha"), "MST Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                        sliderInput(ns("pca_mst_width"), "MST Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                        checkboxInput(ns("pca_mst_labels"), "Show MST Edge Distances", value = FALSE),
                                        conditionalPanel(
                                          condition = sprintf("input['%s']", ns("pca_mst_labels")),
                                          numericInput(ns("pca_mst_label_size"), "MST Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px')
                                        )
                                      )
                                    )
                                  ),
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
                                  conditionalPanel(
                                    condition = sprintf("!input['%s']", ns("pca_interactive")),
                                    downloadButton(ns("download_pca_pdf"), "Download PDF"),
                                    br(),
                                    downloadButton(ns("download_pca_jpeg"), "Download JPEG"),
                                    br()
                                  ),
                                  downloadButton(ns("download_pca_summary"), "Download PCA Summary"),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_interactive")),
                                    br(),
                                    p(em("Use the camera icon in the plot toolbar to download the interactive plot."))
                                  ),
                                  hr()
                           )
                         )
                ),
                
                tabPanel("PCA (3D)",
                         fluidRow(
                           column(9,
                                  plotly::plotlyOutput(ns("plot_pca_3d"), height = "580px")
                           ),
                           column(3,
                                  style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                  br(),
                                  h5("Axis Selection"),
                                  uiOutput(ns("pca_3d_x_selector")),
                                  uiOutput(ns("pca_3d_y_selector")),
                                  uiOutput(ns("pca_3d_z_selector")),
                                  hr(),
                                  numericInput(ns("pca_3d_point_size"), "Point Size:", value = 3, min = 1, max = 10, width = '150px'),
                                  sliderInput(ns("pca_3d_point_alpha"), "Point Opacity:", min = 0.1, max = 1, value = 0.8, step = 0.05, width = '150px'),
                                  hr(),
                                  h5("Centroids"),
                                  checkboxInput(ns("pca_3d_centroids"), "Show Group Centroids", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_3d_centroids")),
                                    numericInput(ns("pca_3d_centroid_size"), "Centroid Size:", value = 4, min = 2, max = 20, width = '150px'),
                                    colourInput(ns("pca_3d_centroid_color"), "Centroid Color:", value = "#000000", showColour = "background")
                                  ),
                                  hr(),
                                  h5("Convex Hulls"),
                                  checkboxInput(ns("pca_3d_hull"), "Show Convex Hulls", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_3d_hull")),
                                    sliderInput(ns("pca_3d_hull_alpha"), "Hull Opacity:", min = 0.05, max = 0.5, value = 0.15, step = 0.05, width = '150px')
                                  ),
                                  hr(),
                                  h5("Spider Plot"),
                                  checkboxInput(ns("pca_3d_spider"), "Show Spider Lines", value = FALSE),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("pca_3d_spider")),
                                    sliderInput(ns("pca_3d_spider_alpha"), "Spider Line Opacity:", min = 0.1, max = 1, value = 0.4, step = 0.05, width = '150px'),
                                    sliderInput(ns("pca_3d_spider_width"), "Spider Line Width:", min = 1, max = 6, value = 2, step = 1, width = '150px')
                                  ),
                                  hr(),
                                  p(em("Use the camera icon in the plot toolbar to download."))
                           )
                         )
                ),
                
                tabPanel("DAPC",
                         fluidRow(
                           column(9,
                                  uiOutput(ns("dapc_plot_ui"))
                           ),
                           column(3,
                                  style = "height: calc(100vh - 120px); overflow-y: auto; padding-right: 15px;",
                                  br(),
                                  tags$div(
                                    style = "background-color: #e8f4f8; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 5px; border-radius: 3px;",
                                    checkboxInput(ns("dapc_interactive"), tags$strong("\U0001f5b1 Interactive Mode"), value = FALSE)
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("dapc_interactive")),
                                    tags$div(
                                      style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin-bottom: 8px; font-size: 0.85em;",
                                      tags$p(style = "margin: 0;", "Hover over points to see specimen IDs. Outline points, spider plot, MST, centroid distances, and theme selection are not available in interactive mode. Use the camera icon in the toolbar to save the plot.")
                                    )
                                  ),
                                  hr(),
                                  numericInput(ns("plot_dapc_height"), "Plot Height (px)", value = 500, min = 200, step = 50, width = '150px'),
                                  conditionalPanel(
                                    condition = sprintf("!input['%s']", ns("dapc_interactive")),
                                    numericInput(ns("plot_dapc_width"), "Plot Width (px)", value = 600, min = 200, step = 50, width = '150px')
                                  ),
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
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("dapc_centroids")),
                                    numericInput(ns("dapc_centroid_size"), "Centroid Size:", value = 4, min = 1, max = 15, width = '150px'),
                                    colourInput(ns("dapc_centroid_color"), "Centroid Color:", value = "#000000", showColour = "background"),
                                    
                                    conditionalPanel(
                                      condition = sprintf("!input['%s']", ns("dapc_interactive")),
                                      checkboxInput(ns("dapc_spider"), "Show Spider Plot", value = FALSE),
                                      conditionalPanel(
                                        condition = sprintf("input['%s']", ns("dapc_spider")),
                                        sliderInput(ns("dapc_spider_alpha"), "Spider Line Transparency:", min = 0.1, max = 1, value = 0.4, step = 0.1, width = '150px'),
                                        sliderInput(ns("dapc_spider_width"), "Spider Line Width:", min = 0.1, max = 2, value = 0.5, step = 0.1, width = '150px')
                                      ),
                                      checkboxInput(ns("dapc_centroid_distances"), "Show Centroid Distances", value = FALSE),
                                      conditionalPanel(
                                        condition = sprintf("input['%s']", ns("dapc_centroid_distances")),
                                        colourInput(ns("dapc_centroid_dist_color"), "Distance Line Color:", value = "#444444", showColour = "background"),
                                        sliderInput(ns("dapc_centroid_dist_width"), "Distance Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                        sliderInput(ns("dapc_centroid_dist_alpha"), "Distance Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                        numericInput(ns("dapc_centroid_dist_label_size"), "Distance Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px'),
                                        helpText("Distances shown are Euclidean distances between group centroids in the 2D DAPC space of LD1 and LD2. These distances are not equivalent to PERMANOVA distances computed from the full multivariate dataset.")
                                      ),
                                      checkboxInput(ns("dapc_mst"), "Show Minimum Spanning Tree", value = FALSE),
                                      conditionalPanel(
                                        condition = sprintf("input['%s']", ns("dapc_mst")),
                                        colourInput(ns("dapc_mst_color"), "MST Line Color:", value = "#222222", showColour = "background"),
                                        sliderInput(ns("dapc_mst_alpha"), "MST Line Transparency:", min = 0.1, max = 1, value = 0.8, step = 0.1, width = '150px'),
                                        sliderInput(ns("dapc_mst_width"), "MST Line Width:", min = 0.1, max = 2, value = 0.8, step = 0.1, width = '150px'),
                                        checkboxInput(ns("dapc_mst_labels"), "Show MST Edge Distances", value = FALSE),
                                        conditionalPanel(
                                          condition = sprintf("input['%s']", ns("dapc_mst_labels")),
                                          numericInput(ns("dapc_mst_label_size"), "MST Label Size:", value = 3, min = 1, max = 10, step = 0.5, width = '150px')
                                        )
                                      )
                                    )
                                  ),
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
                                  conditionalPanel(
                                    condition = sprintf("!input['%s']", ns("dapc_interactive")),
                                    downloadButton(ns("download_dapc_pdf"), "Download PDF"),
                                    br(),
                                    downloadButton(ns("download_dapc_jpeg"), "Download JPEG")
                                  ),
                                  conditionalPanel(
                                    condition = sprintf("input['%s']", ns("dapc_interactive")),
                                    p(em("Use the camera icon in the plot toolbar to download the interactive plot."))
                                  ),
                                  hr()
                           )
                         )
                )
    )
  )
}

mod_visual_server_meristic <- function(id, dataset,
                                       plot_palette, plot_theme,
                                       plot_axis_text_size,
                                       plot_axis_label_size, plot_x_angle, plot_facet_size,
                                       legend_text_size, legend_title_size,
                                       manual_colors_r,
                                       specimen_ids_r = NULL) {
  
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
    
    # PCA axis selectors
    output$pca_x_axis_selector <- renderUI({
      req(dataset())
      df <- dataset()
      data_mat <- df[, -1]
      complete_rows <- complete.cases(data_mat)
      
      if (sum(complete_rows) < 2 || ncol(data_mat) < 2) {
        return(NULL)
      }
      
      # Calculate number of PCs available
      n_pcs <- min(sum(complete_rows), ncol(data_mat))
      pc_choices <- paste0("PC", 1:n_pcs)
      
      selectInput(ns("pca_x_axis"), "X-axis:", 
                  choices = pc_choices, 
                  selected = "PC1",
                  width = '150px')
    })
    
    output$pca_y_axis_selector <- renderUI({
      req(dataset())
      df <- dataset()
      data_mat <- df[, -1]
      complete_rows <- complete.cases(data_mat)
      
      if (sum(complete_rows) < 2 || ncol(data_mat) < 2) {
        return(NULL)
      }
      
      # Calculate number of PCs available
      n_pcs <- min(sum(complete_rows), ncol(data_mat))
      pc_choices <- paste0("PC", 1:n_pcs)
      
      selectInput(ns("pca_y_axis"), "Y-axis:", 
                  choices = pc_choices, 
                  selected = "PC2",
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
    
    # Plotly equivalents — applied to ggplotly outputs since ggplot2 themes
    # are stripped during conversion
    get_plotly_theme_layout <- function(theme_name) {
      base <- list(
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        font          = list(color = "black"),
        xaxis = list(showgrid = FALSE, showline = TRUE,  zeroline = FALSE,
                     linecolor = "black", gridcolor = "#e5e5e5"),
        yaxis = list(showgrid = FALSE, showline = TRUE,  zeroline = FALSE,
                     linecolor = "black", gridcolor = "#e5e5e5")
      )
      switch(theme_name,
             "theme_classic" = base,  # white, no grid, axis lines
             "theme_minimal" = modifyList(base, list(
               xaxis = list(showgrid = TRUE,  showline = FALSE, zeroline = FALSE, gridcolor = "#e5e5e5"),
               yaxis = list(showgrid = TRUE,  showline = FALSE, zeroline = FALSE, gridcolor = "#e5e5e5")
             )),
             "theme_light" = modifyList(base, list(
               plot_bgcolor = "#f8f8f8",
               xaxis = list(showgrid = TRUE,  showline = TRUE, zeroline = FALSE,
                            linecolor = "#cccccc", gridcolor = "white"),
               yaxis = list(showgrid = TRUE,  showline = TRUE, zeroline = FALSE,
                            linecolor = "#cccccc", gridcolor = "white")
             )),
             "theme_dark" = modifyList(base, list(
               paper_bgcolor = "#2d2d2d",
               plot_bgcolor  = "#2d2d2d",
               font          = list(color = "white"),
               xaxis = list(showgrid = TRUE,  showline = FALSE, zeroline = FALSE,
                            gridcolor = "#555555", tickfont = list(color = "white")),
               yaxis = list(showgrid = TRUE,  showline = FALSE, zeroline = FALSE,
                            gridcolor = "#555555", tickfont = list(color = "white"))
             )),
             "theme_void" = modifyList(base, list(
               xaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE,
                            showticklabels = FALSE, title = ""),
               yaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE,
                            showticklabels = FALSE, title = "")
             )),
             "theme_grey" = modifyList(base, list(
               plot_bgcolor = "#ebebeb",
               xaxis = list(showgrid = TRUE,  showline = FALSE, zeroline = FALSE, gridcolor = "white"),
               yaxis = list(showgrid = TRUE,  showline = FALSE, zeroline = FALSE, gridcolor = "white")
             )),
             "theme_bw" = modifyList(base, list(
               xaxis = list(showgrid = TRUE,  showline = TRUE,  zeroline = FALSE,
                            linecolor = "black", gridcolor = "#cccccc"),
               yaxis = list(showgrid = TRUE,  showline = TRUE,  zeroline = FALSE,
                            linecolor = "black", gridcolor = "#cccccc")
             )),
             base  # fallback to classic
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
    
    
    # Theme generator 
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
      req(input$scatter_point_size)  
      
      df <- dataset()
      group_col <- names(df)[1]
      keep_rows <- df[[group_col]] %in% input$scatter_group_filter
      df <- df[keep_rows, ]
      
      # Align specimen IDs to the filtered rows
      ids <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[keep_rows]
      } else {
        as.character(seq_len(nrow(df)))
      }
      df$.label <- ids
      
      p <- ggplot(df, aes(x = .data[[input$scatter_xvar]], 
                          y = .data[[input$scatter_yvar]], 
                          color = .data[[group_col]]))
      
      # Add points with outline option
      if (isTRUE(input$scatter_outline_points)) {
        p <- p + geom_point(
          aes(fill = .data[[group_col]]),
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
      
      if (input$scatter_show_lm) {
        p <- p + geom_smooth(method = "lm", 
                             se = isTRUE(input$scatter_show_lm_se), 
                             linetype = "solid")
      }
      
      if (input$scatter_show_labels) {
        p <- p + geom_text(aes(label = .data[[".label"]]),
                           hjust = 1.1, vjust = 1.1,
                           size = 3, check_overlap = TRUE)
      }
      
      # Update color/fill scales based on outline option
      if (isTRUE(input$scatter_outline_points)) {
        p <- p + get_fill_scale(plot_palette())
        p <- p + get_color_scale(plot_palette()) + ggplot2::guides(color = "none")
      } else {
        p <- p + get_color_scale(plot_palette())
      }
      
      p + get_custom_theme(plot_axis_text_size(), plot_axis_label_size(), 
                           plot_x_angle(), plot_facet_size(), 
                           legend_text_size(), legend_title_size(), plot_theme())
    })
    
    
    # ---- Scatter: switch between static and interactive output ----
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
      req(dataset(), input$scatter_xvar, input$scatter_yvar, input$scatter_group_filter)
      
      df        <- dataset()
      group_col <- names(df)[1]
      keep_rows <- df[[group_col]] %in% input$scatter_group_filter
      df        <- df[keep_rows, ]
      ids       <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[keep_rows]
      } else {
        as.character(seq_len(nrow(df)))
      }
      
      hover_txt <- paste0(
        "ID: ",    ids,
        "<br>Group: ", df[[group_col]],
        "<br>",    input$scatter_xvar, ": ", round(df[[input$scatter_xvar]], 3),
        "<br>",    input$scatter_yvar, ": ", round(df[[input$scatter_yvar]], 3)
      )
      
      p <- ggplot2::ggplot(df, ggplot2::aes(
        x    = .data[[input$scatter_xvar]],
        y    = .data[[input$scatter_yvar]],
        color = .data[[group_col]],
        text  = hover_txt
      )) +
        ggplot2::geom_point(size = input$scatter_point_size %||% 3, alpha = 0.8) +
        get_color_scale(plot_palette()) +
        ggplot2::labs(color = "Group")
      
      if (isTRUE(input$scatter_show_lm)) {
        p <- p + ggplot2::geom_smooth(
          ggplot2::aes(text = NULL),
          method = "lm", se = isTRUE(input$scatter_show_lm_se),
          linetype = "solid", inherit.aes = TRUE
        )
      }
      
      theme_layout <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick  <- plot_axis_text_size()  %||% 10
      ax_label <- plot_axis_label_size() %||% 12
      leg_text  <- legend_text_size()    %||% 10
      leg_title <- legend_title_size()   %||% 12
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          paper_bgcolor = theme_layout$paper_bgcolor,
          plot_bgcolor  = theme_layout$plot_bgcolor,
          font          = theme_layout$font,
          xaxis = modifyList(theme_layout$xaxis, list(
            autorange = TRUE,
            title     = list(text = input$scatter_xvar,
                             font = list(size = ax_label)),
            tickfont  = list(size = ax_tick)
          )),
          yaxis = modifyList(theme_layout$yaxis, list(
            autorange = TRUE,
            title     = list(text = input$scatter_yvar,
                             font = list(size = ax_label)),
            tickfont  = list(size = ax_tick)
          )),
          legend = list(
            font  = list(size = leg_text),
            title = list(font = list(size = leg_title))
          )
        )
    })
    
    output$plot_scatter_plotly <- plotly::renderPlotly({ plot_scatter_plotly() })
    
    plot_box_obj <- reactive({
      req(dataset(), common_plot_inputs_ready(), input$selected_box_traits)
      df <- dataset()
      if (!is.null(input$selected_box_groups)) {
        df <- df[df[[1]] %in% input$selected_box_groups, ]
      }
      traits_to_plot <- input$selected_box_traits
      req(length(traits_to_plot) > 0)
      
      df_long <- tidyr::pivot_longer(df, cols = all_of(traits_to_plot), names_to = "Trait", values_to = "Value")
      
      ggplot2::ggplot(df_long, ggplot2::aes(x = .data[[names(df)[1]]], 
                                            y = .data[["Value"]], 
                                            fill = .data[[names(df)[1]]])) +
        ggplot2::geom_boxplot(outlier.shape = NA, 
                              alpha = 0.7,
                              color = "black",
                              linewidth = input$box_outline_stroke) +
        ggplot2::facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale(plot_palette()) +
        get_custom_theme(plot_axis_text_size(), plot_axis_label_size(),
                         plot_x_angle(), plot_facet_size(),
                         legend_text_size(), legend_title_size(), plot_theme())
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
      violin_outline_width  <- if (isTRUE(input$violin_outline)) input$violin_point_stroke else 0
      
      ggplot2::ggplot(df_long, ggplot2::aes(x = .data[[names(df)[1]]], 
                                            y = .data[["Value"]], 
                                            fill = .data[[names(df)[1]]])) +
        ggplot2::geom_violin(width = 0.7, alpha = 0.6, 
                             color = violin_outline_color, 
                             linewidth = violin_outline_width) +
        ggplot2::facet_wrap(~Trait, scales = "free_y") +
        get_fill_scale(plot_palette()) +
        get_custom_theme(plot_axis_text_size(), plot_axis_label_size(),
                         plot_x_angle(), plot_facet_size(),
                         legend_text_size(), legend_title_size(),
                         plot_theme())
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
      req(input$pca_x_axis, input$pca_y_axis)
      
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
      
      # Get selected PC axes
      pc_x <- input$pca_x_axis
      pc_y <- input$pca_y_axis
      
      # Extract PC numbers
      pc_x_num <- as.numeric(gsub("PC", "", pc_x))
      pc_y_num <- as.numeric(gsub("PC", "", pc_y))
      
      # Calculate % variance explained
      var_explained <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 1)
      pc_x_label <- paste0(pc_x, " (", var_explained[pc_x_num], "%)")
      pc_y_label <- paste0(pc_y, " (", var_explained[pc_y_num], "%)")
      
      # Base plot with selected axes
      p <- ggplot2::ggplot(pca_df, ggplot2::aes(x = .data[[pc_x]], y = .data[[pc_y]])) +
        ggplot2::xlab(pc_x_label) +
        ggplot2::ylab(pc_y_label)
      
      # Add biplot arrows if enabled
      if (isTRUE(input$pca_biplot)) {
        # Get loadings (rotation matrix)
        loadings <- as.data.frame(pca$rotation[, c(pc_x_num, pc_y_num)])
        colnames(loadings) <- c("PC_x", "PC_y")
        loadings$variable <- rownames(loadings)
        
        # Scale arrows to fit nicely on the plot
        # Calculate the range of the data
        data_range_x <- max(abs(range(pca_df[[pc_x]])))
        data_range_y <- max(abs(range(pca_df[[pc_y]])))
        
        # Scale factor - adjust arrows to be ~70% of the plot range
        scale_factor <- min(data_range_x / max(abs(loadings$PC_x)), 
                            data_range_y / max(abs(loadings$PC_y))) * 0.7
        
        loadings$PC_x_scaled <- loadings$PC_x * scale_factor
        loadings$PC_y_scaled <- loadings$PC_y * scale_factor
        
        # Add arrows
        p <- p + ggplot2::geom_segment(
          data = loadings,
          aes(x = 0, y = 0, xend = PC_x_scaled, yend = PC_y_scaled),
          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed"),
          color = input$pca_arrow_color,
          linewidth = input$pca_arrow_size * 0.5,
          alpha = input$pca_arrow_alpha,
          inherit.aes = FALSE
        )
        
        # Add variable labels if enabled
        if (isTRUE(input$pca_biplot_labels)) {
          p <- p + ggplot2::geom_text(
            data = loadings,
            aes(x = PC_x_scaled * 1.1, y = PC_y_scaled * 1.1, label = variable),
            color = input$pca_arrow_color,
            size = input$pca_label_size,
            hjust = ifelse(loadings$PC_x_scaled > 0, 0, 1),
            vjust = ifelse(loadings$PC_y_scaled > 0, 0, 1),
            inherit.aes = FALSE
          )
        }
      }
      
      # Add points
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
            linewidth = input$pca_outline_stroke,
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
          df[chull(df[[pc_x]], df[[pc_y]]), ]
        }), .id = "Group")
        
        if (isTRUE(input$pca_outline)) {
          p <- p + ggplot2::geom_polygon(
            data = hull_df,
            aes(x = .data[[pc_x]], y = .data[[pc_y]], group = Group, fill = Group, color = Group),
            alpha = input$pca_alpha_ellipse,
            linewidth = input$pca_outline_stroke,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
        } else {
          p <- p + ggplot2::geom_polygon(
            data = hull_df,
            aes(x = .data[[pc_x]], y = .data[[pc_y]], group = Group, fill = Group),
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
          dplyr::summarize(
            x_cent = mean(.data[[pc_x]]),
            y_cent = mean(.data[[pc_y]]),
            .groups = "drop"
          )
        
        centroid_size <- if (!is.null(input$pca_centroid_size)) input$pca_centroid_size else 4
        
        # Spider plot
        if (isTRUE(input$pca_spider)) {
          spider_df <- pca_df %>%
            dplyr::left_join(centroids, by = "Group")
          
          p <- p + ggplot2::geom_segment(
            data = spider_df,
            aes(
              x     = .data[[pc_x]],
              y     = .data[[pc_y]],
              xend  = x_cent,
              yend  = y_cent,
              color = Group
            ),
            alpha       = input$pca_spider_alpha,
            linewidth   = input$pca_spider_width,
            inherit.aes = FALSE,
            show.legend = FALSE
          ) +
            get_color_scale(plot_palette())
        }
        
        # MST
        if (isTRUE(input$pca_mst) && nrow(centroids) >= 2) {
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
            color       = input$pca_mst_color,
            alpha       = input$pca_mst_alpha,
            linewidth   = input$pca_mst_width,
            inherit.aes = FALSE
          )
          
          if (isTRUE(input$pca_mst_labels)) {
            p <- p + ggplot2::geom_label(
              data = mst_df,
              aes(x = x_mid, y = y_mid, label = distance),
              size          = input$pca_mst_label_size,
              color         = "black",
              fill          = "white",
              label.padding = ggplot2::unit(0.15, "lines"),
              inherit.aes   = FALSE
            )
          }
        }
        
        # Centroid points
        p <- p + ggplot2::geom_point(
          data = centroids,
          aes(x = x_cent, y = y_cent),
          shape = 8, size = centroid_size,
          color = input$pca_centroid_color,
          fill = "white", stroke = 1,
          inherit.aes = FALSE
        )
        
        # Pairwise centroid distances
        if (isTRUE(input$pca_centroid_distances) && nrow(centroids) >= 2) {
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
              color       = input$pca_centroid_dist_color,
              linewidth   = input$pca_centroid_dist_width,
              alpha       = input$pca_centroid_dist_alpha,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_label(
              data = dist_df,
              aes(x = x_mid, y = y_mid, label = distance),
              size          = input$pca_centroid_dist_label_size,
              color         = "black",
              fill          = "white",
              label.padding = ggplot2::unit(0.15, "lines"),
              inherit.aes   = FALSE
            )
        }
      }
      
      if (isTRUE(input$pca_outline_points)) {
        p <- p + get_fill_scale(plot_palette())
        p <- p + get_color_scale(plot_palette()) + ggplot2::guides(color = "none")
      } else {
        p <- p + get_color_scale(plot_palette())
        p <- p + get_fill_scale(plot_palette()) + ggplot2::guides(fill = "none")
      }
      p + get_custom_theme(plot_axis_text_size(), plot_axis_label_size(), 0, plot_facet_size(),
                           legend_text_size(), legend_title_size(),plot_theme())
      
    })
    
    
    # ---- PCA: switch between static and interactive output ----
    output$pca_plot_ui <- renderUI({
      if (isTRUE(input$pca_interactive)) {
        plotly::plotlyOutput(ns("plot_pca_plotly"),
                             height = paste0(input$plot_pca_height %||% 500, "px"))
      } else {
        plotOutput(ns("plot_pca"),
                   height = paste0(input$plot_pca_height %||% 500, "px"),
                   width  = paste0(input$plot_pca_width  %||% 600, "px"))
      }
    })
    
    plot_pca_plotly <- reactive({
      req(dataset(), input$pca_x_axis, input$pca_y_axis)
      
      df       <- dataset()
      otu_col  <- names(df)[1]
      data_mat <- df[, -1]
      complete_rows <- complete.cases(data_mat)
      req(sum(complete_rows) >= 2, ncol(data_mat) >= 2)
      
      pca    <- prcomp(data_mat[complete_rows, ], center = TRUE, scale. = TRUE)
      pca_df <- as.data.frame(pca$x)
      pca_df$Group <- df[[otu_col]][complete_rows]
      pca_df$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[complete_rows]
      } else {
        as.character(seq_len(sum(complete_rows)))
      }
      
      pc_x     <- input$pca_x_axis
      pc_y     <- input$pca_y_axis
      pc_x_num <- as.numeric(gsub("PC", "", pc_x))
      pc_y_num <- as.numeric(gsub("PC", "", pc_y))
      var_exp  <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 1)
      
      hover_txt <- paste0(
        "ID: ",    pca_df$SpecimenID,
        "<br>Group: ", pca_df$Group,
        "<br>",    pc_x, ": ", round(pca_df[[pc_x]], 3),
        "<br>",    pc_y, ": ", round(pca_df[[pc_y]], 3)
      )
      
      p <- ggplot2::ggplot(pca_df, ggplot2::aes(
        x     = .data[[pc_x]],
        y     = .data[[pc_y]],
        color = Group,
        fill  = Group,
        text  = hover_txt
      )) +
        ggplot2::geom_point(size = input$pca_point_size %||% 3, alpha = 0.8) +
        ggplot2::xlab(paste0(pc_x, " (", var_exp[pc_x_num], "%)")) +
        ggplot2::ylab(paste0(pc_y, " (", var_exp[pc_y_num], "%)")) +
        get_color_scale(plot_palette()) +
        get_fill_scale(plot_palette()) +
        ggplot2::guides(fill = "none")
      
      if (isTRUE(input$pca_ellipse)) {
        p <- p + ggplot2::stat_ellipse(
          ggplot2::aes(group = Group, fill = Group, text = NULL),
          color = NA, type = "norm", geom = "polygon",
          alpha = input$pca_alpha_ellipse %||% 0.3, show.legend = FALSE
        )
      }
      
      if (isTRUE(input$pca_convex)) {
        hull_df <- dplyr::bind_rows(lapply(split(pca_df, pca_df$Group), function(d) {
          d[chull(d[[pc_x]], d[[pc_y]]), ]
        }))
        p <- p + ggplot2::geom_polygon(
          data = hull_df,
          ggplot2::aes(x = .data[[pc_x]], y = .data[[pc_y]], group = Group, fill = Group),
          color = NA, alpha = input$pca_alpha_ellipse %||% 0.3,
          inherit.aes = FALSE, show.legend = FALSE
        )
      }
      
      if (isTRUE(input$pca_centroids)) {
        centroids <- pca_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(x_cent = mean(.data[[pc_x]]),
                           y_cent = mean(.data[[pc_y]]), .groups = "drop")
        p <- p + ggplot2::geom_point(
          data = centroids,
          ggplot2::aes(x = x_cent, y = y_cent),
          shape = 8, size = input$pca_centroid_size %||% 4,
          color = input$pca_centroid_color %||% "#000000",
          inherit.aes = FALSE
        )
      }
      
      theme_layout <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick  <- plot_axis_text_size()  %||% 10
      ax_label <- plot_axis_label_size() %||% 12
      leg_text  <- legend_text_size()    %||% 10
      leg_title <- legend_title_size()   %||% 12
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          paper_bgcolor = theme_layout$paper_bgcolor,
          plot_bgcolor  = theme_layout$plot_bgcolor,
          font          = theme_layout$font,
          xaxis = modifyList(theme_layout$xaxis, list(
            autorange = TRUE,
            title     = list(text = paste0(pc_x, " (", var_exp[pc_x_num], "%)"),
                             font = list(size = ax_label)),
            tickfont  = list(size = ax_tick)
          )),
          yaxis = modifyList(theme_layout$yaxis, list(
            autorange = TRUE,
            title     = list(text = paste0(pc_y, " (", var_exp[pc_y_num], "%)"),
                             font = list(size = ax_label)),
            tickfont  = list(size = ax_tick)
          )),
          legend = list(
            font  = list(size = leg_text),
            title = list(font = list(size = leg_title))
          )
        )
    })
    
    output$plot_pca_plotly <- plotly::renderPlotly({
      tryCatch(plot_pca_plotly(),
               error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(text = paste("Error:", e$message),
                                         showarrow = FALSE))
    })
    
    # ---- 3D PCA axis selectors ----
    make_3d_pc_selector <- function(input_id, label, default_pc) {
      renderUI({
        req(dataset())
        df            <- dataset()
        data_mat      <- df[, -1]
        complete_rows <- complete.cases(data_mat)
        req(sum(complete_rows) >= 2, ncol(data_mat) >= 2)
        n_pcs     <- min(sum(complete_rows), ncol(data_mat))
        pc_choices <- paste0("PC", seq_len(n_pcs))
        selectInput(ns(input_id), label,
                    choices  = pc_choices,
                    selected = pc_choices[min(default_pc, n_pcs)],
                    width    = "150px")
      })
    }
    output$pca_3d_x_selector <- make_3d_pc_selector("pca_3d_x", "X-axis:", 1)
    output$pca_3d_y_selector <- make_3d_pc_selector("pca_3d_y", "Y-axis:", 2)
    output$pca_3d_z_selector <- make_3d_pc_selector("pca_3d_z", "Z-axis:", 3)
    
    plot_pca_3d <- reactive({
      req(dataset(), input$pca_3d_x, input$pca_3d_y, input$pca_3d_z)
      
      df       <- dataset()
      otu_col  <- names(df)[1]
      data_mat <- df[, -1]
      complete_rows <- complete.cases(data_mat)
      req(sum(complete_rows) >= 3, ncol(data_mat) >= 3)
      
      pca    <- prcomp(data_mat[complete_rows, ], center = TRUE, scale. = TRUE)
      pca_df <- as.data.frame(pca$x)
      pca_df$Group <- df[[otu_col]][complete_rows]
      pca_df$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[complete_rows]
      } else {
        as.character(seq_len(sum(complete_rows)))
      }
      
      pc_x     <- input$pca_3d_x
      pc_y     <- input$pca_3d_y
      pc_z     <- input$pca_3d_z
      pc_x_num <- as.numeric(gsub("PC", "", pc_x))
      pc_y_num <- as.numeric(gsub("PC", "", pc_y))
      pc_z_num <- as.numeric(gsub("PC", "", pc_z))
      var_exp  <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 1)
      
      groups   <- levels(factor(pca_df$Group))
      n_groups <- length(groups)
      
      color_map <- if (!is.null(plot_palette()) && plot_palette() == "manual" &&
                       !is.null(manual_colors_r())) {
        # Named manual colors — subset to the groups actually present
        cols <- manual_colors_r()
        cols[as.character(groups)]
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "viridis:")) {
        opt <- sub("viridis:", "", plot_palette())
        setNames(viridis::viridis(n_groups, option = opt), groups)
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "brewer:")) {
        pal_name <- sub("brewer:", "", plot_palette())
        max_n    <- RColorBrewer::brewer.pal.info[pal_name, "maxcolors"]
        setNames(RColorBrewer::brewer.pal(max(3L, min(n_groups, max_n)), pal_name)[seq_len(n_groups)], groups)
      } else if (!is.null(plot_palette()) && startsWith(plot_palette(), "colorblind:")) {
        pal_name <- sub("colorblind:", "", plot_palette())
        setNames(RColorBrewer::brewer.pal(max(3L, min(n_groups, 8L)), pal_name)[seq_len(n_groups)], groups)
      } else {
        setNames(scales::hue_pal()(n_groups), groups)
      }
      
      hover_txt <- paste0(
        "ID: ",    pca_df$SpecimenID,
        "<br>Group: ", pca_df$Group,
        "<br>",    pc_x, ": ", round(pca_df[[pc_x]], 3),
        "<br>",    pc_y, ": ", round(pca_df[[pc_y]], 3),
        "<br>",    pc_z, ": ", round(pca_df[[pc_z]], 3)
      )
      
      plt <- plotly::plot_ly(
        data       = pca_df,
        x          = ~get(pc_x),
        y          = ~get(pc_y),
        z          = ~get(pc_z),
        color      = ~Group,
        colors     = color_map,
        type       = "scatter3d",
        mode       = "markers",
        marker     = list(size = (input$pca_3d_point_size %||% 3) * 2,
                          opacity = input$pca_3d_point_alpha %||% 0.8),
        text       = hover_txt,
        hoverinfo  = "text"
      )
      
      # Centroids
      if (isTRUE(input$pca_3d_centroids)) {
        centroids_3d <- pca_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(
            cx = mean(.data[[pc_x]]),
            cy = mean(.data[[pc_y]]),
            cz = mean(.data[[pc_z]]),
            .groups = "drop"
          )
        for (grp in centroids_3d$Group) {
          row  <- centroids_3d[centroids_3d$Group == grp, ]
          col  <- color_map[as.character(grp)]
          plt  <- plt %>% plotly::add_trace(
            x = row$cx, y = row$cy, z = row$cz,
            type = "scatter3d", mode = "markers",
            marker = list(symbol = "diamond", size = (input$pca_3d_centroid_size %||% 8) * 2,
                          color = input$pca_3d_centroid_color %||% "#000000",
                          line  = list(color = col, width = 2)),
            name = paste0(grp, " (centroid)"),
            hoverinfo = "text",
            text = paste0("Centroid<br>Group: ", grp),
            showlegend = FALSE,
            inherit = FALSE
          )
        }
      }
      
      # Convex hulls
      if (isTRUE(input$pca_3d_hull)) {
        for (grp in groups) {
          sub <- pca_df[pca_df$Group == grp, c(pc_x, pc_y, pc_z)]
          if (nrow(sub) >= 4) {
            col <- color_map[as.character(grp)]
            plt <- plt %>% plotly::add_mesh(
              x = sub[[pc_x]], y = sub[[pc_y]], z = sub[[pc_z]],
              alphahull  = 0,
              opacity    = input$pca_3d_hull_alpha %||% 0.15,
              colorscale = list(c(0, col), c(1, col)),
              intensity  = rep(0, nrow(sub)),
              showscale  = FALSE,
              name       = paste0(grp, " (hull)"),
              showlegend = FALSE,
              hoverinfo  = "skip",
              inherit    = FALSE
            )
          }
        }
      }
      
      
      # Spider lines
      if (isTRUE(input$pca_3d_spider)) {
        centroids_sp <- pca_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(cx = mean(.data[[pc_x]]), cy = mean(.data[[pc_y]]), cz = mean(.data[[pc_z]]), .groups = "drop")
        for (grp in groups) {
          pts <- pca_df[pca_df$Group == grp, ]
          cen <- centroids_sp[centroids_sp$Group == grp, ]
          col <- color_map[as.character(grp)]
          for (i in seq_len(nrow(pts))) {
            plt <- plt %>% plotly::add_trace(
              x = c(pts[[pc_x]][i], cen$cx),
              y = c(pts[[pc_y]][i], cen$cy),
              z = c(pts[[pc_z]][i], cen$cz),
              type = "scatter3d", mode = "lines",
              line = list(color = col,
                          width = input$pca_3d_spider_width %||% 2),
              opacity    = input$pca_3d_spider_alpha %||% 0.4,
              showlegend = FALSE, hoverinfo = "skip", inherit = FALSE
            )
          }
        }
      }
      
      plt %>%
        plotly::layout(
          scene = list(
            xaxis = list(
              title    = list(text = paste0(pc_x, " (", var_exp[pc_x_num], "%)"),
                              font = list(size = plot_axis_label_size() %||% 12)),
              tickfont = list(size = plot_axis_text_size() %||% 10)
            ),
            yaxis = list(
              title    = list(text = paste0(pc_y, " (", var_exp[pc_y_num], "%)"),
                              font = list(size = plot_axis_label_size() %||% 12)),
              tickfont = list(size = plot_axis_text_size() %||% 10)
            ),
            zaxis = list(
              title    = list(text = paste0(pc_z, " (", var_exp[pc_z_num], "%)"),
                              font = list(size = plot_axis_label_size() %||% 12)),
              tickfont = list(size = plot_axis_text_size() %||% 10)
            )
          ),
          legend = list(
            title = list(text = "Group", font = list(size = legend_title_size() %||% 12)),
            font  = list(size = legend_text_size() %||% 10)
          )
        )
    })
    
    output$plot_pca_3d <- plotly::renderPlotly({
      tryCatch(plot_pca_3d() %>%
                 plotly::config(
                   toImageButtonOptions = list(
                     format   = "png",
                     filename = "pca_3d",
                     width    = 1600,
                     height   = 1200,
                     scale    = 3
                   )
                 ),
               error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(text = paste("Need at least 3 PCs and 3 complete rows.", e$message),
                                         showarrow = FALSE))
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
            linewidth = input$dapc_outline_stroke,
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
            linewidth = input$dapc_outline_stroke, 
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
        
        centroid_size <- if (!is.null(input$dapc_centroid_size)) input$dapc_centroid_size else 4
        
        # Spider plot
        if (isTRUE(input$dapc_spider)) {
          spider_df <- dapc_df %>%
            dplyr::left_join(centroids, by = "Group", suffix = c("", "_cent"))
          
          p <- p + ggplot2::geom_segment(
            data = spider_df,
            aes(
              x     = LD1,
              y     = LD2,
              xend  = LD1_cent,
              yend  = LD2_cent,
              color = Group
            ),
            alpha       = input$dapc_spider_alpha,
            linewidth   = input$dapc_spider_width,
            inherit.aes = FALSE,
            show.legend = FALSE
          ) +
            get_color_scale(plot_palette())
        }
        
        # MST
        if (isTRUE(input$dapc_mst) && nrow(centroids) >= 2) {
          cent_mat <- as.matrix(dist(centroids[, c("LD1", "LD2")]))
          rownames(cent_mat) <- centroids$Group
          colnames(cent_mat) <- centroids$Group
          
          mst_obj <- ape::mst(cent_mat)
          mst_idx <- which(mst_obj == 1, arr.ind = TRUE)
          mst_idx <- mst_idx[mst_idx[, 1] < mst_idx[, 2], , drop = FALSE]
          
          mst_df <- data.frame(
            x_start  = centroids$LD1[mst_idx[, 1]],
            y_start  = centroids$LD2[mst_idx[, 1]],
            x_end    = centroids$LD1[mst_idx[, 2]],
            y_end    = centroids$LD2[mst_idx[, 2]],
            x_mid    = (centroids$LD1[mst_idx[, 1]] + centroids$LD1[mst_idx[, 2]]) / 2,
            y_mid    = (centroids$LD2[mst_idx[, 1]] + centroids$LD2[mst_idx[, 2]]) / 2,
            distance = round(sqrt(
              (centroids$LD1[mst_idx[, 2]] - centroids$LD1[mst_idx[, 1]])^2 +
                (centroids$LD2[mst_idx[, 2]] - centroids$LD2[mst_idx[, 1]])^2
            ), 3)
          )
          
          p <- p + ggplot2::geom_segment(
            data = mst_df,
            aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
            color       = input$dapc_mst_color,
            alpha       = input$dapc_mst_alpha,
            linewidth   = input$dapc_mst_width,
            inherit.aes = FALSE
          )
          
          if (isTRUE(input$dapc_mst_labels)) {
            p <- p + ggplot2::geom_label(
              data = mst_df,
              aes(x = x_mid, y = y_mid, label = distance),
              size          = input$dapc_mst_label_size,
              color         = "black",
              fill          = "white",
              label.padding = ggplot2::unit(0.15, "lines"),
              inherit.aes   = FALSE
            )
          }
        }
        
        # Centroid points
        p <- p + ggplot2::geom_point(
          data = centroids,
          aes(x = LD1, y = LD2),
          shape = 8, size = centroid_size,
          color = input$dapc_centroid_color,
          fill = "white", stroke = 1,
          inherit.aes = FALSE
        )
        
        # Pairwise centroid distances
        if (isTRUE(input$dapc_centroid_distances) && nrow(centroids) >= 2) {
          pairs <- combn(nrow(centroids), 2)
          dist_df <- data.frame(
            x_start  = centroids$LD1[pairs[1, ]],
            y_start  = centroids$LD2[pairs[1, ]],
            x_end    = centroids$LD1[pairs[2, ]],
            y_end    = centroids$LD2[pairs[2, ]],
            x_mid    = (centroids$LD1[pairs[1, ]] + centroids$LD1[pairs[2, ]]) / 2,
            y_mid    = (centroids$LD2[pairs[1, ]] + centroids$LD2[pairs[2, ]]) / 2,
            distance = round(sqrt(
              (centroids$LD1[pairs[2, ]] - centroids$LD1[pairs[1, ]])^2 +
                (centroids$LD2[pairs[2, ]] - centroids$LD2[pairs[1, ]])^2
            ), 3)
          )
          
          p <- p +
            ggplot2::geom_segment(
              data = dist_df,
              aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
              color       = input$dapc_centroid_dist_color,
              linewidth   = input$dapc_centroid_dist_width,
              alpha       = input$dapc_centroid_dist_alpha,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_label(
              data = dist_df,
              aes(x = x_mid, y = y_mid, label = distance),
              size          = input$dapc_centroid_dist_label_size,
              color         = "black",
              fill          = "white",
              label.padding = ggplot2::unit(0.15, "lines"),
              inherit.aes   = FALSE
            )
        }
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
          plot_theme()
        )
      
    })
    
    
    # ---- DAPC: switch between static and interactive output ----
    output$dapc_plot_ui <- renderUI({
      if (isTRUE(input$dapc_interactive)) {
        plotly::plotlyOutput(ns("plot_dapc_plotly"),
                             height = paste0(input$plot_dapc_height %||% 500, "px"))
      } else {
        plotOutput(ns("plot_dapc"),
                   height = paste0(input$plot_dapc_height %||% 500, "px"),
                   width  = paste0(input$plot_dapc_width  %||% 600, "px"))
      }
    })
    
    plot_dapc_plotly <- reactive({
      req(dataset(), input$n_pca_dapc, input$n_da_dapc, input$dapc_point_size)
      
      df       <- dataset()
      otu_col  <- names(df)[1]
      data_mat <- df[, -1]
      complete_rows <- complete.cases(data_mat)
      req(sum(complete_rows) >= 2, ncol(data_mat) >= 2,
          dplyr::n_distinct(df[[otu_col]]) >= 2)
      
      data_for_dapc  <- as.data.frame(data_mat[complete_rows, ])
      group_for_dapc <- as.factor(df[[otu_col]][complete_rows])
      
      dapc_res <- tryCatch(
        adegenet::dapc(data_for_dapc, group_for_dapc,
                       n.pca = input$n_pca_dapc, n.da = input$n_da_dapc),
        error = function(e) NULL
      )
      req(!is.null(dapc_res), ncol(dapc_res$ind.coord) >= 2)
      
      dapc_df <- as.data.frame(dapc_res$ind.coord)
      dapc_df$Group <- dapc_res$grp
      dapc_df$SpecimenID <- if (!is.null(specimen_ids_r) && !is.null(specimen_ids_r())) {
        specimen_ids_r()[complete_rows]
      } else {
        as.character(seq_len(sum(complete_rows)))
      }
      
      eig         <- dapc_res$eig
      eig_percent <- round(100 * eig / sum(eig), 1)
      ld1_label   <- paste0("LD1 (", eig_percent[1], "%)")
      ld2_label   <- paste0("LD2 (", eig_percent[2], "%)")
      
      hover_txt <- paste0(
        "ID: ",    dapc_df$SpecimenID,
        "<br>Group: ", dapc_df$Group,
        "<br>LD1: ",   round(dapc_df$LD1, 3),
        "<br>LD2: ",   round(dapc_df$LD2, 3)
      )
      
      p <- ggplot2::ggplot(dapc_df, ggplot2::aes(
        x    = LD1, y = LD2,
        color = Group, fill = Group,
        text  = hover_txt
      )) +
        ggplot2::geom_point(size = input$dapc_point_size %||% 3, alpha = 0.8) +
        ggplot2::xlab(ld1_label) + ggplot2::ylab(ld2_label) +
        get_color_scale(plot_palette()) +
        get_fill_scale(plot_palette()) +
        ggplot2::guides(fill = "none")
      
      if (isTRUE(input$dapc_ellipse)) {
        p <- p + ggplot2::stat_ellipse(
          ggplot2::aes(group = Group, fill = Group, text = NULL),
          color = NA, type = "norm", level = 0.67, geom = "polygon",
          alpha = input$dapc_alpha_ellipse %||% 0.3, show.legend = FALSE
        )
      }
      
      if (isTRUE(input$dapc_convex)) {
        hull_df <- dplyr::bind_rows(lapply(split(dapc_df, dapc_df$Group), function(d) {
          d[chull(d$LD1, d$LD2), ]
        }))
        p <- p + ggplot2::geom_polygon(
          data = hull_df,
          ggplot2::aes(x = LD1, y = LD2, group = Group, fill = Group),
          color = NA, alpha = input$dapc_alpha_ellipse %||% 0.3,
          inherit.aes = FALSE, show.legend = FALSE
        )
      }
      
      if (isTRUE(input$dapc_centroids)) {
        centroids <- dapc_df %>%
          dplyr::group_by(Group) %>%
          dplyr::summarize(LD1 = mean(LD1), LD2 = mean(LD2), .groups = "drop")
        p <- p + ggplot2::geom_point(
          data = centroids,
          ggplot2::aes(x = LD1, y = LD2),
          shape = 8, size = input$dapc_centroid_size %||% 4,
          color = input$dapc_centroid_color %||% "#000000",
          inherit.aes = FALSE
        )
      }
      
      theme_layout <- get_plotly_theme_layout(plot_theme() %||% "theme_classic")
      ax_tick  <- plot_axis_text_size()  %||% 10
      ax_label <- plot_axis_label_size() %||% 12
      leg_text  <- legend_text_size()    %||% 10
      leg_title <- legend_title_size()   %||% 12
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          hoverlabel = list(bgcolor = "white", font = list(size = 12)),
          paper_bgcolor = theme_layout$paper_bgcolor,
          plot_bgcolor  = theme_layout$plot_bgcolor,
          font          = theme_layout$font,
          xaxis = modifyList(theme_layout$xaxis, list(
            autorange = TRUE,
            title     = list(text = ld1_label, font = list(size = ax_label)),
            tickfont  = list(size = ax_tick)
          )),
          yaxis = modifyList(theme_layout$yaxis, list(
            autorange = TRUE,
            title     = list(text = ld2_label, font = list(size = ax_label)),
            tickfont  = list(size = ax_tick)
          )),
          legend = list(
            font  = list(size = leg_text),
            title = list(font = list(size = leg_title))
          )
        )
    })
    
    output$plot_dapc_plotly <- plotly::renderPlotly({
      tryCatch(plot_dapc_plotly(),
               error = function(e) plotly::plot_ly() %>%
                 plotly::add_annotations(text = paste("Error:", e$message),
                                         showarrow = FALSE))
    })
    
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
    
    output$plot_pca <- renderPlot({
      req(!isTRUE(input$pca_interactive))
      plot_pca_obj()
    }, height = function() input$plot_pca_height %||% 500,
    width = function() input$plot_pca_width %||% 700)
    output$plot_dapc <- renderPlot({
      req(!isTRUE(input$dapc_interactive))
      plot_dapc_obj()
    }, height = function() input$plot_dapc_height %||% 500,
    width = function() input$plot_dapc_width %||% 700)
    
    # Default download dimensions (in inches)
    DEFAULT_DOWNLOAD_WIDTH <- 10
    DEFAULT_DOWNLOAD_HEIGHT <- 8 
    
    # The download function for all plots
    create_download_handler <- function(plot_obj_reactive, filename_prefix, type = "pdf", height_input_id, width_input_id) { # Added width_input_id
      downloadHandler(
        filename = function() { paste0(filename_prefix, "_", Sys.Date(), ".", type) },
        content = function(file) {
          # Get user-specified height and width from the numericInputs
          
          plot_height_val_px <- input[[height_input_id]]
          plot_width_val_px <- input[[width_input_id]] 
          
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
    
    # Assign download handlers
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
