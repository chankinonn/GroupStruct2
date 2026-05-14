#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

source("global.R", local = TRUE)

app_ui <- function(request) {
  tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      color = "#337ab7",
      position = "bottom-right"
    ),
    fluidPage(
      titlePanel("GroupStruct2"),
      useShinyjs(),
      tags$head(
        tags$style(HTML("
          .module-active {
            background-color: #337ab7;
            color: white;
            font-weight: bold;
            border-color: #337ab7;
          }
          .module-active:hover, .module-active:focus, .module-active:active {
            background-color: #337ab7;
            color: white;
            font-weight: bold;
            border-color: #337ab7;
            outline: none;
            box-shadow: none;
          }
          .action-button:not(.module-active):hover {
            background-color: #e0e0e0;
            color: #333;
            border-color: #ccc;
          }
          .action-button:not(.module-active):focus,
          .action-button:not(.module-active):active {
            background-color: #337ab7;
            color: white;
            border-color: #337ab7;
            outline: none;
            box-shadow: none;
          }
          #fixed_sidebar {
            width: 250px;
            position: fixed;
            height: 100%;
            overflow-y: auto;
          }
          #main_content {
            margin-left: 260px;
          }
          @media (max-width: 768px) {
            #fixed_sidebar {
              width: 100%;
              position: relative;
              height: auto;
            }
            #main_content {
              margin-left: 0;
            }
          }

        "))
      ),
      sidebarLayout(
        sidebarPanel(
          id = "fixed_sidebar",
          
          # --- Home button (always visible) ---
          actionButton("reset_data_type",
                       label = tagList(icon("home"), "GroupStruct2 Home"),
                       class = "btn btn-primary btn-block"),
          
          # Links row (GitHub, Report Issue, version)
          sidebar_welcome_ui(),
          
          hr(),
          
          # --- Input Data (always visible) ---
          actionButton("go_data", "Input Data", width = "100%"),
          
          # Detected data type indicator
          uiOutput("data_type_indicator_ui"),
          
          # Hidden input — kept so existing conditionalPanel JS logic still works.
          # Updated programmatically by app_server.R when unified module detects data type.
          tags$div(style = "display: none;",
                   selectInput("data_type", label = NULL,
                               choices = c("Please select" = "", "meristic" = "meristic",
                                           "morphometric" = "morphometric", "combined" = "combined"),
                               selected = "")
          ),
          
          # --- Module navigation (visible only once data type is set) ---
          # All buttons are server-rendered together as one block so they
          # appear simultaneously rather than staggered.
          conditionalPanel(
            condition = "input.data_type != ''",
            sidebar_modules_header_ui(),
            uiOutput("module_nav_ui"),
            hr(),
            uiOutput("visual_customization_ui")
          )
        ),
        mainPanel(
          id = "main_content",
          uiOutput("module_ui")
        )
      )
    )
  )
}
