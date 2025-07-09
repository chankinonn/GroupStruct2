#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

source("global.R", local = TRUE)

app_ui <- function(request) {
  tagList(
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
          actionButton("reset_data_type", label = tagList(icon("home"), "GroupStruct2 Home"), 
                       class = "btn btn-primary btn-block"),
          hr(),
          h4("1. Select type of data:"),
          #br(),
          selectInput("data_type", label = NULL,
                      choices = c("Please select" = "", 
                                  "Meristic" = "meristic",
                                  "Morphometric" = "morphometric",
                                  "Mixed data" = "combined"),
                      selected = ""),
          conditionalPanel(
            condition = "input.data_type != ''",
            hr(),
            h4("2. Run modules"),
            actionButton("go_data", "Input Data", width = "100%"),
            actionButton("go_summary", "Summary Statistics", width = "100%"),
            uiOutput("allometry_button_ui"),
            uiOutput("stats_button_ui"),
            uiOutput("mfa_button_ui"),
            actionButton("go_visual", "Visualization", width = "100%"),
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

