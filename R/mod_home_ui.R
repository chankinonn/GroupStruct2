#' @importFrom shiny tagList NS h3 hr p br h4 strong
NULL

#' Landing UI
#'
#' @return Shiny UI for the landing page
#' @export
landing_page_ui <- function() {
  tagList(
    tags$style(HTML("
      #example_meristic table, #example_meristic th, #example_meristic td,
      #example_morphometric table, #example_morphometric th, #example_morphometric td,
      #example_mixed table, #example_mixed th, #example_mixed td {
        text-align: center !important;
      }
    ")),
    h2("Welcome to GroupStruct2"),
    hr(),
    p(strong("If you use GroupStruct2, please cite:")),
    p(em("XXX")),
    p("Please select a type of data from the dropdown on the left to begin."),
    tags$ul(
      tags$li("Meristic: Count-based characters (numeric data only)"),
      tags$li("Morphometric: Measurement-based characters (numeric data only)"),
      tags$li("Mixed: Meristic + Morphometric + Categorical (mixture of any numeric or numeric + categorical)")
    ),
    p("The first column of your dataset MUST be the grouping variable (e.g., species), followed by other trait variables. The dataset must contain at least 2 samples per group (no singletons) and missing data is NOT ALLOWED."),
    hr(),
    p("Example Datasets:"),
    
    h4("Meristic Data"),
    tableOutput("example_meristic"),
    
    h4("Morphometric Data"),
    tableOutput("example_morphometric"),
    
    h4("Mixed Data"),
    tableOutput("example_mixed"),
    hr(),
  )
}

#' Home UI for Meristic Data
#'
#' @param id Namespace ID
#' @return Shiny UI for meristic home screen
#' @export
mod_home_ui_meristic <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Meristic Data"),
    hr(),
    p("Meristic data are discrete, countable traits such as scale counts, fin rays, vertebrae number, etc."),
    p("The first column must contain Group/OTU names (e.g., species or population)."),
    p("All trait values must be numeric and missing values are NOT ALLOWED"),
    br(),
    h4("Proceed to Input Data")
  )
}

#' Home UI for Morphometric Data
#'
#' @param id Namespace ID
#' @return Shiny UI for morphometric home screen
#' @export
mod_home_ui_morphometric <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Morphometric Data"),
    hr(),
    p("Morphometric data are continuous measurements such as lengths and widths."),
    p(strong("Allometric adjustments should be performed to correct for body-size variation in the Allometric Correction module.")),
    p("The first column must contain Group/OTU names (e.g., species or population) and the second column must be the body-size measurement (e.g., snout-vent length)."),
    p("Each Group/OTU must be represented by more than two individuals (to calculate mean) and missing data are NOT ALLOWED for allometric correction."),
    br(),
    h4("Proceed to Input Data")
  )
}

#' Home UI for Combined Data
#'
#' @param id Namespace ID
#' @return Shiny UI for combined/mixed data home screen
#' @export
mod_home_ui_combined <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Mixed Data (Numeric, Categorical)"),
    hr(),
    p("This module handles mixed data in a single dataset. The data can be a mixture of numerical (e.g., meristic + morphometric), numerical + categorical, or any combination of the two."),
    p("The first column must contain Group/OTU names (e.g., species or population). If morphometric data is included, the second column must be the body-size measurement (e.g., snout-vent length)."),
    p("The Allometric Correction module allows body-size correction to be performed on morphometric data only"),
    br(),
    h4("Proceed to Input Data")
  )
}
