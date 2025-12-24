#' @importFrom shiny tagList NS h3 hr p br h4 strong h2 tabsetPanel tabPanel tags em HTML tableOutput
NULL

#' Landing UI
#'
#' @return Shiny UI for the landing page
#' @export
landing_page_ui <- function() {
  tagList(
    h2("Welcome to GroupStruct2"),
    hr(),
    
    tabsetPanel(
      # Overview Tab
      tabPanel("Overview",
               br(),
               p("GroupStruct2 is a user-friendly application for morphological data analysis",
                 "in systematic biology and taxonomy. It contains statistical tools optimized for inferring and evaluating group structure",
                 "to aid in species diagnosis, and visualization of morphometric, meristic, and mixed datasets."),
               
               h4("Available Analyses"),
               tags$ul(
                 tags$li(strong("Summary Statistics:"), "Descriptive statistics and data exploration"),
                 tags$li(strong("Allometric Correction (Morphometric data only):"), "Body-size correction for morphometric data using the Thorpe method"),
                 tags$li(strong("Inferential Statistics:"), "Univariate/multivariate tests (e.g., t-test, ANOVA, PERMANOVA) and PCA/DAPC"),
                 tags$li(strong("Morphometric Delimitation (Morphometric data only):"), 
                         "A Bayesian framework for delimitating morphometric clusters using Gaussian Mixture Models and diagnostic character identification using machine learning"),
                 tags$li(strong("Multiple Factor Analysis (Mixed data only):"), 
                         "Simultaneous analysis of quantitative and qualitative variables"),
                 tags$li(strong("Visualization:"), "Customizable plots including scatterplots, boxplots, violin plots, PCA, DAPC, and more")
               ),
               
               hr(),
               
               tags$div(
                 style = "background-color: #d4edda; border-left: 4px solid #28a745; padding: 15px; margin: 15px 0;",
                 h4(style = "margin-top: 0;", "Citation"),
                 p(style = "margin: 0; margin-bottom: 10px;",
                   strong("If you use GroupStruct2, please cite:")),
                 p(style = "margin: 0; font-style: italic;",
                   "Chan & Grismer (2025). GroupStruct2: A User-Friendly Graphical User Interface for Statistical and Visual Support in Species Diagnosis. Systematic Biology, syaf090. https://doi.org/10.1093/sysbio/syaf090"),
               ),

               hr(),
               
               h4("Getting Started"),
               p("Please read through the Suggested Workflows and Example Data sections."),
               p("When you are ready, select a data type from the dropdown menu on the left to begin:",
                 tags$ul(
                   tags$li(strong("Meristic:"), "Count-based characters (e.g., scale counts, fin rays, vertebrae)"),
                   tags$li(strong("Morphometric:"), "Continuous measurements (e.g., lengths, widths, heights)"),
                   tags$li(strong("Mixed:"), "Combination of meristic, morphometric, and/or categorical data")
                 )),
               hr(),
      ),
      
      # Suggested Workflow Tab
      tabPanel("Suggested Workflows",
               br(),
               p("Follow these recommended workflows to get the most out of GroupStruct2.",
                 "Each workflow is tailored to the specific data type you're working with."),
               
               hr(),
               
               # Common Steps
               h4("Common Steps (All Data Types)"),
               tags$div(
                 style = "background-color: #e9ecef; border-left: 4px solid #6c757d; padding: 15px; margin: 15px 0;",
                 tags$ol(
                   tags$li(strong("Input Data"), "-> Load your dataset and perform the outlier test to check for erroneous data.",
                           "Values flagged by the outlier test are not always wrong.",
                           "You know your data best, so use your own judgement to determine whether a data point is wrong or is a true outlier.",
                           "Remove/correct and reload the cleaned data before proceeding."),
                   tags$li(strong("Summary Statistics"), "-> Explore descriptive statistics and data distributions.",
                           "Use this step to understand your data structure and identify any remaining quality issues.")
                 ),
                 tags$div(
                   style = "background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; padding: 10px; margin-top: 15px;",
                   p(style = "margin: 0;",
                     strong("Pro Tip:"), 
                     "After performing the outlier test and checking summary statistics, examine scatter plots, box plots, and violin plots in the",
                     strong("Visualization"), "module to visually identify outliers and erroneous data.",
                     "Visual inspection often reveals data quality issues that numerical summaries might miss.")
                 )
               ),
               
               hr(),
               
               # Meristic Workflow
               h4("1. Meristic Data Workflow"),
               p(strong("Data type:"), "Count-based characters (numeric only)"),
               
               tags$div(
                 style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 15px; margin: 15px 0;",
                 tags$ol(start = 3,
                         tags$li(strong("Inferential Statistics"),
                                 tags$ul(
                                   tags$li("Run univariate tests (e.g., t-test, ANOVA) for individual traits"),
                                   tags$li("Run PCA to visualize morphological variation"),
                                   tags$li("Run PERMANOVA to test overall group differences")
                                  )),
                         tags$li(strong("Visualization"), "-> Create publication-ready plots")
                 )
               ),
               
               hr(),
               
               # Morphometric Workflow
               h4("2. Morphometric Data Workflow"),
               p(strong("Data type:"), "Continuous measurements (numeric only)"),
               tags$div(
                 style = "background-color: #f8f9fa; border-left: 4px solid #28a745; padding: 15px; margin: 15px 0;",
                 tags$ol(start = 3,
                         tags$li(strong("Allometric Correction"), "(HIGHLY RECOMMENDED)",
                                 tags$ul(
                                   tags$li("Correct for body-size variation using the allometric Thorpe method"),
                                   tags$li("Adjusted data will be used automatically in downstream analyses")
                                 )),
                         tags$li(strong("Inferential Statistics"),
                                 tags$ul(
                                   tags$li("Run univariate tests (e.g., t-test, ANOVA) for individual traits"),
                                   tags$li("Run PCA to visualize morphological variation"),
                                   tags$li("Run PERMANOVA to test overall group differences")
                                 )),
                         tags$li(strong("Morphometric Delimitation"),
                                 tags$ul(
                                   tags$li(strong("Unsupervised Clustering:"), "Discover natural clusters in the data without using pre-defined OTUs"),
                                   tags$li(strong("Supervised Clustering:"), "Iteratively test pairwise mergings of pre-defined OTUs to identify the best-supported delimitation scheme"),
                                   tags$li(strong("Model-based Hypothesis Testing:"), "Compare specific user-defined taxonomic hypotheses"),
                                   tags$li(strong("Diagnostic Characters (Machine Learning):"), "Identify which traits best distinguish groups using Random Forest")
                                 )),
                         tags$li(strong("Visualization"), "-> Create publication-ready plots")
                 )
               ),
               
               tags$div(
                 style = "background-color: #fff3cd; border-left: 4px solid #856404; padding: 10px; margin: 15px 0;",
                 tags$div(
                   p(style = "margin: 0; margin-bottom: 10px;", 
                     strong("Understanding Bayesian Model Testing vs. Inferential Statistics:")),
                   tags$ul(style = "margin: 0;",
                           tags$li(strong("Inferential Statistics"), "(PERMANOVA, univariate tests, PCA):",
                                   tags$ul(
                                     tags$li("Tests whether pre-defined groups differ significantly"),
                                     tags$li("Provides p-values and effect sizes for trait differences"),
                                     tags$li("Visualizes morphological variation and group overlap/separation")
                                   )),
                           tags$li(strong("Morphometric Delimitation"), "(Bayesian GMM approaches):",
                                   tags$ul(
                                     tags$li("Explores how well the morphometric data fits different taxonomic models using a Bayesian framework"),
                                     tags$li("Compares competing hypotheses using BIC and Bayes Factors"),
                                     tags$li("Evaluates relative model support as opposed to relying on p-values")
                                   )),
                           tags$li(strong("Diagnostic Characters"), "(Machine Learning):",
                                   tags$ul(
                                     tags$li("Identifies which traits are most important for distinguishing groups"),
                                     tags$li("Works with any delimitation scheme (original OTUs or best Bayesian model)")
                                   ))
                   ),
                   br(),
                   p(style = "margin: 0;",
                     strong("How to use them together:"),
                     "Both approaches are complementary and answer different questions.",
                     "Use inferential statistics to test significance and quantify differences.",
                     "Use Bayesian GMM analyses to evaluate alternative taxonomic arrangements.",
                     "Use Machine Learning in conjunction with univariate analyses to identify diagnostic characters for taxonomic keys.",
                     "There is no single 'correct' workflow--explore your data using both frameworks to gain comprehensive insights.")
                 )
               ),
               
               tags$div(
                 style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                 p(style = "margin: 0;", 
                   strong("Key Point:"), "Bayesian analyses indicate", em("which"), "taxonomic hypothesis is best supported.",
                   "Inferential statistics tests", em("whether"), "groups differ significantly.",
                   "Both provide valuable but different types of evidence to aid species delimitation.")
               ),
               
               hr(),
               
               # Mixed Data Workflow
               h4("3. Mixed Data Workflow"),
               p(strong("Data type:"), "Combination of meristic, morphometric, and/or categorical variables"),
               tags$div(
                 style = "background-color: #f8f9fa; border-left: 4px solid #dc3545; padding: 15px; margin: 15px 0;",
                 tags$ol(start = 3,
                         tags$li(strong("Allometric Correction"), "(if morphometric variables present)",
                                 tags$ul(
                                   tags$li("Correct morphometric variables only"),
                                   tags$li("Meristic and categorical variables remain unchanged")
                                 )),
                         tags$li(strong("Multiple Factor Analysis (MFA)"),
                                 tags$ul(
                                   tags$li("Define variable groups (e.g., 'Meristic', 'Morphometric', 'Color')"),
                                   tags$li("MFA balances the contribution of each variable type"),
                                   tags$li("Visualize group separation in factor space"),
                                   tags$li("Identify which variable types contribute most to differentiation")
                                 )),
                         tags$li(strong("Visualization"), 
                                 tags$ul(
                                   tags$li("Visualize OTU differentiation in morphospace"),
                                   tags$li("Visualize which variable types contribute most fo differentiation"),
                                   tags$li("Visualize which character contributes most to differentiation"),
                                 )),
                 )
               ),
               
               tags$div(
                 style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                 p(style = "margin: 0;", 
                   strong("Tip:"), "MFA is essential when combining different data types (e.g., measurements + counts + colors).",
                   "It prevents any single data type from dominating the analysis due to scale differences.")
               ),
               
               hr(),
               
               h4("General Tips"),
               tags$ul(
                 tags$li("Before performing any analyses, check data quality and identify outliers"),
                 tags$li("For morphometric data,", strong("allometric correction"), "should be performed before downstream analyses. Select 'No Correction' if your organism does not exhibit allometric growth"),
                 tags$li("Use", strong("Visualization"), "at every step to understand your data and results"),
                 tags$li("Statistical significance != taxonomic validity. Always integrate multiple lines of evidence from other sources of data. Morphology is only a single line of evidence"),
                 ),
               hr(),
      ),
      
      # Example Data Tab
      tabPanel("Example Data",
               br(),
               tags$style(HTML("
                 #example_meristic table, #example_meristic th, #example_meristic td,
                 #example_morphometric table, #example_morphometric th, #example_morphometric td,
                 #example_mixed table, #example_mixed th, #example_mixed td {
                   text-align: center !important;
                 }
               ")),
               h4("Example Data Formats"),
               p("Below are examples of properly formatted datasets for each data type.",
                 "Use these as templates when preparing your own data."),
               p(strong("Important formatting rules:"),
                 tags$ul(
                   tags$li("First column MUST be the grouping variable (e.g., species, population)"),
                   tags$li("For morphometric data: second column MUST be body-size measurement"),
                   tags$li("For mixed data with morphometric variables: second column MUST be body-size measurement"),
                   tags$li("All trait values must be numeric (except categorical variables in mixed data)"),
                   tags$li("No missing values allowed"),
                   tags$li("Minimum 2 samples per group (no singletons)")
                 )),
               
               hr(),
               
               h4("Meristic Data Example"),
               p("Count-based characters only. First column = species, remaining columns = count traits."),
               tableOutput("example_meristic"),
               
               hr(),
               
               h4("Morphometric Data Example"),
               p("Continuous measurements only. First column = species, second column = body size (SVL), remaining columns = morphometric traits."),
               tableOutput("example_morphometric"),
               
               hr(),
               
               h4("Mixed Data Example"),
               p("Combination of count and measurement data. First column = species, second column = body size (SVL), remaining columns = mixed traits."),
               tableOutput("example_mixed"),
               
               hr(),
               
               h4(strong("Ready to start?"), "Select a data type from the dropdown on the left and proceed to Input Data."),
               hr()
      )
    )
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
    h3("Meristic Data (numeric data only)"),
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
    h3("Morphometric Data (numeric data only)"),
    hr(),
    p("Morphometric data are continuous measurements such as lengths and widths. Non-morphometric data that can contribute to group structure such as continuous physiological data can also be analyzed in this module."),
    p(strong("Allometric adjustments should be performed to correct for body-size variation in the Allometric Correction module.")),
    p("The first column must contain Group/OTU names (e.g., species or population) and the second column must be the body-size measurement (e.g., snout-vent length)."),
    p("Each Group/OTU must be represented by more than one individual and missing data are NOT ALLOWED for allometric correction."),
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
    h3("Mixed Data (numeric, categorical)"),
    hr(),
    p("This module handles mixed data in a single dataset. The data can be a mixture of numerical (e.g., meristic + morphometric), numerical + categorical, or any combination of the two."),
    p("The first column must contain Group/OTU names (e.g., species or population). If morphometric data is included, the second column must be the body-size measurement (e.g., snout-vent length)."),
    p("The Allometric Correction module allows body-size correction to be performed on morphometric data only"),
    br(),
    h4("Proceed to Input Data")
  )
}

