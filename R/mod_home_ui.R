#' @importFrom shiny tagList NS h3 hr p br h4 strong h2 tabsetPanel tabPanel tags em HTML tableOutput
NULL

#' Sidebar Welcome Panel
#'
#' Links row displayed below the Return to Home button in the left sidebar.
#'
#' @return Shiny UI for the sidebar links block
#' @export
sidebar_welcome_ui <- function() {
  tags$div(
    style = "padding: 4px 16px 8px 16px;",
    tags$div(
      style = "font-size: 0.9em; color: #6c757d; line-height: 2;",
      tags$a("GitHub", href = "https://github.com/chankinonn/GroupStruct2", target = "_blank",
             style = "color: #337ab7; text-decoration: none;"),
      tags$span(" \u00b7 ", style = "color: #adb5bd;"),
      tags$a("Report Issue", href = "https://github.com/chankinonn/GroupStruct2/issues", target = "_blank",
             style = "color: #337ab7; text-decoration: none;"),
      tags$span(" \u00b7 ", style = "color: #adb5bd;"),
      tags$span("v1.3.1", style = "color: #337ab7;")
    )
  )
}

#' Sidebar Modules Header
#'
#' A styled "Modules" section header to insert in the sidebar between the
#' welcome block and the module navigation links.
#'
#' @return Shiny UI for the Modules section header
#' @export
sidebar_modules_header_ui <- function() {
  tags$div(
    style = "padding: 6px 16px 4px 16px;",
    tags$hr(style = "margin: 0 0 8px 0; border-color: #dee2e6;"),
    tags$span(
      style = "font-size: 0.7em; font-weight: 700; color: #6c757d;
               letter-spacing: 1.2px; text-transform: uppercase;",
      "Modules"
    )
  )
}

#' Landing UI
#'
#' @return Shiny UI for the landing page (main content area)
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
               
               hr(),
               
               h4("Quick Start"),
               p("Click", strong("Input Data"), "in the sidebar to upload your file.",
                 "Once uploaded, select which column contains your species/group labels,",
                 "optionally select a specimen ID column, choose your data type, and click Apply.",
                 "The app will route your data to the appropriate workflow automatically."),
               tags$ul(
                 tags$li(strong("Meristic:"), "Count-based characters (e.g., scale counts, fin rays, vertebrae)."),
                 tags$li(strong("Morphometric:"), "Continuous measurements (e.g., lengths, widths, heights)."),
                 tags$li(strong("Mixed:"), "Any combination of meristic, morphometric, environmental, categorical, or other numeric data.")
               ),
               hr(),
               
               h4("What's New"),
               p("GroupStruct2 is actively developed and updated. Run the command below to install the latest version and access new features and bug fixes:"),
               tags$code(style = "display: block; background: #e9ecef; padding: 8px 12px; border-radius: 4px; margin-bottom: 15px; font-size: 13px;",
                         'pak::pak("chankinonn/GroupStruct2")'),
               tags$div(
                 style = "background-color: #f8f9fa; border-left: 4px solid #6c757d; padding: 15px; margin: 15px 0;",
                 p(style = "margin: 0; margin-bottom: 8px;",
                   strong("v1.3.1"), "(June 2026)"),
                 tags$ul(style = "margin: 0;",
                         tags$li("Additional MFA-based analyses; a centralized data input module that accommodates more flexible data formatting; improved visualizations"),
                         tags$li("Added an Inferential Statistics submodule for Mixed Data")
                 ),
                 p(style = "margin-top: 10px; margin-bottom: 0;",
                   "Full changelog:",
                   tags$a("github.com/chankinonn/GroupStruct2/releases",
                          href = "https://github.com/chankinonn/GroupStruct2/releases",
                          target = "_blank"))
               ),
               
               hr(),
               
               h4("Resources and Feedback"),
               tags$ul(
                 tags$li("Source code and documentation:",
                         tags$a("github.com/chankinonn/GroupStruct2",
                                href = "https://github.com/chankinonn/GroupStruct2",
                                target = "_blank")),
                 tags$li("To report a bug or request a feature, open an issue at:",
                         tags$a("github.com/chankinonn/GroupStruct2/issues",
                                href = "https://github.com/chankinonn/GroupStruct2/issues",
                                target = "_blank")),
                 tags$li("General questions and suggestions are also welcome via the issues page.")
               ),
               
               hr(),
      ),
      
      # Available Analyses Tab
      tabPanel("Available Analyses",
               br(),
               p("GroupStruct2 provides the following analyses depending on your data type.",
                 "All modules are accessible from the sidebar after loading your data."),
               br(),
               
               # Summary Statistics
               tags$div(
                 style = "border-left: 5px solid #337ab7; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;", "Summary Statistics"),
                 tags$p(style = "margin: 0 0 6px 0;",
                        "Descriptive statistics and data summaries for rapid data inspection before analysis.",
                        "Available for all data types."),
                 tags$ul(style = "margin: 0;",
                         tags$li(strong("Per-group summaries:"), "Mean, standard deviation, minimum, maximum, and sample size for each trait and group"),
                         tags$li(strong("Data overview:"), "Variable-level summaries to flag potential data entry errors or outliers before proceeding to downstream analyses.")
                 )
               ),
               
               # Allometric Correction
               tags$div(
                 style = "border-left: 5px solid #5cb85c; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;", "Allometric Correction"),
                 tags$p(style = "margin: 0 0 6px 0;",
                        "Removes body-size variation from morphometric data using the Thorpe (1975) log-linear allometric method,",
                        "isolating shape variation for downstream analyses.",
                        "Available for morphometric and mixed data."),
                 tags$ul(style = "margin: 0;",
                         tags$li(strong("Body-size reference:"), "Select the trait column that serves as the size variable (typically total length or snout\u2013vent length)."),
                         tags$li(strong("Output:"), "Size-corrected trait values that can be used directly in all subsequent analyses.")
                 )
               ),
               
               # Inferential Statistics
               tags$div(
                 style = "border-left: 5px solid #f0ad4e; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;", "Inferential Statistics"),
                 tags$p(style = "margin: 0 0 6px 0;",
                        "A suite of univariate and multivariate tests for evaluating differences among groups",
                        "Available for all data types."),
                 tags$ul(style = "margin: 0;",
                         tags$li(strong("Univariate tests:"), "For each trait, the app automatically assesses normality (Shapiro-Wilk) and variance homogeneity (Levene) and selects the appropriate test. Two-group comparisons use Welch t-test (parametric) or Mann-Whitney U (non-parametric). Three or more groups use one-way ANOVA with Tukey HSD post-hoc (parametric, homogeneous variance) or Kruskal-Wallis with Dunn post-hoc corrections (non-parametric or heterogeneous variance)."),
                         tags$li(strong("PERMANOVA:"), "Non-parametric permutational MANOVA testing overall multivariate differences among groups."),
                         tags$li(strong("Multivariate dispersion:"), "Tests homogeneity of within-group spread using ", code("vegan::betadisper"), ", an important assumption check for PERMANOVA. Heterogeneous dispersion warrants caution when interpreting a significant PERMANOVA result."),
                         tags$li(strong("Pairwise centroid distances:"), "Euclidean distances between group centroids in multivariate trait space, providing a measure of morphological divergence."),
                         tags$li(strong("PCA:"), "Principal Component Analysis for dimensionality reduction and visualization of morphological variation."),
                         tags$li(strong("DAPC:"), "Discriminant Analysis of Principal Components for supervised ordination that maximizes between-group separation relative to within-group variation.")
                 )
               ),
               
               # Morphometric Delimitation
               tags$div(
                 style = "border-left: 5px solid #9b59b6; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;", "Morphometric Delimitation"),
                 tags$p(style = "margin: 0 0 6px 0;",
                        "A Bayesian model-comparison framework based on Gaussian Mixture Models (GMM) for evaluating taxonomic hypotheses directly from morphometric data.",
                        "Alternative grouping scenarios are compared using model-based Bayesian inference,",
                        "with hypotheses ranked by posterior probability and model fit.",
                        "Available for morphometric data only."),
                 tags$ul(style = "margin: 0;",
                         tags$li(strong("Unsupervised clustering:"), "Infer the optimal number of natural clusters in the data without relying on pre-defined group assignments, using Bayesian GMM model selection."),
                         tags$li(strong("Topology-aware hypothesis testing:"), "Evaluate monophyletic taxonomic hypotheses derived from a user-supplied reference phylogenetic tree. Each node in the tree defines a testable hypothesis."),
                         tags$li(strong("User-specified hypothesis testing:"), "Test any explicit user-defined taxonomic hypothesis by specifying custom groupings, allowing flexible comparison of alternative taxonomic arrangements."),
                         tags$li(strong("Diagnostic characters:"), "Identify the traits most important for distinguishing groups using Random Forest variable selection (Boruta algorithm), providing a statistically supported set of diagnostic characters.")
                 )
               ),
               
               # MFA and MFA Delimitation
               tags$div(
                 style = "border-left: 5px solid #e67e22; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;",
                        "Multiple Factor Analysis (MFA) and MFA-based Delimitation"),
                 tags$p(style = "margin: 0 0 6px 0;",
                        "MFA simultaneously ordinates multiple variable groups (meristic, morphometric, environmental, categorical),",
                        "normalizing the contribution of each group so that no single data type dominates the ordination.",
                        "By default, all groups contribute equally to the analysis.",
                        #"Optionally, custom weights can be assigned to individual groups to emphasize or de-emphasize their relative contribution.",
                        "Downstream analyses are applied to the resulting MFA factor scores.",
                        "Available for mixed data."),
                 tags$ul(style = "margin: 0;",
                         tags$li(strong("MFA ordination:"), "Dimension reduction that balances the contribution of each variable group, with individual factor maps and group/variable contribution plots."),
                         tags$li(strong("PERMANOVA and dispersion:"), "Non-parametric multivariate tests and betadisper analysis performed on MFA factor scores."),
                         tags$li(strong("Unsupervised clustering:"), "Bayesian GMM clustering on MFA factor scores to discover natural groupings without relying on pre-defined group assignments."),
                         tags$li(strong("Topology-aware hypothesis testing:"), "Evaluate monophyletic taxonomic hypotheses from a reference phylogenetic tree using Bayesian GMM on MFA factor scores."),
                         tags$li(strong("User-specified hypothesis testing:"), "Test any user-defined taxonomic hypothesis using Bayesian GMM on MFA factor scores, with flexible comparison of alternative taxonomic arrangements."),
                         tags$li(strong("DAMF:"), "Discriminant Analysis of Multiple Factors for supervised ordination maximizing between-group separation in the MFA factor space, analogous to DAPC for PCA scores."),
                         tags$li(strong("Diagnostic characters:"), "Boruta variable selection on MFA factor scores to identify the variable groups and traits most informative for group discrimination.")
                 )
               ),
               
               # Visualization
               tags$div(
                 style = "border-left: 5px solid #337ab7; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;", "Visualization"),
                 tags$p(style = "margin: 0 0 6px 0;",
                        "Highly customizable, publication-ready plots with various optional aesthetics. Available for all data types."),
                 tags$ul(style = "margin: 0;",
                         tags$li(strong("Exploratory plots:"), "Scatter, box, and violin plots for each trait."),
                         tags$li(strong("PCA, DAPC, and DAMF ordination plots:"), "Ordination plots with scree and biplots."),
                         tags$li(strong("MFA factor maps:"), "Individual and variable factor maps from MFA, including group/variable-level contributions."),
                         tags$li(strong("3D plots:"), "Three-dimensional PCA, DAPC, DAMF, and MFA individual factor maps for exploring variation across three axes simultaneously."),
                         tags$li(strong("Interactive mode:"), "Hover tooltips showing specimen ID, group, and axis values for all scatter, PCA, DAPC, DAMF, and MFA plots."),
                         tags$li(strong("Export:"), "Plots can be downloaded in PDF or JPEG format at user-specified dimensions.")
                 )
               ),
               
               hr()
      ),
      
      tabPanel("Example Datasets",
               br(),
               p("GroupStruct2 includes example datasets for learning and testing. Click the buttons in the",
                 strong("Input Data"), "module to load them."),
               br(),
               
               # Gekko Lizards
               tags$div(
                 style = "border-left: 5px solid #17a2b8; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;",
                        "Gekko Lizards"),
                 tags$p(style = "margin: 0 0 8px 0;",
                        strong("Species:"), em("Gekko albomaculatus"), ", ", em("G. albofasciolatus"), ", ", em("G. hulk"), ", ", em("G. smithii")),
                 tags$p(style = "margin: 0 0 8px 0;",
                        strong("Data structure:"), "15 morphometric (SVL, HH, HL, HW, IN, IO, TD, EE, NE, SE, OD, FL, CL, AG, TW)",
                        ", 11 meristic (SL, IS, IL, FS, CS, MB, PVT, LRT, VS, TL1, TL4), and 5 categorical variables (iris color, thin nuchal band, dark nuc band-eye, wht ocelli, drk on body)."),
                 tags$p(style = "margin: 0 0 8px 0;",
                        strong("Available subsets:"),
                        "Meristic, Morphometric, Mixed (Meristic + Morphometric), and Mixed (Meristic + Morphometric + Categorical)."),
                 tags$p(style = "margin: 0 0 8px 0;",
                        strong("Citation:")),
                 tags$p(style = "margin: 0 0 8px 0; font-size: 0.95em;",
                        "Grismer, L. L., del Pinto, L., Quah, E. S. H. H., Anuar, S., Cota, M. M., McGuire, J. A., Iskandar, D. T., Wood Jr, P. L., & Grismer, J. L. (2022). Phylogenetic and multivariate analyses of",
                        em("Gekko smithii"), "Gray, 1842 recover a new species from Peninsular Malaysia and support the resurrection of",
                        em("G. albomaculatus"), "(Giebel, 1861) from Sumatra.",
                        em("Vertebrate Zoology,"), "72, 47\u201380.",
                        tags$a("https://doi.org/10.3897/vz.72.e77702",
                               href = "https://doi.org/10.3897/vz.72.e77702", target = "_blank"))
               ),
               
               # Ctenophorus
               tags$div(
                 style = "border-left: 5px solid #28a745; background-color: #f8f9fa;
                          border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                 tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;",
                        "Australian Sand Dragons"),
                 tags$p(style = "margin: 0 0 8px 0;",
                        strong("Species:"), "11 species of the ", em("Ctenophorus maculatus"), " complex from Australia"),
                 tags$p(style = "margin: 0 0 8px 0;",
                        strong("Data structure:"), "Seven morphometric (SVL, AG, ArmL, FingerL, LegL, ToeL, TailLength), three meristic (Lamel4thF, Lamel4thT, Pores), eight categorical variables (CPE, CPPatt, TM, BGCol, PVL, PVSpots, FPVis, PoreArr), 19 WorldClim bioclimatic layers, one digital elevation model, four soil variables, and five vegetation variables."),
                 tags$p(style = "margin: 0 0 8px 0;",
                        strong("Citations:")),
                 tags$p(style = "margin: 0 0 8px 0; font-size: 0.95em;",
                        "Edwards, D. L., & Hutchinson, M. N. (2023). Sand Dragons: Species of the",
                        em("Ctenophorus maculatus"), "Complex (Squamata: Agamidae) of Australia's Southern and Western Interior.",
                        em("Journal of Herpetology,"), "57(2), 176\u2013196.",
                        tags$a("https://doi.org/10.1670/22-021",
                               href = "https://doi.org/10.1670/22-021", target = "_blank")),
                 tags$p(style = "margin: 0 0 8px 0; font-size: 0.95em;",
                        "Edwards, D. L., & Knowles, L. L. (2014). Species detection and individual assignment in species delimitation: can integrative data increase efficacy?",
                        em("Proceedings of the Royal Society B.,"), "281, 20132765.",
                        tags$a("https://doi.org/10.1098/rspb.2013.2765",
                               href = "https://doi.org/10.1098/rspb.2013.2765", target = "_blank")),
                 tags$p(style = "margin: 0 0 8px 0; font-size: 0.95em;",
                        "Edwards, D. L., Melville, J., Joseph, L., & Keogh, J. S. (2015). Ecological divergence, adaptive diversification, and the evolution of social signaling traits: An empirical study in arid Australian lizards.",
                        em("American Naturalist,"), "186(6), E144\u2013E161.",
                        tags$a("https://doi.org/10.1086/683658",
                               href = "https://doi.org/10.1086/683658", target = "_blank"))
               ),
               
               hr()
      ),
      
      tabPanel("Citations",
               fluidRow(
                 column(10, offset = 1,
                        br(),
                        p("If you use GroupStruct2 or any of its modules, please cite the relevant publications below."),
                        br(),
                        
                        # Main Application
                        tags$div(
                          style = "border-left: 5px solid #337ab7; background-color: #f8f9fa;
                       border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                          tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;",
                                 "Main Application"),
                          tags$p(style = "margin: 0;",
                                 "Chan, K. O., & Grismer, L. L. (2025). GroupStruct2: A user-friendly graphical",
                                 "user interface for statistical and visual support in species diagnosis.",
                                 em("Systematic Biology,"), "syaf090.",
                                 tags$a("https://doi.org/10.1093/sysbio/syaf090",
                                        href = "https://doi.org/10.1093/sysbio/syaf090", target = "_blank"))
                        ),
                        
                        # Allometric Correction
                        tags$div(
                          style = "border-left: 5px solid #5cb85c; background-color: #f8f9fa;
                       border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                          tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;",
                                 "Allometric Correction"),
                          tags$p(style = "margin: 0;",
                                 "Chan, K. O., & Grismer, L. L. (2022). GroupStruct: An R package for allometric",
                                 "size correction.", em("Zootaxa,"), "5124(4), 471\u2013482.",
                                 tags$a("https://doi.org/10.11646/zootaxa.5124.4.4",
                                        href = "https://doi.org/10.11646/zootaxa.5124.4.4", target = "_blank"))
                        ),
                        
                        # Morphometric Delimitation
                        tags$div(
                          style = "border-left: 5px solid #9b59b6; background-color: #f8f9fa;
                       border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                          tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;",
                                 "Morphometric Delimitation"),
                          tags$p(style = "margin: 0 0 8px 0;",
                                 "Chan, K. O., & Grismer, L. L. (2026). Extending GroupStruct2: a Bayesian and",
                                 "machine-learning framework for testing taxonomic hypotheses using morphometric data.",
                                 em("ZooKeys,"), "1276, 125\u2013138.",
                                 tags$a("https://doi.org/10.3897/zookeys.1276.182331",
                                        href = "https://doi.org/10.3897/zookeys.1276.182331", target = "_blank")),
                          tags$p(style = "margin: 0;",
                                 "Tiburtini, M., Scrucca, L., & Peruzzi, L. (2025). Using Gaussian Mixture Models",
                                 "in plant morphometrics.",
                                 em("Perspectives in Plant Ecology, Evolution, and Systematics,"), "69, 125902.",
                                 tags$a("https://doi.org/10.1016/j.ppees.2025.125902",
                                        href = "https://doi.org/10.1016/j.ppees.2025.125902", target = "_blank"))
                        ),
                        
                        # MFA
                        tags$div(
                          style = "border-left: 5px solid #e67e22; background-color: #f8f9fa;
                       border-radius: 0 4px 4px 0; padding: 14px 18px; margin-bottom: 16px;",
                          tags$p(style = "font-weight: bold; margin: 0 0 8px 0; font-size: 1.05em;",
                                 "Multiple Factor Analysis (MFA)"),
                          tags$p(style = "margin: 0;",
                                 "Grismer, L. L. (2025). Introducing multiple factor analysis (MFA) as a diagnostic",
                                 "taxonomic tool complementing principal component analysis (PCA).",
                                 em("ZooKeys,"), "1248, 93\u2013109.",
                                 tags$a("https://doi.org/10.3897/zookeys.1248.159516",
                                        href = "https://doi.org/10.3897/zookeys.1248.159516", target = "_blank"))
                        ),
                        
                        br()
                 )
               )
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
    h3("Meristic Data"),
    hr(),
    p("Meristic data are discrete, countable traits such as scale counts, fin rays, vertebrae number, etc."),
    p("All trait values must be numeric and missing values are NOT ALLOWED."),
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
    p("The body-size measurement can be in any trait column and is selected in the Allometric Correction module."),
    p("Each group must be represented by more than one individual and missing data are NOT ALLOWED for allometric correction."),
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
    h3("Mixed Data"),
    hr(),
    p("This module handles mixed data \u2014 any combination of meristic, morphometric, environmental, categorical, or other numeric data."),
    p("The Allometric Correction module allows body-size correction to be performed on morphometric data."),
    p("If morphometric data is included, the body-size measurement can be in any trait column and is selected in the Allometric Correction module."),
    br(),
    h4("Proceed to Input Data")
  )
}
