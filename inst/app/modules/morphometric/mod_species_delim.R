# Module UI
mod_species_delim_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Gaussian Mixture Models (GMM) for Morphometric Species Delimitation"),
    hr(),
    p("Traditional morphometric analyses use univariate/multivariate statistics",
      "to test the significance of trait differences between pre-defined OTUs.",
      "In contrast, the GMM approach uses unsupervised and supervised methods under a Bayesian framework,",
      "allowing for rigorous testing of competing taxonomic hypotheses."),
    
    tags$div(
      style = "background-color: #d4edda; border-left: 4px solid #28a745; padding: 15px; margin: 15px 0;",
      p(style = "margin: 0; margin-bottom: 10px;",
        strong("Citation:"),
        "These analyses were inspired and adapted from the original work of Tiburtini et al. (2025).",
        "If you use any of these methods, please cite:"),
      p(style = "margin: 0; font-style: italic;",
        "Tiburtini, M., Scrucca, L. & Peruzzi, L. (2025) Using Gaussian Mixture Models in plant morphometrics.",
        em("Perspectives in Plant Ecology, Evolution and Systematics"), ", 69:125902.")
    ),
    
    tags$div(
      style = "background-color: #fff3cd; border: 2px solid #dc3545; border-radius: 5px; padding: 15px; margin: 15px 0;",
      p(style = "color: #dc3545; margin: 0;",
        strong("⚠ IMPORTANT CAVEAT:"),
        "Statistical support for splitting OTUs does",
        strong("NOT"), "automatically indicate distinct species.",
        "Morphological structure may reflect population-level or geographic variation,",
        "rather than species boundaries.",
        strong("Other independent lines of evidence"), 
        "(e.g., genetics, ecology, geographic/reproductive isolation)",
        "that support lineage independence are needed to distinguish between population structure and species boundaries.",
        "Use these methods as a hypothesis-generating tool, or as additional support for other lines of evidence."
      )
    ),
    br(),
    
    tabsetPanel(
      id = ns("delim_tabs"),
      
      tabPanel("Unsupervised Clustering",
               fluidRow(
                 column(12,
                        h4("Unsupervised Model-Based Clustering"),
                        p("This analysis uses unsupervised Gaussian Mixture Models (GMM)", 
                          "to discover morphological clusters in the data", 
                          strong("without relying on pre-designated species groupings."), 
                          "The algorithm tests different numbers of clusters (G) and covariance structures,",
                          "selecting the best model using Bayesian Information Criterion (BIC)."),
                        hr(),
                        fluidRow(
                          column(3,
                                 numericInput(ns("max_clusters"), 
                                              label = strong("Maximum clusters (G):"),
                                              value = 4, min = 1, max = 10, step = 1)
                          ),
                          column(9,
                                 p(style = "margin-top: 25px;", 
                                   em("Note: G should represent the number of plausible clusters and typically should not exceed the number of geographic populations."))
                          )
                        ),
                        actionButton(ns("run_unsupervised"), 
                                     "Run Unsupervised Analysis", 
                                     icon = icon("play")),
                        hr(),
                        h4("Results"),
                        
                        # Add tabset for different result views
                        tabsetPanel(
                          id = ns("unsupervised_results_tabs"),
                          
                          tabPanel("Summary",
                                   br(),
                                   verbatimTextOutput(ns("cluster_summary")),
                                   hr(),
                                   h4("Cluster-Species Correspondence"),
                                   tableOutput(ns("cluster_species_table")),
                                   br(),
                                   h5("Interpretation Guide:"),
                                   p("The correspondence table shows how unsupervised clusters inferred from the data align with your OTUs.",
                                     "Each column represents a cluster discovered by the algorithm, and the rows show",
                                     "how many specimens from each OTU fall into that cluster."),
                                   tags$ul(
                                     tags$li(strong("Perfect correspondence:"), "Each cluster contains specimens from only one OTU,",
                                             "indicating strong morphological differentiation between OTUs"),
                                     tags$li(strong("Mixed clusters:"), "A cluster contains specimens from multiple OTUs,",
                                             "suggesting morphological overlap or similarity between those OTUs"),
                                     tags$li(strong("Split species:"), "One OTU is distributed across multiple clusters,",
                                             "indicating potential cryptic diversity or intraspecific variation that rivals interspecific differences.")
                                   )
                          ),
                          
                          tabPanel("Top Models",
                                   br(),
                                   h4("Model Comparison"),
                                   p("All models tested, ranked by BIC. Models with ∆BIC < 2 have substantial support,",
                                     "∆BIC 2-6 have moderate support, ∆BIC 6-10 have weak support, and ∆BIC > 10 have essentially no support."),
                                   DT::dataTableOutput(ns("top_models_table")),
                                   hr(),
                                   downloadButton(ns("download_top_models_csv"), "Download Top Models Table"),
                                   hr()
                          ),
                          
                          tabPanel("Model Details",
                                   br(),
                                   h4("Best Model Details"),
                                   verbatimTextOutput(ns("model_details")),
                                   hr(),
                                   h4("Covariance Model Interpretation"),
                                   p("The covariance structure determines how clusters can differ in volume, shape, and orientation:"),
                                   tags$ul(
                                     tags$li(strong("E (Equal)"), "= parameter is the same across all clusters"),
                                     tags$li(strong("V (Variable)"), "= parameter varies across clusters"),
                                     tags$li(strong("I (Identity)"), "= special constraint (spherical or diagonal)")
                                   ),
                                   p("Model code interpretation:"),
                                   tags$ul(
                                     tags$li(strong("EII"), "= Spherical, equal volume"),
                                     tags$li(strong("VII"), "= Spherical, unequal volume"),
                                     tags$li(strong("EEI"), "= Diagonal, equal volume and shape"),
                                     tags$li(strong("VEI"), "= Diagonal, varying volume, equal shape"),
                                     tags$li(strong("EVI"), "= Diagonal, equal volume, varying shape"),
                                     tags$li(strong("VVI"), "= Diagonal, varying volume and shape"),
                                     tags$li(strong("EEE"), "= Ellipsoidal, equal volume, shape, and orientation"),
                                     tags$li(strong("EVE"), "= Ellipsoidal, equal volume and orientation"),
                                     tags$li(strong("VEE"), "= Ellipsoidal, equal shape and orientation"),
                                     tags$li(strong("VVE"), "= Ellipsoidal, equal orientation"),
                                     tags$li(strong("EEV"), "= Ellipsoidal, equal volume and shape"),
                                     tags$li(strong("VEV"), "= Ellipsoidal, equal shape"),
                                     tags$li(strong("EVV"), "= Ellipsoidal, equal volume"),
                                     tags$li(strong("VVV"), "= Ellipsoidal, varying volume, shape, and orientation")
                                   )
                          )
                        ),
                        
                        hr(),
                        
                        tags$div(
                          style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0; margin-bottom: 10px;", 
                            strong("Understanding Unsupervised vs. Supervised Clustering:")),
                          p(style = "margin: 0;",
                            strong("Unsupervised Clustering"), "must solve two problems simultaneously:",
                            tags$ul(style = "margin: 5px 0;",
                                    tags$li(strong("How many groups?"), "- Testing different numbers of clusters (G = 1, 2, 3, ...)"),
                                    tags$li(strong("What structure?"), "- Testing different covariance patterns (spherical, ellipsoidal, etc.)")
                            ),
                            "Without knowing the 'true' groups in advance, the algorithm must infer both cluster membership",
                            "and model structure from the data alone. This double uncertainty often favors simpler models."
                          ),
                          br(),
                          p(style = "margin: 0;",
                            strong("Supervised Clustering"), "uses your pre-defined OTU labels, so it only needs to:",
                            tags$ul(style = "margin: 5px 0;",
                                    tags$li("Evaluate which grouping scheme (lumping/splitting) best fits the data"),
                                    tags$li("The group membership is known, reducing uncertainty")
                            ),
                            "By using labeled data, discriminant analysis can detect structure that unsupervised methods miss.",
                            "This is why supervised analyses often reveal differentiation even when unsupervised finds no clusters."
                          )
                        ),
                        
                        hr(),
                        
                        p(strong("Note:"), "For visualizations of these patterns, see the",
                          strong("Visualization > Morphometric Delimitation"), "tab for BIC plots and PCA cluster plots."),
                        hr(),
                        downloadButton(ns("download_unsupervised_csv"), "Download Correspondence Table"),
                        hr()
                 )
               )
      ),
      
      tabPanel("Supervised Clustering",
               fluidRow(
                 column(12,
                        h4("Supervised Clustering Based on Pre-defined OTU Groupings"),
                        p("This analysis uses supervised Gaussian Mixture Models (GMM)", 
                          "to evaluate competing taxonomic hypotheses through a", 
                          strong("stepwise merging algorithm."), 
                          "Starting with the original OTU groupings, the algorithm iteratively tests all possible",
                          strong("pairwise"), "mergings at each step,",
                          "computing Bayesian Information Criterion (BIC) and Bayes Factors for each scenario.",
                          "The best-supported merge is selected, and the process repeats with the reduced set of groups",
                          "until all taxa are lumped into a single cluster."),
                        
                        p(strong("Method:"), 
                          "At each iteration, the algorithm generates all possible pairs of taxa for merging",
                          "using the EDDA (Eigenvalue Decomposition Discriminant Analysis) model",
                          "and selects the merge with the highest BIC.",
                          "This continues until only one taxon remains, generating a hierarchical sequence",
                          "of increasingly lumped delimitation schemes ranked by BIC.",
                          "Higher BIC indicates better fit; Bayes Factors quantify strength of evidence between competing models."),
                        
                        p(strong("Important:"), 
                          "This is a", strong("greedy stepwise search"), "that follows a single optimal merging path,",
                          "not an exhaustive test of all possible partitions.",
                          "For comprehensive hypothesis testing of specific delimitation schemes,",
                          "use the", strong("Hypothesis Testing"), "tab where you can define and compare",
                          "all taxonomic hypotheses of interest.",
                          "For best results, assign the most plausible splitty grouping scheme to your original uploaded data."),
                        
                        tags$div(
                          style = "background-color: #fff3cd; border-left: 4px solid #856404; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0;", 
                            strong("⚠ Performance Warning:"),
                            "Datasets with >10 initial OTUs may take several minutes to hours to complete.",
                            "For complex scenarios, consider using", strong("Bayesian Species Delimitation"), 
                            "to test specific hypotheses instead of exhaustive search.")
                        ),
                        hr(),
                        actionButton(ns("run_supervised"), 
                                     "Run Supervised Analysis", 
                                     icon = icon("play")),
                        hr(),
                        h4("Bayes Factor Comparison"),
                        DT::dataTableOutput(ns("bayes_factor_table")),
                        hr(),
                        
                        tags$div(
                          style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0;",
                            strong("Key Difference from Unsupervised:"),
                            "This analysis uses your", strong("pre-assigned OTU labels"), "to fit discriminant models.",
                            "Because group membership is known, discriminant analysis can detect morphological structure",
                            "that unsupervised clustering might miss. Supervised clustering often achieves higher BIC values",
                            "than unsupervised methods because it doesn't need to simultaneously infer both group and structure.",
                            "If unsupervised found G=1 but supervised supports K≥2, this indicates",
                            strong("weak but real differentiation"), "detectable when groups are defined.")
                        ),
                        hr(),
                        uiOutput(ns("supervised_interpretation")),  
                        hr(),
                        downloadButton(ns("download_supervised_csv"), "Download CSV"),
                        downloadButton(ns("download_supervised_xlsx"), "Download Excel"),
                        hr()
                 )
               )
      ),
      
      tabPanel("Bayesian Hypothesis Testing",
               fluidRow(
                 column(12,
                        h4("Bayesian Hypothesis Testing Using Gaussian Mixture Models"),
                        
                        p("This analysis evaluates competing taxonomic hypotheses using Bayesian model comparison.",
                          "You can test hypotheses using either", strong("raw morphometric data"), 
                          "or", strong("PCA scores.")),
                        
                        tags$div(
                          style = "background-color: #fff3cd; border-left: 4px solid #856404; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0; margin-bottom: 10px;",
                            strong("Why EDDA Instead of Classical DA?")),
                          p(style = "margin: 0; margin-bottom: 8px;",
                            strong("Classical Discriminant Analysis (DA/LDA)"), 
                            "assumes all groups have the", strong("same covariance structure"), 
                            "(homoscedasticity) - a single pooled covariance matrix across all groups."),
                            
                          p(style = "margin: 0;",
                            strong("EDDA (Eigenvalue Decomposition Discriminant Analysis)"), 
                            "allows each group to have its", strong("own covariance structure"), 
                            "(heteroscedasticity), accommodating biological reality where taxonomic groups",
                            "differ in their amount and pattern of variation.",
                            "EDDA is more robust because: (1) classical DA is a special case of EDDA",
                            "(equal covariances), so if groups truly have equal variance, EDDA will detect this",
                            "and favor the simpler model via BIC; (2) if covariances differ, DA gives biased results",
                            "while EDDA remains accurate; (3) EDDA naturally integrates with Bayesian model comparison",
                            "via BIC.")
                        ),
                        
                        tags$div(
                          style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0; margin-bottom: 10px;",
                            strong("Two Analysis Options:")),
                          tags$ul(style = "margin: 5px 0;",
                                  tags$li(strong("Raw Morphometric Data (default):"), 
                                          "EDDA applied directly to trait measurements. Provides direct biological interpretation.",
                                          "Best when n > p (more specimens than traits) and you want to understand which specific traits drive delimitation."),
                                  tags$li(strong("PCA Scores:"), 
                                          "EDDA applied to principal component scores.",
                                          "More robust when p ≈ n (high-dimensional data) and ensures methodological",
                                          "consistency with the MFA-based approach (used for mixed data types).")
                          )
                        ),
                        
                        tags$div(
                          style = "background-color: #e7f3ff; border-left: 4px solid #2196F3; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0;",
                            icon("info-circle"), strong(" Visualization:"),
                            "After running this analysis, you can visualize how different hypotheses",
                            "group specimens in PCA space by going to",
                            strong("Visualization > PCA"), "tab.",
                            "A 'Grouping Selection' dropdown will appear, allowing you to toggle between",
                            "different taxonomic hypotheses. The PCA coordinates remain constant - only",
                            "the grouping/coloring changes based on the selected hypothesis.")
                        ),
                        
                        p(strong("Method:"), 
                          "The analysis uses", code("modelType = 'EDDA'"), 
                          "with G=1 (one Gaussian component per group), which assumes each taxonomic group",
                          "can be described by a single multivariate Gaussian distribution.",
                          "BIC and Bayes Factors quantify support for competing hypotheses."),
                        
                        p(strong("Priors:"),
                          "The analysis uses flat (uninformative) priors, treating all hypotheses as equally likely",
                          "before examining the data. Posterior probabilities are then calculated based solely on",
                          "how well each hypothesis fits the morphometric data."),
                        
                        hr(),
                        
                        h4("Upload Hypothesis File"),
                        p("Upload a CSV file with your taxonomic hypotheses.",
                          strong("Important:"), "The order of rows in this file", 
                          strong("must match exactly"), "the order of specimens in your original morphometric input data."),
                        
                        p("File format: Each column represents one hypothesis to be tested.",
                          "Column names are hypothesis names (e.g., HYP_1, HYP_2, Lump_AB, Split_A, etc.)."),
                        p(strong("DO NOT include morphometric data in the hypothesis file.")),
                        
                        p(strong("Example:")),
                        
                        # Table showing example format
                        div(style = "overflow-x: auto; margin-bottom: 20px;",
                            tags$table(class = "table table-bordered table-condensed", 
                                       style = "width: auto; font-size: 12px;",
                                       tags$thead(
                                         tags$tr(
                                           tags$th("Original OTU"),
                                           tags$th("1 species"),
                                           tags$th("Lump A_B"),
                                           tags$th("Lump B_C"),
                                           tags$th("Split A")
                                         )
                                       ),
                                       tags$tbody(
                                         tags$tr(tags$td("A"), tags$td("A"), tags$td("A+B"), tags$td("A"), tags$td("A1")),
                                         tags$tr(tags$td("A"), tags$td("A"), tags$td("A+B"), tags$td("A"), tags$td("A1")),
                                         tags$tr(tags$td("A"), tags$td("A"), tags$td("A+B"), tags$td("A"), tags$td("A2")),
                                         tags$tr(tags$td("A"), tags$td("A"), tags$td("A+B"), tags$td("A"), tags$td("A2")),
                                         tags$tr(tags$td("B"), tags$td("A"), tags$td("A+B"), tags$td("B+C"), tags$td("B")),
                                         tags$tr(tags$td("B"), tags$td("A"), tags$td("A+B"), tags$td("B+C"), tags$td("B")),
                                         tags$tr(tags$td("B"), tags$td("A"), tags$td("A+B"), tags$td("B+C"), tags$td("B")),
                                         tags$tr(tags$td("B"), tags$td("A"), tags$td("A+B"), tags$td("B+C"), tags$td("B")),
                                         tags$tr(tags$td("C"), tags$td("A"), tags$td("C"), tags$td("B+C"), tags$td("C")),
                                         tags$tr(tags$td("C"), tags$td("A"), tags$td("C"), tags$td("B+C"), tags$td("C")),
                                         tags$tr(tags$td("C"), tags$td("A"), tags$td("C"), tags$td("B+C"), tags$td("C")),
                                         tags$tr(tags$td("C"), tags$td("A"), tags$td("C"), tags$td("B+C"), tags$td("C"))
                                       )
                            )
                        ),
                        
                        fileInput(ns("hyp_file"), 
                                  "Choose CSV File", 
                                  accept = ".csv"),
                        
                        actionButton(ns("load_example_hyp"), 
                                     "Load Example Hypothesis File",
                                     icon = icon("table")),
                        br(), 
                        p(style = "color: #856404; font-style: italic; margin-top: 10px;",
                          strong("Note:"), 
                          "The example hypothesis file is designed to work with the example morphometric dataset.",
                          "Please load the example morphometric dataset from the 'Input Data' module before loading this hypothesis file."),
                        
                        br(),
                        
                        uiOutput(ns("hyp_upload_status")),
                        
                        conditionalPanel(
                          condition = sprintf("output['%s']", ns("hyp_file_loaded")),
                          h5("Preview of Loaded Hypotheses:"),
                          DTOutput(ns("hyp_preview"))
                        ),
                        
                        hr(),
                        
                        h4("Analysis Options"),
                        
                        checkboxInput(ns("use_pca_scores"), 
                                      "Use PCA scores instead of raw data", 
                                      value = FALSE),
                        
                        conditionalPanel(
                          condition = sprintf("input['%s']", ns("use_pca_scores")),
                          
                          h5("PC Selection Method:"),
                          radioButtons(ns("pc_selection_method"),
                                       NULL,
                                       choices = c(
                                         "Cumulative Variance Threshold" = "variance",
                                         "Fixed Number of PCs" = "fixed",
                                         "All PCs" = "all"
                                       ),
                                       selected = "variance"),
                          
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'variance'", ns("pc_selection_method")),
                            sliderInput(ns("pca_variance_threshold"),
                                        "Variance to retain (%):",
                                        min = 70, max = 100, value = 90, step = 5),
                            p(style = "color: #666; font-style: italic; font-size: 0.9em;",
                              "Minimum number of PCs needed to explain this percentage of variance will be used.")
                          ),
                          
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'fixed'", ns("pc_selection_method")),
                            numericInput(ns("pca_n_components"),
                                         "Number of PCs to use:",
                                         value = 5, min = 2, max = 30, step = 1),
                            p(style = "color: #666; font-style: italic; font-size: 0.9em;",
                              "Manually specify how many principal components to use.")
                          ),
                          
                          conditionalPanel(
                            condition = sprintf("input['%s'] == 'all'", ns("pc_selection_method")),
                            p(style = "color: #666; font-style: italic;",
                              "All available PCs will be used (no dimensionality reduction).")
                          ),
                          
                          tags$div(
                            style = "background-color: #f8f9fa; border-left: 3px solid #6c757d; padding: 8px; margin: 10px 0;",
                            p(style = "margin: 0; font-size: 0.9em;",
                              strong("Recommendation:"), 
                              "Use variance threshold (90%) as default. Use fixed number if you have",
                              "a priori reasons. Use all PCs only if your sample size is much larger than",
                              "the number of traits.")
                          )
                        ),
                        
                        hr(),
                        
                        actionButton(ns("run_bayesian"), 
                                     "Run Bayesian Delimitation", 
                                     icon = icon("play"),
                                     style = "background-color: white; color: black;"),
                        
                        hr(),
                        
                        h4("Results"),
                        DT::dataTableOutput(ns("bayesian_results_table")),
                        hr(),
                        uiOutput(ns("bayesian_interpretation")),
                        hr(),
                        downloadButton(ns("download_bayesian_csv"), "Download CSV"),
                        downloadButton(ns("download_bayesian_xlsx"), "Download Excel"),
                        hr(),
                 )
               )
      ),
      
      tabPanel("Diagnostic Characters (Machine Learning)",
               fluidRow(
                 column(12,
                        h3("Diagnostic Character Identification using Machine Learning"),
                        p("This analysis is based on the Boruta feature selection algorithim that uses Random Forest to identify",
                          "morphometric variables that are statistically important for distinguishing between",
                          "taxonomic groups. It compares the importance of real variables against randomly",
                          "permuted 'shadow' variables to determine which features are truly informative."),
                        
                        p(strong("How it works:"),
                          "The algorithm iteratively removes variables that perform worse than the best",
                          "shadow variable (rejected), keeps those that consistently perform better (confirmed),",
                          "and marks uncertain variables as tentative. This results in a robust set of",
                          "diagnostic characters that can reliably separate your groups."),
                        
                        p(strong("What is 'Importance'?"),
                          "Importance measures how much a variable contributes to correctly classifying specimens into groups.",
                          "The Random Forest algorithm calculates importance by randomly shuffling each variable's values",
                          "and measuring how much classification accuracy drops.",
                          "Variables that cause large accuracy drops when shuffled are considered important because they contain",
                          "information that helps distinguish groups.",
                          "The Boruta algorithm compares each real variable's importance to randomized 'shadow' variables:",
                          tags$ul(
                            tags$li("Variables consistently more important than the best shadow → ", strong("Confirmed")),
                            tags$li("Variables less important than the best shadow → ", strong("Rejected")),
                            tags$li("Variables with uncertain importance → ", strong("Tentative"))
                          )),
                        
                        p(strong("Interpretation:"),
                          tags$ul(
                            tags$li(strong("Confirmed:"), "Variables that are significantly more important",
                                    "than random noise (shadow variables). These are your diagnostic characters—traits that reliably distinguish your groups."),
                            tags$li(strong("Tentative:"), "Variables where the algorithm couldn't reach",
                                    "a confident decision (may need more iterations with higher maxRuns)."),
                            tags$li(strong("Rejected:"), "Variables that don't contribute meaningful",
                                    "information for distinguishing groups—their importance is no better than random.")
                          )),
                        
                        tags$div(
                          style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                          p(style = "margin: 0;", 
                            strong("Key Point:"), "Importance is", em("relative"), "—it tells you which variables",
                            "are most useful for distinguishing groups compared to random noise.",
                            "It doesn't tell you", em("how different"), "groups are (use univariate tests for that),",
                            "but rather", em("which traits best discriminate"), "between groups.",
                            "Note that this is NOT a Bayesian approach. The algorithm uses machine learning to measure importance, then uses p-values to test the significance of that importance. ")
                        ),
                        
                        p("If you've run Bayesian Species Delimitation, you can select a hypothesis",
                          "to test which variables are diagnostic for that particular taxonomic scheme.",
                          "Otherwise, the analysis uses your original species labels."),
                        
                        hr(),
                        
                        h4("Step 1: Select Hypothesis"),
                        uiOutput(ns("boruta_hypothesis_selector")),
                        
                        hr(),
                        
                        h4("Step 2: Select Groups to Compare"),
                        p("Select at least 2 groups to compare. More groups = more comprehensive analysis."),
                        uiOutput(ns("boruta_group_selector")),
                        
                        hr(),
                        
                        h4("Step 3: Set Parameters"),
                        fluidRow(
                          column(3,
                                 numericInput(ns("boruta_maxruns"),
                                              "Maximum Iterations:",
                                              value = 1000,
                                              min = 100,
                                              max = 5000,
                                              step = 100)
                          ),
                          column(3,
                                 numericInput(ns("boruta_pvalue"),
                                              "P-value threshold:",
                                              value = 0.01,
                                              min = 0.001,
                                              max = 0.1,
                                              step = 0.01)
                          ),
                          column(3,
                                 numericInput(ns("boruta_seed"),
                                              "Random seed:",
                                              value = 123,
                                              min = 1,
                                              max = 10000)
                          ),
                          column(3,
                                 p(style = "margin-top: 25px;",
                                   em("Higher maxRuns = more reliable results but slower"))
                          )
                        ),
                        
                        hr(),
                        
                        actionButton(ns("run_boruta"),
                                     "Run Boruta Analysis",
                                     icon = icon("play"),
                                     style = "background-color: white; color: black;"),
                        
                        hr(),
                        
                        h4("Results"),
                        
                        h5("Variable Importance Results:"),
                        p("All variables ranked by mean importance, showing decision status (Confirmed/Tentative/Rejected)"),
                        DT::dataTableOutput(ns("boruta_confirmed_table")),
                        
                        br(),
                        
                        h5("Descriptive Statistics (Confirmed Variables Only):"),
                        p("Mean ± SD [min-max] for each confirmed diagnostic variable by group"),
                        DT::dataTableOutput(ns("boruta_descriptive_table")),
                        
                        br(),
                        
                        p(strong("Note:"), "For visualizations of variable importance, see the",
                          strong("Visualization > Species Delimitation > Boruta Importance"), "tabs."),
                        
                        hr(),
                        
                        downloadButton(ns("download_boruta_confirmed_csv"), "Download Descriptive Stats (CSV)"),
                        downloadButton(ns("download_boruta_confirmed_xlsx"), "Download Descriptive Stats (Excel)"),
                        hr(),
                 )
               )
      )
    )
  )
}

mod_species_delim_server <- function(id, dataset_r) {
  moduleServer(id, function(input, output, session) {
    
    
    # Reactive values to store results
    unsupervised_results <- reactiveVal(NULL)
    supervised_results <- reactiveVal(NULL)
    hypothesis_data <- reactiveVal(NULL)
    bayesian_results <- reactiveVal(NULL)
    boruta_results <- reactiveVal(NULL)
    
    ## Boruta analysis
    
    # Hypothesis selector for Boruta
    output$boruta_hypothesis_selector <- renderUI({
      # Check if Bayesian results exist
      if (!is.null(bayesian_results()) && !is.null(hypothesis_data())) {
        hyp_names <- colnames(hypothesis_data())
        
        selectInput(session$ns("boruta_hypothesis"),
                    "Select Hypothesis:",
                    choices = c("Original Species" = "original", hyp_names),
                    selected = "original")
      } else {
        p(em("No Bayesian results available. Using original species labels."))
      }
    })
    
    # Group selector for Boruta
    output$boruta_group_selector <- renderUI({
      req(dataset_r())
      
      data <- dataset_r()
      
      # Determine which grouping to use
      if (!is.null(input$boruta_hypothesis) && input$boruta_hypothesis != "original" && 
          !is.null(hypothesis_data())) {
        groups <- unique(hypothesis_data()[[input$boruta_hypothesis]])
      } else {
        # Use first column (original species)
        groups <- unique(data[[1]])
      }
      
      groups <- sort(groups)
      
      checkboxGroupInput(session$ns("boruta_groups"),
                         "Groups to compare:",
                         choices = groups,
                         selected = groups,
                         inline = FALSE)
    })
    
    # Run Boruta analysis
    observeEvent(input$run_boruta, {
      
      # Check if data is loaded
      if (is.null(dataset_r()) || nrow(dataset_r()) == 0) {
        showModal(modalDialog(
          title = "No Data Loaded",
          "Please load data in the 'Input Data' module before running Boruta analysis.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      # Check if at least 2 groups selected
      if (is.null(input$boruta_groups) || length(input$boruta_groups) < 2) {
        showNotification("Please select at least 2 groups to compare", type = "error")
        return()
      }
      
      withProgress(message = 'Running Boruta analysis...', value = 0, {
        
        data <- dataset_r()
        
        # Get morphometric data (exclude first column = species)
        morpho_data <- data[, -1, drop = FALSE]
        
        # Ensure all columns are numeric
        if (!all(sapply(morpho_data, is.numeric))) {
          non_numeric <- names(morpho_data)[!sapply(morpho_data, is.numeric)]
          showNotification(paste("Error: Non-numeric columns found:", paste(non_numeric, collapse = ", ")), 
                           type = "error")
          return(NULL)
        }
        
        # Determine grouping variable
        if (!is.null(input$boruta_hypothesis) && input$boruta_hypothesis != "original" && 
            !is.null(hypothesis_data())) {
          group_labels <- hypothesis_data()[[input$boruta_hypothesis]]
        } else {
          group_labels <- data[[1]]
        }
        
        # Filter to selected groups only
        selected_indices <- group_labels %in% input$boruta_groups
        morpho_data_filtered <- morpho_data[selected_indices, , drop = FALSE]
        group_labels_filtered <- factor(group_labels[selected_indices])
        
        # Check if we have enough data
        if (nrow(morpho_data_filtered) < 10) {
          showNotification("Not enough specimens in selected groups (minimum 10 required)", type = "error")
          return()
        }
        
        incProgress(0.2, detail = "Preparing data...")
        
        # Combine into data frame for Boruta
        boruta_data <- morpho_data_filtered
        boruta_data$Group <- group_labels_filtered
        
        incProgress(0.3, detail = "Running Boruta algorithm...")
        
        tryCatch({
          # Set seed for reproducibility
          set.seed(input$boruta_seed)
          
          # Run Boruta
          boruta_result <- Boruta::Boruta(
            Group ~ .,
            data = boruta_data,
            doTrace = 0,
            maxRuns = input$boruta_maxruns,
            pValue = input$boruta_pvalue
          )
          
          incProgress(0.3, detail = "Processing results...")
          
          # Extract final decisions
          final_decision <- tibble::tibble(
            Variable = names(boruta_result$finalDecision),
            Decision = as.character(boruta_result$finalDecision),
            Mean_Importance = apply(boruta_result$ImpHistory[, names(boruta_result$finalDecision)], 
                                    2, mean, na.rm = TRUE)
          ) %>%
            dplyr::arrange(desc(Mean_Importance))  # Sort all by importance
          
          # Get only confirmed variables for descriptive stats
          confirmed_vars <- final_decision %>%
            dplyr::filter(Decision == "Confirmed")
          
          # Calculate descriptive statistics for confirmed variables only
          if (nrow(confirmed_vars) > 0) {
            confirmed_var_names <- confirmed_vars$Variable
            
            # Create descriptive stats
            descriptive_stats <- boruta_data %>%
              dplyr::select(Group, all_of(confirmed_var_names)) %>%
              dplyr::group_by(Group) %>%
              dplyr::summarise(
                dplyr::across(
                  dplyr::where(is.numeric),
                  ~ paste0(sprintf("%.2f", mean(.x, na.rm = TRUE)), 
                           " ± ", 
                           sprintf("%.2f", sd(.x, na.rm = TRUE)),
                           " [",
                           sprintf("%.2f", min(.x, na.rm = TRUE)),
                           "-",
                           sprintf("%.2f", max(.x, na.rm = TRUE)),
                           "]"),
                  .names = "{.col}"
                )
              ) %>%
              dplyr::ungroup()
          } else {
            descriptive_stats <- NULL
            showNotification("No confirmed variables found. Try increasing maxRuns or adjusting pValue.", 
                             type = "warning")
          }
          
          incProgress(0.2, detail = "Finalizing...")
          
          # Store results
          boruta_results(list(
            boruta_object = boruta_result,
            all_decisions = final_decision,  # ALL variables with decisions
            confirmed = confirmed_vars,       # Only confirmed
            descriptive = descriptive_stats,  # Stats for confirmed only
            groups_used = input$boruta_groups,
            hypothesis_used = input$boruta_hypothesis %||% "original"
          ))
          
          showNotification("Boruta analysis complete!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })
    
    # Display boruta results
    output$boruta_confirmed_table <- DT::renderDataTable({
      req(boruta_results())
      req(boruta_results()$all_decisions)
      
      all_vars <- boruta_results()$all_decisions
      
      # Round mean importance
      all_vars$Mean_Importance <- round(all_vars$Mean_Importance, 3)
      
      DT::datatable(
        all_vars,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
      )
    })
    
    # Display descriptive statistics table (confirmed only)
    output$boruta_descriptive_table <- DT::renderDataTable({
      req(boruta_results())
      req(boruta_results()$descriptive)
      
      DT::datatable(
        boruta_results()$descriptive,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
      )
    })
    
    # Download confirmed variables CSV
    output$download_boruta_confirmed_csv <- downloadHandler(
      filename = function() {
        paste0("boruta_descriptive_stats_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(boruta_results())
        req(boruta_results()$descriptive)
        write.csv(boruta_results()$descriptive, file, row.names = FALSE)
      }
    )
    
    # Download descriptive stats Excel (renamed from confirmed)
    output$download_boruta_confirmed_xlsx <- downloadHandler(
      filename = function() {
        paste0("boruta_descriptive_stats_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(boruta_results())
        req(boruta_results()$descriptive)
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Descriptive Statistics")
        openxlsx::writeData(wb, "Descriptive Statistics", boruta_results()$descriptive)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    
    ## Bayesian species delimitation
    # Flag for conditional panel
    output$hyp_file_loaded <- reactive({
      !is.null(hypothesis_data())
    })
    outputOptions(output, "hyp_file_loaded", suspendWhenHidden = FALSE)
    
    # Load hypothesis file
    observeEvent(input$hyp_file, {
      req(input$hyp_file)
      
      tryCatch({
        hyp_df <- read.csv(input$hyp_file$datapath, stringsAsFactors = FALSE)
        
        # Validate file has at least one column
        if (ncol(hyp_df) < 1) {
          output$hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     "Error: File must have at least 1 hypothesis column")
          })
          hypothesis_data(NULL)
          return()
        }
        
        # Check that columns are not purely numeric (to prevent uploading morphometric data)
        all_numeric_cols <- sapply(hyp_df, function(col) {
          # Check if all non-NA values are numeric
          all(grepl("^-?[0-9]*\\.?[0-9]+$", col[!is.na(col)]))
        })
        
        if (any(all_numeric_cols)) {
          problematic_cols <- names(hyp_df)[all_numeric_cols]
          output$hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: The following columns contain only numeric values: ",
                            paste(problematic_cols, collapse = ", "),
                            ". Hypothesis labels should be categorical (e.g., 'Species_A', 'Lump_AB', 'DEL', 'MACRO').",
                            " It appears you may have uploaded morphometric data instead of hypothesis labels."))
          })
          hypothesis_data(NULL)
          return()
        }
        
        # Check if number of rows matches input data
        req(dataset_r())
        if (nrow(hyp_df) != nrow(dataset_r())) {
          output$hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: Hypothesis file has ", nrow(hyp_df), 
                            " rows but input data has ", nrow(dataset_r()), 
                            " rows. They must match exactly."))
          })
          hypothesis_data(NULL)
          return()
        }
        
        # Store hypothesis data
        hypothesis_data(hyp_df)
        
        output$hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-success",
                   paste0("✓ Hypothesis file loaded successfully. Found ", 
                          ncol(hyp_df), " hypotheses: ", 
                          paste(colnames(hyp_df), collapse = ", ")))
        })
        
      }, error = function(e) {
        output$hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste0("Error loading file: ", e$message))
        })
        hypothesis_data(NULL)
      })
    })
    
    # Load example hypothesis file
    observeEvent(input$load_example_hyp, {
      
      # Check if morphometric data is loaded first
      if (is.null(dataset_r()) || nrow(dataset_r()) == 0) {
        showModal(modalDialog(
          title = "No Data Loaded",
          "Please load morphometric data in the 'Input Data' module before loading hypothesis file.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      example_path <- system.file("examples", "Gekko_hypothesis_file.csv", package = "GroupStruct2")
      
      if (!file.exists(example_path)) {
        output$hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   "Error: Example hypothesis file not found in package")
        })
        return()
      }
      
      tryCatch({
        hyp_df <- read.csv(example_path, stringsAsFactors = FALSE)
        
        # Validate file has at least one column
        if (ncol(hyp_df) < 1) {
          output$hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     "Error: File must have at least 1 hypothesis column")
          })
          hypothesis_data(NULL)
          return()
        }
        
        # Check that columns are not purely numeric (to prevent uploading morphometric data)
        all_numeric_cols <- sapply(hyp_df, function(col) {
          all(grepl("^-?[0-9]*\\.?[0-9]+$", col[!is.na(col)]))
        })
        
        if (any(all_numeric_cols)) {
          problematic_cols <- names(hyp_df)[all_numeric_cols]
          output$hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: The following columns contain only numeric values: ",
                            paste(problematic_cols, collapse = ", "),
                            ". Hypothesis labels should be categorical."))
          })
          hypothesis_data(NULL)
          return()
        }
        
        # Check if number of rows matches input data
        if (nrow(hyp_df) != nrow(dataset_r())) {
          output$hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-warning",
                     paste0("Warning: Example hypothesis file has ", nrow(hyp_df), 
                            " rows but your current data has ", nrow(dataset_r()), 
                            " rows. Please load the corresponding example morphometric dataset first.",
                            " This example hypothesis file is designed to work with 'Morphometric-only.csv'."))
          })
          hypothesis_data(NULL)
          return()
        }
        
        # Store hypothesis data
        hypothesis_data(hyp_df)
        
        output$hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-success",
                   paste0("✓ Example hypothesis file loaded successfully. Found ", 
                          ncol(hyp_df), " hypotheses: ", 
                          paste(colnames(hyp_df), collapse = ", ")))
        })
        
      }, error = function(e) {
        output$hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste0("Error loading example file: ", e$message))
        })
        hypothesis_data(NULL)
      })
    })
    
    # Preview hypothesis data
    output$hyp_preview <- renderDT({
      req(hypothesis_data())
      
      DT::datatable(
        hypothesis_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tip'
        ),
        rownames = FALSE
      )
    })
    
    # Run Bayesian analysis
    observeEvent(input$run_bayesian, {
      
      # Check if data is loaded
      if (is.null(dataset_r()) || nrow(dataset_r()) == 0) {
        showModal(modalDialog(
          title = "No Data Loaded",
          "Please load data in the 'Input Data' module before running Bayesian delimitation.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      # Check if hypotheses are loaded
      if (is.null(hypothesis_data())) {
        showNotification("Please upload a hypothesis file", type = "error")
        return()
      }
      
      withProgress(message = 'Running Bayesian delimitation...', value = 0, {
        
        data <- dataset_r()
        hyp_df <- hypothesis_data()
        
        # Get all hypothesis names (all columns are hypotheses)
        hyp_names <- colnames(hyp_df)
        
        # Set flat priors (0.2 for all hypotheses)
        priors <- rep(0.2, length(hyp_names))
        names(priors) <- hyp_names
        
        # Extract morphometric data (exclude first column = species)
        morpho_data <- data[, -1, drop = FALSE]
        
        # Ensure all columns are numeric
        if (!all(sapply(morpho_data, is.numeric))) {
          non_numeric <- names(morpho_data)[!sapply(morpho_data, is.numeric)]
          showNotification(paste("Error: Non-numeric columns found:", paste(non_numeric, collapse = ", ")), 
                           type = "error")
          return(NULL)
        }
        
        if (ncol(morpho_data) == 0) {
          showNotification("No morphometric columns found in data", type = "error")
          return(NULL)
        }
        
        # Determine if using PCA or raw data
        use_pca <- isTRUE(input$use_pca_scores)
        
        if (use_pca) {
          incProgress(0.1, detail = "Performing PCA...")
          
          # Run PCA
          pca_res <- prcomp(morpho_data, scale. = TRUE, center = TRUE)
          
          # Calculate cumulative variance for all PCs
          cumvar <- cumsum(pca_res$sdev^2 / sum(pca_res$sdev^2) * 100)
          total_pcs <- length(cumvar)
          
          # Determine number of PCs based on selection method
          pc_method <- input$pc_selection_method
          
          if (pc_method == "variance") {
            # Variance threshold method
            n_pcs <- which(cumvar >= input$pca_variance_threshold)[1]
            if (is.na(n_pcs)) {
              n_pcs <- total_pcs  # Use all if threshold not reached
            }
            method_msg <- sprintf("Using %d PCs (%.1f%% variance)", n_pcs, cumvar[n_pcs])
            
          } else if (pc_method == "fixed") {
            # Fixed number method
            n_pcs <- min(input$pca_n_components, total_pcs)
            method_msg <- sprintf("Using %d PCs (%.1f%% variance)", n_pcs, cumvar[n_pcs])
            
          } else if (pc_method == "all") {
            # Use all PCs
            n_pcs <- total_pcs
            method_msg <- sprintf("Using all %d PCs (100%% variance)", n_pcs)
            
          } else {
            # Default fallback
            n_pcs <- which(cumvar >= 90)[1]
            if (is.na(n_pcs)) n_pcs <- total_pcs
            method_msg <- sprintf("Using %d PCs (%.1f%% variance)", n_pcs, cumvar[n_pcs])
          }
          
          # Use PC scores for analysis
          data_for_edda <- pca_res$x[, 1:n_pcs, drop = FALSE]
          
          showNotification(method_msg, type = "message", duration = 5)
          
          incProgress(0.1, detail = paste("Fitting EDDA models on", n_pcs, "PCs..."))
        } else {
          # Use raw morphometric data
          data_for_edda <- as.matrix(morpho_data)
          incProgress(0.2, detail = "Fitting EDDA models on raw data...")
        }
        
        # Fit EDDA models for each hypothesis
        tryCatch({
          models <- list()
          
          for (i in seq_along(hyp_names)) {
            hyp_name <- hyp_names[i]
            
            # Get class labels for this hypothesis
            class_labels <- hyp_df[[hyp_name]]
            
            incProgress(0.6 / length(hyp_names), 
                        detail = paste("Fitting", hyp_name))
            
            # Fit EDDA model (G=1, different covariance per class)
            models[[hyp_name]] <- mclust::MclustDA(
              data_for_edda,
              factor(class_labels),
              modelType = "EDDA",
              verbose = FALSE
            )
          }
          
          incProgress(0.2, detail = "Computing Bayes Factors...")
          
          # Compute Bayes Factors with flat priors using custom function
          results_table <- do.call(GMMBayesFactorTable, 
                                   c(models, list(prior = priors)))
          
          # Sort by BIC (descending - best model first)
          results_table <- results_table[order(-results_table$BIC), ]
          
          bayesian_results(results_table)
          
          showNotification("Bayesian delimitation complete!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })
    
    # Display Bayesian results table
    output$bayesian_results_table <- DT::renderDataTable({
      req(bayesian_results())
      
      DT::datatable(
        bayesian_results(),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
      )
    })
    
    # Bayesian interpretation guide
    output$bayesian_interpretation <- renderUI({
      req(bayesian_results())
      
      tagList(
        h5("Interpretation Guide:"),
        tags$ul(
          tags$li(strong("BIC (Bayesian Information Criterion):"),
                  "Higher values indicate better model fit (note: this follows the mclust convention where BIC = 2·log-likelihood - penalty). The best hypothesis has the highest BIC (∆BIC = 0)."),
          tags$li(strong("∆BIC:"),
                  "Difference from the best model.",
                  tags$ul(
                    tags$li("∆BIC < 2: Weak evidence against the hypothesis"),
                    tags$li("∆BIC 2-6: Positive evidence against"),
                    tags$li("∆BIC 6-10: Strong evidence against"),
                    tags$li("∆BIC > 10: Very strong evidence against")
                  )),
          tags$li(strong("Bayes Factor (BF):"),
                  "Strength of evidence against each hypothesis relative to the best (BF = exp(∆BIC/2)).",
                  tags$ul(
                    tags$li("BF = 1: Equally supported as best hypothesis"),
                    tags$li("BF > 3: Positive evidence against"),
                    tags$li("BF > 20: Strong evidence against"),
                    tags$li("BF > 150: Very strong evidence against")
                  )),
          tags$li(strong("PostMod (Posterior Model Probability):"),
                  "Probability each hypothesis is correct, given the data and flat priors (0.2 for each).",
                  "The hypothesis with highest PostMod is most supported by the data.",
                  tags$ul(
                    tags$li("PostMod > 0.75: Strong support"),
                    tags$li("PostMod 0.50-0.75: Moderate support"),
                    tags$li("PostMod < 0.50: Weak support")
                  ))
        ),
        p(strong("Note:"), "If no single hypothesis strongly dominates (PostMod < 0.75),",
          "examine whether similar hypotheses collectively have high probability,",
          "which may indicate consistent patterns in the data.")
      )
    })
    
    # Download CSV
    output$download_bayesian_csv <- downloadHandler(
      filename = function() {
        paste0("bayesian_delimitation_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(bayesian_results())
        write.csv(bayesian_results(), file, row.names = FALSE)
      }
    )
    
    # Download Excel
    output$download_bayesian_xlsx <- downloadHandler(
      filename = function() {
        paste0("bayesian_delimitation_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(bayesian_results())
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Bayesian Results")
        openxlsx::writeData(wb, "Bayesian Results", bayesian_results())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Run unsupervised clustering
    observeEvent(input$run_unsupervised, {
      
      if (is.null(dataset_r()) || nrow(dataset_r()) == 0) {
        showModal(modalDialog(
          title = "No Data Loaded",
          "Please load data in the 'Input Data' module before running species delimitation analysis.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      withProgress(message = 'Running unsupervised clustering...', value = 0, {
        
        tryCatch({
          data <- dataset_r()
          
          # Identify morphometric columns (numeric, exclude first column which is grouping)
          morpho_cols <- sapply(data[-1], is.numeric)
          morpho_data <- data[, c(FALSE, morpho_cols), drop = FALSE]
          
          # Check if we have data
          if (ncol(morpho_data) == 0) {
            showNotification("No morphometric columns found in data", type = "error")
            return(NULL)
          }
          
          incProgress(0.2, detail = "Fitting GMM models...")
          
          # Fit Mclust
          data_mod <- mclust::Mclust(morpho_data, G = 1:input$max_clusters)
          
          if (is.null(data_mod)) {
            showNotification("Mclust failed to fit models. Check your data for issues.", type = "error")
            return(NULL)
          }
          
          incProgress(0.2, detail = "Computing model comparisons...")
          
          # Extract BIC for all models
          if (is.null(data_mod$BIC)) {
            showNotification("No BIC values available from Mclust. Check data quality.", type = "error")
            return(NULL)
          }
          
          # Convert BIC to long format manually (more robust)
          bic_matrix <- data_mod$BIC
          n_G <- nrow(bic_matrix)
          model_names <- colnames(bic_matrix)
          
          # Create long format manually
          bic_list <- list()
          for (i in 1:n_G) {
            for (j in 1:length(model_names)) {
              bic_value <- bic_matrix[i, j]
              if (!is.na(bic_value)) {
                bic_list[[length(bic_list) + 1]] <- data.frame(
                  G = as.numeric(rownames(bic_matrix)[i]),
                  Model = model_names[j],
                  BIC = bic_value
                )
              }
            }
          }
          
          # Combine all into one dataframe
          bic_long <- dplyr::bind_rows(bic_list) %>%
            dplyr::arrange(desc(BIC))
          
          if (nrow(bic_long) == 0) {
            showNotification("No valid BIC values after filtering", type = "error")
            return(NULL)
          }
          
          # Calculate delta BIC from best model
          best_bic <- max(bic_long$BIC, na.rm = TRUE)
          bic_long <- bic_long %>%
            dplyr::mutate(
              Delta_BIC = best_bic - BIC,
              Rank = row_number()
            )
          
          # Get top models (top 20 or all within delta BIC < 10)
          top_models <- bic_long %>%
            dplyr::filter(Rank <= 20 | Delta_BIC < 10) %>%
            dplyr::select(Rank, G, Model, BIC, Delta_BIC)
          
          incProgress(0.2, detail = "Creating cluster-species table...")
          
          # Create cluster-species correspondence table
          species_col <- data[[1]]
          
          if (is.null(data_mod$classification)) {
            showNotification("No classification available from Mclust", type = "error")
            return(NULL)
          }
          
          cluster_table <- table(Species = species_col, 
                                 Cluster = data_mod$classification)
          
          incProgress(0.2, detail = "Finalizing results...")
          
          # Store results
          results <- list(
            model = data_mod,
            cluster_table = cluster_table,
            species_col = species_col,
            morpho_data = morpho_data,
            top_models = top_models,
            all_bic = bic_long
          )
          
          unsupervised_results(results)
          
          showNotification("Unsupervised clustering complete!", type = "message")
          
        }, error = function(e) {
          # Show the actual error message
          showNotification(paste("Error in unsupervised clustering:", e$message), 
                           type = "error", 
                           duration = 10)
          # Also print to console for debugging
          message("Full error in unsupervised clustering:")
          print(e)
        })
      })
    })
    
    # Display top models table
    output$top_models_table <- DT::renderDataTable({
      req(unsupervised_results())
      
      results <- unsupervised_results()
      req(results$top_models)
      
      DT::datatable(
        results$top_models,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'tip'
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("BIC", "Delta_BIC"), digits = 2) %>%
        DT::formatStyle(
          'Delta_BIC',
          backgroundColor = DT::styleInterval(
            cuts = c(2, 6, 10),
            values = c('#90EE90', '#FFFFE0', '#FFE4B5', '#FFB6C1')
          )
        )
    })
    
    # Display detailed model summary
    output$model_details <- renderPrint({
      req(unsupervised_results())
      
      results <- unsupervised_results()
      model <- results$model
      
      cat("═══════════════════════════════════════════════════════\n")
      cat("           BEST MODEL DETAILED SUMMARY\n")
      cat("═══════════════════════════════════════════════════════\n\n")
      
      cat("Model Selection:\n")
      cat("  Model Type:    ", model$modelName, "\n", sep = "")
      cat("  # Clusters:    ", model$G, "\n", sep = "")
      cat("  BIC:           ", round(model$bic, 2), "\n", sep = "")
      cat("  Log-likelihood:", round(model$loglik, 2), "\n", sep = "")
      cat("  # Parameters:  ", model$df, "\n", sep = "")
      cat("  # Observations:", model$n, "\n\n", sep = "")
      
      cat("Classification:\n")
      cat("  Cluster sizes:\n")
      sizes <- table(model$classification)
      for (i in seq_along(sizes)) {
        cat(sprintf("    Cluster %d: %d specimens\n", i, sizes[i]))
      }
      cat("\n")
      
      cat("Model Interpretation:\n")
      interpretation <- get_model_interpretation(model$modelName)
      cat("  ", model$modelName, " = ", interpretation, "\n\n", sep = "")
      
      # Show top 5 alternative models
      if (!is.null(results$top_models)) {
        top_5 <- results$top_models[1:min(5, nrow(results$top_models)), ]
        cat("Top 5 Models by BIC:\n")
        print(top_5, row.names = FALSE)
      }
    })
    
    # Download top models
    output$download_top_models_csv <- downloadHandler(
      filename = function() {
        paste0("unsupervised_top_models_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(unsupervised_results())
        req(unsupervised_results()$top_models)
        write.csv(unsupervised_results()$top_models, file, row.names = FALSE)
      }
    )
    
    # Run supervised GMM
    observeEvent(input$run_supervised, {
      
      # Check if data is loaded
      if (is.null(dataset_r()) || nrow(dataset_r()) == 0) {
        showModal(modalDialog(
          title = "No Data Loaded",
          "Please load data in the 'Input Data' module before running species delimitation analysis.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      withProgress(message = 'Running supervised GMM analysis...', value = 0, {
        
        data <- dataset_r()
        
        # Identify morphometric columns (numeric, exclude first column which is grouping)
        morpho_cols <- sapply(data[-1], is.numeric)
        morpho_data <- data[, c(FALSE, morpho_cols), drop = FALSE]
        
        # Check if we have data
        if (ncol(morpho_data) == 0) {
          showNotification("No morphometric columns found in data", type = "error")
          return(NULL)
        }
        
        # Get species labels
        species_col <- data[[1]]
        
        # Check minimum number of species
        if (length(unique(species_col)) < 2) {
          showNotification("Error: Need at least 2 species/groups for supervised analysis", type = "error")
          return(NULL)
        }
        
        incProgress(0.3, detail = "Running Bayes Factor analysis...")
        
        # Run MclustBayesFactorClassMerge
        tryCatch({
          cl_merge <- MclustBayesFactorClassMerge(
            morpho_data,
            class = species_col,
            modelType = "EDDA"
          )
          
          incProgress(0.4, detail = "Summarizing results...")
          
          # Create summary table
          summary_table <- summarize_class_merge(cl_merge, digits = 3)
          
          incProgress(0.3, detail = "Finalizing...")
          
          # Store results
          supervised_results(summary_table)
          
          showNotification("Supervised GMM analysis complete!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })
    
    # Display cluster summary
    output$cluster_summary <- renderPrint({
      req(unsupervised_results())
      
      results <- unsupervised_results()
      model <- results$model
      
      cat("═══════════════════════════════════════════════════════\n")
      cat("        UNSUPERVISED CLUSTERING RESULTS\n")
      cat("═══════════════════════════════════════════════════════\n\n")
      
      cat("Best Model:\n")
      cat("  Model Type: ", model$modelName, "\n", sep = "")
      cat("  Optimal G:  ", model$G, " clusters\n", sep = "")
      cat("  BIC:        ", round(model$bic, 2), "\n\n", sep = "")
      
      cat("Cluster Sizes:\n")
      print(table(model$classification))
      
    })
    
    # Display cluster-species table
    output$cluster_species_table <- renderTable({
      req(unsupervised_results())
      
      results <- unsupervised_results()
      as.data.frame.matrix(results$cluster_table)
    }, rownames = TRUE, striped = TRUE, hover = TRUE)
    
    # Display Bayes Factor table
    output$bayes_factor_table <- DT::renderDataTable({
      req(supervised_results())
      
      DT::datatable(
        supervised_results(),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
      )
    })
    
    
    # Display interpretation guide
    output$supervised_interpretation <- renderUI({
      req(supervised_results())
      
      tagList(
        h4("Interpretation Guide:"),
        p(strong("BIC (Bayesian Information Criterion):"), 
          "Higher BIC values indicate better model fit. The model with the highest BIC is considered the best-supported delimitation scheme (note: this follows the mclust convention where BIC = 2·log-likelihood - penalty). K = number of clusters"),
        p(strong("∆BIC (BIC difference):"), 
          "Difference from the best model. The best model has ∆BIC = 0.",
          tags$ul(
            tags$li("∆BIC < 2: Weak evidence against this model"),
            tags$li("∆BIC 2-6: Positive evidence against this model"),
            tags$li("∆BIC 6-10: Strong evidence against this model"),
            tags$li("∆BIC > 10: Very strong evidence against this model")
          )
        ),
        p(strong("Bayes Factor (BF):"), 
          "BF = exp(∆BIC/2). Quantifies strength of evidence against each model compared to the best model.",
          tags$ul(
            tags$li("BF = 1: Equally supported as best model"),
            tags$li("BF > 3: Positive evidence against this model"),
            tags$li("BF > 20: Strong evidence against this model"),
            tags$li("BF > 150: Very strong evidence against this model")
          )
        ),
        
        p(strong("PostMod (Posterior Model Probability):"), 
          "Probability that each delimitation scheme is correct, assuming all schemes are equally likely a priori.",
          "The scheme with the highest PostMod is the most probable given the data.")
      )
    })
    
    # Download CSV
    output$download_unsupervised_csv <- downloadHandler(
      filename = function() {
        paste0("unsupervised_clustering_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(unsupervised_results())
        
        results <- unsupervised_results()
        model <- results$model
        
        # Create summary data frame
        summary_df <- data.frame(
          Metric = c("Model Type", "Optimal G", "BIC", "Log-likelihood", "DF"),
          Value = c(
            model$modelName,
            as.character(model$G),
            round(model$bic, 3),
            round(model$loglik, 3),
            as.character(model$df)
          )
        )
        
        # Cluster sizes
        cluster_sizes <- as.data.frame(table(model$classification))
        names(cluster_sizes) <- c("Cluster", "Count")
        
        # Cluster-species table
        cluster_species <- as.data.frame.matrix(results$cluster_table)
        cluster_species <- cbind(Cluster = rownames(cluster_species), cluster_species)
        
        # Write to CSV (multiple tables)
        write.csv(summary_df, file, row.names = FALSE)
        write.table("\n", file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table("Cluster Sizes:", file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table(cluster_sizes, file, append = TRUE, row.names = FALSE, sep = ",")
        write.table("\n", file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table("Cluster-Species Correspondence:", file, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.csv(cluster_species, file, append = TRUE, row.names = FALSE)
      }
    )
    
    
    # Download supervised CSV
    output$download_supervised_csv <- downloadHandler(
      filename = function() {
        paste0("supervised_gmm_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(supervised_results())
        write.csv(supervised_results(), file, row.names = FALSE)
      }
    )
    
    # Download supervised Excel
    output$download_supervised_xlsx <- downloadHandler(
      filename = function() {
        paste0("supervised_gmm_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(supervised_results())
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Bayes Factor Comparison")
        openxlsx::writeData(wb, "Bayes Factor Comparison", supervised_results())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Placeholder for other tabs
    output$boruta_plot <- renderPlot({
      plot(1, 1, main = "Click 'Identify Diagnostic Characters' to generate plot")
    })
    
    output$boruta_table <- DT::renderDataTable({
      data.frame(Message = "Click 'Identify Diagnostic Characters' to see results")
    })
    
    # Return results for visualization module
    return(reactive({
      list(
        unsupervised = unsupervised_results(),
        supervised = supervised_results(),
        bayesian = bayesian_results(),
        hypothesis_data = hypothesis_data(),  
        boruta = boruta_results()
      )
    }))
    
  })
}