# Module UI
mod_species_delim_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Gaussian Mixture Models (GMM) for Morphometric Species Delimitation"),
    hr(),
    p("Traditional morphometric analyses use univariate/multivariate statistics",
      "to test the significance of trait differences between pre-defined groups",
      "In contrast, the GMM approach uses Eigenvalue Decomposition Discriminant Analysis (EDDA)",
      "to test competing taxonomic hypotheses under a Bayesian framework."),
    
    tags$div(
      style = "background-color: #fff3cd; border: 2px solid #dc3545; border-radius: 5px; padding: 15px; margin: 15px 0;",
      p(style = "color: #dc3545; margin: 0;",
        strong("⚠ IMPORTANT CAVEAT:"),
        "Morphometric delimitation is",
        strong("NOT THE SAME"), "as species delimitation.", 
        "Analyses are purely based on morphometric data and statistical support for splitting groups does",
        strong("NOT"), "necessarily indicate distinct species.",
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
                          "to discover natural morphological clusters in the data", 
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
                                     icon = icon("play"),
                                     class = "btn-primary"),
                        hr(),
                        h4("Results"),
                        
                        # Add tabset for different result views
                        tabsetPanel(
                          id = ns("unsupervised_results_tabs"),
                          
                          tabPanel("Summary",
                                   br(),
                                   verbatimTextOutput(ns("cluster_summary")),
                                   hr(),
                                   h4("Per-specimen Cluster Assignment"),
                                   p("Specimen ID, original OTU, assigned cluster, and posterior probability of belonging to each cluster."),
                                   DT::dataTableOutput(ns("cluster_specimen_table")),
                                   br(),
                                   downloadButton(ns("download_unsupervised_csv"), "Download Table (CSV)"),
                                   hr(),
                                   h5("Interpretation Guide:"),
                                   p("The table shows cluster assignments and posterior probabilities for each specimen.",
                                     "Each column represents a cluster discovered by the algorithm, and the rows show",
                                     "how many specimens from each OTU fall into that cluster."),
                                   tags$ul(
                                     tags$li(strong("Perfect correspondence:"), "Each cluster contains specimens from only one OTU,",
                                             "indicating strong morphological differentiation between OTUs"),
                                     tags$li(strong("Mixed clusters:"), "A cluster contains specimens from multiple OTUs,",
                                             "suggesting morphological overlap or similarity between those OTUs"),
                                     tags$li(strong("Split species:"), "One OTU is distributed across multiple clusters,",
                                             "indicating potential cryptic diversity or intraspecific variation that rivals interspecific differences."),
                                     tags$li(strong("Posterior probabilities:"), "Values close to 1 indicate high confidence in cluster assignment.",
                                             "Values close to 0.5 indicate a specimen is morphologically intermediate between clusters.")
                                   )
                          ),
                          hr(),
                          tabPanel("Top Models",
                                   br(),
                                   h4("Model Comparison"),
                                   p("All models tested, ranked by BIC. Models with \u0394BIC < 2 have substantial support,",
                                     "\u0394BIC 2-6 have moderate support, \u0394BIC 6-10 have weak support, and \u0394BIC > 10 have essentially no support."),
                                   DT::dataTableOutput(ns("top_models_table")),
                                   hr(),
                                   downloadButton(ns("download_top_models_csv"), "Download Top Models Table")
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
                        )
                        
                 )
               )
      ),
      
      tabPanel("Topology-aware Hypothesis Testing",
               fluidRow(
                 column(12,
                        h4("Topology-aware Hypothesis Testing"),
                        p("This analysis generates and tests monophyletic hypotheses consistent with a",
                          "reference tree topology. Every hypothesis corresponds to a biologically valid",
                          "partition \u2014 one where each group is a clade on the tree.",
                          "Models are compared simultaneously using EDDA and ranked by BIC and Bayes Factors."),
                        p(strong("Method:"),
                          "Given a tree topology, monophyletic partitions are enumerated by testing all valid",
                          "combinations of non-nested internal nodes. For tractability, enumeration is capped",
                          "at 20,000 combinations. Each is fit with EDDA directly on the morphometric data and ranked by BIC."),
                        hr(),
                        
                        h5("\u276f Step 1: Upload Tree"),
                        tags$div(
                          class = "alert alert-info",
                          style = "margin-bottom: 10px;",
                          tags$b("Accepted formats:"),
                          tags$ul(style = "margin: 6px 0 0 0;",
                                  tags$li(tags$b("Newick"), " \u2014 IQ-TREE (.treefile, .contree), RAxML (.tre), or any plain Newick file."),
                                  tags$li(tags$b("NEXUS"), " \u2014 MrBayes consensus tree (.con.tre) or BEAST maximum clade credibility tree (.tree, .trees)."),
                                  tags$li("Tips can be", tags$b("OTU names"), " (direct match) or", tags$b("individual specimen IDs"),
                                          " \u2014 if tips don't match OTU names, a specimen-to-OTU mapping step will appear.")
                          )
                        ),
                        
                        fileInput(ns("morpho_phylo_newick_file"),
                                  "Choose tree file:",
                                  accept = c(".nwk", ".newick", ".tre", ".tree", ".txt",
                                             ".treefile", ".contree", ".con.tre",
                                             ".nexus", ".nex", ".xml"),
                                  placeholder = "No file selected"),
                        p("Or load the example tree (works with the morphometric example dataset):"),
                        actionButton(ns("load_morpho_example_tree"), "Load Example Tree",
                                     icon = icon("tree"), class = "btn-default btn-sm"),
                        uiOutput(ns("morpho_phylo_tree_status_ui")),
                        uiOutput(ns("morpho_phylo_tip_assign_ui")),
                        uiOutput(ns("morpho_phylo_tree_preview_ui")),
                        hr(),
                        
                        h5("\u276f Step 2: Run Analysis"),
                        actionButton(ns("run_morpho_phylo"),
                                     "Run Topology-aware Hypothesis Testing",
                                     icon = icon("play"),
                                     class = "btn-primary"),
                        hr(),
                        
                        h4("Results"),
                        p(em("Results sorted by BIC. Top 10 hypotheses shown; download for full results.")),
                        DT::dataTableOutput(ns("morpho_phylo_results_table")),
                        hr(),
                        downloadButton(ns("download_morpho_phylo_csv"),  "Download CSV"),
                        downloadButton(ns("download_morpho_phylo_xlsx"), "Download Excel"),
                        hr(),
                        uiOutput(ns("morpho_phylo_interpretation")),
                        hr()
                 )
               )
      ),
      
      tabPanel("User-specified Hypothesis Testing",
               fluidRow(
                 column(12,
                        h4("User-specified Hypothesis Testing"),
                        p("This analysis tests explicit user-defined taxonomic hypotheses.",
                          "Each hypothesis defines how OTUs should be grouped (lumped or split).",
                          "Models are compared simultaneously using EDDA and ranked by BIC and Bayes Factors."),
                        p(strong("Method:"),
                          "Each hypothesis is fit with EDDA directly on the morphometric data.",
                          "Flat priors are applied across all hypotheses.",
                          "Results are ranked by BIC; Bayes Factors quantify evidence between competing models."),
                        hr(),
                        
                        h4("Upload Hypothesis File"),
                        p("Upload a CSV file where each column is one hypothesis to be tested.",
                          "Column names are hypothesis labels (e.g., HYP_1, Lump_AB).",
                          strong("Row order must match exactly the order of specimens in your morphometric data."),
                          strong("Do not include morphometric data in this file.")),
                        
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
                        
                        actionButton(ns("run_bayesian"), 
                                     "Run User-specified Hypothesis Testing", 
                                     icon = icon("play"),
                                     class = "btn-primary"),
                        
                        hr(),
                        h4("Results"),
                        DT::dataTableOutput(ns("bayesian_results_table")),
                        hr(),
                        downloadButton(ns("download_bayesian_csv"), "Download CSV"),
                        downloadButton(ns("download_bayesian_xlsx"), "Download Excel"),
                        hr(),
                        uiOutput(ns("bayesian_interpretation")),
                        hr(),
                 )
               )
      ),
      
      tabPanel("Diagnostic Characters (Machine Learning)",
               fluidRow(
                 column(12,
                        h4("Diagnostic Character Identification using Machine Learning"),
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
                        
                        p("If you've run the User-specified Hypothesis Testing analysis, you can select a hypothesis",
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
                                     class = "btn-primary"),
                        
                        hr(),
                        
                        h4("Results"),
                        
                        h5("Variable Importance Results:"),
                        p("All variables ranked by mean importance, showing decision status (Confirmed/Tentative/Rejected)"),
                        DT::dataTableOutput(ns("boruta_confirmed_table")),
                        
                        br(),
                        
                        downloadButton(ns("download_boruta_importance_csv"), "Download Importance Table (CSV)"),
                        downloadButton(ns("download_boruta_importance_xlsx"), "Download Importance Table (Excel)"),
                        
                        br(), br(),
                        
                        
                        h5("Descriptive Statistics (Confirmed Variables Only):"),
                        p("Mean ± SD [min-max] for each confirmed diagnostic variable by group"),
                        DT::dataTableOutput(ns("boruta_descriptive_table")),
                        
                        br(),
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
    ns <- session$ns
    
    
    # Reactive values to store results
    unsupervised_results <- reactiveVal(NULL)
    supervised_results <- reactiveVal(NULL)
    hypothesis_data <- reactiveVal(NULL)
    bayesian_results <- reactiveVal(NULL)
    bayesian_models  <- reactiveVal(NULL)
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
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Boruta analysis is running — this may take several minutes depending on dataset size and maxRuns..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
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
      
      # Combine into data frame for Boruta
      boruta_data <- morpho_data_filtered
      boruta_data$Group <- group_labels_filtered
      
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
        
        # Extract final decisions
        final_decision <- tibble::tibble(
          Variable = names(boruta_result$finalDecision),
          Decision = as.character(boruta_result$finalDecision),
          Mean_Importance = apply(boruta_result$ImpHistory[, names(boruta_result$finalDecision)],
                                  2, mean, na.rm = TRUE)
        ) %>%
          dplyr::arrange(desc(Mean_Importance))
        
        # Get only confirmed variables for descriptive stats
        confirmed_vars <- final_decision %>%
          dplyr::filter(Decision == "Confirmed")
        
        # Calculate descriptive statistics for confirmed variables only
        if (nrow(confirmed_vars) > 0) {
          confirmed_var_names <- confirmed_vars$Variable
          
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
        
        # Store results
        boruta_results(list(
          boruta_object = boruta_result,
          all_decisions = final_decision,
          confirmed = confirmed_vars,
          descriptive = descriptive_stats,
          groups_used = input$boruta_groups,
          hypothesis_used = input$boruta_hypothesis %||% "original"
        ))
        
        showNotification("Boruta analysis complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
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
    
    # Download importance table CSV (all variables)
    output$download_boruta_importance_csv <- downloadHandler(
      filename = function() {
        paste0("boruta_importance_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(boruta_results())
        req(boruta_results()$all_decisions)
        tbl <- boruta_results()$all_decisions
        tbl$Mean_Importance <- round(tbl$Mean_Importance, 3)
        write.csv(tbl, file, row.names = FALSE)
      }
    )
    
    # Download importance table Excel (all variables)
    output$download_boruta_importance_xlsx <- downloadHandler(
      filename = function() {
        paste0("boruta_importance_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(boruta_results())
        req(boruta_results()$all_decisions)
        tbl <- boruta_results()$all_decisions
        tbl$Mean_Importance <- round(tbl$Mean_Importance, 3)
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Variable Importance")
        openxlsx::writeData(wb, "Variable Importance", tbl)
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
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Fitting EDDA models for each hypothesis — this may take several minutes..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      data    <- dataset_r()
      hyp_df  <- hypothesis_data()
      
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
      
      # Fit EDDA models for each hypothesis
      tryCatch({
        models <- list()
        
        for (i in seq_along(hyp_names)) {
          hyp_name     <- hyp_names[i]
          class_labels <- hyp_df[[hyp_name]]
          
          models[[hyp_name]] <- mclust::MclustDA(
            as.matrix(morpho_data),
            factor(class_labels),
            modelType = "EDDA",
            verbose   = FALSE
          )
        }
        
        # Compute Bayes Factors with flat priors
        results_table <- do.call(GMMBayesFactorTable,
                                 c(models, list(prior = priors)))
        
        # Sort by BIC (descending — best model first)
        results_table <- results_table[order(-results_table$BIC), ]
        
        bayesian_results(results_table)
        bayesian_models(list(
          models      = models,
          morpho_data = as.matrix(morpho_data),
          species_col = data[[1]],
          hyp_df      = hyp_df
        ))
        
        showNotification("Bayesian delimitation complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
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
          "which may indicate consistent patterns in the data."),
        hr()
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
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Fitting GMM models — this may take a moment depending on dataset size and number of clusters..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      tryCatch({
        data <- dataset_r()
        
        # Identify morphometric columns (numeric, exclude first column which is grouping)
        morpho_cols <- sapply(data[-1], is.numeric)
        morpho_data <- data[, c(FALSE, morpho_cols), drop = FALSE]
        
        if (ncol(morpho_data) == 0) {
          showNotification("No morphometric columns found in data", type = "error")
          return(NULL)
        }
        
        # Fit Mclust
        data_mod <- mclust::Mclust(morpho_data, G = 1:input$max_clusters)
        
        if (is.null(data_mod)) {
          showNotification("Mclust failed to fit models. Check your data for issues.", type = "error")
          return(NULL)
        }
        
        if (is.null(data_mod$BIC)) {
          showNotification("No BIC values available from Mclust. Check data quality.", type = "error")
          return(NULL)
        }
        
        # Convert BIC to long format
        bic_matrix  <- data_mod$BIC
        model_names <- colnames(bic_matrix)
        bic_list    <- list()
        for (i in seq_len(nrow(bic_matrix))) {
          for (j in seq_along(model_names)) {
            bic_value <- bic_matrix[i, j]
            if (!is.na(bic_value)) {
              bic_list[[length(bic_list) + 1]] <- data.frame(
                G     = as.numeric(rownames(bic_matrix)[i]),
                Model = model_names[j],
                BIC   = bic_value
              )
            }
          }
        }
        
        bic_long <- dplyr::bind_rows(bic_list) %>% dplyr::arrange(desc(BIC))
        
        if (nrow(bic_long) == 0) {
          showNotification("No valid BIC values after filtering", type = "error")
          return(NULL)
        }
        
        best_bic <- max(bic_long$BIC, na.rm = TRUE)
        bic_long <- bic_long %>%
          dplyr::mutate(Delta_BIC = best_bic - BIC, Rank = dplyr::row_number())
        
        top_models <- bic_long %>%
          dplyr::filter(Rank <= 20 | Delta_BIC < 10) %>%
          dplyr::select(Rank, G, Model, BIC, Delta_BIC)
        
        species_col <- data[[1]]
        
        if (is.null(data_mod$classification)) {
          showNotification("No classification available from Mclust", type = "error")
          return(NULL)
        }
        
        cluster_table <- table(Species = species_col, Cluster = data_mod$classification)
        
        unsupervised_results(list(
          model         = data_mod,
          cluster_table = cluster_table,
          species_col   = species_col,
          morpho_data   = morpho_data,
          top_models    = top_models,
          all_bic       = bic_long
        ))
        
        showNotification("Unsupervised clustering complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error in unsupervised clustering:", e$message),
                         type = "error", duration = 10)
        message("Full error in unsupervised clustering:")
        print(e)
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
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Running supervised GMM analysis — this may take several minutes for datasets with many OTUs..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      data <- dataset_r()
      
      # Identify morphometric columns (numeric, exclude first column which is grouping)
      morpho_cols <- sapply(data[-1], is.numeric)
      morpho_data <- data[, c(FALSE, morpho_cols), drop = FALSE]
      
      if (ncol(morpho_data) == 0) {
        showNotification("No morphometric columns found in data", type = "error")
        return(NULL)
      }
      
      species_col <- data[[1]]
      
      if (length(unique(species_col)) < 2) {
        showNotification("Error: Need at least 2 species/groups for supervised analysis", type = "error")
        return(NULL)
      }
      
      tryCatch({
        cl_merge <- MclustBayesFactorClassMerge(
          morpho_data,
          class     = species_col,
          modelType = "EDDA"
        )
        
        summary_table <- summarize_class_merge(cl_merge, digits = 3)
        
        supervised_results(summary_table)
        
        showNotification("Supervised GMM analysis complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
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
    # Per-specimen cluster assignment table with posteriors
    output$cluster_specimen_table <- DT::renderDataTable({
      req(unsupervised_results())
      results <- unsupervised_results()
      model   <- results$model
      ids     <- as.character(seq_len(length(results$species_col)))
      post    <- as.data.frame(round(model$z, 4))
      names(post) <- paste0("Post_Cluster", seq_len(ncol(post)))
      tbl <- data.frame(
        Specimen_ID = ids,
        OTU         = as.character(results$species_col),
        Cluster     = as.character(model$classification),
        post,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      DT::datatable(tbl, options = list(pageLength = 15, scrollX = TRUE, dom = "tp"),
                    rownames = FALSE)
    })
    
    # Download comprehensive per-specimen table
    output$download_unsupervised_csv <- downloadHandler(
      filename = function() paste0("unsupervised_clustering_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(unsupervised_results())
        results <- unsupervised_results()
        model   <- results$model
        ids     <- as.character(seq_len(length(results$species_col)))
        post    <- as.data.frame(round(model$z, 4))
        names(post) <- paste0("Post_Cluster", seq_len(ncol(post)))
        tbl <- data.frame(
          Specimen_ID = ids,
          OTU         = as.character(results$species_col),
          Cluster     = as.character(model$classification),
          post,
          check.names = FALSE, stringsAsFactors = FALSE
        )
        write.csv(tbl, file, row.names = FALSE)
      }
    )
    
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
    
    # ── Topology-aware Hypothesis Testing (morphometric) ──────────────────────
    morpho_phylo_results_r <- reactiveVal(NULL)  # results table (for display)
    morpho_phylo_sup_r     <- reactiveVal(NULL)   # full sup_data (for tree plot)
    
    ## ---- Morpho tree reactives ----
    morpho_phylo_tree_r        <- reactiveVal(NULL)
    morpho_phylo_raw_tree_r    <- reactiveVal(NULL)
    morpho_phylo_outgroup_r    <- reactiveVal(character(0))
    morpho_phylo_mode_r        <- reactiveVal("otu")
    morpho_phylo_unassigned_r  <- reactiveVal(character(0))
    morpho_phylo_assignments_r <- reactiveVal(list())
    morpho_phylo_rooted_r      <- reactiveVal(FALSE)
    
    is_properly_rooted_morpho <- function(tree) {
      root_node     <- length(tree$tip.label) + 1L
      root_children <- sum(tree$edge[, 1L] == root_node)
      root_children == 2L
    }
    
    ## Parse any supported tree format (Newick or NEXUS, single tree only)
    load_any_format_tree_morpho <- function(path) {
      # Try Newick first (IQ-TREE .treefile/.contree, RAxML .tre, plain Newick)
      tree <- tryCatch(ape::read.tree(path), error = function(e) NULL)
      if (!is.null(tree) && inherits(tree, "phylo")) {
        fmt <- "Newick"
      } else {
        # Try NEXUS (MrBayes .con.tre, BEAST MCC .tree)
        result <- tryCatch(ape::read.nexus(path), error = function(e) NULL)
        if (is.null(result)) {
          showNotification(
            paste0("Could not parse tree file. Supported formats: ",
                   "Newick (.nwk, .treefile, .contree) and single-tree NEXUS ",
                   "(.con.tre from MrBayes, MCC .tree from BEAST). ",
                   "Do not upload full posterior tree samples (.t, .trees)."),
            type = "error", duration = 12)
          return(NULL)
        }
        # Reject multi-tree files (posterior samples, etc.)
        if (inherits(result, "multiPhylo")) {
          showNotification(
            paste0("This file contains ", length(result), " trees. ",
                   "Only a single consensus tree is accepted. ",
                   "For MrBayes, use the .con.tre file. ",
                   "For BEAST, export the MCC tree as a single-tree file."),
            type = "error", duration = 12)
          return(NULL)
        }
        tree <- result
        fmt  <- "NEXUS"
      }
      if (!inherits(tree, "phylo")) {
        showNotification("Could not extract a valid phylogenetic tree from this file.",
                         type = "error"); return(NULL)
      }
      tree
    }
    
    ## Reset helper
    reset_morpho_phylo_assignment <- function() {
      morpho_phylo_raw_tree_r(NULL)
      morpho_phylo_tree_r(NULL)
      morpho_phylo_outgroup_r(character(0))
      morpho_phylo_rooted_r(FALSE)
      morpho_phylo_mode_r("otu")
      morpho_phylo_unassigned_r(character(0))
      morpho_phylo_assignments_r(list())
    }
    
    observeEvent(input$morpho_phylo_newick_file, {
      req(input$morpho_phylo_newick_file)
      reset_morpho_phylo_assignment()
      tree <- load_any_format_tree_morpho(input$morpho_phylo_newick_file$datapath)
      if (is.null(tree)) return()
      already_rooted <- is_properly_rooted_morpho(tree)
      morpho_phylo_raw_tree_r(tree)
      morpho_phylo_rooted_r(already_rooted)
      morpho_phylo_unassigned_r(sort(tree$tip.label))
      showNotification(
        paste0("Tree parsed: ", length(tree$tip.label), " tips. ",
               if (already_rooted)
                 "Tree appears to be rooted. Designate an outgroup below to re-root, or confirm as-is."
               else
                 "Tree is unrooted \u2014 designate an outgroup below to root it."),
        type = if (already_rooted) "message" else "warning", duration = 7)
    })
    
    observeEvent(input$load_morpho_example_tree, {
      path <- system.file("examples", "Gekko_tree.txt", package = "GroupStruct2")
      if (!file.exists(path)) {
        showNotification("Example tree file not found.", type = "error"); return()
      }
      reset_morpho_phylo_assignment()
      tree <- load_any_format_tree_morpho(path)
      if (!is.null(tree)) {
        morpho_phylo_raw_tree_r(tree)
        morpho_phylo_rooted_r(is_properly_rooted_morpho(tree))
        morpho_phylo_unassigned_r(sort(tree$tip.label))
      }
    })
    
    output$morpho_phylo_tree_status_ui <- renderUI({
      tree   <- morpho_phylo_raw_tree_r()
      if (is.null(tree)) return(NULL)
      og     <- morpho_phylo_outgroup_r()
      rooted <- morpho_phylo_rooted_r()
      
      rooting_msg <- if (length(og) > 0)
        tagList(strong(paste0(length(og), " outgroup tip(s) designated: ")),
                paste(og, collapse = ", "),
                " \u2014 tree will be rooted here and outgroup pruned.")
      else if (rooted)
        tagList(icon("check"), " Tree is already rooted. ",
                "Optionally designate an outgroup to re-root, or confirm as-is.")
      else
        tagList(icon("exclamation-triangle"), strong(" Tree is unrooted. "),
                "You must designate an outgroup tip below before confirming.")
      
      alert_class <- if (!rooted && length(og) == 0) "alert alert-warning" else "alert alert-info"
      tags$div(class = alert_class, style = "margin-top: 8px;",
               length(tree$tip.label), " tips parsed. ", rooting_msg)
    })
    
    ## ---- Unified tip assignment + outgroup UI (Step 1b) ----
    output$morpho_phylo_tip_assign_ui <- renderUI({
      raw_tree <- morpho_phylo_raw_tree_r()
      if (is.null(raw_tree)) return(NULL)
      
      og           <- morpho_phylo_outgroup_r()
      unassigned   <- morpho_phylo_unassigned_r()
      assignments  <- morpho_phylo_assignments_r()
      data         <- dataset_r()
      data_species <- if (!is.null(data)) sort(unique(as.character(data[[1]]))) else character(0)
      
      ingroup_tips <- setdiff(raw_tree$tip.label, og)
      otu_mode     <- (length(setdiff(ingroup_tips, data_species)) == 0) &&
        (length(setdiff(data_species, ingroup_tips)) == 0)
      
      if (!otu_mode && (length(assignments) == 0 || !all(data_species %in% names(assignments)))) {
        init_asgn <- setNames(vector("list", length(data_species)), data_species)
        for (nm in names(init_asgn)) init_asgn[[nm]] <- assignments[[nm]] %||% character(0)
        isolate(morpho_phylo_assignments_r(init_asgn))
        assignments <- init_asgn
      }
      
      all_assigned <- otu_mode || (length(unassigned) == 0)
      
      # Outgroup row (amber)
      og_row <- if (length(og) > 0) {
        list(tags$tr(
          style = "background-color: #fff3cd;",
          tags$td(style = "font-weight: bold; color: #856404; white-space: nowrap;",
                  icon("arrow-left"), " Outgroup"),
          tags$td(style = "font-size: 12px; color: #856404; word-break: break-word;",
                  paste(og, collapse = ", ")),
          tags$td(style = "text-align: center; color: #856404;", length(og)),
          tags$td(style = "text-align: center;",
                  actionButton(ns("morpho_phylo_clear_og_btn"), label = "", icon = icon("times"),
                               class = "btn-xs btn-warning",
                               onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'})",
                                                 ns("morpho_phylo_clear_og_clicked"))))
        ))
      } else list()
      
      # OTU assignment rows
      otu_rows <- if (!otu_mode) {
        lapply(data_species, function(otu) {
          tips_for_otu <- assignments[[otu]]
          tags$tr(
            tags$td(style = "font-weight: bold; white-space: nowrap;", otu),
            tags$td(style = "font-size: 12px; word-break: break-word;",
                    if (length(tips_for_otu) == 0) em("(none)") else paste(tips_for_otu, collapse = ", ")),
            tags$td(style = "text-align: center;", length(tips_for_otu)),
            tags$td(style = "text-align: center;",
                    if (length(tips_for_otu) > 0)
                      actionButton(
                        ns(paste0("morpho_clear_otu_", gsub("[^A-Za-z0-9_]", "_", otu))),
                        label = "", icon = icon("times"), class = "btn-xs btn-danger",
                        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                          ns("morpho_clear_otu_clicked"), otu)))
          )
        })
      } else list()
      
      tags$div(
        style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 15px; margin-top: 10px; background: #fafafa;",
        h5(icon("sitemap"), strong(" Step 1b: Assign Tips")),
        p(style = "font-size: 0.9em; color: #555;",
          "Hold ", tags$kbd("Ctrl"), " (Windows/Linux) or ", tags$kbd("\u2318 Cmd"), " (Mac)",
          " to select multiple tips."),
        
        fluidRow(
          column(6,
                 if (otu_mode) {
                   tagList(
                     tags$div(class = "alert alert-success",
                              style = "margin: 0 0 8px 0; padding: 8px; font-size: 0.9em;",
                              icon("check"),
                              paste0(" All ", length(ingroup_tips),
                                     " ingroup tips match OTU names \u2014 no assignment needed.")),
                     tags$div(style = "margin-bottom: 4px; font-size: 0.88em; color: #555;",
                              "Select tips to designate as outgroup (optional):"),
                     tags$select(
                       id = ns("morpho_phylo_tip_select"),
                       multiple = "multiple",
                       size = as.character(min(10, max(4, length(ingroup_tips)))),
                       style = paste0("width: 100%; font-family: monospace; font-size: 12px;",
                                      " border: 1px solid #ced4da; border-radius: 4px; padding: 4px;",
                                      " background: white; min-height: 80px;"),
                       lapply(ingroup_tips, function(tip) tags$option(value = tip, tip))
                     )
                   )
                 } else {
                   tagList(
                     tags$div(style = "margin-bottom: 6px;",
                              strong(paste0("Unassigned specimens (", length(unassigned), " remaining):"))),
                     if (length(unassigned) == 0) {
                       tags$div(class = "alert alert-success", style = "margin: 0; padding: 8px;",
                                icon("check"), " All specimens assigned!")
                     } else {
                       tags$select(
                         id = ns("morpho_phylo_tip_select"),
                         multiple = "multiple",
                         size = as.character(min(14, max(5, length(unassigned)))),
                         style = paste0("width: 100%; font-family: monospace; font-size: 12px;",
                                        " border: 1px solid #ced4da; border-radius: 4px; padding: 4px;",
                                        " background: white; min-height: 100px;"),
                         lapply(unassigned, function(tip) tags$option(value = tip, tip))
                       )
                     }
                   )
                 }
          ),
          column(6,
                 if (!otu_mode) {
                   tagList(
                     tags$div(style = "margin-bottom: 4px;", strong("Assign to OTU:")),
                     selectInput(ns("morpho_phylo_target_otu"), label = NULL,
                                 choices = data_species, selected = data_species[1], width = "100%"),
                     actionButton(ns("morpho_phylo_assign_btn"),
                                  label = tagList(icon("arrow-right"), " Assign to OTU"),
                                  class = "btn-primary btn-block",
                                  disabled = if (length(unassigned) == 0) "disabled" else NULL),
                     tags$hr(style = "margin: 8px 0; border-top: 1px dashed #ccc;"),
                     tags$p(style = "text-align: center; margin: 4px 0; font-size: 0.85em; color: #888;", "\u2014 or \u2014")
                   )
                 },
                 actionButton(ns("morpho_phylo_designate_og_btn"),
                              label = tagList(icon("arrow-left"), " Designate as Outgroup"),
                              class = "btn-warning btn-block",
                              disabled = if (length(unassigned) == 0) "disabled" else NULL),
                 tags$p(style = "font-size: 0.8em; color: #888; margin-top: 4px;",
                        if (!morpho_phylo_rooted_r())
                          "Required: tree is unrooted. Select outgroup tip(s) to root the tree. Outgroup will be excluded from analysis."
                        else
                          "Optional: re-root on a specific outgroup tip. Leave empty to use the existing root."),
                 if (!otu_mode) {
                   tagList(
                     tags$hr(style = "margin: 8px 0;"),
                     actionButton(ns("morpho_phylo_clear_all_btn"),
                                  label = tagList(icon("trash"), " Clear All"),
                                  class = "btn-danger btn-sm btn-block")
                   )
                 }
          )
        ),
        
        hr(style = "margin: 12px 0;"),
        h6(strong("Assignments:")),
        tags$div(
          style = "overflow-x: auto;",
          tags$table(
            class = "table table-condensed table-bordered",
            style = "font-size: 12px; width: 100%; background: white;",
            tags$thead(tags$tr(
              tags$th("Group"), tags$th("Tips"),
              tags$th(style = "text-align: center;", "N"),
              tags$th(style = "text-align: center;", "Clear")
            )),
            tags$tbody(c(og_row, otu_rows))
          )
        ),
        
        if (!morpho_phylo_rooted_r() && length(og) == 0) {
          tags$div(
            class = "alert alert-danger", style = "margin-top: 10px;",
            icon("times-circle"), " ",
            strong("Cannot confirm: "), "tree is unrooted. Select at least one outgroup tip above."
          )
        } else if (all_assigned) {
          tags$div(
            style = "margin-top: 10px;",
            tags$p(style = "font-size: 0.9em; margin-bottom: 8px;",
                   icon("info-circle"), " ",
                   strong("Rooting: "),
                   if (length(og) > 0)
                     paste0("Outgroup (", paste(og, collapse = ", "),
                            ") will root the tree and be pruned.")
                   else
                     "Existing root will be used as-is."),
            actionButton(ns("morpho_phylo_confirm_mapping"),
                         label = tagList(icon("check-circle"), " Confirm & Build Tree"),
                         class = "btn-primary btn-block")
          )
        } else {
          tags$p(style = "color: #888; font-size: 0.88em; margin-top: 10px;",
                 icon("info-circle"), " ",
                 paste0(length(unassigned), " tip(s) still unassigned."))
        }
      )
    })
    
    ## Designate as outgroup
    observeEvent(input$morpho_phylo_designate_og_btn, {
      selected <- input$morpho_phylo_tip_select
      if (is.null(selected) || length(selected) == 0) {
        showNotification("No tips selected.", type = "warning"); return()
      }
      morpho_phylo_outgroup_r(sort(union(morpho_phylo_outgroup_r(), selected)))
      morpho_phylo_unassigned_r(setdiff(morpho_phylo_unassigned_r(), selected))
      morpho_phylo_assignments_r(lapply(morpho_phylo_assignments_r(), function(tips) setdiff(tips, selected)))
      morpho_phylo_tree_r(NULL)
    })
    
    ## Clear outgroup
    observeEvent(input$morpho_phylo_clear_og_clicked, {
      og <- morpho_phylo_outgroup_r()
      if (length(og) == 0) return()
      morpho_phylo_unassigned_r(sort(c(morpho_phylo_unassigned_r(), og)))
      morpho_phylo_outgroup_r(character(0))
      morpho_phylo_tree_r(NULL)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    observeEvent(input$morpho_phylo_assign_btn, {
      selected_tips <- input$morpho_phylo_tip_select
      target_otu    <- input$morpho_phylo_target_otu
      if (is.null(selected_tips) || length(selected_tips) == 0) {
        showNotification("No specimens selected. Use Ctrl/Cmd+click to select.", type = "warning"); return()
      }
      asgn <- morpho_phylo_assignments_r()
      asgn[[target_otu]] <- unique(c(asgn[[target_otu]], selected_tips))
      morpho_phylo_assignments_r(asgn)
      morpho_phylo_unassigned_r(setdiff(morpho_phylo_unassigned_r(), selected_tips))
      morpho_phylo_tree_r(NULL)
    })
    
    observeEvent(input$morpho_phylo_clear_all_btn, {
      data         <- dataset_r()
      data_species <- if (!is.null(data)) sort(unique(as.character(data[[1]]))) else character(0)
      raw_tree     <- morpho_phylo_raw_tree_r()
      og           <- morpho_phylo_outgroup_r()
      if (!is.null(raw_tree)) morpho_phylo_unassigned_r(sort(setdiff(raw_tree$tip.label, og)))
      init_asgn <- setNames(vector("list", length(data_species)), data_species)
      for (nm in names(init_asgn)) init_asgn[[nm]] <- character(0)
      morpho_phylo_assignments_r(init_asgn)
      morpho_phylo_tree_r(NULL)
    })
    
    observeEvent(input$morpho_clear_otu_clicked, {
      otu  <- input$morpho_clear_otu_clicked
      asgn <- morpho_phylo_assignments_r()
      if (!otu %in% names(asgn)) return()
      tips_to_restore <- asgn[[otu]]
      asgn[[otu]] <- character(0)
      morpho_phylo_assignments_r(asgn)
      morpho_phylo_unassigned_r(sort(c(morpho_phylo_unassigned_r(), tips_to_restore)))
      morpho_phylo_tree_r(NULL)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$morpho_phylo_confirm_mapping, {
      raw_tree       <- morpho_phylo_raw_tree_r(); req(raw_tree)
      og             <- morpho_phylo_outgroup_r()
      already_rooted <- morpho_phylo_rooted_r()
      ingroup_tips   <- setdiff(raw_tree$tip.label, og)
      data           <- dataset_r()
      data_species   <- if (!is.null(data)) sort(unique(as.character(data[[1]]))) else character(0)
      otu_mode       <- (length(setdiff(ingroup_tips, data_species)) == 0) &&
        (length(setdiff(data_species, ingroup_tips)) == 0)
      
      # Safety guard
      if (!already_rooted && length(og) == 0) {
        showNotification(
          "Tree is unrooted and no outgroup has been designated. Please select an outgroup.",
          type = "error", duration = 8); return()
      }
      
      if (!otu_mode) {
        assignments <- morpho_phylo_assignments_r()
        empty_otus  <- data_species[sapply(data_species, function(o) length(assignments[[o]]) == 0)]
        if (length(empty_otus) > 0) {
          showNotification(
            paste0("Cannot confirm: OTUs with no specimens assigned: ",
                   paste(empty_otus, collapse = ", "), "."),
            type = "error", duration = 10); return()
        }
      }
      
      if (length(og) > 0) {
        tree_rooted <- tryCatch(
          ape::root(raw_tree, outgroup = og, resolve.root = TRUE),
          error = function(e) {
            showNotification(paste("Outgroup rooting failed:", e$message), type = "error"); NULL
          }
        )
        if (is.null(tree_rooted)) return()
      } else {
        tree_rooted <- raw_tree  # already rooted, use as-is
      }
      
      tree_ingroup <- if (length(og) > 0)
        ape::keep.tip(tree_rooted, ingroup_tips)
      else
        tree_rooted
      
      tree_final <- if (otu_mode) tree_ingroup else
        collapse_individuals_to_otu_morpho(tree_ingroup, morpho_phylo_assignments_r())
      
      morpho_phylo_tree_r(tree_final)
      root_msg <- if (length(og) > 0)
        paste0("Rooted on outgroup (", paste(og, collapse = ", "), "), outgroup pruned.")
      else
        "Existing root used as-is."
      showNotification(
        paste0("Tree built: ", length(tree_final$tip.label), " taxa. ", root_msg),
        type = "message")
    })
    
    output$morpho_phylo_tree_preview_ui <- renderUI({
      if (is.null(morpho_phylo_tree_r())) return(NULL)
      tagList(
        hr(),
        h5(strong("OTU-level tree preview:")),
        plotOutput(ns("morpho_phylo_tree_preview"), height = "380px")
      )
    })
    
    output$morpho_phylo_tree_preview <- renderPlot({
      tree <- morpho_phylo_tree_r(); req(tree)
      n     <- length(tree$tip.label)
      cex_t <- max(0.55, min(1.0, 20 / n))
      has_nl <- !is.null(tree$node.label) && any(nchar(trimws(tree$node.label)) > 0)
      par(mar = c(1, 1, 2, max(4, nchar(max(tree$tip.label)) * 0.55)))
      ape::plot.phylo(tree, type = "cladogram", use.edge.length = FALSE,
                      show.tip.label = TRUE, cex = cex_t,
                      show.node.label = isTRUE(input$morpho_phylo_show_support) && has_nl,
                      label.offset = 0.5, no.margin = FALSE)
      title(main = paste0("OTU-level tree (", n, " taxa)"), cex.main = 1.1)
    })
    
    ## Helper (module-local)
    collapse_individuals_to_otu_morpho <- function(raw_tree, assignments) {
      otus     <- names(assignments)
      rep_tips <- sapply(otus, function(otu) sort(assignments[[otu]])[1])
      tree_pruned <- ape::keep.tip(raw_tree, unname(rep_tips))
      for (otu in otus) {
        idx <- which(tree_pruned$tip.label == rep_tips[[otu]])
        if (length(idx) > 0) tree_pruned$tip.label[idx] <- otu
      }
      tree_pruned
    }
    
    observeEvent(input$run_morpho_phylo, {
      tree <- morpho_phylo_tree_r()
      data <- dataset_r()
      if (is.null(tree)) {
        showNotification(
          "Please upload a tree file and complete the specimen-to-OTU mapping (if required) before running.",
          type = "error", duration = 8); return()
      }
      if (is.null(data) || nrow(data) == 0) {
        showNotification("Please load morphometric data first.", type = "error"); return()
      }
      
      species_col  <- as.character(data[[1]])
      morpho_data  <- data[, -1, drop = FALSE]
      tree_for_hyp <- morpho_phylo_tree_r()
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Generating and testing topology-aware hypotheses..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      tryCatch({
        result    <- generate_all_phylo_hypotheses(tree_for_hyp, species_col)
        if (!is.null(result$error)) {
          showNotification(result$error, type = "error", duration = 10); return()
        }
        phylo_hyps <- result$hyp_df
        n_hyp      <- ncol(phylo_hyps)
        priors     <- rep(1 / n_hyp, n_hyp); names(priors) <- colnames(phylo_hyps)
        
        models <- list()
        for (hn in colnames(phylo_hyps)) {
          models[[hn]] <- mclust::MclustDA(
            as.matrix(morpho_data),
            factor(phylo_hyps[[hn]]),
            modelType = "EDDA", verbose = FALSE)
        }
        results <- do.call(GMMBayesFactorTable, c(models, list(prior = priors)))
        results <- results[order(-results$BIC), ]
        if (nrow(results) > 10)
          showNotification(paste0("Showing top 10 of ", nrow(results),
                                  " hypotheses. Download for full results."),
                           type = "message", duration = 6)
        morpho_phylo_results_r(results)
        morpho_phylo_sup_r(list(
          summary_table    = results,
          species_col      = species_col,
          hyp_df           = phylo_hyps,
          ordered_hyp_keys = colnames(phylo_hyps),
          tree_used        = tree_for_hyp
        ))
        showNotification("Topology-aware hypothesis testing complete!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    output$morpho_phylo_results_table <- DT::renderDataTable({
      req(morpho_phylo_results_r())
      tbl <- head(morpho_phylo_results_r(), 10)
      tbl <- tbl[, !names(tbl) %in% "Taxonomic groupings", drop = FALSE]
      DT::datatable(tbl, class = "display nowrap",
                    options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
                    rownames = FALSE)
    })
    
    output$morpho_phylo_interpretation <- renderUI({
      req(morpho_phylo_results_r())
      tagList(
        h5("Interpretation:"),
        tags$ul(
          tags$li(strong("BIC:"), "Higher = better fit. Best hypothesis has \u0394BIC = 0."),
          tags$li(strong("\u0394BIC < 2:"), "Weak evidence against; 2\u20136 positive; > 10 very strong."),
          tags$li(strong("PostMod:"), "Posterior probability under uniform priors across all tested hypotheses.")
        )
      )
    })
    
    output$download_morpho_phylo_csv <- downloadHandler(
      filename = function() paste0("topology_hypotheses_", Sys.Date(), ".csv"),
      content  = function(f) { req(morpho_phylo_results_r()); write.csv(morpho_phylo_results_r(), f, row.names = FALSE) }
    )
    output$download_morpho_phylo_xlsx <- downloadHandler(
      filename = function() paste0("topology_hypotheses_", Sys.Date(), ".xlsx"),
      content  = function(f) {
        req(morpho_phylo_results_r())
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Topology Hypotheses")
        openxlsx::writeData(wb, "Topology Hypotheses", morpho_phylo_results_r())
        openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
      }
    )
    # ── End topology-aware ────────────────────────────────────────────────────
    
    # Placeholder for other tabs
    output$boruta_plot <- renderPlot({
      plot(1, 1, main = "Click 'Identify Diagnostic Characters' to generate plot")
    })
    
    output$boruta_table <- DT::renderDataTable({
      data.frame(Message = "Click 'Identify Diagnostic Characters' to see results")
    })
    
    # Return results for visualization module
    return(list(
      results_r = reactive({
        list(
          unsupervised = unsupervised_results(),
          supervised   = supervised_results(),
          bayesian     = bayesian_results(),
          boruta       = boruta_results()
        )
      }),
      bayesian_models_r      = bayesian_models,
      morpho_phylo_results_r = morpho_phylo_results_r,
      morpho_phylo_tree_r    = morpho_phylo_tree_r,
      morpho_phylo_sup_r     = morpho_phylo_sup_r
    ))
    
  })
}