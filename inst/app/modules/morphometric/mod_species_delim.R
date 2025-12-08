# Module UI
mod_species_delim_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Gaussian Mixture Models (GMM) for Morphometric Species Delimitation"),
    hr(),
    p("Traditional morphometric analyses uses univariate/multivariate statistics",
      "to test the significance of trait differences among pre-defined OTUs.",
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
        "(e.g., genetics, ecology, geographic/eproductive isolation)",
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
                          strong("without relying on pre-designated species groupings"), 
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
                        h4("Unsupervised Model-Based Clustering"),
                        hr(),
                        h4("Results"),
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
                          strong("Visualization > Species Delimitation"), "tab for BIC plots and PCA cluster plots."),
                        hr(),
                        downloadButton(ns("download_unsupervised_csv"), "Download CSV"),
                        hr()
                 )
               )
      ),
      
      tabPanel("Supervised Clustering",
               fluidRow(
                 column(12,
                        h4("Supervised Lumping"),
                        p("This analysis uses supervised Gaussian Mixture Models (GMM)", 
                          "to evaluate competing taxonomic hypotheses by", 
                          strong("using assigned OTU labels"), 
                          "to train discriminant models.",
                          "Starting with the original OTU groupings, the algorithm systematically tests all possible ways of lumping taxa,",
                          "computing Bayesian Information Criterion (BIC) and Bayes Factors for each delimitation scheme."),
                        
                        p(strong("Method:"), 
                          "Based on the original OTU groupings, the algorithm iteratively generates all possible pairs of taxa for merging",
                          "using the EDDA (Eigenvalue Decomposition Discriminant Analysis) model,",
                          "assessing model fit with BIC at each step until only one taxon remains.",
                          "Higher BIC indicates better fit; Bayes Factors quantify strength of evidence between competing models.",
                          "For best results, assign the most plausible splitty grouping scheme to your  original uploaded data."),
                        
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
                            "than unsupervised methods because it doesn't need to simultaneously infer both groups and structure.",
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
      
      tabPanel("Model-based Hypothesis Testing",
               fluidRow(
                 column(12,
                        h4("Bayesian Model-based Testing of Explicit Taxonomic Hypotheses"),
                        p("This analysis evaluates competing taxonomic hypotheses using Bayesian model comparison.",
                          "In a separate file, the user defines multiple hypotheses about how OTUs should be grouped (lumped or split),",
                          "and the algorithm computes Bayesian Information Criterion (BIC) and posterior probabilities",
                          "to determine which hypothesis is best supported by the morphometric data."),
                        
                        p(strong("Method:"), 
                          "The analysis uses", code("modelType = 'EDDA'"), 
                          "with G=1 (one Gaussian component per group), which assumes each taxonomic group",
                          "can be described by a single multivariate Gaussian distribution.",
                          "If you suspect substructure within a group",
                          "(e.g., cryptic species or distinct populations), test this by creating",
                          "additional split hypotheses."),
                        
                        p(strong("Priors:"),
                          "The analysis uses flat (uninformative) priors of 0.2 for each hypothesis,",
                          "treating all hypotheses as equally likely before examining the data.",
                          "Posterior probabilities are then calculated based solely on how well",
                          "each hypothesis fits the morphometric data."),
                        
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
                        
                        uiOutput(ns("hyp_upload_status")),
                        
                        conditionalPanel(
                          condition = sprintf("output['%s']", ns("hyp_file_loaded")),
                          h5("Preview of Loaded Hypotheses:"),
                          DTOutput(ns("hyp_preview"))
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
                        p("Boruta is a feature selection algorithm that uses Random Forest to identify",
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
                          "A large drop in accuracy means the variable is important for distinguishing groups.",
                          "Boruta compares each real variable's importance to randomized 'shadow' variables:",
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
        
        incProgress(0.2, detail = "Fitting EDDA models...")
        
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
              as.matrix(morpho_data),
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
                  "Higher values indicate better model fit. The best hypothesis has the highest BIC (∆BIC = 0)."),
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
      
      withProgress(message = 'Running unsupervised clustering...', value = 0, {
        
        data <- dataset_r()
        
        # Identify morphometric columns (numeric, exclude first column which is grouping)
        morpho_cols <- sapply(data[-1], is.numeric)
        morpho_data <- data[, c(FALSE, morpho_cols), drop = FALSE]
        
        # Check if we have data
        if (ncol(morpho_data) == 0) {
          showNotification("No morphometric columns found in data", type = "error")
          return(NULL)
        }
        
        incProgress(0.3, detail = "Fitting models...")
        
        # Fit Mclust
        tryCatch({
          data_mod <- mclust::Mclust(morpho_data, G = 1:input$max_clusters)
          
          incProgress(0.4, detail = "Creating cluster-species table...")
          
          # Create cluster-species correspondence table
          species_col <- data[[1]]
          cluster_table <- table(Species = species_col, 
                                 Cluster = data_mod$classification)
          
          incProgress(0.3, detail = "Finalizing results...")
          
          # Store results
          results <- list(
            model = data_mod,
            cluster_table = cluster_table,
            species_col = species_col,
            morpho_data = morpho_data
          )
          
          unsupervised_results(results)
          
          showNotification("Unsupervised clustering complete!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })
    
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
          "Higher BIC values indicate better model fit. The model with the highest BIC is considered the best-supported delimitation scheme. K = number of clusters"),
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
        cluster_species <- cbind(Species = rownames(cluster_species), cluster_species)
        
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
        boruta = boruta_results()
      )
    }))
    
  })
}