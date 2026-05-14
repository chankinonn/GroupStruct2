
mod_mfa_delim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("MFA Delimitation"),
    hr(),
    h4("GMM-based Delimitation on MFA Factor Scores"),
    
    p("This module evaluates morphological structure in mixed datasets using",
      "Gaussian Mixture Models (GMM) applied to", strong("MFA factor scores"),
      "(continuous variables derived from mixed morphometric, meristic, and categorical data).",
      "rather than on raw variables."),
    
    tags$div(
      style = "background-color: #fff3cd; border: 2px solid #dc3545; border-radius: 5px; padding: 15px; margin: 15px 0;",
      p(style = "color: #dc3545; margin: 0 0 10px 0;",
        strong("\u26a0 IMPORTANT: This is not the same as Morphometric Delimitation")),
      p(style = "margin: 0 0 10px 0;",
        "Morphometric Delimitation fits models directly to your original measurements.",
        "This module instead works on", strong("MFA factor scores"), "\u2014 compressed summaries of your",
        "mixed data computed in a prior step. This makes it possible to analyse datasets combining",
        "morphometric, meristic, and categorical traits together, but it comes with trade-offs:"),
      tags$ul(style = "margin: 0 0 10px 0; padding-left: 20px;",
              tags$li("Some information is always lost during the MFA compression step.",
                      " Patterns in the discarded dimensions will not be detected."),
              tags$li("Clusters are found in factor space, not in your original measurements,",
                      " so they cannot be interpreted as directly as trait-based clusters."),
              tags$li("Model fit statistics assume the factor scores are known exactly,",
                      " which slightly overstates confidence in the results."),
              tags$li("Results can be sensitive to how many MFA dimensions are retained.",
                      " Always check whether your conclusions hold across different threshold settings.")
      ),
      p(style = "margin: 0;",
        "Treat this module as", strong("exploratory"), "\u2014 a way to generate hypotheses about",
        "grouping structure in mixed datasets, not to confirm species boundaries.",
        "Independent evidence (genetics, ecology, biogeography) is always needed.")
    ),
    
    p(strong("Important:"),
      "You must run MFA first (see 'MFA Analysis & Results' tab) before performing any of these analyses."),
    
    hr(),
    
    h4("MFA Dimension Selection"),
    p("All three analyses below use MFA factor scores restricted to the most informative dimensions.",
      strong("Run the threshold sensitivity test first"), "to see how the number of clusters",
      "changes across thresholds, then select a threshold for the full analysis."),
    
    hr(),
    
    tabsetPanel(
      id = ns("bedda_subtabs"),
      
      ## ---- Unsupervised Clustering ----
      tabPanel("Unsupervised Clustering",
               br(),
               h4("Unsupervised Model-Based Clustering on MFA Scores"),
               p("This analysis uses unsupervised Gaussian Mixture Models (GMM)",
                 "to discover morphological clusters in the MFA factor space",
                 strong("without relying on pre-designated OTU groupings."),
                 "The algorithm tests different numbers of clusters (G) and covariance structures,",
                 "selecting the best model using Bayesian Information Criterion (BIC)."),
               hr(),
               fluidRow(
                 column(3,
                        numericInput(ns("mfa_bedda_max_clusters"),
                                     label = strong("Maximum clusters:"),
                                     value = NA, min = 1, max = 10, step = 1)
                 ),
                 column(9,
                        p(style = "margin-top: 25px;",
                          em("Should not exceed the number of plausible OTUs or geographic populations."))
                 )
               ),
               hr(),
               
               ## ---- Step 1: Sensitivity ----
               h4("\u276f Step 1: Threshold Sensitivity Test"),
               p("Tests GMM clustering across all variance thresholds (70\u2013100% in 5% steps).",
                 "Use this to decide which threshold to use in Step 2."),
               actionButton(ns("run_mfa_sensitivity"),
                            "Run Sensitivity Test",
                            icon = icon("search"),
                            class = "btn-default"),
               br(), br(),
               uiOutput(ns("mfa_sensitivity_results_ui")),
               hr(),
               
               ## ---- Step 2: Full analysis ----
               h4("\u276f Step 2: Full Analysis at Selected Threshold"),
               p("Select a threshold informed by Step 1, then run the full analysis."),
               sliderInput(ns("mfa_bedda_variance_threshold"),
                           "Cumulative Variance Threshold (%):",
                           min = 70, max = 100, value = 90, step = 5),
               actionButton(ns("run_mfa_unsupervised"),
                            "Run Unsupervised Clustering",
                            icon = icon("play"),
                            class = "btn-primary"),
               hr(),
               h4("Results"),
               tabsetPanel(
                 id = ns("mfa_unsup_results_tabs"),
                 tabPanel("Summary",
                          br(),
                          verbatimTextOutput(ns("mfa_unsup_cluster_summary")),
                          hr(),
                          h4("Per-specimen Cluster Assignment"),
                          p("Specimen ID, original OTU, assigned cluster, and posterior probability of belonging to each cluster."),
                          DT::dataTableOutput(ns("mfa_unsup_cluster_specimen_table")),
                          br(),
                          downloadButton(ns("download_mfa_unsup_csv"), "Download Table (CSV)"),
                          hr(),
                          h5("Interpretation Guide:"),
                          p("The table shows cluster assignments and posterior probabilities for each specimen."),
                          tags$ul(
                            tags$li(strong("Perfect correspondence:"),
                                    "Each cluster contains specimens from only one OTU \u2014 strong morphological differentiation."),
                            tags$li(strong("Mixed clusters:"),
                                    "A cluster contains specimens from multiple OTUs \u2014 morphological overlap between those OTUs."),
                            tags$li(strong("Split OTU:"),
                                    "One OTU spans multiple clusters \u2014 possible cryptic diversity or intraspecific variation."),
                            tags$li(strong("Posterior probabilities:"),
                                    "Values close to 1 indicate high confidence. Values near 0.5 indicate a morphologically intermediate specimen.")
                          )
                 ),
                 tabPanel("Top Models",
                          br(),
                          h4("Model Comparison"),
                          p("All models tested, ranked by BIC.",
                            "\u2206BIC < 2: substantial support;",
                            "\u2206BIC 2\u20136: moderate support;",
                            "\u2206BIC 6\u201310: weak support;",
                            "\u2206BIC > 10: essentially no support."),
                          DT::dataTableOutput(ns("mfa_unsup_top_models_table")),
                          hr(),
                          downloadButton(ns("download_mfa_unsup_top_models_csv"), "Download Top Models Table"),
                          hr()
                 ),
                 tabPanel("Model Details",
                          br(),
                          h4("Best Model Details"),
                          verbatimTextOutput(ns("mfa_unsup_model_details")),
                          hr(),
                          h4("Covariance Model Interpretation"),
                          p("The covariance structure determines how clusters differ in volume, shape, and orientation:"),
                          tags$ul(
                            tags$li(strong("E (Equal)"), "= parameter is the same across all clusters"),
                            tags$li(strong("V (Variable)"), "= parameter varies across clusters"),
                            tags$li(strong("I (Identity)"), "= special constraint (spherical or diagonal)")
                          )
                 )
               ),
               hr()
      ),
      
      ## ---- Phylogenetic Hypothesis Testing ----
      tabPanel("Topology-aware Hypothesis Testing",
               br(),
               h4("Topology-aware Bayesian Hypothesis Testing"),
               p("This analysis generates and tests monophyletic hypotheses consistent with a",
                 "reference tree topology. Every hypothesis corresponds to a biologically valid",
                 "partition \u2014 one where each group is a clade on the tree.",
                 "Models are compared simultaneously using EDDA and ranked by BIC and Bayes Factors."),
               p(strong("Method:"),
                 "Given a tree topology, monophyletic partitions are enumerated by testing all valid",
                 "combinations of non-nested internal nodes. For tractability, enumeration is capped",
                 "at 20,000 combinations. Each is fit with EDDA on the MFA factor scores and ranked by BIC."),
               hr(),
               
               ## Step 1: Upload tree
               h4("\u276f Step 1: Upload Tree"),
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
               p("The tree can include branch lengths and bootstrap/posterior support values.",
                 "Only the topology is used for hypothesis generation."),
               fileInput(ns("phylo_newick_file"),
                         "Choose tree file:",
                         accept = c(".nwk", ".newick", ".tre", ".tree", ".txt",
                                    ".treefile", ".contree", ".con.tre",
                                    ".nexus", ".nex", ".xml"),
                         placeholder = "No file selected"),
               p("Or load an example tree (works with the corresponding example datasets):"),
               actionButton(ns("load_example_tree"), "Gekko Tree",
                            icon = icon("tree"), class = "btn-default btn-sm"),
               actionButton(ns("load_example_tree_ctenophorus"), "Ctenophorus Tree",
                            icon = icon("tree"), class = "btn-default btn-sm"),
               uiOutput(ns("phylo_tree_status_ui")),
               uiOutput(ns("phylo_tip_assign_ui")),
               uiOutput(ns("phylo_tree_preview_ui")),
               hr(),
               
               ## Step 2: Run
               h4("\u276f Step 2: Run Analysis"),
               sliderInput(ns("mfa_phylo_variance_threshold"),
                           "Cumulative Variance Threshold (%):",
                           min = 70, max = 100, value = 90, step = 5),
               actionButton(ns("run_phylo_analysis"),
                            "Run Phylogenetic Hypothesis Testing",
                            icon = icon("play"),
                            class = "btn-primary"),
               hr(),
               
               ## Results
               h4("Results"),
               p(em("Results sorted by BIC. Top 10 hypotheses shown; download for full results.")),
               DT::dataTableOutput(ns("mfa_phylo_results_table")),
               hr(),
               downloadButton(ns("download_mfa_phylo_csv"),  "Download CSV"),
               downloadButton(ns("download_mfa_phylo_xlsx"), "Download Excel"),
               hr(),
               uiOutput(ns("mfa_phylo_interpretation")),
               hr(),
               
               ## Sensitivity test (Option B: re-fits same hypotheses at each threshold)
               h4("\u276f Step 3: Threshold Sensitivity Test"),
               p("Re-fits the same hypotheses generated from your tree across all variance thresholds",
                 "(70\u2013100% in 5% steps) to check whether the top-ranked hypothesis is stable.",
                 strong("Run the full analysis first (Step 2) before running this.")),
               actionButton(ns("run_phylo_sensitivity"),
                            "Run Sensitivity Test",
                            icon = icon("search"),
                            class = "btn-default"),
               br(), br(),
               uiOutput(ns("mfa_phylo_sensitivity_ui")),
               hr()
      ),
      
      ## ---- Hypothesis Testing ----
      tabPanel("User-specified Hypothesis Testing",
               br(),
               h4("Bayesian Model-Testing of Explicit Taxonomic Hypotheses"),
               p("This analysis evaluates competing taxonomic hypotheses using Bayesian model comparison.",
                 "In a separate file, the user defines multiple hypotheses about how OTUs should be grouped,",
                 "and the algorithm computes BIC and posterior probabilities",
                 "to determine which hypothesis is best supported by the MFA factor scores."),
               p(strong("Method:"),
                 "The analysis uses", code("modelType = 'EDDA'"),
                 "with one Gaussian component per group, assuming each taxonomic group",
                 "can be described by a single multivariate Gaussian distribution in MFA factor space.",
                 "Priors are flat (all hypotheses treated as equally likely before examining the data)."),
               hr(),
               h4("Upload Hypothesis File"),
               p("Upload a CSV file with your taxonomic hypotheses.",
                 strong("Important:"), "The order of rows in this file",
                 strong("must match exactly"), "the order of specimens in your original input data."),
               p("File format: Each column represents one hypothesis to be tested.",
                 "Column names are hypothesis names (e.g., HYP_1, HYP_2, Lump_AB, Split_A, etc.)."),
               p(strong("DO NOT include morphometric data in the hypothesis file.")),
               p(strong("Example:")),
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
               fileInput(ns("mfa_hyp_file"),
                         "Choose CSV File",
                         accept = ".csv"),
               actionButton(ns("load_example_mfa_hyp"),
                            "Load Example Hypothesis File",
                            icon = icon("table")),
               br(),
               p(style = "color: #856404; font-style: italic; margin-top: 10px;",
                 strong("Note:"),
                 "The example hypothesis file is designed to work with the example mixed data dataset.",
                 "Please load the example mixed dataset from the 'Input Data' module before loading this file."),
               br(),
               uiOutput(ns("mfa_hyp_upload_status")),
               conditionalPanel(
                 condition = sprintf("output['%s']", ns("mfa_hyp_file_loaded")),
                 h5("Preview of Loaded Hypotheses:"),
                 DTOutput(ns("mfa_hyp_preview"))
               ),
               hr(),
               actionButton(ns("run_mfa_bedda"),
                            "Run User-specified Hypothesis Testing",
                            icon = icon("play"),
                            class = "btn-primary"),
               hr(),
               h4("Results"),
               DT::dataTableOutput(ns("mfa_bedda_results_table")),
               hr(),
               downloadButton(ns("download_mfa_bedda_csv"), "Download CSV"),
               downloadButton(ns("download_mfa_bedda_xlsx"), "Download Excel"),
               hr(),
               uiOutput(ns("mfa_bedda_interpretation")),
               hr()
      ),
      
      ## ---- Diagnostic Characters (Boruta) ----
      tabPanel("Diagnostic Characters (Machine Learning)",
               br(),
               h4("Diagnostic Character Identification using Machine Learning"),
               p("This analysis uses the Boruta feature selection algorithm (Random Forest) to identify",
                 "variables from your", strong("original mixed dataset"), "(morphometric, meristic, and categorical)",
                 "that are statistically important for distinguishing between taxonomic groups.",
                 "By operating on the original variables rather than MFA factor scores,",
                 "it produces biologically interpretable results: which specific traits reliably distinguish groups."),
               
               p(strong("How it works:"),
                 "The algorithm iteratively compares each variable's importance against randomly permuted",
                 "'shadow' variables, confirming variables that consistently outperform random noise,",
                 "rejecting those that don't, and marking uncertain ones as tentative.",
                 tags$ul(
                   tags$li("Variables consistently more important than the best shadow \u2192 ", strong("Confirmed")),
                   tags$li("Variables less important than the best shadow \u2192 ", strong("Rejected")),
                   tags$li("Variables with uncertain importance \u2192 ", strong("Tentative"))
                 )),
               
               tags$div(
                 style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                 p(style = "margin: 0;",
                   strong("Key Point:"), "Importance is", em("relative"), "\u2014 it tells you which traits",
                   "are most useful for distinguishing groups compared to random noise.",
                   "Categorical variables are handled natively by Random Forest.",
                   "If you have run User-specified Hypothesis Testing, you can select a hypothesis",
                   "to test which variables are diagnostic for that particular taxonomic scheme.")
               ),
               
               p(strong("Note:"), "This is NOT a Bayesian approach. The algorithm uses machine learning",
                 "to measure importance, then uses p-values to test the significance of that importance.",
                 "For visualizations of variable importance, see the",
                 strong("Visualization \u2192 MFA Delimitation"), "tab."),
               
               hr(),
               
               h4("Step 1: Select Hypothesis"),
               uiOutput(ns("mfa_boruta_hypothesis_selector")),
               
               hr(),
               
               h4("Step 2: Select Groups to Compare"),
               p("Select at least 2 groups to compare."),
               uiOutput(ns("mfa_boruta_group_selector")),
               
               hr(),
               
               h4("Step 3: Set Parameters"),
               fluidRow(
                 column(3,
                        numericInput(ns("mfa_boruta_maxruns"), "Maximum Iterations:",
                                     value = 1000, min = 100, max = 5000, step = 100)
                 ),
                 column(3,
                        numericInput(ns("mfa_boruta_pvalue"), "P-value threshold:",
                                     value = 0.01, min = 0.001, max = 0.1, step = 0.01)
                 ),
                 column(3,
                        numericInput(ns("mfa_boruta_seed"), "Random seed:",
                                     value = 123, min = 1, max = 10000)
                 ),
                 column(3,
                        p(style = "margin-top: 25px;",
                          em("Higher maxRuns = more reliable results but slower"))
                 )
               ),
               
               hr(),
               
               actionButton(ns("run_mfa_boruta"),
                            "Run Boruta Analysis",
                            icon = icon("play"),
                            class = "btn-primary"),
               
               hr(),
               
               h4("Results"),
               
               h5("Variable Importance Results:"),
               p("All variables ranked by mean importance, showing decision status (Confirmed/Tentative/Rejected)"),
               DT::dataTableOutput(ns("mfa_boruta_importance_table")),
               
               br(),
               downloadButton(ns("download_mfa_boruta_importance_csv"),  "Download Importance Table (CSV)"),
               downloadButton(ns("download_mfa_boruta_importance_xlsx"), "Download Importance Table (Excel)"),
               
               br(), br(),
               
               h5("Descriptive Statistics (Confirmed Numeric Variables Only):"),
               p("Mean \u00b1 SD [min\u2013max] for each confirmed numeric diagnostic variable by group"),
               DT::dataTableOutput(ns("mfa_boruta_descriptive_table")),
               
               br(),
               downloadButton(ns("download_mfa_boruta_desc_csv"),  "Download Descriptive Stats (CSV)"),
               downloadButton(ns("download_mfa_boruta_desc_xlsx"), "Download Descriptive Stats (Excel)"),
               hr()
      )
      
    ) # end bedda_subtabs tabsetPanel
  )
}


mod_mfa_delim_server <- function(id, mfa_results_r, mfa_data_for_analysis_r, group_col_name_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Reactive values for BEDDA (returned to app_server for visualization module)
    mfa_bedda_unsup_results  <- reactiveVal(NULL)
    mfa_bedda_boruta_results <- reactiveVal(NULL)
    mfa_hypothesis_data      <- reactiveVal(NULL)
    mfa_bedda_results        <- reactiveVal(NULL)
    mfa_bedda_dim_info       <- reactiveVal(NULL)
    mfa_bedda_sup_results    <- reactiveVal(NULL)
    mfa_bedda_sup_models     <- reactiveVal(NULL)  # stores full cl_merge object
    
    # Alias: the extracted server code calls data_for_mfa_source_r() directly;
    # in this module it is passed as mfa_data_for_analysis_r.
    data_for_mfa_source_r <- mfa_data_for_analysis_r
    
    ## ============================================================================
    ## BEDDA SERVER: GMM SPECIES DELIMITATION ON MFA SCORES
    ## ============================================================================
    
    # -- Helper: extract MFA scores for BEDDA based on the shared variance slider --
    get_mfa_scores_for_bedda <- function() {
      mfa_res <- mfa_results_r()
      if (is.null(mfa_res) || is.null(mfa_res$ind$coord)) return(NULL)
      mfa_scores_full <- mfa_res$ind$coord
      cumvar    <- cumsum(mfa_res$eig[, 2])
      threshold <- input$mfa_bedda_variance_threshold
      n_dims    <- which(cumvar >= threshold)[1]
      if (is.na(n_dims)) {
        n_dims <- ncol(mfa_scores_full)
        showNotification(
          sprintf("Variance threshold (%.0f%%) not reached. Using all %d MFA dimensions (%.1f%% variance).",
                  threshold, n_dims, cumvar[n_dims]),
          type = "warning", duration = 6
        )
      }
      # Clamp to stored dimensions: mfa_res$eig has all eigenvalues but
      # mfa_res$ind$coord only stores ncp columns; without this, high thresholds
      # (especially 100%) resolve to an eig index beyond ncol(mfa_scores_full).
      n_dims <- min(n_dims, ncol(mfa_scores_full))
      list(
        scores     = as.matrix(mfa_scores_full[, seq_len(n_dims), drop = FALSE]),
        n_dims     = n_dims,
        total_dims = ncol(mfa_scores_full),
        cumvar_pct = cumvar[n_dims],
        threshold  = threshold
      )
    }
    
    ## ---- UNSUPERVISED CLUSTERING ----
    
    ## ---- Sensitivity test ----
    mfa_sensitivity_results <- reactiveVal(NULL)
    
    # Shared static info box used by both sensitivity tests
    stability_info_box <- tags$div(
      style = "background-color: #f8f9fa; border-left: 4px solid #6c757d; padding: 10px; margin-top: 10px; border-radius: 3px; font-size: 0.88em;",
      tags$p(style = "margin: 0;",
             tags$strong("About threshold stability:"),
             " Results may vary across variance thresholds. A consistent result across thresholds",
             " suggests a more robust signal, but consistency alone does not confirm the result is correct.",
             " A result that stabilises at a particular threshold range should be interpreted alongside",
             " taxonomic, geographic, and genetic evidence before drawing conclusions.")
    )
    
    observeEvent(input$run_mfa_sensitivity, {
      req(mfa_results_r(), mfa_results_r()$ind$coord)
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Testing thresholds 70\u2013100%\u2014 this may take a moment..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      mfa_res_full    <- mfa_results_r()
      mfa_scores_full <- mfa_res_full$ind$coord
      cumvar          <- cumsum(mfa_res_full$eig[, 2])
      thresholds      <- seq(70, 100, by = 5)
      max_g           <- as.integer(input$mfa_bedda_max_clusters)
      if (is.na(max_g) || max_g < 1) {
        showNotification("Please enter a value for Maximum clusters before running.", type = "warning")
        return(NULL)
      }
      
      rows <- lapply(thresholds, function(thr) {
        n_dims <- which(cumvar >= thr)[1]
        if (is.na(n_dims)) n_dims <- ncol(mfa_scores_full)
        n_dims <- max(2L, min(ncol(mfa_scores_full), n_dims))
        scores_thr <- as.matrix(mfa_scores_full[, seq_len(n_dims), drop = FALSE])
        mod <- tryCatch(
          mclust::Mclust(scores_thr, G = 1:max_g, verbose = FALSE),
          error = function(e) NULL
        )
        data.frame(
          `Threshold (%)` = thr,
          `MFA dimensions` = n_dims,
          `Number of clusters` = if (is.null(mod)) NA_integer_ else as.integer(mod$G),
          `Best model`         = if (is.null(mod)) NA_character_ else mod$modelName,
          check.names = FALSE, stringsAsFactors = FALSE
        )
      })
      
      result_df <- dplyr::bind_rows(rows)
      mfa_sensitivity_results(result_df)
    })
    
    output$mfa_sensitivity_results_ui <- renderUI({
      df <- mfa_sensitivity_results()
      if (is.null(df)) return(NULL)
      tagList(
        tableOutput(ns("mfa_sensitivity_table")),
        stability_info_box
      )
    })
    
    output$mfa_sensitivity_table <- renderTable({
      req(mfa_sensitivity_results())
      mfa_sensitivity_results()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "—")
    
    ## ---- Unsupervised clustering (full) ----
    observeEvent(input$run_mfa_unsupervised, {
      if (is.null(mfa_results_r()) || is.null(mfa_results_r()$ind$coord)) {
        showModal(modalDialog(
          title = "No MFA Results",
          "Please run MFA first (see 'MFA Analysis & Results' tab) before performing this analysis.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Fitting GMM models on MFA scores \u2014 this may take a moment..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      dim_data <- get_mfa_scores_for_bedda()
      if (is.null(dim_data)) return(NULL)
      mfa_scores <- dim_data$scores
      
      max_g <- as.integer(input$mfa_bedda_max_clusters)
      if (is.na(max_g) || max_g < 1) {
        showNotification("Please enter a value for Maximum clusters before running.", type = "warning")
        return(NULL)
      }
      
      group_col   <- group_col_name_r()
      data_used   <- mfa_data_for_analysis_r()
      species_col <- data_used[[group_col]]
      
      tryCatch({
        data_mod <- mclust::Mclust(mfa_scores, G = 1:max_g)
        
        if (is.null(data_mod)) {
          showNotification("Mclust failed to fit models. Check your data and dimension selection.", type = "error")
          return(NULL)
        }
        
        # Build top-models BIC table
        bic_matrix  <- data_mod$BIC
        model_names <- colnames(bic_matrix)
        bic_list    <- list()
        for (i in seq_len(nrow(bic_matrix))) {
          for (j in seq_along(model_names)) {
            bv <- bic_matrix[i, j]
            if (!is.na(bv)) {
              bic_list[[length(bic_list) + 1]] <- data.frame(
                G     = as.numeric(rownames(bic_matrix)[i]),
                Model = model_names[j],
                BIC   = bv
              )
            }
          }
        }
        bic_long  <- dplyr::bind_rows(bic_list) %>% dplyr::arrange(desc(BIC))
        best_bic  <- max(bic_long$BIC, na.rm = TRUE)
        bic_long  <- bic_long %>%
          dplyr::mutate(Delta_BIC = best_bic - BIC, Rank = dplyr::row_number())
        top_models <- bic_long %>%
          dplyr::filter(Rank <= 20 | Delta_BIC < 10) %>%
          dplyr::select(Rank, G, Model, BIC, Delta_BIC)
        
        cluster_table <- table(OTU = species_col, Cluster = data_mod$classification)
        
        mfa_bedda_unsup_results(list(
          model         = data_mod,
          cluster_table = cluster_table,
          species_col   = species_col,
          top_models    = top_models,
          dim_info      = dim_data
        ))
        
        showNotification("Unsupervised clustering complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error in unsupervised clustering:", e$message),
                         type = "error", duration = 10)
      })
    })
    
    output$mfa_unsup_cluster_summary <- renderPrint({
      req(mfa_bedda_unsup_results())
      results <- mfa_bedda_unsup_results()
      model   <- results$model
      di      <- results$dim_info
      
      cat("===========================================================\n")
      cat("     UNSUPERVISED CLUSTERING ON MFA SCORES\n")
      cat("===========================================================\n\n")
      cat(sprintf("MFA dimensions used: %d of %d (%.1f%% cumulative variance)\n\n",
                  di$n_dims, di$total_dims, di$cumvar_pct))
      cat("Best Model:\n")
      cat("  Model Type: ", model$modelName, "\n", sep = "")
      cat("  Optimal G:  ", model$G, " clusters\n", sep = "")
      cat("  BIC:        ", round(model$bic, 2), "\n\n", sep = "")
      cat("Cluster Sizes:\n")
      print(table(model$classification))
    })
    
    # Per-specimen cluster assignment table with posteriors
    output$mfa_unsup_cluster_specimen_table <- DT::renderDataTable({
      req(mfa_bedda_unsup_results())
      results <- mfa_bedda_unsup_results()
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
    
    output$download_mfa_unsup_csv <- downloadHandler(
      filename = function() paste0("mfa_unsupervised_clustering_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(mfa_bedda_unsup_results())
        results <- mfa_bedda_unsup_results()
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
    
    output$mfa_unsup_top_models_table <- DT::renderDataTable({
      req(mfa_bedda_unsup_results())
      DT::datatable(
        mfa_bedda_unsup_results()$top_models,
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("BIC", "Delta_BIC"), digits = 2) %>%
        DT::formatStyle(
          "Delta_BIC",
          backgroundColor = DT::styleInterval(
            cuts   = c(2, 6, 10),
            values = c("#90EE90", "#FFFFE0", "#FFE4B5", "#FFB6C1")
          )
        )
    })
    
    output$mfa_unsup_model_details <- renderPrint({
      req(mfa_bedda_unsup_results())
      results <- mfa_bedda_unsup_results()
      model   <- results$model
      
      cat("===========================================================\n")
      cat("           BEST MODEL DETAILED SUMMARY\n")
      cat("===========================================================\n\n")
      cat("Model Selection:\n")
      cat("  Model Type:    ", model$modelName, "\n", sep = "")
      cat("  # Clusters:    ", model$G, "\n", sep = "")
      cat("  BIC:           ", round(model$bic, 2), "\n", sep = "")
      cat("  Log-likelihood:", round(model$loglik, 2), "\n", sep = "")
      cat("  # Parameters:  ", model$df, "\n", sep = "")
      cat("  # Observations:", model$n, "\n\n", sep = "")
      cat("Covariance Model:\n")
      cat("  ", model$modelName, " = ", get_model_interpretation(model$modelName), "\n\n", sep = "")
      if (!is.null(results$top_models)) {
        top5 <- results$top_models[seq_len(min(5, nrow(results$top_models))), ]
        cat("Top 5 Models by BIC:\n")
        print(top5, row.names = FALSE)
      }
    })
    
    output$download_mfa_unsup_top_models_csv <- downloadHandler(
      filename = function() paste0("mfa_unsup_top_models_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(mfa_bedda_unsup_results()$top_models)
        write.csv(mfa_bedda_unsup_results()$top_models, file, row.names = FALSE)
      }
    )
    
    ## ---- SUPERVISED CLUSTERING ----
    
    ## ---- PHYLOGENETIC HYPOTHESIS TESTING ----
    
    # Helper: enumerate all monophyletic partitions consistent with a tree
    # generate_all_phylo_hypotheses is defined in global.R
    
    ## ---- Tree reactives ----
    phylo_tree_r        <- reactiveVal(NULL)
    phylo_raw_tree_r    <- reactiveVal(NULL)
    phylo_outgroup_r    <- reactiveVal(character(0))
    phylo_mode_r        <- reactiveVal("otu")
    phylo_unassigned_r  <- reactiveVal(character(0))
    phylo_assignments_r <- reactiveVal(list())
    phylo_rooted_r      <- reactiveVal(FALSE)  # TRUE = tree arrived already rooted
    
    ## A tree is properly rooted when its root node has exactly 2 children
    is_properly_rooted_mfa <- function(tree) {
      root_node     <- length(tree$tip.label) + 1L
      root_children <- sum(tree$edge[, 1L] == root_node)
      root_children == 2L
    }
    
    ## Parse any supported tree format (Newick or NEXUS, single tree only)
    load_any_format_tree_mfa <- function(path) {
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
    
    ## Collapse individual-specimen tree to OTU-level tree after mapping is confirmed
    ## (defined below near confirm observer for scoping clarity)
    
    ## Helper to reset all tree/assignment state
    reset_phylo_assignment_mfa <- function() {
      phylo_raw_tree_r(NULL)
      phylo_tree_r(NULL)
      phylo_outgroup_r(character(0))
      phylo_rooted_r(FALSE)
      phylo_mode_r("otu")
      phylo_unassigned_r(character(0))
      phylo_assignments_r(list())
    }
    
    ## Upload observer
    observeEvent(input$phylo_newick_file, {
      req(input$phylo_newick_file)
      reset_phylo_assignment_mfa()
      
      tree <- load_any_format_tree_mfa(input$phylo_newick_file$datapath)
      if (is.null(tree)) return()
      
      already_rooted <- is_properly_rooted_mfa(tree)
      phylo_raw_tree_r(tree)
      phylo_rooted_r(already_rooted)
      phylo_unassigned_r(sort(tree$tip.label))
      showNotification(
        paste0("Tree parsed: ", length(tree$tip.label), " tips. ",
               if (already_rooted)
                 "Tree appears to be rooted. Designate an outgroup below to re-root, or confirm as-is."
               else
                 "Tree is unrooted \u2014 designate an outgroup below to root it."),
        type = if (already_rooted) "message" else "warning", duration = 7)
    })
    
    observeEvent(input$load_example_tree, {
      path <- system.file("examples", "Gekko_tree.txt", package = "GroupStruct2")
      if (!file.exists(path)) {
        showNotification("Example tree file not found.", type = "error"); return()
      }
      reset_phylo_assignment_mfa()
      tree <- load_any_format_tree_mfa(path)
      if (!is.null(tree)) {
        phylo_raw_tree_r(tree)
        phylo_rooted_r(is_properly_rooted_mfa(tree))
        phylo_unassigned_r(sort(tree$tip.label))
      }
    })
    
    observeEvent(input$load_example_tree_ctenophorus, {
      path <- system.file("examples", "Ctenophorus_tree.txt", package = "GroupStruct2")
      if (!file.exists(path)) {
        showNotification("Ctenophorus tree file not found.", type = "error"); return()
      }
      reset_phylo_assignment_mfa()
      tree <- load_any_format_tree_mfa(path)
      if (!is.null(tree)) {
        phylo_raw_tree_r(tree)
        phylo_rooted_r(is_properly_rooted_mfa(tree))
        phylo_unassigned_r(sort(tree$tip.label))
      }
    })
    
    ## Status banner
    output$phylo_tree_status_ui <- renderUI({
      tree    <- phylo_raw_tree_r()
      if (is.null(tree)) return(NULL)
      og      <- phylo_outgroup_r()
      rooted  <- phylo_rooted_r()
      
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
               length(tree$tip.label), " tips parsed. ",
               rooting_msg)
    })
    
    ## ---- Unified tip assignment + outgroup UI (Step 1b) ----
    output$phylo_tip_assign_ui <- renderUI({
      raw_tree <- phylo_raw_tree_r()
      if (is.null(raw_tree)) return(NULL)
      
      og           <- phylo_outgroup_r()
      unassigned   <- phylo_unassigned_r()
      assignments  <- phylo_assignments_r()
      data_used    <- mfa_data_for_analysis_r()
      group_col    <- group_col_name_r()
      data_species <- if (!is.null(data_used))
        sort(unique(as.character(data_used[[group_col]]))) else character(0)
      
      ingroup_tips <- setdiff(raw_tree$tip.label, og)
      otu_mode     <- (length(setdiff(ingroup_tips, data_species)) == 0) &&
        (length(setdiff(data_species, ingroup_tips)) == 0)
      
      # Initialise assignment slots if needed
      if (!otu_mode && (length(assignments) == 0 || !all(data_species %in% names(assignments)))) {
        init_asgn <- setNames(vector("list", length(data_species)), data_species)
        for (nm in names(init_asgn)) init_asgn[[nm]] <- assignments[[nm]] %||% character(0)
        isolate(phylo_assignments_r(init_asgn))
        assignments <- init_asgn
      }
      
      all_assigned <- otu_mode || (length(unassigned) == 0)
      
      # Outgroup row (amber) for the summary table
      og_row <- if (length(og) > 0) {
        list(tags$tr(
          style = "background-color: #fff3cd;",
          tags$td(style = "font-weight: bold; color: #856404; white-space: nowrap;",
                  icon("arrow-left"), " Outgroup"),
          tags$td(style = "font-size: 12px; color: #856404; word-break: break-word;",
                  paste(og, collapse = ", ")),
          tags$td(style = "text-align: center; color: #856404;", length(og)),
          tags$td(style = "text-align: center;",
                  actionButton(ns("phylo_clear_og_btn"), label = "", icon = icon("times"),
                               class = "btn-xs btn-warning",
                               onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'})",
                                                 ns("phylo_clear_og_clicked"))))
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
                        ns(paste0("phylo_clear_otu_", gsub("[^A-Za-z0-9_]", "_", otu))),
                        label = "", icon = icon("times"), class = "btn-xs btn-danger",
                        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                          ns("phylo_clear_otu_clicked"), otu)))
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
          ## Left: listbox — content differs by mode
          column(6,
                 if (otu_mode) {
                   ## OTU mode: tips already match — show success + outgroup-selection listbox
                   tagList(
                     tags$div(class = "alert alert-success",
                              style = "margin: 0 0 8px 0; padding: 8px; font-size: 0.9em;",
                              icon("check"),
                              paste0(" All ", length(ingroup_tips),
                                     " ingroup tips match OTU names \u2014 no assignment needed.")),
                     tags$div(style = "margin-bottom: 4px; font-size: 0.88em; color: #555;",
                              "Select tips to designate as outgroup (optional):"),
                     tags$select(
                       id = ns("phylo_tip_select"),
                       multiple = "multiple",
                       size = as.character(min(10, max(4, length(ingroup_tips)))),
                       style = paste0("width: 100%; font-family: monospace; font-size: 12px;",
                                      " border: 1px solid #ced4da; border-radius: 4px; padding: 4px;",
                                      " background: white; min-height: 80px;"),
                       lapply(ingroup_tips, function(tip) tags$option(value = tip, tip))
                     )
                   )
                 } else {
                   ## Individual mode: show unassigned specimen pool
                   tagList(
                     tags$div(style = "margin-bottom: 6px;",
                              strong(paste0("Unassigned specimens (", length(unassigned), " remaining):"))),
                     if (length(unassigned) == 0) {
                       tags$div(class = "alert alert-success", style = "margin: 0; padding: 8px;",
                                icon("check"), " All specimens assigned!")
                     } else {
                       tags$select(
                         id = ns("phylo_tip_select"),
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
          ## Right: assign to OTU OR designate as outgroup
          column(6,
                 if (!otu_mode) {
                   tagList(
                     tags$div(style = "margin-bottom: 4px;", strong("Assign to OTU:")),
                     selectInput(ns("phylo_target_otu"), label = NULL,
                                 choices = data_species, selected = data_species[1], width = "100%"),
                     actionButton(ns("phylo_assign_btn"),
                                  label = tagList(icon("arrow-right"), " Assign to OTU"),
                                  class = "btn-primary btn-block",
                                  disabled = if (length(unassigned) == 0) "disabled" else NULL),
                     tags$hr(style = "margin: 8px 0; border-top: 1px dashed #ccc;"),
                     tags$p(style = "text-align: center; margin: 4px 0; font-size: 0.85em; color: #888;", "\u2014 or \u2014")
                   )
                 },
                 actionButton(ns("phylo_designate_og_btn"),
                              label = tagList(icon("arrow-left"), " Designate as Outgroup"),
                              class = "btn-warning btn-block",
                              disabled = if (length(unassigned) == 0) "disabled" else NULL),
                 tags$p(style = "font-size: 0.8em; color: #888; margin-top: 4px;",
                        if (!phylo_rooted_r())
                          "Required: tree is unrooted. Select outgroup tip(s) to root the tree. Outgroup will be excluded from analysis."
                        else
                          "Optional: re-root on a specific outgroup tip. Leave empty to use the existing root."),
                 if (!otu_mode) {
                   tagList(
                     tags$hr(style = "margin: 8px 0;"),
                     actionButton(ns("phylo_clear_all_btn"),
                                  label = tagList(icon("trash"), " Clear All"),
                                  class = "btn-danger btn-sm btn-block")
                   )
                 }
          )
        ),
        
        ## Summary table
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
        
        ## Confirm button
        if (!phylo_rooted_r() && length(og) == 0) {
          ## Unrooted, no outgroup — block
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
            actionButton(ns("phylo_confirm_mapping"),
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
    observeEvent(input$phylo_designate_og_btn, {
      selected <- input$phylo_tip_select
      if (is.null(selected) || length(selected) == 0) {
        showNotification("No tips selected.", type = "warning"); return()
      }
      phylo_outgroup_r(sort(union(phylo_outgroup_r(), selected)))
      phylo_unassigned_r(setdiff(phylo_unassigned_r(), selected))
      phylo_assignments_r(lapply(phylo_assignments_r(), function(tips) setdiff(tips, selected)))
      phylo_tree_r(NULL)
    })
    
    ## Clear outgroup
    observeEvent(input$phylo_clear_og_clicked, {
      og <- phylo_outgroup_r()
      if (length(og) == 0) return()
      phylo_unassigned_r(sort(c(phylo_unassigned_r(), og)))
      phylo_outgroup_r(character(0))
      phylo_tree_r(NULL)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    ## Assign button
    observeEvent(input$phylo_assign_btn, {
      selected_tips <- input$phylo_tip_select
      target_otu    <- input$phylo_target_otu
      if (is.null(selected_tips) || length(selected_tips) == 0) {
        showNotification("No specimens selected. Use Ctrl/Cmd+click to select.", type = "warning")
        return()
      }
      asgn <- phylo_assignments_r()
      asgn[[target_otu]] <- unique(c(asgn[[target_otu]], selected_tips))
      phylo_assignments_r(asgn)
      phylo_unassigned_r(setdiff(phylo_unassigned_r(), selected_tips))
      phylo_tree_r(NULL)
    })
    
    ## Clear-all assignments button
    observeEvent(input$phylo_clear_all_btn, {
      data_used    <- mfa_data_for_analysis_r()
      group_col    <- group_col_name_r()
      data_species <- if (!is.null(data_used))
        sort(unique(as.character(data_used[[group_col]]))) else character(0)
      raw_tree <- phylo_raw_tree_r()
      og       <- phylo_outgroup_r()
      # Restore all non-outgroup tips to unassigned
      if (!is.null(raw_tree))
        phylo_unassigned_r(sort(setdiff(raw_tree$tip.label, og)))
      init_asgn <- setNames(vector("list", length(data_species)), data_species)
      for (nm in names(init_asgn)) init_asgn[[nm]] <- character(0)
      phylo_assignments_r(init_asgn)
      phylo_tree_r(NULL)
    })
    
    ## Per-OTU clear button
    observeEvent(input$phylo_clear_otu_clicked, {
      otu  <- input$phylo_clear_otu_clicked
      asgn <- phylo_assignments_r()
      if (!otu %in% names(asgn)) return()
      tips_to_restore <- asgn[[otu]]
      asgn[[otu]] <- character(0)
      phylo_assignments_r(asgn)
      phylo_unassigned_r(sort(c(phylo_unassigned_r(), tips_to_restore)))
      phylo_tree_r(NULL)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ## Confirm mapping: root on outgroup (if given), prune outgroup, collapse to OTU tree
    observeEvent(input$phylo_confirm_mapping, {
      raw_tree    <- phylo_raw_tree_r(); req(raw_tree)
      og          <- phylo_outgroup_r()
      already_rooted <- phylo_rooted_r()
      ingroup_tips <- setdiff(raw_tree$tip.label, og)
      
      data_used    <- mfa_data_for_analysis_r()
      group_col    <- group_col_name_r()
      data_species <- if (!is.null(data_used))
        sort(unique(as.character(data_used[[group_col]]))) else character(0)
      
      # Safety guard — should not be reachable due to UI block, but belt-and-suspenders
      if (!already_rooted && length(og) == 0) {
        showNotification(
          "Tree is unrooted and no outgroup has been designated. Please select an outgroup.",
          type = "error", duration = 8); return()
      }
      
      # Check specimen mapping completeness
      otu_mode <- (length(setdiff(ingroup_tips, data_species)) == 0) &&
        (length(setdiff(data_species, ingroup_tips)) == 0)
      
      if (!otu_mode) {
        assignments <- phylo_assignments_r()
        empty_otus  <- data_species[sapply(data_species, function(o) length(assignments[[o]]) == 0)]
        if (length(empty_otus) > 0) {
          showNotification(
            paste0("Cannot confirm: OTUs with no specimens assigned: ",
                   paste(empty_otus, collapse = ", "), "."),
            type = "error", duration = 10); return()
        }
      }
      
      # Step 1: Root on outgroup if provided; otherwise use existing root as-is
      if (length(og) > 0) {
        tree_rooted <- tryCatch(
          ape::root(raw_tree, outgroup = og, resolve.root = TRUE),
          error = function(e) {
            showNotification(paste("Outgroup rooting failed:", e$message),
                             type = "error"); NULL
          }
        )
        if (is.null(tree_rooted)) return()
      } else {
        tree_rooted <- raw_tree  # already rooted, use as-is
      }
      
      # Step 2: Prune outgroup tips
      tree_ingroup <- if (length(og) > 0)
        ape::keep.tip(tree_rooted, ingroup_tips)
      else
        tree_rooted
      
      # Step 3: Collapse individual tips to OTU labels if needed
      tree_final <- if (otu_mode) tree_ingroup else
        collapse_individuals_to_otu_mfa(tree_ingroup, phylo_assignments_r())
      
      phylo_tree_r(tree_final)
      root_msg <- if (length(og) > 0)
        paste0("Rooted on outgroup (", paste(og, collapse = ", "), "), outgroup pruned.")
      else
        "Existing root used as-is."
      showNotification(
        paste0("Tree built: ", length(tree_final$tip.label), " taxa. ", root_msg),
        type = "message")
    })
    
    output$phylo_tree_preview_ui <- renderUI({
      if (is.null(phylo_tree_r())) return(NULL)
      tagList(
        hr(),
        h5(strong("OTU-level tree preview:")),
        plotOutput(ns("phylo_tree_preview"), height = "420px")
      )
    })
    
    output$phylo_tree_preview <- renderPlot({
      tree <- phylo_tree_r(); req(tree)
      n     <- length(tree$tip.label)
      cex_t <- max(0.55, min(1.0, 20 / n))
      has_nl <- !is.null(tree$node.label) && any(nchar(trimws(tree$node.label)) > 0)
      par(mar = c(1, 1, 2, max(4, nchar(max(tree$tip.label)) * 0.55)))
      ape::plot.phylo(tree, type = "cladogram", use.edge.length = FALSE,
                      show.tip.label = TRUE, cex = cex_t,
                      show.node.label = isTRUE(input$phylo_show_support) && has_nl,
                      label.offset = 0.5, no.margin = FALSE)
      title(main = paste0("OTU-level tree (", n, " taxa)"), cex.main = 1.1)
    })
    
    ## Helper (placed here for module-local scope)
    collapse_individuals_to_otu_mfa <- function(raw_tree, assignments) {
      otus     <- names(assignments)
      rep_tips <- sapply(otus, function(otu) sort(assignments[[otu]])[1])
      tree_pruned <- ape::keep.tip(raw_tree, unname(rep_tips))
      for (otu in otus) {
        idx <- which(tree_pruned$tip.label == rep_tips[[otu]])
        if (length(idx) > 0) tree_pruned$tip.label[idx] <- otu
      }
      tree_pruned
    }
    
    observeEvent(input$run_phylo_analysis, {
      if (is.null(mfa_results_r()) || is.null(mfa_results_r()$ind$coord)) {
        showModal(modalDialog(
          title = "No MFA Results",
          "Please run MFA first before performing this analysis.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return(NULL)
      }
      if (is.null(phylo_tree_r())) {
        showNotification(
          "Please upload a tree file and complete the specimen-to-OTU mapping (if required) before running.",
          type = "error", duration = 8)
        return()
      }
      
      group_col    <- group_col_name_r()
      data_used    <- mfa_data_for_analysis_r()
      species_col  <- data_used[[group_col]]
      
      # Prune tree to only tips matching OTU names in the dataset
      tree_for_hyp <- phylo_tree_r()
      keep_tips    <- intersect(tree_for_hyp$tip.label, unique(as.character(species_col)))
      if (length(keep_tips) < 2) {
        showNotification(
          paste0("Only ", length(keep_tips), " tree tip(s) match OTU names in the dataset.",
                 " Check that tip labels match OTU names exactly."),
          type = "error", duration = 10)
        return()
      }
      if (length(keep_tips) < length(tree_for_hyp$tip.label))
        tree_for_hyp <- ape::keep.tip(tree_for_hyp, keep_tips)
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Generating and testing all phylogenetic hypotheses \u2014 this may take a moment..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      # Extract MFA scores at selected threshold
      mfa_res_full    <- mfa_results_r()
      mfa_scores_full <- mfa_res_full$ind$coord
      cumvar          <- cumsum(mfa_res_full$eig[, 2])
      threshold       <- input$mfa_phylo_variance_threshold %||% 90
      n_dims <- which(cumvar >= threshold)[1]
      if (is.na(n_dims)) n_dims <- ncol(mfa_scores_full)
      n_dims <- max(2L, min(ncol(mfa_scores_full), n_dims))
      mfa_scores <- as.matrix(mfa_scores_full[, seq_len(n_dims), drop = FALSE])
      
      # Generate all phylogenetically valid hypotheses
      phylo_hyps <- generate_all_phylo_hypotheses(tree_for_hyp, species_col)
      if (!is.null(phylo_hyps$error)) {
        showNotification(phylo_hyps$error, type = "error")
        return()
      }
      
      if (phylo_hyps$capped)
        showNotification(
          paste0("Hypothesis enumeration capped at 20,000 combinations for large trees.",
                 " Some composite hypotheses may not be represented."),
          type = "warning", duration = 8
        )
      
      hyp_df        <- phylo_hyps$hyp_df
      display_names <- phylo_hyps$display_names
      k_values      <- phylo_hyps$k_values
      n_hyp         <- ncol(hyp_df)
      
      if (n_hyp == 0) {
        showNotification("No valid hypotheses generated from this tree.", type = "error")
        return()
      }
      
      priors <- setNames(rep(1 / n_hyp, n_hyp), names(hyp_df))
      
      tryCatch({
        models <- list()
        for (hyp_name in names(hyp_df)) {
          models[[hyp_name]] <- mclust::MclustDA(
            as.matrix(mfa_scores),
            class     = factor(hyp_df[[hyp_name]]),
            modelType = "EDDA",
            verbose   = FALSE
          )
        }
        
        results_raw  <- do.call(GMMBayesFactorTable, c(models, list(prior = priors)))
        # GMMBayesFactorTable already contains Hypothesis, Taxonomic groupings, K columns.
        # Sort by BIC and extract ordered internal keys from the Hypothesis column.
        results_table <- results_raw[order(-results_raw[["BIC"]]), ]
        rownames(results_table) <- NULL
        ordered_keys  <- results_table[["Hypothesis"]]  # internal keys, BIC-ordered
        
        mfa_bedda_sup_results(results_table)
        mfa_bedda_sup_models(list(
          summary_table    = results_table,
          species_col      = species_col,
          mfa_scores       = mfa_scores,
          hyp_df           = hyp_df,
          display_names    = display_names,
          k_values         = k_values,
          ordered_hyp_keys = ordered_keys,
          tree_used        = phylo_hyps$tree_used
        ))
        
        showNotification(
          paste0("Done! Tested ", n_hyp, " phylogenetic hypotheses."),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    output$mfa_phylo_results_table <- DT::renderDataTable({
      req(mfa_bedda_sup_results())
      tbl <- head(mfa_bedda_sup_results(), 10)
      DT::datatable(
        tbl,
        class = "display nowrap",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "t"
        ),
        rownames = FALSE
      )
    })
    
    output$mfa_phylo_interpretation <- renderUI({
      req(mfa_bedda_sup_results())
      tagList(
        h5("Interpretation Guide:"),
        p(strong("BIC:"), "Higher = better model fit. The top-ranked hypothesis is best supported (\u0394BIC = 0)."),
        p(strong("\u0394BIC:"), "Difference from the best hypothesis.",
          tags$ul(
            tags$li("\u0394BIC < 2: Weak evidence against this hypothesis"),
            tags$li("\u0394BIC 2\u20136: Positive evidence against"),
            tags$li("\u0394BIC 6\u201310: Strong evidence against"),
            tags$li("\u0394BIC > 10: Very strong evidence against")
          )
        ),
        p(strong("BF:"), "Bayes Factor = exp(\u0394BIC / 2). Evidence against each hypothesis relative to the best."),
        p(strong("PostMod:"), "Posterior probability that each hypothesis is correct, given equal priors.")
      )
    })
    
    output$download_mfa_phylo_csv <- downloadHandler(
      filename = function() paste0("mfa_phylo_hypotheses_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(mfa_bedda_sup_results())
        write.csv(mfa_bedda_sup_results(), file, row.names = FALSE)
      }
    )
    output$download_mfa_phylo_xlsx <- downloadHandler(
      filename = function() paste0("mfa_phylo_hypotheses_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(mfa_bedda_sup_results())
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Phylo Hypotheses")
        openxlsx::writeData(wb, "Phylo Hypotheses", mfa_bedda_sup_results())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    ## ---- Phylogenetic sensitivity test (Option B) ----
    # Re-fits the same hypothesis set at each threshold; tree and hypotheses fixed.
    mfa_phylo_sensitivity_results <- reactiveVal(NULL)
    
    observeEvent(input$run_phylo_sensitivity, {
      sup <- mfa_bedda_sup_models()
      if (is.null(sup) || is.null(sup$hyp_df)) {
        showNotification(
          "Run the full Topology-aware Hypothesis Testing first (Step 2).",
          type = "error"); return()
      }
      if (is.null(mfa_results_r()) || is.null(mfa_results_r()$ind$coord)) {
        showNotification("MFA results not available.", type = "error"); return()
      }
      
      hyp_df          <- sup$hyp_df
      n_hyp           <- ncol(hyp_df)
      mfa_res_full    <- mfa_results_r()
      mfa_scores_full <- mfa_res_full$ind$coord
      cumvar          <- cumsum(mfa_res_full$eig[, 2])
      thresholds      <- seq(70, 100, by = 5)
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = paste0("Sweeping thresholds 70\u2013100% across ", n_hyp,
                      " hypotheses \u2014 this may take a moment...")
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      rows <- lapply(thresholds, function(thr) {
        n_dims <- which(cumvar >= thr)[1]
        if (is.na(n_dims)) n_dims <- ncol(mfa_scores_full)
        n_dims <- max(2L, min(ncol(mfa_scores_full), n_dims))
        scores_thr <- as.matrix(mfa_scores_full[, seq_len(n_dims), drop = FALSE])
        
        priors <- rep(1 / n_hyp, n_hyp)
        names(priors) <- colnames(hyp_df)
        
        models <- tryCatch({
          lapply(setNames(colnames(hyp_df), colnames(hyp_df)), function(hn) {
            mclust::MclustDA(scores_thr, factor(hyp_df[[hn]]),
                             modelType = "EDDA", verbose = FALSE)
          })
        }, error = function(e) NULL)
        
        if (is.null(models)) {
          return(data.frame(
            `Threshold (%)` = thr, `MFA dims` = n_dims,
            `Top hypothesis` = NA_character_,
            check.names = FALSE, stringsAsFactors = FALSE))
        }
        
        res <- tryCatch(
          do.call(GMMBayesFactorTable, c(models, list(prior = priors))),
          error = function(e) NULL)
        
        if (is.null(res)) {
          return(data.frame(
            `Threshold (%)` = thr, `MFA dims` = n_dims,
            `Top hypothesis` = NA_character_,
            check.names = FALSE, stringsAsFactors = FALSE))
        }
        
        best <- res[which.max(res$BIC), ]
        data.frame(
          `Threshold (%)` = thr,
          `MFA dims`      = n_dims,
          `Top hypothesis` = as.character(best$Hypothesis),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      })
      
      mfa_phylo_sensitivity_results(dplyr::bind_rows(rows))
      showNotification("Topology-aware sensitivity test complete.", type = "message")
    })
    
    output$mfa_phylo_sensitivity_ui <- renderUI({
      df <- mfa_phylo_sensitivity_results()
      if (is.null(df)) return(NULL)
      tagList(
        tableOutput(ns("mfa_phylo_sensitivity_table")),
        stability_info_box
      )
    })
    
    output$mfa_phylo_sensitivity_table <- renderTable({
      req(mfa_phylo_sensitivity_results())
      mfa_phylo_sensitivity_results()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "\u2014")
    
    ## ---- HYPOTHESIS TESTING ----
    
    # Flag for conditionalPanel
    output$mfa_hyp_file_loaded <- reactive({ !is.null(mfa_hypothesis_data()) })
    outputOptions(output, "mfa_hyp_file_loaded", suspendWhenHidden = FALSE)
    
    # Shared validation helper for hypothesis data frames
    validate_and_store_hyp <- function(hyp_df, source_label = "file") {
      original_data <- data_for_mfa_source_r()
      
      if (ncol(hyp_df) < 1) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   "Error: File must have at least 1 hypothesis column.")
        })
        mfa_hypothesis_data(NULL); return(FALSE)
      }
      
      all_numeric_cols <- sapply(hyp_df, function(col) {
        all(grepl("^-?[0-9]*\\.?[0-9]+$", as.character(col[!is.na(col)])))
      })
      if (any(all_numeric_cols)) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste0("Error: The following columns contain only numeric values: ",
                          paste(names(hyp_df)[all_numeric_cols], collapse = ", "),
                          ". Hypothesis labels must be categorical (e.g., 'Species_A', 'Lump_AB')."))
        })
        mfa_hypothesis_data(NULL); return(FALSE)
      }
      
      if (!is.null(original_data) && nrow(hyp_df) != nrow(original_data)) {
        msg <- if (source_label == "example") {
          paste0("Warning: Example hypothesis file has ", nrow(hyp_df),
                 " rows but your current data has ", nrow(original_data),
                 " rows. Please load the corresponding example mixed dataset first.")
        } else {
          paste0("Error: Hypothesis file has ", nrow(hyp_df),
                 " rows but input data has ", nrow(original_data),
                 " rows. They must match exactly.")
        }
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = if (source_label == "example") "alert alert-warning" else "alert alert-danger", msg)
        })
        mfa_hypothesis_data(NULL); return(FALSE)
      }
      
      mfa_hypothesis_data(hyp_df)
      output$mfa_hyp_upload_status <- renderUI({
        tags$div(class = "alert alert-success",
                 paste0("\u2713 Hypothesis file loaded successfully. Found ",
                        ncol(hyp_df), " hypotheses: ",
                        paste(colnames(hyp_df), collapse = ", ")))
      })
      return(TRUE)
    }
    
    observeEvent(input$mfa_hyp_file, {
      req(input$mfa_hyp_file)
      tryCatch({
        hyp_df <- read.csv(input$mfa_hyp_file$datapath, stringsAsFactors = FALSE)
        validate_and_store_hyp(hyp_df, source_label = "file")
      }, error = function(e) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger", paste0("Error loading file: ", e$message))
        })
        mfa_hypothesis_data(NULL)
      })
    })
    
    observeEvent(input$load_example_mfa_hyp, {
      original_data <- data_for_mfa_source_r()
      if (is.null(original_data) || nrow(original_data) == 0) {
        showModal(modalDialog(
          title = "No Data Loaded",
          "Please load data in the 'Input Data' module before loading the example hypothesis file.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return(NULL)
      }
      example_path <- system.file("examples", "Gekko_hypothesis_file.csv", package = "GroupStruct2")
      if (!file.exists(example_path)) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger", "Error: Example hypothesis file not found in package.")
        })
        return()
      }
      tryCatch({
        hyp_df <- read.csv(example_path, stringsAsFactors = FALSE)
        validate_and_store_hyp(hyp_df, source_label = "example")
      }, error = function(e) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger", paste0("Error loading example file: ", e$message))
        })
        mfa_hypothesis_data(NULL)
      })
    })
    
    output$mfa_hyp_preview <- renderDT({
      req(mfa_hypothesis_data())
      DT::datatable(mfa_hypothesis_data(),
                    options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
                    rownames = FALSE)
    })
    
    observeEvent(input$run_mfa_bedda, {
      if (is.null(mfa_results_r()) || is.null(mfa_results_r()$ind$coord)) {
        showModal(modalDialog(
          title = "No MFA Results",
          "Please run MFA first (see 'MFA Analysis & Results' tab) before performing Bayesian hypothesis testing.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return(NULL)
      }
      if (is.null(mfa_hypothesis_data())) {
        showNotification("Please upload a hypothesis file first.", type = "error")
        return()
      }
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Fitting EDDA models for each hypothesis \u2014 this may take several minutes..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      dim_data   <- get_mfa_scores_for_bedda()
      if (is.null(dim_data)) return(NULL)
      mfa_scores <- dim_data$scores
      
      # Snapshot dimension info for the interpretation box
      mfa_bedda_dim_info(dim_data)
      
      hyp_df    <- mfa_hypothesis_data()
      hyp_names <- colnames(hyp_df)
      n_hyp     <- length(hyp_names)
      priors    <- rep(1 / n_hyp, n_hyp)
      names(priors) <- hyp_names
      
      tryCatch({
        models <- list()
        for (hyp_name in hyp_names) {
          models[[hyp_name]] <- mclust::MclustDA(
            as.matrix(mfa_scores),
            class     = factor(hyp_df[[hyp_name]]),
            modelType = "EDDA",
            verbose   = FALSE
          )
        }
        results_table <- do.call(GMMBayesFactorTable, c(models, list(prior = priors)))
        results_table <- results_table[order(-results_table$BIC), ]
        mfa_bedda_results(results_table)
        showNotification("Bayesian hypothesis testing complete!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    output$mfa_bedda_results_table <- DT::renderDataTable({
      req(mfa_bedda_results())
      DT::datatable(mfa_bedda_results(),
                    options = list(pageLength = 20, scrollX = TRUE, dom = "tp"),
                    rownames = FALSE)
    })
    
    output$mfa_bedda_interpretation <- renderUI({
      req(mfa_bedda_results())
      dim_info <- mfa_bedda_dim_info()
      tagList(
        if (!is.null(dim_info)) {
          tags$div(
            style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
            p(style = "margin: 0;",
              strong("Dimensions used in this analysis: "),
              sprintf("%d of %d MFA dimensions (%.1f%% cumulative variance, threshold: %.0f%%)",
                      dim_info$n_dims, dim_info$total_dims,
                      dim_info$cumvar_pct, dim_info$threshold)
            )
          )
        },
        h5("Interpretation Guide:"),
        p(strong("BIC (Bayesian Information Criterion):"),
          "Higher values indicate better model fit (mclust convention: BIC = 2\u00b7log-likelihood \u2212 penalty).",
          "The hypothesis with the highest BIC is best supported (\u2206BIC = 0)."),
        p(strong("\u2206BIC:"), "Difference from the best model.",
          tags$ul(
            tags$li("\u2206BIC < 2: Weak evidence against this hypothesis"),
            tags$li("\u2206BIC 2\u20136: Positive evidence against"),
            tags$li("\u2206BIC 6\u201310: Strong evidence against"),
            tags$li("\u2206BIC > 10: Very strong evidence against")
          )
        ),
        p(strong("Bayes Factor (BF):"), "BF = exp(\u2206BIC/2).",
          tags$ul(
            tags$li("BF = 1: Equally supported as best hypothesis"),
            tags$li("BF > 3: Positive evidence against"),
            tags$li("BF > 20: Strong evidence against"),
            tags$li("BF > 150: Very strong evidence against")
          )
        ),
        p(strong("PostMod:"),
          "Posterior probability that each hypothesis is correct, given flat priors.",
          tags$ul(
            tags$li("PostMod > 0.75: Strong support"),
            tags$li("PostMod 0.50\u20130.75: Moderate support"),
            tags$li("PostMod < 0.50: Weak support")
          )
        ),
        p(strong("Note:"), "If no single hypothesis dominates (PostMod < 0.75),",
          "examine whether similar hypotheses collectively carry high probability.")
      )
    })
    
    output$download_mfa_bedda_csv <- downloadHandler(
      filename = function() paste0("mfa_bedda_delimitation_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(mfa_bedda_results())
        write.csv(mfa_bedda_results(), file, row.names = FALSE)
      }
    )
    
    output$download_mfa_bedda_xlsx <- downloadHandler(
      filename = function() paste0("mfa_bedda_delimitation_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(mfa_bedda_results())
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "GMM Delimitation Results")
        openxlsx::writeData(wb, "GMM Delimitation Results", mfa_bedda_results())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    ## ---- BORUTA DIAGNOSTIC CHARACTERS ----
    
    # Hypothesis selector: original OTU or any loaded hypothesis
    output$mfa_boruta_hypothesis_selector <- renderUI({
      if (!is.null(mfa_hypothesis_data()) && ncol(mfa_hypothesis_data()) > 0) {
        hyp_names <- colnames(mfa_hypothesis_data())
        selectInput(ns("mfa_boruta_hypothesis"),
                    "Select Hypothesis (grouping to test):",
                    choices  = c("Original OTU" = "original", hyp_names),
                    selected = "original")
      } else {
        p(em("No Bayesian hypothesis results available. Using original OTU labels."))
      }
    })
    
    # Group selector: populated from the chosen hypothesis
    output$mfa_boruta_group_selector <- renderUI({
      req(data_for_mfa_source_r())
      df        <- data_for_mfa_source_r()
      group_col <- group_col_name_r()
      
      if (!is.null(input$mfa_boruta_hypothesis) &&
          input$mfa_boruta_hypothesis != "original" &&
          !is.null(mfa_hypothesis_data())) {
        groups <- unique(mfa_hypothesis_data()[[input$mfa_boruta_hypothesis]])
      } else {
        groups <- unique(df[[group_col]])
      }
      groups <- sort(as.character(groups))
      
      checkboxGroupInput(ns("mfa_boruta_groups"),
                         "Groups to compare:",
                         choices  = groups,
                         selected = groups,
                         inline   = FALSE)
    })
    
    # Run Boruta
    observeEvent(input$run_mfa_boruta, {
      
      df <- data_for_mfa_source_r()
      if (is.null(df) || nrow(df) == 0) {
        showModal(modalDialog(
          title = "No Data",
          "Please load data in the 'Input Data' module first.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      if (is.null(input$mfa_boruta_groups) || length(input$mfa_boruta_groups) < 2) {
        showNotification("Please select at least 2 groups to compare.", type = "error")
        return()
      }
      
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        text = "Boruta analysis is running on mixed data \u2014 this may take several minutes..."
      )
      on.exit(shinybusy::remove_modal_spinner())
      
      group_col <- group_col_name_r()
      
      # Determine group labels from selected hypothesis
      if (!is.null(input$mfa_boruta_hypothesis) &&
          input$mfa_boruta_hypothesis != "original" &&
          !is.null(mfa_hypothesis_data())) {
        group_labels <- mfa_hypothesis_data()[[input$mfa_boruta_hypothesis]]
      } else {
        group_labels <- df[[group_col]]
      }
      
      # All trait columns (exclude group column)
      trait_cols <- setdiff(names(df), group_col)
      feat_data  <- df[, trait_cols, drop = FALSE]
      
      # Ensure factor columns stay as factors; coerce character to factor
      feat_data <- as.data.frame(lapply(feat_data, function(col) {
        if (is.character(col)) as.factor(col) else col
      }))
      
      # Filter to selected groups
      selected_idx        <- group_labels %in% input$mfa_boruta_groups
      feat_filtered       <- feat_data[selected_idx, , drop = FALSE]
      group_labels_filtered <- factor(group_labels[selected_idx])
      
      if (nrow(feat_filtered) < 10) {
        showNotification("Not enough specimens in selected groups (minimum 10 required).",
                         type = "error")
        return()
      }
      
      # Remove any columns that are all-NA after filtering
      all_na_cols <- sapply(feat_filtered, function(col) all(is.na(col)))
      if (any(all_na_cols)) {
        feat_filtered <- feat_filtered[, !all_na_cols, drop = FALSE]
        showNotification(
          paste0("Removed ", sum(all_na_cols), " column(s) with all-NA values after group filtering."),
          type = "warning", duration = 5
        )
      }
      
      if (ncol(feat_filtered) == 0) {
        showNotification("No valid columns remain after filtering.", type = "error")
        return()
      }
      
      boruta_data        <- feat_filtered
      boruta_data$Group  <- group_labels_filtered
      
      tryCatch({
        set.seed(input$mfa_boruta_seed)
        
        boruta_result <- Boruta::Boruta(
          Group ~ .,
          data    = boruta_data,
          doTrace = 0,
          maxRuns = input$mfa_boruta_maxruns,
          pValue  = input$mfa_boruta_pvalue
        )
        
        # All decisions ranked by importance
        real_vars <- names(boruta_result$finalDecision)
        all_decisions <- tibble::tibble(
          Variable         = real_vars,
          Decision         = as.character(boruta_result$finalDecision),
          Mean_Importance  = apply(
            boruta_result$ImpHistory[, real_vars, drop = FALSE],
            2, mean, na.rm = TRUE
          )
        ) %>% dplyr::arrange(desc(Mean_Importance))
        
        # Descriptive stats: confirmed NUMERIC variables only
        confirmed_numeric <- all_decisions %>%
          dplyr::filter(Decision == "Confirmed") %>%
          dplyr::filter(Variable %in% names(feat_filtered)[sapply(feat_filtered, is.numeric)])
        
        descriptive_stats <- if (nrow(confirmed_numeric) > 0) {
          stat_data <- cbind(
            Group = group_labels_filtered,
            feat_filtered[, confirmed_numeric$Variable, drop = FALSE]
          )
          stat_data %>%
            dplyr::group_by(Group) %>%
            dplyr::summarise(
              dplyr::across(
                dplyr::where(is.numeric),
                ~ paste0(sprintf("%.2f", mean(.x, na.rm = TRUE)),
                         " \u00b1 ", sprintf("%.2f", sd(.x, na.rm = TRUE)),
                         " [", sprintf("%.2f", min(.x, na.rm = TRUE)),
                         "\u2013", sprintf("%.2f", max(.x, na.rm = TRUE)), "]"),
                .names = "{.col}"
              ),
              .groups = "drop"
            )
        } else {
          showNotification(
            "No confirmed numeric variables found. Try increasing maxRuns or adjusting p-value.",
            type = "warning"
          )
          NULL
        }
        
        mfa_bedda_boruta_results(list(
          boruta_object   = boruta_result,
          all_decisions   = all_decisions,
          descriptive     = descriptive_stats,
          groups_used     = input$mfa_boruta_groups,
          hypothesis_used = input$mfa_boruta_hypothesis %||% "original"
        ))
        
        showNotification("Boruta analysis complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Display importance table
    output$mfa_boruta_importance_table <- DT::renderDataTable({
      req(mfa_bedda_boruta_results())
      tbl <- mfa_bedda_boruta_results()$all_decisions
      tbl$Mean_Importance <- round(tbl$Mean_Importance, 3)
      DT::datatable(tbl, options = list(pageLength = 20, scrollX = TRUE, dom = "tp"),
                    rownames = FALSE)
    })
    
    # Display descriptive stats table
    output$mfa_boruta_descriptive_table <- DT::renderDataTable({
      req(mfa_bedda_boruta_results())
      req(mfa_bedda_boruta_results()$descriptive)
      DT::datatable(mfa_bedda_boruta_results()$descriptive,
                    options = list(pageLength = 20, scrollX = TRUE, dom = "tp"),
                    rownames = FALSE)
    })
    
    # Downloads
    output$download_mfa_boruta_importance_csv <- downloadHandler(
      filename = function() paste0("mfa_boruta_importance_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(mfa_bedda_boruta_results())
        tbl <- mfa_bedda_boruta_results()$all_decisions
        tbl$Mean_Importance <- round(tbl$Mean_Importance, 3)
        write.csv(tbl, file, row.names = FALSE)
      }
    )
    output$download_mfa_boruta_importance_xlsx <- downloadHandler(
      filename = function() paste0("mfa_boruta_importance_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(mfa_bedda_boruta_results())
        tbl <- mfa_bedda_boruta_results()$all_decisions
        tbl$Mean_Importance <- round(tbl$Mean_Importance, 3)
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Variable Importance")
        openxlsx::writeData(wb, "Variable Importance", tbl)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    output$download_mfa_boruta_desc_csv <- downloadHandler(
      filename = function() paste0("mfa_boruta_descriptive_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(mfa_bedda_boruta_results())
        req(mfa_bedda_boruta_results()$descriptive)
        write.csv(mfa_bedda_boruta_results()$descriptive, file, row.names = FALSE)
      }
    )
    output$download_mfa_boruta_desc_xlsx <- downloadHandler(
      filename = function() paste0("mfa_boruta_descriptive_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(mfa_bedda_boruta_results())
        req(mfa_bedda_boruta_results()$descriptive)
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Descriptive Statistics")
        openxlsx::writeData(wb, "Descriptive Statistics", mfa_bedda_boruta_results()$descriptive)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    return(list(
      mfa_bedda_unsup_results_r  = mfa_bedda_unsup_results,
      mfa_bedda_boruta_results_r = mfa_bedda_boruta_results,
      mfa_bedda_sup_results_r    = mfa_bedda_sup_results,
      mfa_bedda_sup_models_r     = mfa_bedda_sup_models,
      mfa_hypothesis_data_r      = mfa_hypothesis_data
    ))
  })
}
