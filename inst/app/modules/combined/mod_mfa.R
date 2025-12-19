
mod_mfa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Multiple Factor Analysis (MFA)"),
    hr(),
    p("The number of dimensions to retain for the MFA (ncp) is dynamically selected based on what the data can support, but capped at 30. All retained dimensions are used in the PERMANOVA analysis."),
    p("If you use this module, please cite: Grismer, L. L. (2025). Introducing multiple factor analysis (MFA) as a diagnostic taxonomic tool complementing principal component analysis (PCA). ZooKeys, 1248, 93–109."),
    br(),
    tabsetPanel(id = ns("mfa_main_tabs"), 
                tabPanel("MFA Analysis & Results", 
                         br(),
                         hr(),
                         h4("MFA Configuration: Create and Assign Traits to Groups"),
                         p("Create custom groups (e.g., meristic, morphometric, bodyshape, color, etc) and assign traits to them. Each group must only contain one type of data (e.g., numerical or categorical. Do not mix numerical and categorical data in a single group). You can create multiple numerical or categorical groups as long as each group contains only a single type of data. Unselected traits will be excluded from the MFA."),
                         p(strong("Note 1:"), "If allometric correction was performed on morphometric data, it will automatically be used in the MFA. Otherwise, all traits will come from raw combined data."),
                         p(strong("Note 2:"), "At least 2 groups must be created."),
                         uiOutput(ns("variable_group_selection")),
                         br(),
                         actionButton(ns("perform_mfa"), "Perform MFA"),
                         br(),
                         textOutput(ns("mfa_status_message")), 
                         br(),
                         hr(),
                         h4("MFA Results Tables"),
                         tabsetPanel(
                           tabPanel("Eigenvalues",
                                    DTOutput(ns("mfa_eigen_table"))
                           ),
                           tabPanel("Group Contributions",
                                    DTOutput(ns("mfa_group_contrib_table"))
                           ),
                           tabPanel("Quantitative Variable Contributions",
                                    DTOutput(ns("mfa_quanti_contrib_table"))
                           ),
                           
                           tabPanel("Individuals Coordinates",
                                    DTOutput(ns("mfa_ind_coord_table"))
                           )
                         ),
                         br(),
                         downloadButton(ns("download_all_mfa_results"), "Download All MFA Tables"),
                         hr()
                ), 
                
                tabPanel("PERMANOVA on MFA Scores", 
                         br(),
                         h4("PERMANOVA Analysis on MFA Scores"),
                         p("This analysis tests for significant differences among group centroids in the ",
                           strong("MFA multivariate space"), " (defined by your calculated MFA dimensions)."),
                         
                         fluidRow(
                           column(6, numericInput(ns("permanova_permutations_mfa"), "Number of Permutations:", value = 50000, min = 100, step = 100)),
                           column(6, selectInput(ns("permanova_distance_method_mfa"), "Distance Method:",
                                                 choices = c("euclidean", "manhattan", "bray", "jaccard", "altGower"),
                                                 selected = "euclidean"))
                         ),
                         actionButton(ns("run_permanova_mfa"), "Run PERMANOVA on MFA Scores"),
                         br(), br(),
                         h5("Main PERMANOVA Results (adonis2) on MFA Scores:"),
                         verbatimTextOutput(ns("permanova_main_results_mfa")),
                         downloadButton(ns("download_permanova_main_mfa"), "Download Main Results"),
                         br(),
                         h5("Pairwise PERMANOVA Results on MFA Scores:"),
                         DTOutput(ns("permanova_pairwise_results_mfa")),
                         downloadButton(ns("download_permanova_pairwise_mfa"), "Download Pairwise Results")
                ),
                
                tabPanel("Bayesian Hypothesis Testing",
                         br(),
                         h4("Bayesian Hypothesis Testing on MFA Scores"),
                         
                         p("This analysis evaluates competing taxonomic hypotheses using Bayesian model comparison",
                           "on the", strong("MFA factor scores"), "(continuous variables derived from mixed data types).",
                           "The algorithm applies EDDA (Eigenvalue Decomposition Discriminant Analysis) to the MFA scores",
                           "and computes Bayesian Information Criterion (BIC) to determine which hypothesis",
                           "is best supported by the multivariate morphological structure."),
                         
                         tags$div(
                           style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                           p(style = "margin: 0;",
                             strong("How this works:"),
                             tags$ol(
                               tags$li("MFA transforms your mixed data (morphometric + meristic + categorical) into continuous factor scores"),
                               tags$li("EDDA models these scores under different taxonomic hypotheses"),
                               tags$li("BIC comparison identifies which hypothesis best fits the data"),
                               tags$li("This extends Bayesian species delimitation to datasets with mixed data types")
                             ))
                         ),
                         
                         p(strong("Important:"), 
                           "You must run MFA first (see 'MFA Analysis & Results' tab) before performing this analysis.",
                           "The hypothesis file must have the same number of rows as your original dataset."),
                         
                         hr(),
                         
                         h4("Upload Hypothesis File"),
                         p("Upload a CSV file with your taxonomic hypotheses.",
                           strong("Important:"), "The order of rows in this file", 
                           strong("must match exactly"), "the order of specimens in your original input data."),
                         
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
                         
                         fileInput(ns("mfa_hyp_file"), 
                                   "Choose CSV File", 
                                   accept = ".csv"),
                         
                         actionButton(ns("load_example_mfa_hyp"), 
                                      "Load Example Hypothesis File",
                                      icon = icon("table")),
                         br(),
                         p(style = "color: #856404; font-style: italic; margin-top: 10px;",
                           strong("Note:"), 
                           "The example hypothesis file is designed to work with the example mixed data dataset."),
                         
                         br(),
                         
                         uiOutput(ns("mfa_hyp_upload_status")),
                         
                         conditionalPanel(
                           condition = sprintf("output['%s']", ns("mfa_hyp_file_loaded")),
                           h5("Preview of Loaded Hypotheses:"),
                           DTOutput(ns("mfa_hyp_preview"))
                         ),
                         
                         hr(),
                         
                         h4("MFA Dimension Selection"),
                         p("Select which MFA dimensions to include in the analysis.",
                           "Using fewer dimensions reduces complexity and can improve model stability."),
                         
                         radioButtons(ns("mfa_dim_selection_method"),
                                      "Dimension Selection Method:",
                                      choices = c(
                                        "Cumulative Variance Explained (%)" = "variance",
                                        "Fixed Number of Dimensions" = "fixed",
                                        "All Dimensions" = "all"
                                      ),
                                      selected = "variance"),
                         
                         conditionalPanel(
                           condition = sprintf("input['%s'] == 'variance'", ns("mfa_dim_selection_method")),
                           sliderInput(ns("mfa_variance_threshold"),
                                       "Cumulative Variance Threshold (%):",
                                       min = 70, max = 100, value = 90, step = 5),
                           tags$div(
                             style = "background-color: #e7f3ff; border-left: 4px solid #2196F3; padding: 8px; margin: 10px 0;",
                             p(style = "margin: 0; font-size: 0.9em;",
                               icon("info-circle"), strong(" Recommended:"),
                               "90% variance threshold (default) retains dimensions that explain meaningful variation",
                               "while excluding low-variance dimensions that primarily contain noise.")
                           )
                         ),
                         
                         conditionalPanel(
                           condition = sprintf("input['%s'] == 'fixed'", ns("mfa_dim_selection_method")),
                           numericInput(ns("mfa_n_dimensions"),
                                        "Number of Dimensions:",
                                        value = 5, min = 2, max = 30, step = 1),
                           p(style = "color: #666; font-size: 0.9em;",
                             em("Manually specify number of dimensions. Use with caution."))
                         ),
                         
                         conditionalPanel(
                           condition = sprintf("input['%s'] == 'all'", ns("mfa_dim_selection_method")),
                           tags$div(
                             style = "background-color: #fff3cd; border-left: 4px solid #856404; padding: 10px; margin: 10px 0;",
                             p(style = "margin: 0;",
                               icon("exclamation-triangle"), strong(" Warning:"),
                               "Using all dimensions is generally", strong("not recommended"), "for hypothesis testing.",
                               "Low-variance dimensions contain mostly noise (measurement error, within-group variation)",
                               "which can obscure real group differences and inflate BIC penalties, often favoring",
                               "simpler models (fewer species) artificially. In high dimensions, discriminant analysis",
                               "becomes unstable and overfitting becomes severe. Use this option only if you have strong",
                               em("a priori"), "reasons or if sample size greatly exceeds the number of dimensions (n >> p).")
                           ),
                           p(style = "color: #666; font-style: italic;",
                             "All available MFA dimensions will be used (no dimensionality reduction).")
                         ),
                         
                         tags$div(
                           style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
                           p(style = "margin: 0;",
                             strong("Best Practices:"),
                             tags$ul(style = "margin: 5px 0;",
                                     tags$li(strong("Default (90% variance):"), 
                                             "Balances signal retention with noise exclusion."),
                                     tags$li(strong("Lower threshold (70-80%):"), 
                                             "Consider if sample size is small (<50) or you have many dimensions (>20)."),
                                     tags$li(strong("Fixed number:"), 
                                             "Use only if you have", em("a priori"), "justification from pilot studies or literature."),
                                     tags$li(strong("All dimensions:"), 
                                             strong("Not recommended."), "Including noise dimensions can artificially favor",
                                             "lumped models and reduce discriminatory power.")
                             ))
                         ),
                         
                         hr(),
                         
                         actionButton(ns("run_mfa_bayesian"), 
                                      "Run Bayesian Hypothesis Testing on MFA Scores", 
                                      icon = icon("play"),
                                      style = "background-color: white; color: black;"),
                         
                         hr(),
                         
                         h4("Results"),
                         DT::dataTableOutput(ns("mfa_bayesian_results_table")),
                         hr(),
                         uiOutput(ns("mfa_bayesian_interpretation")),
                         hr(),
                         downloadButton(ns("download_mfa_bayesian_csv"), "Download CSV"),
                         downloadButton(ns("download_mfa_bayesian_xlsx"), "Download Excel"),
                         hr()
                )
    ) 
  )
}

mod_mfa_server <- function(id, raw_combined_data_r, allometry_adjusted_data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mfa_results_r <- reactiveVal(NULL) 
    mfa_data_for_analysis_r <- reactiveVal(NULL)
    mfa_status_message_r <- reactiveVal("Make sure at least 2 groups are created.")
    
    # Stores user-defined custom trait groupings
    custom_trait_groups <- reactiveValues(groups = list())
    
    # Reactive values for PERMANOVA results
    permanova_main_mfa_results_r <- reactiveVal(NULL)
    permanova_pairwise_mfa_results_r <- reactiveVal(NULL)
    
    # Reactive values for Bayesian hypothesis testing
    mfa_hypothesis_data <- reactiveVal(NULL)
    mfa_bayesian_results <- reactiveVal(NULL)
    mfa_dimension_info <- reactiveVal(NULL)  # Store dimension selection info
    
    # UI to display the MFA status message
    output$mfa_status_message <- renderText({
      mfa_status_message_r()
    })
    
    # Reactive for the ultimate data used for trait display and MFA
    data_for_mfa_source_r <- reactive({
      data_to_use <- NULL
      if (!is.null(allometry_adjusted_data_r()) && nrow(allometry_adjusted_data_r()) > 0) {
        data_to_use <- allometry_adjusted_data_r()
      } else if (!is.null(raw_combined_data_r()) && !is.null(raw_combined_data_r()$data) && nrow(raw_combined_data_r()$data) > 0) {
        data_to_use <- raw_combined_data_r()$data
      } else {
      }
      return(data_to_use)
    })
    
    # Reactive for the group/species column name (first column)
    group_col_name_r <- reactive({
      df <- data_for_mfa_source_r()
      req(df)
      col_name <- names(df)[1]
      col_name
    })
    
    # UI to display the identified group variable
    output$group_variable_display <- renderUI({
      req(group_col_name_r())
      p(strong("Species/Group Identifier Column:"), tags$code(group_col_name_r()))
    })
    
    # UI to inform the user about the data source (raw or allometry-adjusted)
    output$data_source_status <- renderUI({
      NULL  # Remove all messaging about data source
    })
    
    
    # Reactive for all potential traits (excluding the group column)
    all_numeric_and_factor_traits_r <- reactive({
      df <- data_for_mfa_source_r()
      req(df, ncol(df) > 1) # Ensure df exists and has more than just the group column
      
      group_col <- group_col_name_r()
      if (is.null(group_col)) return(character(0))
      
      traits <- names(df)[!names(df) %in% group_col]
      
      # Filter to only include numeric or factor/character columns
      valid_traits <- traits[sapply(df[traits], function(x) is.numeric(x) || is.factor(x) || is.character(x))]
      return(valid_traits)
    })
    
    trait_group_df_r <- reactive({
      map_dfr(names(custom_trait_groups$groups), function(group_name) {
        tibble(Variable = custom_trait_groups$groups[[group_name]], Type = group_name)
      })
    })
    
    # Dynamic UI for variable group inputs 
    output$variable_group_selection <- renderUI({
      all_traits <- all_numeric_and_factor_traits_r()
      if (length(all_traits) == 0) {
        return(p("No valid traits found in the dataset for MFA. Please ensure your data is loaded and contains numeric or categorical columns besides the species identifier."))
      }
      
      tagList(
        h5(strong("Create groups and assign traits to them:")),
        textInput(ns("new_group_name"), "Group Name:", placeholder = "e.g., ShapeTraits"),
        uiOutput(ns("trait_selector_ui")),
        actionButton(ns("add_group"), "Add to Group", class = "btn-success mt-2"),
        actionButton(ns("reset_groups"), "Reset All Groups", class = "btn-danger mt-2"),
        hr(),
        h5("Current Groups and Assigned Traits:"),
        uiOutput(ns("current_group_display"))
      )
    })
    
    output$trait_selector_ui <- renderUI({
      all_traits <- all_numeric_and_factor_traits_r()
      already_assigned <- unlist(custom_trait_groups$groups)
      available_traits <- setdiff(all_traits, already_assigned)
      
      if (length(available_traits) == 0) {
        return(p("All traits have been assigned to groups."))
      }
      
      checkboxGroupInput(ns("new_group_traits"), "Select Traits for This Group:",
                         choices = available_traits,
                         inline = TRUE)
    })
    
    observeEvent(input$add_group, {
      req(input$new_group_name, input$new_group_traits)
      group_name <- input$new_group_name
      traits <- input$new_group_traits
      
      # Prevent trait overlap across groups
      existing_traits <- unlist(custom_trait_groups$groups)
      if (any(traits %in% existing_traits)) {
        showNotification("Some traits already assigned to another group. Please resolve overlap first.", type = "error")
        return()
      }
      
      custom_trait_groups$groups[[group_name]] <- traits
    })
    
    observeEvent(input$reset_groups, {
      custom_trait_groups$groups <- list()
    })
    
    output$current_group_display <- renderUI({
      if (length(custom_trait_groups$groups) == 0) {
        return(p("No groups defined yet."))
      }
      
      tagList(
        lapply(names(custom_trait_groups$groups), function(group_name) {
          traits <- custom_trait_groups$groups[[group_name]]
          div(strong(group_name), ": ", paste(traits, collapse = ", "))
        })
      )
    })
    
    observeEvent(input$perform_mfa, {
      
      # Reset status message
      mfa_status_message_r("Performing MFA...")
      mfa_results_r(NULL) # Clear previous results
      permanova_main_mfa_results_r(NULL) # Clear PERMANOVA results
      permanova_pairwise_mfa_results_r(NULL) # Clear PERMANOVA results
      
      df <- data_for_mfa_source_r()
      if (is.null(df)) {
        mfa_status_message_r("Error: No data available for MFA. Please upload data first.")
        return()
      }
      
      group_col <- group_col_name_r()
      if (is.null(group_col)) {
        mfa_status_message_r("Error: Group/Species column not identified.")
        return()
      }
      
      # Get selected traits from UI inputs
      group_map <- custom_trait_groups$groups
      all_active_traits <- unlist(group_map)
      
      group_map <- custom_trait_groups$groups
      all_active_traits <- unlist(group_map)
      
      print(custom_trait_groups$groups)
      
      
      missing_traits <- setdiff(all_active_traits, names(df))
      if (length(missing_traits) > 0) {
        mfa_status_message_r(paste0("Internal Error: The following traits were selected but not found in the dataset: ", paste(missing_traits, collapse = ", ")))
        return()
      }
      
      
      if (length(all_active_traits) == 0) {
        mfa_status_message_r("Warning: Please define at least one group with traits for MFA.")
        return()
      }
      
      if (length(unique(all_active_traits)) != length(all_active_traits)) {
        mfa_status_message_r("Error: A trait appears in more than one group. Please ensure each trait is assigned to only one group.")
        return()
      }
      
      ## DEBUG check
      missing_traits <- setdiff(all_active_traits, names(df))
      if (length(missing_traits) > 0) {
        mfa_status_message_r(paste0("Internal Error: The following traits were selected but not found in the dataset: ", paste(missing_traits, collapse = ", ")))
        return()
      }
      
      # Subset to selected columns + group/species column
      cols_to_select <- unique(c(group_col, all_active_traits))
      missing_traits <- setdiff(cols_to_select, names(df))
      if (length(missing_traits) > 0) {
        mfa_status_message_r(paste0("Internal Error: The following traits were selected but not found in the dataset: ", paste(missing_traits, collapse = ", ")))
        return()
      }
      
      data_for_mfa <- df[, cols_to_select, drop = FALSE]
      
      # Data Type Coercion and Validation
      # Ensure the species column is a factor for MFA
      if (!is.factor(data_for_mfa[[group_col]])) {
        data_for_mfa[[group_col]] <- as.factor(data_for_mfa[[group_col]])
      }
      if (nlevels(data_for_mfa[[group_col]]) < 2) {
        mfa_status_message_r(paste0("Error: The Species/Group column '", group_col, "' has fewer than two unique levels. MFA requires at least two groups."))
        return()
      }
      
      # Unified trait coercion based on user-defined groups
      for (group_name in names(group_map)) {
        trait_list <- group_map[[group_name]]
        
        # Determine if group is numeric
        is_numeric_group <- all(sapply(df[trait_list], is.numeric))
        
        for (col in trait_list) {
          if (!col %in% names(data_for_mfa)) {
            mfa_status_message_r(paste0("Internal Error: Trait '", col, "' not found in data_for_mfa."))
            return()
          }
          
          if (is_numeric_group) {
            if (!is.numeric(data_for_mfa[[col]])) {
              temp_col <- suppressWarnings(as.numeric(data_for_mfa[[col]]))
              
              if (length(temp_col) != nrow(data_for_mfa) || any(is.na(temp_col) & !is.na(data_for_mfa[[col]]))) {
                mfa_status_message_r(paste0("Error: Trait '", col, "' in group '", group_name,
                                            "' could not be safely coerced to numeric. Please clean your data."))
                return()
              }
              
              data_for_mfa[[col]] <- temp_col
            }
          } else {
            if (!is.factor(data_for_mfa[[col]])) {
              data_for_mfa[[col]] <- as.factor(data_for_mfa[[col]])
            }
            if (nlevels(data_for_mfa[[col]]) < 2) {
              mfa_status_message_r(paste0("Warning: Trait '", col, "' in group '", group_name,
                                          "' has fewer than two unique levels. It may not contribute meaningfully to MFA."))
            }
          }
        }
      }
      
      # Check for missing values AFTER all type conversions
      if (any(is.na(data_for_mfa))) {
        mfa_status_message_r("Error: Missing values detected in selected traits after type conversion. MFA requires complete cases. Please clean your data first (e.g., using 'Impute Missing Data' module).")
        return()
      }
      
      # Initialize MFA parameters
      group_lengths <- c(1, sapply(group_map, length))  # first is species
      group_names   <- c(group_col, names(group_map))
      group_types   <- c("n", sapply(group_map, function(traits) {
        if (all(sapply(df[traits], is.numeric))) {
          return("s")  # quantitative
        } else {
          return("n")  # nominal
        }
      }))
      
      # Final validation for MFA input
      if (sum(group_lengths) != ncol(data_for_mfa)) {
        mfa_status_message_r(paste0("Internal error: Mismatch between selected variables and group configuration for MFA. Sum of group lengths (", sum(group_lengths), ") does not match number of columns in data (", ncol(data_for_mfa), "). Please report this issue."))
        return()
      }
      # If only species is selected as a group, and no other active traits
      if (length(group_lengths) == 1 && group_lengths[1] == 1 && group_names[1] == group_col) {
        mfa_status_message_r("Warning: Only the species/group column was detected. Please select additional active traits for MFA.")
        return()
      }
      
      message("MFA is about to be performed...") 
      
      ## ncp selection
      # Active trait columns (exclude the supplementary species/group column)
      active_cols <- setdiff(names(data_for_mfa), group_col)
      
      # Effective dimensionality contributed by active variables:
      # - numeric variables contribute 1 df each
      # - factors contribute (levels - 1) df each (MCA-style)
      p_quanti <- sum(sapply(data_for_mfa[active_cols], is.numeric))
      p_quali  <- sum(sapply(data_for_mfa[active_cols], function(x) {
        if (is.factor(x)) max(0, nlevels(x) - 1) else 0
      }))
      
      # Rank is limited by both the variables' effective df and sample size (n - 1)
      n         <- nrow(data_for_mfa)
      rank_max  <- max(1, min(n - 1, p_quanti + p_quali))
      
      # Practical ceiling to keep objects/UI snappy; tweak if you like
      ncp_cap     <- 30
      ncp_to_use  <- min(rank_max, ncp_cap)
      message(sprintf("MFA: using ncp = %d (rank_max = %d, cap = %d)", ncp_to_use, rank_max, ncp_cap))
      ## --- end ncp selection ---
      
      # Perform MFA
      mfa_res <- tryCatch({
        FactoMineR::MFA(
          base = data_for_mfa,
          group = group_lengths,
          type = group_types,
          name.group = group_names,
          num.group.sup = 1, # Designate the first group (species) as supplementary
          ncp = ncp_to_use,
          graph = FALSE
        )
      }, error = function(e) {
        mfa_status_message_r(paste("MFA failed:", e$message))
        message("MFA failed with error:", e$message) 
        mfa_results_r(NULL)
        return(NULL)
      })
      
      mfa_results_r(mfa_res)
      mfa_data_for_analysis_r(data_for_mfa) 
      
      if (!is.null(mfa_res)) {
        mfa_status_message_r("MFA performed successfully! See results below.")
        message("MFA performed successfully!") 
      }
    })
    
    # MFA Results Outputs
    
    output$mfa_results_output <- renderUI({
      mfa_res <- mfa_results_r()
      if (is.null(mfa_res)) {
        return(p("MFA results will appear here after analysis. Ensure you have configured traits and clicked 'Perform MFA'."))
      }
      
      tagList(
        tabsetPanel(
          tabPanel("Eigenvalues",
                   DTOutput(ns("mfa_eigen_table"))
          ),
          tabPanel("Group Contributions",
                   DTOutput(ns("mfa_group_contrib_table"))
          ),
          tabPanel("Quantitative Variable Contributions",
                   DTOutput(ns("mfa_quanti_contrib_table"))
          ),
          
          tabPanel("Individuals Coordinates",
                   DTOutput(ns("mfa_ind_coord_table"))
          )
        ),
        br(),
        downloadButton(ns("download_all_mfa_results"), "Download All MFA Tables"),
        hr()
      )
    })
    
    # Individual result tables
    output$mfa_eigen_table <- renderDT({
      req(mfa_results_r())
      eig_df <- as.data.frame(mfa_results_r()$eig)
      colnames(eig_df) <- c("Eigenvalue", "Percentage of variance", "Cumulative percentage of variance")
      datatable(round(eig_df, 3), options = list(dom = 't', scrollX = TRUE))
    })
    
    output$mfa_group_contrib_table <- renderDT({
      req(mfa_results_r(), mfa_results_r()$group$contrib)
      contrib_df <- as.data.frame(mfa_results_r()$group$contrib)
      datatable(round(contrib_df, 3), options = list(dom = 'tip', scrollX = TRUE))
    })
    
    # Separate Quantitative Variable Contributions table
    output$mfa_quanti_contrib_table <- renderDT({
      req(mfa_results_r())
      mfa_res <- mfa_results_r()
      if (!is.null(mfa_res$quanti.var) && !is.null(mfa_res$quanti.var$contrib)) {
        quanti_df <- as.data.frame(mfa_res$quanti.var$contrib)
        datatable(round(quanti_df, 3), options = list(dom = 'tip', scrollX = TRUE))
      } else {
        datatable(data.frame(Message = "No quantitative variables selected or contributions available."), options = list(dom = 't'))
      }
    })
    
    output$mfa_ind_coord_table <- renderDT({
      req(mfa_results_r(), mfa_results_r()$ind$coord)
      ind_coord_df <- as.data.frame(mfa_results_r()$ind$coord)
      datatable(round(ind_coord_df, 3), options = list(dom = 'tip', scrollX = TRUE))
    })
    
    # Download All MFA Results
    output$download_all_mfa_results <- downloadHandler(
      filename = function() paste0("mfa_results_", Sys.Date(), ".zip"),
      content = function(file) {
        req(mfa_results_r())
        mfa_res <- mfa_results_r()
        
        owd <- setwd(tempdir()); on.exit(setwd(owd))
        files_to_zip <- c()
        
        # helper: safely suffix colnames if object exists and has columns
        suffix_df <- function(x, suffix) {
          if (is.null(x)) return(NULL)
          df <- as.data.frame(x)
          if (NCOL(df) == 0) return(NULL)
          names(df) <- paste0(names(df), suffix)
          df
        }
        
        # 1) Eigenvalues
        if (!is.null(mfa_res$eig) && NCOL(mfa_res$eig) > 0) {
          eig_df <- as.data.frame(mfa_res$eig)
          colnames(eig_df) <- c("Eigenvalue","Percentage of variance","Cumulative percentage of variance")
          write.csv(eig_df, "mfa_eigenvalues.csv", row.names = TRUE)
          files_to_zip <- c(files_to_zip, "mfa_eigenvalues.csv")
        }
        
        # 2) Individuals Coordinates
        if (!is.null(mfa_res$ind$coord) && NCOL(mfa_res$ind$coord) > 0) {
          write.csv(as.data.frame(mfa_res$ind$coord), "mfa_individuals_coordinates.csv", row.names = TRUE)
          files_to_zip <- c(files_to_zip, "mfa_individuals_coordinates.csv")
        }
        
        # 3) Quantitative Variables Summary (Coord, Cos2, Contrib)
        if (!is.null(mfa_res$quanti.var)) {
          parts <- list(
            suffix_df(mfa_res$quanti.var$coord,  "_coord"),
            suffix_df(mfa_res$quanti.var$cos2,   "_cos2"),
            suffix_df(mfa_res$quanti.var$contrib,"_contrib")
          )
          parts <- Filter(Negate(is.null), parts)
          if (length(parts) > 0) {
            quanti_var_df <- dplyr::bind_cols(parts)
            write.csv(quanti_var_df, "mfa_quantitative_variables_summary.csv", row.names = TRUE)
            files_to_zip <- c(files_to_zip, "mfa_quantitative_variables_summary.csv")
          }
        }
        
        # 4) Qualitative Variables Summary (Coord, Cos2, Contrib, V-test)
        if (!is.null(mfa_res$quali.var)) {
          parts_q <- list(
            suffix_df(mfa_res$quali.var$coord,  "_coord"),
            suffix_df(mfa_res$quali.var$cos2,   "_cos2"),
            suffix_df(mfa_res$quali.var$contrib,"_contrib"),
            suffix_df(mfa_res$quali.var$v.test, "_vtest")
          )
          parts_q <- Filter(Negate(is.null), parts_q)
          if (length(parts_q) > 0) {
            quali_var_df <- dplyr::bind_cols(parts_q)
            write.csv(quali_var_df, "mfa_qualitative_variables_summary.csv", row.names = TRUE)
            files_to_zip <- c(files_to_zip, "mfa_qualitative_variables_summary.csv")
          }
        }
        
        # 5) Group Summary (Coord, Cos2, Contrib)
        if (!is.null(mfa_res$group)) {
          parts_g <- list(
            suffix_df(mfa_res$group$coord, "_coord"),
            suffix_df(mfa_res$group$cos2,  "_cos2"),
            suffix_df(mfa_res$group$contrib,"_contrib")
          )
          parts_g <- Filter(Negate(is.null), parts_g)
          if (length(parts_g) > 0) {
            group_df <- dplyr::bind_cols(parts_g)
            write.csv(group_df, "mfa_group_summary.csv", row.names = TRUE)
            files_to_zip <- c(files_to_zip, "mfa_group_summary.csv")
          }
        }
        
        if (length(files_to_zip) > 0) {
          zip(file, files_to_zip, flags = "-j")
        } else {
          # create an info file so the download isn't empty
          writeLines("No MFA result tables were available to export for this run.", con = file)
        }
      }
    )
    
    
    # PERMANOVA on MFA Scores Logic
    observeEvent(input$run_permanova_mfa, {
      req(mfa_results_r())
      mfa_res <- mfa_results_r()
      group_col <- group_col_name_r()
      data_for_mfa <- mfa_data_for_analysis_r() # Use the stored data that went into MFA
      
      if (is.null(mfa_res$ind$coord)) {
        showNotification("MFA individuals coordinates not available. Please run MFA first.", type = "error")
        return()
      }
      
      # Extract MFA scores (individual coordinates)
      mfa_scores <- as.data.frame(mfa_res$ind$coord)
      
      # Ensure the group column from the original data aligns with the MFA scores
      # The rows of mfa_scores correspond to the rows of data_for_mfa
      if (is.null(data_for_mfa) || !(group_col %in% names(data_for_mfa))) {
        showNotification("Original data or group column not found for PERMANOVA. This is an internal error.", type = "error")
        return()
      }
      
      # Extract the group factor from the original data used for MFA
      group_factor <- data_for_mfa[[group_col]]
      
      if (!is.factor(group_factor)) {
        group_factor <- as.factor(group_factor)
      }
      
      if (nlevels(group_factor) < 2) {
        showNotification("PERMANOVA requires at least two groups in the species/group column.", type = "error")
        return()
      }
      
      permutations <- input$permanova_permutations_mfa
      distance_method <- input$permanova_distance_method_mfa
      
      # Main PERMANOVA (adonis2)
      main_permanova_res <- tryCatch({
        adonis2(mfa_scores ~ group_factor, data = data.frame(group_factor = group_factor),
                permutations = permutations, method = distance_method)
      }, error = function(e) {
        showNotification(paste("Error running main PERMANOVA:", e$message), type = "error")
        return(NULL)
      })
      
      permanova_main_mfa_results_r(main_permanova_res)
      
      # Pairwise PERMANOVA (if main PERMANOVA was successful)
      pairwise_permanova_res <- NULL
      if (!is.null(main_permanova_res) && !is.na(main_permanova_res$Pr[1]) && main_permanova_res$Pr[1] < 0.05) {
        # Only run pairwise if the main test is significant
        pairwise_permanova_res <- tryCatch({
          
          group_levels <- levels(group_factor)
          if (length(group_levels) > 1) {
            combinations <- combn(group_levels, 2, simplify = FALSE)
            pairwise_results_list <- lapply(combinations, function(pair) {
              sub_data_indices <- which(group_factor %in% pair)
              sub_mfa_scores <- mfa_scores[sub_data_indices, , drop = FALSE]
              sub_group_factor <- factor(group_factor[sub_data_indices]) # Re-factor to drop unused levels
              
              if (nlevels(sub_group_factor) < 2 || nrow(sub_mfa_scores) < 2) {
                # Skip if a subset has only one level or too few data points
                return(NULL)
              }
              
              pairwise_mod <- adonis2(sub_mfa_scores ~ sub_group_factor, data = data.frame(sub_group_factor = sub_group_factor),
                                      permutations = permutations, method = distance_method)
              
              data.frame(
                Comparison = paste(pair[1], "vs", pair[2]),
                R2 = round(pairwise_mod$R2[1], 4),
                F.value = round(pairwise_mod$F[1], 4),
                p.value = round(pairwise_mod$`Pr(>F)`[1], 4)
              )
            })
            
            # Filter out NULL results (e.g., from insufficient data in a subset)
            pairwise_results_list <- Filter(Negate(is.null), pairwise_results_list)
            
            if (length(pairwise_results_list) > 0) {
              do.call(rbind, pairwise_results_list)
            } else {
              NULL
            }
          } else {
            NULL
          }
        }, error = function(e) {
          showNotification(paste("Error running pairwise PERMANOVA:", e$message), type = "error")
          return(NULL)
        })
      } else {
        showNotification("Main PERMANOVA not significant or failed, skipping pairwise tests.", type = "info")
      }
      permanova_pairwise_mfa_results_r(pairwise_permanova_res)
    })
    
    output$permanova_main_results_mfa <- renderPrint({
      req(permanova_main_mfa_results_r())
      permanova_main_mfa_results_r()
    })
    
    output$permanova_pairwise_results_mfa <- renderDT({
      req(permanova_pairwise_mfa_results_r())
      datatable(permanova_pairwise_mfa_results_r(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$download_permanova_main_mfa <- downloadHandler(
      filename = function() {
        paste("permanova_main_mfa_results_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        req(permanova_main_mfa_results_r())
        capture.output(permanova_main_mfa_results_r(), file = file)
      }
    )
    
    output$download_permanova_pairwise_mfa <- downloadHandler(
      filename = function() {
        paste("permanova_pairwise_mfa_results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(permanova_pairwise_mfa_results_r())
        write.csv(permanova_pairwise_mfa_results_r(), file, row.names = FALSE)
      }
    )
    
    ## ============================================================================
    ## BAYESIAN HYPOTHESIS TESTING ON MFA SCORES
    ## ============================================================================
    
    # Flag for conditional panel
    output$mfa_hyp_file_loaded <- reactive({
      !is.null(mfa_hypothesis_data())
    })
    outputOptions(output, "mfa_hyp_file_loaded", suspendWhenHidden = FALSE)
    
    # Load hypothesis file
    observeEvent(input$mfa_hyp_file, {
      req(input$mfa_hyp_file)
      
      tryCatch({
        hyp_df <- read.csv(input$mfa_hyp_file$datapath, stringsAsFactors = FALSE)
        
        # Validate file has at least one column
        if (ncol(hyp_df) < 1) {
          output$mfa_hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     "Error: File must have at least 1 hypothesis column")
          })
          mfa_hypothesis_data(NULL)
          return()
        }
        
        # Check that columns are not purely numeric
        all_numeric_cols <- sapply(hyp_df, function(col) {
          all(grepl("^-?[0-9]*\\.?[0-9]+$", col[!is.na(col)]))
        })
        
        if (any(all_numeric_cols)) {
          problematic_cols <- names(hyp_df)[all_numeric_cols]
          output$mfa_hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: The following columns contain only numeric values: ",
                            paste(problematic_cols, collapse = ", "),
                            ". Hypothesis labels should be categorical (e.g., 'Species_A', 'Lump_AB')."))
          })
          mfa_hypothesis_data(NULL)
          return()
        }
        
        # Check if number of rows matches original input data
        original_data <- data_for_mfa_source_r()
        req(original_data)
        if (nrow(hyp_df) != nrow(original_data)) {
          output$mfa_hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: Hypothesis file has ", nrow(hyp_df), 
                            " rows but input data has ", nrow(original_data), 
                            " rows. They must match exactly."))
          })
          mfa_hypothesis_data(NULL)
          return()
        }
        
        # Store hypothesis data
        mfa_hypothesis_data(hyp_df)
        
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-success",
                   paste0("✓ Hypothesis file loaded successfully. Found ", 
                          ncol(hyp_df), " hypotheses: ", 
                          paste(colnames(hyp_df), collapse = ", ")))
        })
        
      }, error = function(e) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste0("Error loading file: ", e$message))
        })
        mfa_hypothesis_data(NULL)
      })
    })
    
    # Load example hypothesis file
    observeEvent(input$load_example_mfa_hyp, {
      
      # Check if data is loaded first
      original_data <- data_for_mfa_source_r()
      if (is.null(original_data) || nrow(original_data) == 0) {
        showModal(modalDialog(
          title = "No Data Loaded",
          "Please load data in the 'Input Data' module before loading hypothesis file.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      example_path <- system.file("examples", "Gekko_hypothesis_file.csv", package = "GroupStruct2")
      
      if (!file.exists(example_path)) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   "Error: Example hypothesis file not found in package")
        })
        return()
      }
      
      tryCatch({
        hyp_df <- read.csv(example_path, stringsAsFactors = FALSE)
        
        # Validate
        if (ncol(hyp_df) < 1) {
          output$mfa_hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     "Error: File must have at least 1 hypothesis column")
          })
          mfa_hypothesis_data(NULL)
          return()
        }
        
        # Check that columns are not purely numeric
        all_numeric_cols <- sapply(hyp_df, function(col) {
          all(grepl("^-?[0-9]*\\.?[0-9]+$", col[!is.na(col)]))
        })
        
        if (any(all_numeric_cols)) {
          problematic_cols <- names(hyp_df)[all_numeric_cols]
          output$mfa_hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-danger",
                     paste0("Error: The following columns contain only numeric values: ",
                            paste(problematic_cols, collapse = ", "),
                            ". Hypothesis labels should be categorical."))
          })
          mfa_hypothesis_data(NULL)
          return()
        }
        
        # Check if number of rows matches input data
        if (nrow(hyp_df) != nrow(original_data)) {
          output$mfa_hyp_upload_status <- renderUI({
            tags$div(class = "alert alert-warning",
                     paste0("Warning: Example hypothesis file has ", nrow(hyp_df), 
                            " rows but your current data has ", nrow(original_data), 
                            " rows. Please load the corresponding example dataset first."))
          })
          mfa_hypothesis_data(NULL)
          return()
        }
        
        # Store hypothesis data
        mfa_hypothesis_data(hyp_df)
        
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-success",
                   paste0("✓ Example hypothesis file loaded successfully. Found ", 
                          ncol(hyp_df), " hypotheses: ", 
                          paste(colnames(hyp_df), collapse = ", ")))
        })
        
      }, error = function(e) {
        output$mfa_hyp_upload_status <- renderUI({
          tags$div(class = "alert alert-danger",
                   paste0("Error loading example file: ", e$message))
        })
        mfa_hypothesis_data(NULL)
      })
    })
    
    # Preview hypothesis data
    output$mfa_hyp_preview <- renderDT({
      req(mfa_hypothesis_data())
      
      DT::datatable(
        mfa_hypothesis_data(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tip'
        ),
        rownames = FALSE
      )
    })
    
    # Run Bayesian hypothesis testing on MFA scores
    observeEvent(input$run_mfa_bayesian, {
      
      # Check if MFA has been run
      if (is.null(mfa_results_r()) || is.null(mfa_results_r()$ind$coord)) {
        showModal(modalDialog(
          title = "No MFA Results",
          "Please run MFA first (see 'MFA Analysis & Results' tab) before performing Bayesian hypothesis testing.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      # Check if hypotheses are loaded
      if (is.null(mfa_hypothesis_data())) {
        showNotification("Please upload a hypothesis file", type = "error")
        return()
      }
      
      withProgress(message = 'Running Bayesian hypothesis testing on MFA scores...', value = 0, {
        
        mfa_res <- mfa_results_r()
        hyp_df <- mfa_hypothesis_data()
        
        # Get all hypothesis names
        hyp_names <- colnames(hyp_df)
        
        # Set flat priors (equal for all hypotheses)
        n_hyp <- length(hyp_names)
        priors <- rep(1/n_hyp, n_hyp)
        names(priors) <- hyp_names
        
        # Extract MFA scores (individual coordinates)
        mfa_scores_full <- mfa_res$ind$coord
        
        # Ensure MFA scores are numeric matrix
        if (!is.numeric(mfa_scores_full)) {
          showNotification("Error: MFA scores are not numeric", type = "error")
          return(NULL)
        }
        
        if (ncol(mfa_scores_full) == 0) {
          showNotification("No MFA dimensions found", type = "error")
          return(NULL)
        }
        
        # ======================================================================
        # DIMENSION SELECTION
        # ======================================================================
        
        # Get eigenvalues and variance explained
        eigenvalues <- mfa_res$eig[, 1]
        variance_explained <- mfa_res$eig[, 2]  # Percentage of variance
        cumvar <- cumsum(variance_explained)
        
        # Select dimensions based on user input
        if (input$mfa_dim_selection_method == "variance") {
          # Use cumulative variance threshold
          n_dims <- which(cumvar >= input$mfa_variance_threshold)[1]
          
          if (is.na(n_dims)) {
            n_dims <- ncol(mfa_scores_full)  # Use all if threshold not reached
            showNotification(
              sprintf("Variance threshold (%.0f%%) not reached. Using all %d dimensions (%.1f%% variance).",
                      input$mfa_variance_threshold, n_dims, cumvar[n_dims]),
              type = "warning", duration = 5
            )
          } else {
            showNotification(
              sprintf("Using first %d dimensions (%.1f%% cumulative variance)",
                      n_dims, cumvar[n_dims]),
              type = "message", duration = 5
            )
          }
          
          mfa_scores <- mfa_scores_full[, 1:n_dims, drop = FALSE]
          
        } else if (input$mfa_dim_selection_method == "fixed") {
          # Use fixed number of dimensions
          n_dims <- min(input$mfa_n_dimensions, ncol(mfa_scores_full))
          mfa_scores <- mfa_scores_full[, 1:n_dims, drop = FALSE]
          
          showNotification(
            sprintf("Using first %d dimensions (%.1f%% cumulative variance)",
                    n_dims, cumvar[n_dims]),
            type = "message", duration = 5
          )
          
        } else {  # "all"
          # Use all dimensions
          mfa_scores <- mfa_scores_full
          n_dims <- ncol(mfa_scores)
          
          showNotification(
            sprintf("Using all %d dimensions (%.1f%% cumulative variance)",
                    n_dims, cumvar[n_dims]),
            type = "message", duration = 5
          )
        }
        
        # Store dimension info for later display
        mfa_dimension_info(list(
          n_dims_used = n_dims,
          total_dims = ncol(mfa_scores_full),
          variance_used = cumvar[n_dims],
          method = input$mfa_dim_selection_method,
          threshold = if (input$mfa_dim_selection_method == "variance") input$mfa_variance_threshold else NA
        ))
        
        incProgress(0.2, detail = paste("Fitting EDDA models on", n_dims, "dimensions..."))
        
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
              mfa_scores,
              class = factor(class_labels),
              modelType = "EDDA",
              verbose = FALSE
            )
          }
          
          incProgress(0.2, detail = "Computing Bayes Factors...")
          
          # Compute Bayes Factors with flat priors
          results_table <- do.call(GMMBayesFactorTable, 
                                   c(models, list(prior = priors)))
          
          # Sort by BIC (descending - best model first)
          results_table <- results_table[order(-results_table$BIC), ]
          
          mfa_bayesian_results(results_table)
          
          showNotification("Bayesian hypothesis testing complete!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    })
    
    # Display results table
    output$mfa_bayesian_results_table <- DT::renderDataTable({
      req(mfa_bayesian_results())
      
      DT::datatable(
        mfa_bayesian_results(),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'tp'
        ),
        rownames = FALSE
      )
    })
    
    # Interpretation guide
    output$mfa_bayesian_interpretation <- renderUI({
      req(mfa_bayesian_results())
      
      # Get dimension info if available
      dim_info <- mfa_dimension_info()
      
      tagList(
        # Add dimension info box at top
        if (!is.null(dim_info)) {
          tags$div(
            style = "background-color: #d1ecf1; border-left: 4px solid #0c5460; padding: 10px; margin: 15px 0;",
            p(style = "margin: 0;",
              strong("Dimensions Used:"), 
              sprintf("%d of %d MFA dimensions (%.1f%% cumulative variance)", 
                      dim_info$n_dims_used, dim_info$total_dims, dim_info$variance_used),
              br(),
              if (dim_info$method == "variance") {
                sprintf("Selection method: Cumulative variance threshold (%.0f%%)", dim_info$threshold)
              } else if (dim_info$method == "fixed") {
                "Selection method: Fixed number of dimensions"
              } else {
                "Selection method: All dimensions"
              })
          )
        },
        
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
                  "Probability each hypothesis is correct, given the data and flat priors.",
                  "The hypothesis with highest PostMod is most supported by the data.",
                  tags$ul(
                    tags$li("PostMod > 0.75: Strong support"),
                    tags$li("PostMod 0.50-0.75: Moderate support"),
                    tags$li("PostMod < 0.50: Weak support")
                  ))
        ),
        
        tags$div(
          style = "background-color: #fff3cd; border-left: 4px solid #856404; padding: 10px; margin: 15px 0;",
          p(style = "margin: 0;",
            strong("Important Note:"),
            "This analysis assumes MFA scores are approximately multivariate Gaussian.",
            "While MFA scores are continuous, they may violate normality assumptions.",
            "Results should be interpreted as exploratory and validated with other evidence",
            "(genetics, ecology, biogeography).")
        )
      )
    })
    
    # Download CSV
    output$download_mfa_bayesian_csv <- downloadHandler(
      filename = function() {
        paste0("mfa_bayesian_delimitation_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(mfa_bayesian_results())
        write.csv(mfa_bayesian_results(), file, row.names = FALSE)
      }
    )
    
    # Download Excel
    output$download_mfa_bayesian_xlsx <- downloadHandler(
      filename = function() {
        paste0("mfa_bayesian_delimitation_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(mfa_bayesian_results())
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Bayesian Results")
        openxlsx::writeData(wb, "Bayesian Results", mfa_bayesian_results())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    return(list(
      mfa_results_r = mfa_results_r,
      trait_group_df_r = trait_group_df_r,
      mfa_hypothesis_data_r = mfa_hypothesis_data  # ADD THIS
    ))
  })
}