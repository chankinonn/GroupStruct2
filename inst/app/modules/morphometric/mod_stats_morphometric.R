
mod_inferential_ui_morphometric <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Inferential Statistics"),
    hr(),
    br(),
    
    tabsetPanel(id = ns("main_tabs"), 
                tabPanel("Univariate",
                         br(), 
                         h4("Univariate Analysis"),
                         p("Perform univariate analyses to determine which traits are (or are not) statistically different between groups. This module automatically detects the number of groups, tests for assumptions of normality and homogeneity of variance, and selects the appropriate statistical tests (t-test or Wilcoxon/Mann-Whitney if only 2 groups are detected, ANOVA or Kruskal-Wallis if >2 groups, followed by the appropriate ad hoc tests)."),
                         
                         div(style = "white-space: nowrap;",
                             checkboxInput(ns("force_parametric"),
                                           label = HTML("<strong style='color: red;'>*Perform parametric tests even though assumptions are not met</strong>"),
                                           value = FALSE)),
                         uiOutput(ns("trait_buttons")),
                         br(),
                         textOutput(ns("trait_name")),
                         verbatimTextOutput(ns("test_results")),
                         br(),
                         uiOutput(ns("select_trait_download")),
                         br(),
                         downloadButton(ns("download_all_summary"), "Download Summary of All Traits"),
                         hr(),
                         h5("Significant Traits by Pairwise Comparison:"),
                         uiOutput(ns("alpha_selector_ui")), 
                         uiOutput(ns("significant_traits_ui")), 
                         downloadButton(ns("download_significant_traits"), "Download Significant Traits"),
                         hr(),
                ),
                
                tabPanel("PCA",
                         br(), 
                         h4("Standard PCA"),
                         p("This section shows the results of a standard PCA using the R base function prcomp()."),
                         br(),
                         verbatimTextOutput(ns("pca_summary_raw")),
                         downloadButton(ns("download_pca_summary"), "Download PCA Summary"),
                         hr(),
                ),
                
                tabPanel("PCAtest",
                         br(), 
                         h4("PCAtest Analysis sensu Camargo (2022)"),
                         p("The PCAtest performs permutation-based statistical tests to evaluate the overall significance of a PCA, the significance of each PC axis, and the contributions of each trait to the significant axes. This analysis aids in the interpretation of the PCA results and determines which axes are informative for downstream analysis."),
                         tags$p(
                           style = "color: red; font-weight: bold;",
                           "Caveat: The PCAtest can be overly conservative and misleading if a disproportionately large amount of variation is captured in the first few PCs, as is common in phenotypic data. For the DAPC plot, we recommend using the number of PCs that capture 80–90% of variation instead of the number of significant PCs inferred from the PCAtest. We encourage users to compare results using different methods and strategies."
                         ),
                         p("If you use PCAtest, please cite:"),
                         p("Camargo, A. (2022). PCAtest: testing the statistical significance of Principal Component Analysis in R. PeerJ, 10:e12967. https://doi.org/10.7717/peerj.12967"),
                         numericInput(ns("pcatest_permutations"), "Number of Permutations:", value = 1000, min = 100, step = 100),
                         actionButton(ns("run_pcatest"), "Run PCAtest"),
                         br(), br(),
                         h5("Main PCAtest Results:"),
                         verbatimTextOutput(ns("pcatest_main_results")),
                         downloadButton(ns("download_pcatest_main"), "Download PCATest Results"),
                         hr(),
                ),
                
                tabPanel("Multivariate (PERMANOVA)",
                         br(), 
                         h4("PERMANOVA Analysis"),
                         p("The PERMANOVA analysis tests for significant differences among group centroids in multivariate trait space. You can choose to perform this on the original data or on PCA scores. Performing PERMANOVA on PCA scores is recommended to accompany a PCA."),
                         
                         fluidRow(
                           column(6, numericInput(ns("permanova_permutations"), "Number of Permutations:", value = 50000, min = 100, step = 100)),
                           column(6, selectInput(ns("permanova_distance_method"), "Distance Method:",
                                                 choices = c("euclidean", "manhattan", "bray", "jaccard", "altGower"),
                                                 selected = "euclidean"))
                         ),
                         tags$div(
                           style = "font-size: 1.1em; color: red; font-weight: bold; margin-top: 10px;",
                           checkboxInput(ns("use_pca"), HTML("Use PCA Scores for PERMANOVA"), value = FALSE)
                         ),
                         p(em("Note: Euclidean distance is generally the most appropriate method when using PCA scores for PERMANOVA.")),
                         
                         actionButton(ns("run_permanova"), "Run PERMANOVA"),
                         br(), br(),
                         h5("Main PERMANOVA Results (adonis2):"),
                         verbatimTextOutput(ns("permanova_main_results")),
                         downloadButton(ns("download_permanova_main"), "Download Main Results"),
                         br(),
                         h5("Pairwise PERMANOVA Results:"),
                         DTOutput(ns("permanova_pairwise_results")),
                         downloadButton(ns("download_permanova_pairwise"), "Download Pairwise Results"),
                         hr(),
                )
 
    )
  )
}


mod_inferential_server_morphometric <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pairwise_result_r <- reactiveVal(NULL)
    selected_trait <- reactiveVal(NULL)
    permanova_main_results_r <- reactiveVal(NULL)
    permanova_pairwise_results_r <- reactiveVal(NULL)
    pcatest_results_r <- reactiveVal(NULL) 
    
    # Reactive for PCA results
    # Reactive for PCA results based on allometric data
    pca_results_r <- reactive({
      df <- data_r()  # Get the data from the allometric module
      req(df)  # Ensure data is available
      
      # Ensure data_mat is numeric and has no NA values for PCA
      data_mat_numeric <- as.data.frame(lapply(df[, -1], as.numeric))
      complete_rows <- complete.cases(data_mat_numeric)
      
      if (sum(complete_rows) < 2 || ncol(data_mat_numeric) < 2) {
        return(NULL)  # Return NULL if data is insufficient
      }
      
      # Perform PCA
      prcomp(data_mat_numeric[complete_rows, ], center = TRUE, scale. = TRUE)
    })
    
    # Reactive for PCA summary results table
    pca_summary_results_r <- reactive({
      pca <- pca_results_r()
      req(pca)
      
      # Eigenvalues
      eigenvalues <- pca$sdev^2
      eigen_df <- data.frame(
        Metric = "Eigenvalue",
        t(as.data.frame(eigenvalues))
      )
      colnames(eigen_df)[-1] <- paste0("PC", 1:length(eigenvalues))
      
      # Variance explained
      variance_explained <- summary(pca)$importance[2, ]
      variance_df <- data.frame(
        Metric = "Proportion of Variance",
        t(as.data.frame(variance_explained))
      )
      colnames(variance_df)[-1] <- paste0("PC", 1:length(variance_explained))
      
      # Cumulative variance
      cumulative_variance <- summary(pca)$importance[3, ]
      cumulative_df <- data.frame(
        Metric = "Cumulative Proportion",
        t(as.data.frame(cumulative_variance))
      )
      colnames(cumulative_df)[-1] <- paste0("PC", 1:length(cumulative_variance))
      
      # Loadings
      loadings_df <- as.data.frame(pca$rotation) %>%
        rownames_to_column("Trait_Loading")
      
      # Combine all into one data frame
      all_pc_cols <- unique(c(colnames(eigen_df)[-1], colnames(loadings_df)[-1]))
      
      # Function to ensure data frame has all required PC columns
      ensure_pc_cols <- function(df, cols) {
        missing_cols <- setdiff(cols, colnames(df))
        if (length(missing_cols) > 0) {
          for (mc in missing_cols) {
            df[[mc]] <- NA
          }
        }
        # Order PC columns numerically
        ordered_pc_cols <- cols[order(as.numeric(gsub("PC", "", cols)))]
        df %>% dplyr::select(Metric, all_of(ordered_pc_cols))
        
      }
      
      combined_df <- bind_rows(
        eigen_df %>% ensure_pc_cols(all_pc_cols),
        variance_df %>% ensure_pc_cols(all_pc_cols),
        cumulative_df %>% ensure_pc_cols(all_pc_cols),
        loadings_df %>% rename(Metric = Trait_Loading) # Rename for consistent binding
      )
      
      # Format numeric columns to 4 decimal places for display/download
      numeric_cols <- names(combined_df)[sapply(combined_df, is.numeric)]
      combined_df[numeric_cols] <- lapply(combined_df[numeric_cols], function(x) round(x, 4))
      
      return(combined_df)
    })
    
    
    output$download_pca_summary <- downloadHandler(
      filename = function() {
        paste0("pca_summary_morphometric_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(pca_summary_results_r())
        write.csv(pca_summary_results_r(), file, row.names = FALSE)
      }
    )
    
    output$pca_summary_raw <- renderPrint({
      pca <- pca_results_r()
      req(pca)
      cat("PCA Summary:\n")
      print(summary(pca))
      cat("\nLoadings:\n")
      print(pca$rotation)
    })
    
    
    
    # Univariate Tests 
    observeEvent(input$main_tabs, {
      if (input$main_tabs == "Univariate") {
        
        output$trait_buttons <- renderUI({
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          
          if (is.null(df) || nrow(df) == 0) {
            return(p("Please upload data and perform allometric correction in the 'Allometric Correction' module first."))
          }
          
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, names(df)[1])
          
          if (length(traits) == 0) return(p("No numeric traits found in the adjusted data."))
          
          btns <- lapply(traits, function(trait) {
            active <- !is.null(selected_trait()) && selected_trait() == trait
            style <- if (active) "background-color: #337ab7; color: white;" else ""
            actionButton(ns(paste0("btn_", trait)), label = trait, width = "150px", style = style)
          })
          do.call(tagList, btns)
        })
        
        output$select_trait_download <- renderUI({
          req(selected_trait())
          downloadButton(ns("download_summary"),
                         label = paste0("Download Results for '", selected_trait(), "'"))
        })
        
        # Initialize selected_trait and set up button observers when adjusted data loads/changes
        observeEvent(data_r(), {
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          
          if (!is.null(df) && nrow(df) > 0) {
            traits <- names(df)[sapply(df, is.numeric)]
            traits <- setdiff(traits, names(df)[1])
            
            if (length(traits) > 0) {
              selected_trait(traits[1])
            } else {
              selected_trait(NULL)
            }
            
            lapply(traits, function(current_trait) {
              btn_id <- paste0("btn_", current_trait)
              observeEvent(input[[btn_id]], {
                selected_trait(current_trait)
              }, ignoreInit = TRUE)
            })
          } else {
            selected_trait(NULL)
          }
        }, ignoreNULL = FALSE)
        
        output$trait_name <- renderText({
          trait <- selected_trait()
          if (is.null(trait)) {
            "Select a trait above to run inferential statistics."
          } else {
            paste("Running inferential statistics on adjusted trait:", trait)
          }
        })
        
        # Reactive expression for the filtered data based on selected_trait
        filtered_data_r <- reactive({
          trait <- selected_trait()
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          
          if (is.null(trait) || is.null(df) || nrow(df) == 0) {
            return(NULL)
          }
          
          group <- names(df)[1]
          df <- df %>% mutate(!!sym(group) := droplevels(as.factor(.data[[group]])))
          
          df_filtered <- df %>%
            mutate(temp_trait = as.numeric(.data[[trait]]),
                   temp_group = factor(.data[[group]])) %>%
            filter(!is.na(temp_trait))
          return(df_filtered)
        })
        
        # Reactive for Shapiro-Wilk test results (using base R shapiro.test)
        shapiro_results_r <- reactive({
          df_filtered <- filtered_data_r()
          trait <- selected_trait()
          if (is.null(df_filtered) || is.null(trait)) return(NULL)
          
          shapiro_list <- list()
          groups <- levels(df_filtered$temp_group)
          
          for (g in groups) {
            group_data <- df_filtered %>% filter(temp_group == g)
            if (nrow(group_data) < 3) {
              shapiro_list[[g]] <- data.frame(temp_group = g, variable = trait, statistic = NA, p = NA,
                                              message = "Not enough data points (<3) for Shapiro-Wilk test")
            } else if (length(unique(group_data$temp_trait)) == 1) {
              shapiro_list[[g]] <- data.frame(temp_group = g, variable = trait, statistic = NA, p = NA,
                                              message = "Constant values in group, Shapiro-Wilk test not applicable")
            } else {
              test_result <- tryCatch(stats::shapiro.test(group_data$temp_trait), error = function(e) {
                warning(paste0("Error in Shapiro-Wilk for group ", g, ": ", e$message))
                NULL
              })
              if (!is.null(test_result)) {
                shapiro_list[[g]] <- data.frame(temp_group = g, variable = trait,
                                                statistic = test_result$statistic, p = test_result$p.value)
              } else {
                shapiro_list[[g]] <- data.frame(temp_group = g, variable = trait, statistic = NA, p = NA,
                                                message = "Shapiro-Wilk test failed")
              }
            }
          }
          
          if (length(shapiro_list) > 0) {
            return(do.call(rbind, shapiro_list) %>% as_tibble())
          } else {
            return(NULL)
          }
        })
        
        # Reactive for Levene's test results
        levene_results_r <- reactive({
          df_filtered <- filtered_data_r()
          trait <- selected_trait()
          if (is.null(df_filtered) || is.null(trait)) return(NULL)
          
          if (length(unique(df_filtered$temp_trait)) == 1) {
            warning("Levene's test not applicable: Trait has constant values across all samples.")
            return(NULL)
          }
          
          levene_res <- tryCatch(car::leveneTest(temp_trait ~ temp_group, data = df_filtered), error = function(e) {
            warning("Error in Levene's test: ", e$message)
            NULL
          })
          return(levene_res)
        })
        
        # Run tests and print results for selected trait
        output$test_results <- renderPrint({
          df_filtered <- filtered_data_r()
          trait <- selected_trait()
          
          if (is.null(df_filtered) || is.null(trait)) {
            cat("No trait selected or no adjusted data available. Please upload data and perform allometric correction in the 'Allometric Correction' module.\n")
            return()
          }
          
          cat(sprintf("Adjusted Trait '%s': %d samples.\n\n", trait, nrow(df_filtered)))
          cat("Sample sizes per group:\n")
          print(table(df_filtered$temp_group))
          
          ngroups <- n_distinct(df_filtered$temp_group)
          if (ngroups < 2) {
            cat("\nNot enough groups to perform statistical test (requires at least 2 groups).\n")
            return()
          }
          
          zero_variance_groups <- df_filtered %>%
            group_by(temp_group) %>%
            summarise(sd_val = sd(temp_trait, na.rm = TRUE), .groups = 'drop') %>%
            filter(sd_val == 0)
          
          if (nrow(zero_variance_groups) > 0) {
            cat("\n⚠ Warning: Zero variance detected in the following groups for this trait, which may cause some tests to fail:\n")
            print(zero_variance_groups$temp_group)
          }
          
          if (length(unique(df_filtered$temp_trait)) == 1) {
            cat("\n⚠ Warning: Adjusted trait has constant values across all samples, statistical tests cannot be performed.\n")
            return()
          }
          
          cat("\n--- Testing Assumptions ---\n")
          cat("1) Normality per group: Shapiro-Wilk test\n")
          cat("2) Homogeneity of variance: Levene's test\n\n")
          
          shapiro <- shapiro_results_r()
          levene <- levene_results_r()
          
          if (is.null(shapiro) || all(is.na(shapiro$p))) {
            cat("⚠ Could not perform Shapiro-Wilk test or results are not meaningful (e.g., due to insufficient data points per group, or constant values within groups).\n")
          } else {
            valid_shapiro_p_values <- shapiro$p[!is.na(shapiro$p)]
            if (length(valid_shapiro_p_values) > 0 && all(valid_shapiro_p_values > 0.05)) {
              cat(paste0("\nOverall: Normality assumption is MET for '", trait, "' (all valid p-values > 0.05).\n"))
            } else {
              cat(paste0("\nOverall: Normality assumption is NOT MET for '", trait, "' (at least one valid p-value <= 0.05).\n"))
            }
          }
          
          if (is.null(levene)) {
            cat("\n⚠ Could not perform Levene's test.\n")
          } else {
            cat("\nLevene's test for homogeneity of variance:\n")
            print(levene)
          }
          
          normality_met <- !is.null(shapiro) && all(shapiro$p[!is.na(shapiro$p)] > 0.05)
          variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
          
          force_parametric <- isTRUE(input$force_parametric)
          
          cat(sprintf("\nAssumptions summary:\n - Normality met? %s\n - Homogeneity of variance met? %s\n",
                      ifelse(normality_met, "Yes", "No"),
                      ifelse(variance_met, "Yes", "No")))
          
          cat("\n--- Statistical Testing ---\n")
          
          tryCatch({
            if (ngroups == 2) {
              cat("Comparing 2 groups:\n")
              current_trait_zero_variance <- any(zero_variance_groups$temp_group %in% levels(df_filtered$temp_group))
              if ((normality_met && variance_met && !current_trait_zero_variance) || force_parametric) {
                if ((normality_met && variance_met && !current_trait_zero_variance) || force_parametric) {
                  if (force_parametric && (!normality_met || !variance_met || current_trait_zero_variance)) {
                    cat("⚠ Forcing parametric test despite violated assumptions.\n\n")
                  }
                  cat("Performing parametric Student's t-test.\n\n")
                  
                } else {
                  cat("Assumptions met, performing parametric Student's t-test.\n\n")
                }
                fit <- t.test(temp_trait ~ temp_group, data = df_filtered)
                cat("t-test results:\n")
                print(fit)
              } else {
                cat("Assumptions violated or zero variance, performing non-parametric Wilcoxon test.\n\n")
                fit <- wilcox.test(temp_trait ~ temp_group, data = df_filtered)
                cat("Wilcoxon test results:\n")
                print(fit)
              }
            } else if (ngroups > 2) {
              cat(sprintf("Comparing %d groups:\n", ngroups))
              current_trait_zero_variance <- any(zero_variance_groups$temp_group %in% levels(df_filtered$temp_group))
              
              if ((normality_met && variance_met && !current_trait_zero_variance) || force_parametric) {
                if ((normality_met && variance_met && !current_trait_zero_variance) || force_parametric) {
                  if (force_parametric && (!normality_met || !variance_met || current_trait_zero_variance)) {
                    cat("⚠ Forcing parametric test despite violated assumptions.\n\n")
                  }
                  cat("Performing parametric ANOVA with Tukey HSD post-hoc.\n\n")
                  
                } else {
                  cat("Assumptions met, performing parametric ANOVA with Tukey HSD post-hoc.\n\n")
                }
                fit <- aov(temp_trait ~ temp_group, data = df_filtered)
                cat("ANOVA summary:\n")
                print(summary(fit))
                tukey <- TukeyHSD(fit)[[1]] %>% as.data.frame() %>% rownames_to_column("Comparison")
                cat("\nTukey HSD post-hoc results:\n")
                print(tukey)
              } else {
                cat("Assumptions violated or zero variance, performing non-parametric Kruskal-Wallis test with Dunn post-hoc.\n\n")
                kw <- df_filtered %>% rstatix::kruskal_test(temp_trait ~ temp_group)
                dunn <- df_filtered %>% rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
                  mutate(Comparison = paste(group1, "vs", group2)) %>%
                  select(Comparison, everything(), -group1, -group2)
                cat("Kruskal-Wallis test results:\n")
                print(kw)
                cat("\nDunn post-hoc results (Bonferroni adjusted):\n")
                print(dunn)
              }
            }
          }, error = function(e) {
            cat("Error during statistical testing:\n")
            cat(conditionMessage(e), "\n")
          })
        })
        
        # Compute all pairwise p-value summaries for all traits
        standardize_comparison <- function(a, b) {
          paste(sort(c(a, b)), collapse = " vs ")
        }
        
        # Compute all pairwise p-value summaries for all traits
        all_pairwise_results <- reactive({
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          req(df)
          
          group_col <- names(df)[1]
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, group_col)
          
          results_list <- list()
          
          for (trait in traits) {
            df_trait <- df %>%
              mutate(temp_trait = as.numeric(.data[[trait]]),
                     temp_group = factor(.data[[group_col]])) %>%
              filter(!is.na(temp_trait))
            
            ngroups <- n_distinct(df_trait$temp_group)
            if (ngroups < 2) next
            
            # Assumption checks
            shapiro <- tryCatch(df_trait %>%
                                  group_by(temp_group) %>%
                                  rstatix::shapiro_test(temp_trait), error = function(e) NULL)
            levene <- tryCatch(car::leveneTest(temp_trait ~ temp_group, data = df_trait), error = function(e) NULL)
            
            normality_met <- !is.null(shapiro) && all(shapiro$p > 0.05)
            variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
            force_parametric <- isTRUE(input$force_parametric)
            
            if (ngroups == 2) {
              groups <- levels(df_trait$temp_group)
              comp <- paste(sort(groups), collapse = " vs ")
              
              if (normality_met && variance_met || force_parametric) {
                pval <- tryCatch(t.test(temp_trait ~ temp_group, data = df_trait)$p.value, error = function(e) NA)
                method <- "t-test"
              } else {
                pval <- tryCatch(wilcox.test(temp_trait ~ temp_group, data = df_trait)$p.value, error = function(e) NA)
                method <- "Wilcoxon"
              }
              
              result <- data.frame(
                Comparison = comp,
                p_value = signif(pval, 4),
                Method = method,
                stringsAsFactors = FALSE
              )
              
            } else {
              if (normality_met && variance_met || force_parametric) {
                fit <- aov(temp_trait ~ temp_group, data = df_trait)
                tukey <- TukeyHSD(fit)[[1]] %>%
                  as.data.frame() %>%
                  rownames_to_column("Comparison") %>%
                  mutate(Method = "ANOVA (Tukey)") %>%
                  dplyr::select(Comparison, p_value = `p adj`, Method)
                result <- tukey
              } else {
                dunn <- df_trait %>%
                  rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
                  mutate(Comparison = paste(pmin(group1, group2), "vs", pmax(group1, group2)),
                         Method = "Kruskal (Dunn)") %>%
                  dplyr::select(Comparison, p_value = p.adj, Method)
                result <- dunn
              }
            }
            
            # Normalize all comparisons
            result <- result %>%
              mutate(Comparison = sapply(strsplit(Comparison, "[-]|[ ]vs[ ]", perl = TRUE),
                                         function(x) paste(sort(trimws(x)), collapse = " vs "))) %>%
              group_by(Comparison) %>%
              slice(1) %>%
              ungroup()
            
            results_list[[trait]] <- result
          }
          
          return(results_list)
        })
        
        significant_trait_summary <- reactive({
          pairwise_list <- all_pairwise_results()
          req(pairwise_list)
          
          alpha <- as.numeric(input$alpha_level)
          if (is.null(alpha)) alpha <- 0.05
          
          # Combine the list of data frames into one wide data frame
          all_comparisons <- unique(unlist(lapply(pairwise_list, function(df) df$Comparison)))
          combined_df <- data.frame(Comparison = all_comparisons, stringsAsFactors = FALSE)
          
          for (trait in names(pairwise_list)) {
            df <- pairwise_list[[trait]]
            # Rename columns to have trait prefix
            df <- df %>% 
              dplyr::select(Comparison, p_value, Method) %>% 
              dplyr::rename(!!paste0(trait, "_p_value") := p_value,
                            !!paste0(trait, "_method") := Method)
            combined_df <- merge(combined_df, df, by = "Comparison", all.x = TRUE)
          }
          
          # Now find p-value columns ending with "_p_value"
          pval_cols <- grep("_p_value$", names(combined_df), value = TRUE)
          if (length(pval_cols) == 0) {
            return("No p-value columns found in the combined results.")
          }
          
          # Normalize comparison strings for grouping
          combined_df$NormComparison <- sapply(strsplit(as.character(combined_df$Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
                                               function(x) paste(sort(trimws(x)), collapse = " vs "))
          
          sig_list <- lapply(unique(combined_df$NormComparison), function(norm_comp) {
            rows <- combined_df[combined_df$NormComparison == norm_comp, ]
            sig_traits <- c()
            for (col in pval_cols) {
              pvals <- as.numeric(rows[[col]])
              if (any(!is.na(pvals) & pvals < alpha)) {
                trait <- sub("_p_value$", "", col)
                sig_traits <- c(sig_traits, trait)
              }
            }
            if (length(sig_traits) > 0) {
              return(paste0(norm_comp, ": ", paste(sig_traits, collapse = ", ")))
            }
            return(NULL)
          })
          
          sig_list <- sig_list[!sapply(sig_list, is.null)]
          
          if (length(sig_list) == 0) {
            return("No traits are significant at the selected alpha level.")
          } else {
            return(paste0("List of significant traits:\n\n", paste(trimws(sig_list), collapse = "\n")))
          }
        })
        
        significant_trait_summary_df <- reactive({
          pairwise_list <- all_pairwise_results()
          req(pairwise_list)
          
          alpha <- as.numeric(input$alpha_level)
          if (is.null(alpha)) alpha <- 0.05
          
          # Combine the list of data frames into one wide data frame
          all_comparisons <- unique(unlist(lapply(pairwise_list, function(df) df$Comparison)))
          combined_df <- data.frame(Comparison = all_comparisons, stringsAsFactors = FALSE)
          
          for (trait in names(pairwise_list)) {
            df <- pairwise_list[[trait]]
            df <- df %>% 
              dplyr::select(Comparison, p_value, Method) %>% 
              dplyr::rename(!!paste0(trait, "_p_value") := p_value,
                            !!paste0(trait, "_method") := Method)
            combined_df <- merge(combined_df, df, by = "Comparison", all.x = TRUE)
          }
          
          pval_cols <- grep("_p_value$", names(combined_df), value = TRUE)
          if (length(pval_cols) == 0) {
            return(data.frame(
              Comparison = "No p-value columns found",
              Significant_Traits = "",
              stringsAsFactors = FALSE
            ))
          }
          
          # Normalize comparison strings for grouping
          combined_df$NormComparison <- sapply(strsplit(as.character(combined_df$Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
                                               function(x) paste(sort(trimws(x)), collapse = " vs "))
          
          sig_list <- lapply(unique(combined_df$NormComparison), function(norm_comp) {
            rows <- combined_df[combined_df$NormComparison == norm_comp, ]
            sig_traits <- c()
            for (col in pval_cols) {
              pvals <- as.numeric(rows[[col]])
              if (any(!is.na(pvals) & pvals < alpha)) {
                trait <- sub("_p_value$", "", col)
                sig_traits <- c(sig_traits, trait)
              }
            }
            if (length(sig_traits) > 0) {
              return(data.frame(
                Comparison = norm_comp,
                Significant_Traits = paste(sig_traits, collapse = ", "),
                stringsAsFactors = FALSE
              ))
            }
            return(NULL)
          })
          
          sig_list <- sig_list[!sapply(sig_list, is.null)]
          
          if (length(sig_list) == 0) {
            return(data.frame(
              Comparison = "No significant traits",
              Significant_Traits = "",
              stringsAsFactors = FALSE
            ))
          } else {
            return(do.call(rbind, sig_list))
          }
        })
        
        
        output$download_all_summary <- downloadHandler(
          filename = function() {
            paste0("summary_all_traits_", Sys.Date(), ".csv")
          },
          content = function(file) {
            # Reuse existing reactive
            pairwise_list <- all_pairwise_results()
            if (is.null(pairwise_list)) {
              writeLines("No valid results to export.", file)
              return()
            }
            
            all_comparisons <- sort(unique(unlist(lapply(pairwise_list, function(df) df$Comparison))))
            combined_df <- data.frame(Comparison = all_comparisons, stringsAsFactors = FALSE)
            
            for (trait in names(pairwise_list)) {
              df <- pairwise_list[[trait]]
              df <- df[, c("Comparison", "p_value", "Method")]
              colnames(df)[2:3] <- c(paste0(trait, "_p_adj"), paste0(trait, "_method"))
              combined_df <- merge(combined_df, df, by = "Comparison", all.x = TRUE)
            }
            
            write.csv(combined_df, file, row.names = FALSE)
          },
          contentType = "text/csv"
        )
        
        
        output$download_summary <- downloadHandler(
          filename = function() {
            trait <- selected_trait()
            if (is.null(trait)) trait <- "no_trait_selected"
            paste0("univariate_results_", trait, "_", Sys.Date(), ".csv")
          },
          content = function(file) {
            trait <- selected_trait()
            if (is.null(trait)) {
              writeLines("No trait selected.", file)
              return()
            }
            
            df_source <- data_r()
            df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
            req(df)
            
            group <- names(df)[1]
            df <- df %>% mutate(!!sym(group) := droplevels(as.factor(.data[[group]])))
            df_filtered <- df %>%
              mutate(temp_trait = as.numeric(.data[[trait]]),
                     temp_group = factor(.data[[group]])) %>%
              filter(!is.na(temp_trait))
            
            ngroups <- n_distinct(df_filtered$temp_group)
            if (ngroups < 2 || length(unique(df_filtered$temp_trait)) == 1) {
              writeLines("Not enough valid data to perform test.", file)
              return()
            }
            
            shapiro <- tryCatch(df_filtered %>% group_by(temp_group) %>% rstatix::shapiro_test(temp_trait), error = function(e) NULL)
            levene <- tryCatch(car::leveneTest(temp_trait ~ temp_group, data = df_filtered), error = function(e) NULL)
            
            normality_met <- !is.null(shapiro) && all(shapiro$p > 0.05)
            variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
            force_parametric <- isTRUE(input$force_parametric)
            
            result <- tryCatch({
              if (ngroups == 2) {
                if (normality_met && variance_met || force_parametric) {
                  broom::tidy(t.test(temp_trait ~ temp_group, data = df_filtered))
                } else {
                  broom::tidy(wilcox.test(temp_trait ~ temp_group, data = df_filtered))
                }
              } else {
                if (normality_met && variance_met || force_parametric) {
                  fit <- aov(temp_trait ~ temp_group, data = df_filtered)
                  TukeyHSD(fit)[[1]] %>%
                    as.data.frame() %>%
                    rownames_to_column("Comparison")
                } else {
                  df_filtered %>%
                    rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
                    dplyr::mutate(
                      Comparison = paste(group1, "vs", group2),
                      Method = "Kruskal (Dunn)"
                    ) %>%
                    dplyr::select(Comparison, `p adj` = p.adj, Method)
                }
              }
            }, error = function(e) {
              data.frame(Message = paste("Error:", conditionMessage(e)))
            })
            
            write.csv(result, file, row.names = FALSE)
          },
          contentType = "text/csv"
        )
        
        # Alpha level radio buttons
        output$alpha_selector_ui <- renderUI({
          radioButtons(
            ns("alpha_level"),
            label = strong("Select alpha level for significance"),
            choices = c("0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001),
            selected = 0.05,
            inline = TRUE
          )
        })
        
        # Display of significant traits
        output$significant_traits_ui <- renderUI({
          verbatimTextOutput(ns("significant_traits"))
        })
        
        output$download_significant_traits <- downloadHandler(
          filename = function() {
            alpha <- as.numeric(input$alpha_level)
            if (is.null(alpha)) alpha <- 0.05
            paste0("significant_traits_alpha_", alpha, "_", Sys.Date(), ".csv")
          },
          content = function(file) {
            df <- significant_trait_summary_df()
            req(df)
            write.csv(df, file, row.names = FALSE)
          }
        )
        
        output$significant_traits <- renderText({
          significant_trait_summary()
        })

      } 
    }) 
    
    # PERMANOVA 
    observeEvent(input$main_tabs, {
      if (input$main_tabs == "Multivariate (PERMANOVA)") {
        
        # Helper function for pairwise.adonis, now using adonis2
        pairwise.adonis <- function(x, factors, sim.method = 'euclidean', p.adjust.m ='bonferroni', permutations = 50000) {
          co = combn(unique(as.character(factors)),2)
          pairs = c()
          F.Model =c()
          R2 = c()
          p.value = c()
          
          total_pairs <- ncol(co)
          progress_start_value <- 0.5
          progress_step_size <- (1.0 - progress_start_value) / total_pairs
          
          for(elem in 1:total_pairs){
            grp1 = co[1,elem]
            grp2 = co[2,elem]
            
            x_subset = x[factors %in% c(grp1, grp2),, drop = FALSE]
            factors_subset = factors[factors %in% c(grp1, grp2)]
            
            factors_subset = droplevels(as.factor(factors_subset))
            
            # Check for constant values within subsetted traits data
            if (ncol(x_subset) > 0 && all(apply(x_subset, 2, function(col) length(unique(col)) == 1))) {
              warning(paste("All traits in subset for", grp1, "vs", grp2, "are constant. Cannot compute distance matrix."))
              pairs = c(pairs, paste(grp1, 'vs', grp2));
              F.Model =c(F.Model, NA);
              R2 = c(R2, NA);
              p.value =c(p.value, NA);
              incProgress(progress_step_size, detail = paste("Skipping (constant data):", grp1, "vs", grp2))
              next
            }
            
            # Calculate distance matrix for the subsetted data
            x1 = vegan::vegdist(x_subset, method=sim.method)
            
            ad = vegan::adonis2(x1 ~ factors_subset, permutations = permutations);
            
            pairs = c(pairs, paste(grp1, 'vs', grp2));
            F.Model =c(F.Model, ad$F[1]);
            R2 = c(R2, ad$R2[1]);
            p.value =c(p.value, ad$`Pr(>F)`[1]);
            
            incProgress(progress_step_size, detail = paste("Comparing", grp1, "vs", grp2))
          }
          p.adjusted = p.adjust(p.value,method=p.adjust.m)
          sig = c(rep('',length(p.adjusted)))
          sig[p.adjusted <= 0.05] <-'.'
          sig[p.adjusted <= 0.01] <-'*'
          sig[p.adjusted <= 0.001] <-'**'
          sig[p.adjusted <= 0.0001] <-'***'
          
          pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
          return(pairw.res)
        }
        
        # Observe for PERMANOVA
        observeEvent(input$run_permanova, {
          shinyjs::addClass(id = "run_permanova", class = "module-active")
          
          withProgress(message = 'PERMANOVA Analysis in progress', value = 0, {
            incProgress(0.1, detail = "Checking data and parameters...")
            req(data_r())
            
            df_source <- data_r()
            df_for_permanova <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
            
            group_col_name <- names(df_for_permanova)[1]
            
            # Get numeric traits data
            traits_data_raw <- df_for_permanova %>% dplyr::select(where(is.numeric))
            
            if (group_col_name %in% names(traits_data_raw)) {
              traits_data_raw <- traits_data_raw %>% dplyr::select(-!!sym(group_col_name))
            }
            
            complete_cases_idx <- complete.cases(traits_data_raw, df_for_permanova[[group_col_name]])
            if (!any(complete_cases_idx)) {
              showNotification("No complete cases found for selected traits. PERMANOVA cannot be performed.", type = "error")
              permanova_main_results_r(NULL)
              permanova_pairwise_results_r(NULL)
              shinyjs::removeClass(id = "run_permanova", class = "module-active")
              return()
            }
            
            traits_data_clean <- traits_data_raw[complete_cases_idx, , drop = FALSE]
            species_data_clean <- df_for_permanova[[group_col_name]][complete_cases_idx]
            species_data_clean <- droplevels(as.factor(species_data_clean)) # Ensure factor levels are correct
            
            # Check for sufficient data after cleaning
            if (nrow(traits_data_clean) < 2 || ncol(traits_data_clean) < 1 || n_distinct(species_data_clean) < 2) {
              showNotification("Not enough complete data (rows < 2), numeric traits (columns < 1), or distinct groups (groups < 2) to perform PERMANOVA. Please check your data.", type = "error")
              permanova_main_results_r(NULL)
              permanova_pairwise_results_r(NULL)
              shinyjs::removeClass(id = "run_permanova", class = "module-active")
              return()
            }
            
            # Check for constant values in traits_data_clean
            if (all(apply(traits_data_clean, 2, function(col) length(unique(col)) == 1))) {
              showNotification("All selected numeric traits have constant values. PERMANOVA cannot be performed on constant data.", type = "error")
              permanova_main_results_r(NULL)
              permanova_pairwise_results_r(NULL)
              shinyjs::removeClass(id = "run_permanova", class = "module-active")
              return()
            }
            
            # Data preparation based on PCA option 
            permanova_data_input <- traits_data_clean
            analysis_description <- "original trait space"
            
            if (input$use_pca) {
              incProgress(0.2, detail = "Using PCA for PERMANOVA...")
              
              # Get PCA results from pca_results_r (already existing)
              pca <- pca_results_r()
              req(pca)
              
              # Use PCA scores for PERMANOVA
              permanova_data_input <- pca$x[, apply(pca$x, 2, var) > 1e-9, drop = FALSE]  # Filter out PCs with effectively zero variance
              analysis_description <- "PCA scores space"
              
              if (ncol(permanova_data_input) == 0) {
                showNotification("PCA resulted in no components with variance. PERMANOVA cannot be performed on PCA scores.", type = "error")
                permanova_main_results_r(NULL)
                permanova_pairwise_results_r(NULL)
                shinyjs::removeClass(id = "run_permanova", class = "module-active")
                return()
              }
            }
            
            # Run main adonis2 (PERMANOVA)
            incProgress(0.3, detail = paste("Calculating main PERMANOVA in", analysis_description, "..."))
            main_result <- tryCatch({
              vegan::adonis2(formula = permanova_data_input ~ species_data_clean,
                             permutations = input$permanova_permutations,
                             method = input$permanova_distance_method)
            }, error = function(e) {
              showNotification(paste("Error running main PERMANOVA:", e$message), type = "error")
              permanova_main_results_r(NULL)
              permanova_pairwise_results_r(NULL)
              shinyjs::removeClass(id = "run_permanova", class = "module-active")
              return(NULL)
            })
            
            permanova_main_results_r(main_result)
            
            # Check if significant and run pairwise comparisons if necessary
            if (!is.null(main_result) && "Pr(>F)" %in% names(main_result) &&
                main_result$`Pr(>F)`[1] < 0.05 && n_distinct(species_data_clean) > 2) {
              incProgress(0.5, detail = "Main PERMANOVA significant. Running pairwise comparisons...")
              pairwise_result <- tryCatch({
                pairwise.adonis(x = permanova_data_input,
                                factors = species_data_clean,
                                sim.method = input$permanova_distance_method,
                                permutations = input$permanova_permutations)
              }, error = function(e) {
                showNotification(paste("Error running pairwise PERMANOVA:", e$message), type = "error")
                return(NULL)
              })
              permanova_pairwise_results_r(pairwise_result)
            } else {
              reason <- if (is.null(main_result)) "Main PERMANOVA failed" else if (main_result$`Pr(>F)`[1] >= 0.05) "Main PERMANOVA not significant" else "Less than 3 groups"
              permanova_pairwise_results_r(data.frame(Message = paste("Pairwise test not performed (", reason, ").")))
              incProgress(0.9, detail = paste("Skipping pairwise test:", reason))
            }
            
            Sys.sleep(0.5)
            
            incProgress(1, detail = "Analysis complete.")
            showNotification("PERMANOVA analysis completed. Results are displayed below.", type = "default")
          })
          
          shinyjs::removeClass(id = "run_permanova", class = "module-active")
        })
        
        output$permanova_main_results <- renderPrint({
          req(permanova_main_results_r())
          print(permanova_main_results_r())
        })
        
        output$permanova_pairwise_results <- renderDT({
          req(permanova_pairwise_results_r())
          if ("Message" %in% names(permanova_pairwise_results_r())) {
            datatable(permanova_pairwise_results_r(), options = list(dom = 't', paging = FALSE, searching = FALSE))
          } else {
            datatable(permanova_pairwise_results_r(), options = list(dom = 't', paging = FALSE, searching = FALSE)) %>%
              formatRound(columns = c("p.value", "p.adjusted"), digits = 4)
          }
        })
        
        output$download_permanova_main <- downloadHandler(
          filename = function() { paste0("permanova_main_results_", Sys.Date(), ".txt") },
          content = function(file) {
            req(permanova_main_results_r())
            sink(file)
            print(permanova_main_results_r())
            sink()
          }
        )
        
        output$download_permanova_pairwise <- downloadHandler(
          filename = function() { paste0("permanova_pairwise_results_", Sys.Date(), ".csv") },
          content = function(file) {
            req(permanova_pairwise_results_r())
            write.csv(permanova_pairwise_results_r(), file, row.names = FALSE)
          }
        )
      } 
    }) 
    
    # PCAtest 
    pcatest_output_text_r <- reactiveVal(NULL)
    
    observeEvent(input$main_tabs, {
      if (input$main_tabs == "PCAtest") {
        
        observeEvent(input$run_pcatest, {
          shinyjs::addClass(id = "run_pcatest", class = "module-active")
          
          withProgress(message = 'PCAtest Analysis in progress', value = 0, {
            incProgress(0.1, detail = "Preparing data for PCAtest...")
            req(data_r())
            
            df_source <- data_r()
            df_for_pcatest <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
            
            group_col_name <- names(df_for_pcatest)[1]
            traits_data_raw <- df_for_pcatest %>% dplyr::select(where(is.numeric))
            
            if (group_col_name %in% names(traits_data_raw)) {
              traits_data_raw <- traits_data_raw %>% dplyr::select(-!!sym(group_col_name))
            }
            
            complete_cases_idx <- complete.cases(traits_data_raw)
            if (!any(complete_cases_idx)) {
              showNotification("No complete cases found for selected traits. PCAtest cannot be performed.", type = "error")
              pcatest_results_r(NULL)
              pcatest_output_text_r(NULL)
              shinyjs::removeClass(id = "run_pcatest", class = "module-active")
              return()
            }
            
            traits_data_clean <- traits_data_raw[complete_cases_idx, , drop = FALSE]
            
            if (nrow(traits_data_clean) < 2 || ncol(traits_data_clean) < 2) {
              showNotification("Not enough complete data (rows < 2) or numeric traits (columns < 2) to perform PCAtest. Please check your data.", type = "error")
              pcatest_results_r(NULL)
              pcatest_output_text_r(NULL)
              shinyjs::removeClass(id = "run_pcatest", class = "module-active")
              return()
            }
            
            if (all(apply(traits_data_clean, 2, function(col) length(unique(col)) == 1))) {
              showNotification("All selected numeric traits have constant values. PCAtest cannot be performed on constant data.", type = "error")
              pcatest_results_r(NULL)
              pcatest_output_text_r(NULL)
              shinyjs::removeClass(id = "run_pcatest", class = "module-active")
              return()
            }
            
            incProgress(0.3, detail = "Running PCAtest (this may take a while)...")
            
            pcatest_capture <- tryCatch({
              output_con <- textConnection("output_lines", "w", local = TRUE)
              message_con <- textConnection("message_lines", "w", local = TRUE)
              
              sink(output_con, type = "output")
              sink(message_con, type = "message")
              
              result <- PCAtest::PCAtest(
                x = traits_data_clean,
                nperm = input$pcatest_permutations,
                indload = TRUE,
                plot = FALSE,
                counter = FALSE  
              )
              
              # Restore output
              sink(type = "message")
              sink(type = "output")
              
              # Close connections
              close(output_con)
              close(message_con)
              
              # Combine and filter output
              full_output <- c(output_lines, message_lines)
              
              # Find the starting point of the actual results
              start_index <- grep("===.*Test of PCA significance", full_output)
              
              if (length(start_index) > 0) {
                # Keep everything from the header to the end
                filtered_output <- full_output[start_index[1]:length(full_output)]
                filtered_output <- paste(filtered_output, collapse = "\n")
              } else {
                # Fallback to full output if pattern not found
                filtered_output <- paste(full_output, collapse = "\n")
              }
              
              list(result = result, output = filtered_output)
            }, error = function(e) {
              showNotification(paste("Error running PCAtest:", e$message), type = "error")
              return(NULL)
            }, finally = {
              # Safely restore all sinks and connections
              silent_sink_restore <- function() {
                try(sink(type = "message"), silent = TRUE)
                try(sink(type = "output"), silent = TRUE)
                if (exists("output_con")) try(close(output_con), silent = TRUE)
                if (exists("message_con")) try(close(message_con), silent = TRUE)
              }
              silent_sink_restore()
            })
            
            if (!is.null(pcatest_capture)) {
              pcatest_results_r(pcatest_capture$result)
              pcatest_output_text_r(pcatest_capture$output)
            }
            
            Sys.sleep(0.5)
            incProgress(1, detail = "Analysis complete.")
            showNotification("PCAtest analysis completed. Results are displayed below.", type = "default")
          })
          
          shinyjs::removeClass(id = "run_pcatest", class = "module-active")
        })
        
        # Render captured PCAtest output
        output$pcatest_main_results <- renderPrint({
          req(pcatest_output_text_r())
          cat(pcatest_output_text_r())
        })
        
        output$pcatest_trait_contributions <- renderDT({
          req(pcatest_results_r())
          pcatest_loadings <- pcatest_results_r()$indexloadobs
          if (!is.null(pcatest_loadings) && nrow(pcatest_loadings) > 0) {
            datatable(pcatest_loadings, options = list(dom = 't', paging = FALSE, searching = FALSE)) %>%
              formatRound(columns = names(pcatest_loadings)[sapply(pcatest_loadings, is.numeric)], digits = 4)
          } else {
            data.frame(Message = "Trait contributions (index loadings) not available in PCAtest results. This may occur if no significant PCs were found or if there was an issue with the analysis.")
          }
        })
        
        output$download_pcatest_main <- downloadHandler(
          filename = function() { paste0("pcatest_main_results_", Sys.Date(), ".txt") },
          content = function(file) {
            req(pcatest_output_text_r()) # Use the captured text for download
            writeLines(pcatest_output_text_r(), file)
          }
        )
        
        output$download_pcatest_contributions <- downloadHandler(
          filename = function() { paste0("pcatest_trait_contributions_", Sys.Date(), ".csv") },
          content = function(file) {
            req(pcatest_results_r())
            pcatest_loadings <- pcatest_results_r()$indexloadobs
            if (!is.null(pcatest_loadings) && nrow(pcatest_loadings) > 0) {
              write.csv(pcatest_loadings, file, row.names = FALSE)
            } else {
              writeLines("No trait contributions data available.", file)
            }
          }
        )
      }
    })
    
    return(data_r)
  })
}
