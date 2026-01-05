mod_inferential_ui_meristic <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Inferential Statistics"),
    hr(),
    br(),
    
    tabsetPanel(id = ns("main_tabs"), 
                tabPanel("Univariate",
                         br(),
                         h4("Univariate Analysis"),
                         p("Perform univariate analyses to determine which traits are (or are not) statistically different between groups. This module automatically detects the number of groups, tests for assumptions of normality and homogeneity of variance, and selects the appropriate statistical tests (t-test or Wilcoxon/Mann-Whitney if only two groups are detected, ANOVA or Kruskal-Wallis if >2 groups, followed by the appropriate ad hoc tests)."),
                         
                         br(), 
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
                         downloadButton(ns("download_all_trait_summary"), "Download Summary of All Traits"),
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
                         hr(),
                         p(strong("Note:"), "See the",
                           strong("Visualization > PCA/DAPC"), "tab for the PCA/DAPC plots."),
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
                         p(em("Camargo, A. (2022). PCAtest: testing the statistical significance of Principal Component Analysis in R. PeerJ, 10:e12967. https://doi.org/10.7717/peerj.12967")),
                         
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

mod_inferential_server_meristic <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    
    selected_trait <- reactiveVal(NULL)
    permanova_main_results_r <- reactiveVal(NULL) 
    permanova_pairwise_results_r <- reactiveVal(NULL) 
    pcatest_results_r <- reactiveVal(NULL) 
    pcatest_output_text_r <- reactiveVal(NULL) 


    # PCA calculation 
    pca_results_r <- reactive({
      df <- data_r()
      req(df)
      data_mat_numeric <- as.data.frame(lapply(df[, -1], as.numeric))
      complete_rows <- complete.cases(data_mat_numeric)
      
      if (sum(complete_rows) < 2 || ncol(data_mat_numeric) < 2) return(NULL)
      
      prcomp(data_mat_numeric[complete_rows, ], center = TRUE, scale. = TRUE)
    })
    
    # PCA summary table 
    pca_summary_results_r <- reactive({
      pca <- pca_results_r()
      req(pca)
      
      eigenvalues <- pca$sdev^2
      eigen_df <- data.frame(Metric = "Eigenvalue", t(as.data.frame(eigenvalues)))
      colnames(eigen_df)[-1] <- paste0("PC", seq_along(eigenvalues))
      
      variance_explained <- summary(pca)$importance[2, ]
      variance_df <- data.frame(Metric = "Proportion of Variance", t(as.data.frame(variance_explained)))
      colnames(variance_df)[-1] <- paste0("PC", seq_along(variance_explained))
      
      cumulative_variance <- summary(pca)$importance[3, ]
      cumulative_df <- data.frame(Metric = "Cumulative Proportion", t(as.data.frame(cumulative_variance)))
      colnames(cumulative_df)[-1] <- paste0("PC", seq_along(cumulative_variance))
      
      loadings_df <- as.data.frame(pca$rotation) %>%
        tibble::rownames_to_column("Trait_Loading")
      
      all_pc_cols <- unique(c(colnames(eigen_df)[-1], colnames(loadings_df)[-1]))
      
      ensure_pc_cols <- function(df, cols) {
        missing_cols <- setdiff(cols, colnames(df))
        for (mc in missing_cols) df[[mc]] <- NA
        ordered_pc_cols <- cols[order(as.numeric(gsub("PC", "", cols)))]
        df %>% dplyr::select(Metric, all_of(ordered_pc_cols))
      }
      
      combined_df <- bind_rows(
        ensure_pc_cols(eigen_df, all_pc_cols),
        ensure_pc_cols(variance_df, all_pc_cols),
        ensure_pc_cols(cumulative_df, all_pc_cols),
        loadings_df %>% rename(Metric = Trait_Loading)
      )
      
      numeric_cols <- names(combined_df)[sapply(combined_df, is.numeric)]
      combined_df[numeric_cols] <- lapply(combined_df[numeric_cols], function(x) round(x, 4))
      
      combined_df
    })
    
    output$pca_summary_table <- renderDataTable({
      req(pca_summary_results_r())
      pca_summary_results_r()
    }, options = list(scrollX = TRUE))
    
    
    output$download_pca_summary <- downloadHandler(
      filename = function() {
        paste0("pca_summary_meristic_", Sys.Date(), ".csv")
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
      
      cat("\nLoadings (Rotation matrix):\n")
      print(pca$rotation)
    })
    
    
    # Univariate tests
    observeEvent(input$main_tabs, {
      if (input$main_tabs == "Univariate") {
        
        output$trait_buttons <- renderUI({
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          req(df)
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, names(df)[1])
          if (length(traits) == 0) return(p("No numeric traits found in data"))
          
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
        
        
        # Initialize selected_trait with first numeric trait when data loads
        observeEvent(data_r(), {
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          req(df)
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, names(df)[1])
          if (length(traits) > 0) {
            selected_trait(traits[1])
          }
        }, ignoreNULL = TRUE)
        
        observe({
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          req(df)
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, names(df)[1])
          for (trait in traits) {
            local({
              mytrait <- trait
              btn_id <- paste0("btn_", mytrait)
              observeEvent(input[[btn_id]], { 
                selected_trait(mytrait)
              }, ignoreInit = TRUE)
            })
          }
        })
        
        output$trait_name <- renderText({
          trait <- selected_trait()
          if (is.null(trait)) {
            "Select a trait above to run inferential statistics."
          } else {
            paste("Running inferential statistics on trait:", trait)
          }
        })
        
        # This reactive depends on selected_trait() AND input$force_parametric
        output$test_results <- renderPrint({
          trait <- selected_trait()
          if (is.null(trait)) {
            cat("No trait selected.\n")
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
          
          cat(sprintf("Trait '%s': %d samples after filtering missing values.\n\n", trait, nrow(df_filtered)))
          cat("Sample sizes per group:\n")
          print(table(df_filtered$temp_group))
          
          ngroups <- n_distinct(df_filtered$temp_group)
          if (ngroups < 2) {
            cat("\nNot enough groups to perform statistical test.\n")
            return()
          }
          
          cat("\n--- Testing Assumptions ---\n")
          cat("1) Normality per group: Shapiro-Wilk test\n")
          cat("2) Homogeneity of variance: Levene's test\n\n")
          
          shapiro <- tryCatch(df_filtered %>% group_by(temp_group) %>% rstatix::shapiro_test(temp_trait), error = function(e) NULL)
          levene <- tryCatch(car::leveneTest(temp_trait ~ temp_group, data = df_filtered), error = function(e) NULL)
          
          if (is.null(shapiro)) {
            cat("⚠ Could not perform Shapiro-Wilk test.\n")
          } else {
            cat("Shapiro-Wilk test p-values by group:\n")
            print(shapiro)
          }
          
          if (is.null(levene)) {
            cat("⚠ Could not perform Levene's test.\n")
          } else {
            cat("\nLevene's test for homogeneity of variance:\n")
            print(levene)
          }
          
          normality_met <- !is.null(shapiro) && all(shapiro$p > 0.05)
          variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
          
          # Access input$force_parametric directly (it's namespaced via moduleServer)
          force_parametric <- isTRUE(input$force_parametric) 
          
          cat(sprintf("\nAssumptions summary:\n - Normality met? %s\n - Homogeneity of variance met? %s\n",
                      ifelse(normality_met, "Yes", "No"),
                      ifelse(variance_met, "Yes", "No")))
          
          cat("\n--- Statistical Testing ---\n")
          
          tryCatch({
            if (ngroups == 2) {
              cat("Comparing 2 groups:\n")
              if (normality_met && variance_met || force_parametric) {
                if (force_parametric && (!normality_met || !variance_met)) {
                  cat("⚠ Forcing parametric test despite violated assumptions.\n\n")
                }
                cat("Performing parametric Student's t-test.\n\n")
                fit <- t.test(temp_trait ~ temp_group, data = df_filtered)
                cat("t-test results:\n")
                print(fit)
              } else {
                cat("Assumptions violated, performing non-parametric Wilcoxon test.\n\n")
                fit <- wilcox.test(temp_trait ~ temp_group, data = df_filtered)
                cat("Wilcoxon test results:\n")
                print(fit)
              }
            } else if (ngroups > 2) {
              cat(sprintf("Comparing %d groups:\n", ngroups))
              if (normality_met && variance_met || force_parametric) {
                if (force_parametric && (!normality_met || !variance_met)) {
                  cat("⚠ Forcing parametric test despite violated assumptions.\n\n")
                }
                cat("Performing parametric ANOVA with Tukey HSD post-hoc.\n\n")
                fit <- aov(temp_trait ~ temp_group, data = df_filtered)
                cat("ANOVA summary:\n")
                print(summary(fit))
                tukey <- TukeyHSD(fit)[[1]] %>% as.data.frame() %>% rownames_to_column("Comparison")
                cat("\nTukey HSD post-hoc results:\n")
                print(tukey)
              } else {
                cat("Assumptions violated, performing non-parametric Kruskal-Wallis test with Dunn post-hoc.\n\n")
                kw <- df_filtered %>% rstatix::kruskal_test(temp_trait ~ temp_group)
                dunn <- df_filtered %>% rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
                  mutate(Comparison = paste(group1, "vs", group2)) %>%
                  dplyr::select(Comparison, everything(), -group1, -group2)
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
        
        # Compute all pairwise p-value summaries
        # This reactive depends on data_r() and input$force_parametric
        all_pairwise_results <- reactive({
          # Explicitly depend on the checkbox state
          force_parametric <- input$force_parametric
          
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          req(df)
          
          group <- names(df)[1]
          df <- df %>% mutate(!!sym(group) := droplevels(as.factor(.data[[group]])))
          
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, names(df)[1])
          
          if (length(traits) == 0) return(NULL)
          
          # Initialize progress
          withProgress(message = 'Calculating pairwise results', value = 0, {
            pairwise_list <- lapply(seq_along(traits), function(i) {
              trait <- traits[i]
              incProgress(1/length(traits), detail = paste("Processing", trait))
              
              df_trait_filtered <- df %>%
                mutate(temp_trait = as.numeric(.data[[trait]]),
                       temp_group = factor(.data[[group]])) %>%
                filter(!is.na(temp_trait))
              
              ngroups <- n_distinct(df_trait_filtered$temp_group)
              if (ngroups < 2) return(NULL)
              
              # Calculate assumptions
              shapiro <- tryCatch({
                df_trait_filtered %>% 
                  group_by(temp_group) %>% 
                  rstatix::shapiro_test(temp_trait)
              }, error = function(e) NULL)
              
              levene <- tryCatch({
                car::leveneTest(temp_trait ~ temp_group, data = df_trait_filtered)
              }, error = function(e) NULL)
              
              normality_met <- !is.null(shapiro) && all(shapiro$p > 0.05)
              variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
              
              # Perform appropriate test based on assumptions and checkbox
              result <- tryCatch({
                if (ngroups == 2) {
                  if (normality_met && variance_met || force_parametric) {
                    # Parametric t-test
                    tt <- t.test(temp_trait ~ temp_group, data = df_trait_filtered)
                    data.frame(
                      Comparison = paste(levels(df_trait_filtered$temp_group), collapse = " vs "),
                      p_value = signif(tt$p.value, 3),
                      Method = "t-test",
                      stringsAsFactors = FALSE
                    )
                  } else {
                    # Non-parametric Wilcoxon
                    wt <- wilcox.test(temp_trait ~ temp_group, data = df_trait_filtered)
                    data.frame(
                      Comparison = paste(levels(df_trait_filtered$temp_group), collapse = " vs "),
                      p_value = signif(wt$p.value, 3),
                      Method = "Wilcoxon",
                      stringsAsFactors = FALSE
                    )
                  }
                } else {
                  # More than 2 groups
                  if (normality_met && variance_met || force_parametric) {
                    # Parametric ANOVA with Tukey HSD
                    aov_mod <- aov(temp_trait ~ temp_group, data = df_trait_filtered)
                    tukey <- TukeyHSD(aov_mod)[[1]] %>%
                      as.data.frame() %>%
                      rownames_to_column("Comparison") %>%
                      dplyr::select(Comparison, `p adj`) %>%
                      rename(p_value = `p adj`) %>%
                      mutate(Method = "ANOVA (Tukey)")
                  } else {
                    # Non-parametric Kruskal-Wallis with Dunn
                    kw <- df_trait_filtered %>%
                      rstatix::kruskal_test(temp_trait ~ temp_group)
                    
                    dunn <- df_trait_filtered %>%
                      rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
                      mutate(Comparison = paste(group1, "vs", group2),
                             Method = "Kruskal (Dunn)") %>%
                      dplyr::select(Comparison, p_value = p.adj, Method)
                    
                    # Return only the Dunn post-hoc for the download
                    dunn
                    
                  }
                }
              }, error = function(e) {
                data.frame(
                  Comparison = paste0(trait, " - Error: ", conditionMessage(e)),
                  p_value = NA,
                  Method = "Error",
                  stringsAsFactors = FALSE
                )
              })
              
              if (!is.null(result)) {
                # Rename p_value column to include trait name
                names(result)[names(result) == "p_value"] <- paste0(trait, "_p-value")
              }
              return(result)
            })
            
            # Filter out NULL results
            pairwise_list <- pairwise_list[!sapply(pairwise_list, is.null)]
            
            if (length(pairwise_list) == 0) return(NULL)
            
            # Combine all results
            if (length(pairwise_list) == 1) {
              return(pairwise_list[[1]])
            } else {
              # Full join all data frames by Comparison and Method columns
              combined <- Reduce(function(x, y) {
                full_join(x, y, by = c("Comparison", "Method"))
              }, pairwise_list)
              return(combined)
            }
          })
        })
        
        significant_trait_summary <- reactive({
          df <- all_pairwise_results()
          req(df)
          
          alpha <- as.numeric(input$alpha_level)
          if (is.null(alpha)) alpha <- 0.05
          
          pval_cols <- grep("_p-value$", names(df), value = TRUE)
          req(length(pval_cols) > 0)
          
          # Normalize all comparisons first: split, sort, join back
          df$NormComparison <- sapply(strsplit(as.character(df$Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
                                      function(x) paste(sort(trimws(x)), collapse = " vs "))
          
          # Group rows by normalized comparison, aggregate significant traits for each pair
          sig_list <- lapply(unique(df$NormComparison), function(norm_comp) {
            rows <- df[df$NormComparison == norm_comp, ]
            sig_traits <- c()
            for (col in pval_cols) {
              pvals <- as.numeric(rows[[col]])
              if (any(!is.na(pvals) & pvals < alpha)) {
                trait <- sub("_p-value$", "", col)
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
          df <- all_pairwise_results()
          req(df)
          
          alpha <- as.numeric(input$alpha_level)
          if (is.null(alpha)) alpha <- 0.05
          
          pval_cols <- grep("_p-value$", names(df), value = TRUE)
          req(length(pval_cols) > 0)
          
          # Normalize all comparisons first: split, sort, join back
          df$NormComparison <- sapply(strsplit(as.character(df$Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
                                      function(x) paste(sort(trimws(x)), collapse = " vs "))
          
          # Group rows by normalized comparison, aggregate significant traits for each pair
          sig_list <- lapply(unique(df$NormComparison), function(norm_comp) {
            rows <- df[df$NormComparison == norm_comp, ]
            sig_traits <- c()
            for (col in pval_cols) {
              pvals <- as.numeric(rows[[col]])
              if (any(!is.na(pvals) & pvals < alpha)) {
                trait <- sub("_p-value$", "", col)
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
        
        output$download_all_trait_summary <- downloadHandler(
          filename = function() {
            paste0("summary_all_traits_", Sys.Date(), ".csv")
          },
          content = function(file) {
            df_source <- data_r()
            df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
            req(df)
            
            group <- names(df)[1]
            traits <- names(df)[sapply(df, is.numeric)]
            traits <- setdiff(traits, group)
            
            results_list <- list()
            
            for (trait in traits) {
              df_trait <- df %>%
                mutate(temp_trait = as.numeric(.data[[trait]]),
                       temp_group = factor(.data[[group]])) %>%
                filter(!is.na(temp_trait))
              
              ngroups <- n_distinct(df_trait$temp_group)
              if (ngroups < 2) next
              
              # Assumption checks
              shapiro <- tryCatch(df_trait %>% group_by(temp_group) %>% rstatix::shapiro_test(temp_trait), error = function(e) NULL)
              levene <- tryCatch(car::leveneTest(temp_trait ~ temp_group, data = df_trait), error = function(e) NULL)
              
              normality_met <- !is.null(shapiro) && all(shapiro$p > 0.05)
              variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
              force_parametric <- isTRUE(input$force_parametric)
              
              if (ngroups == 2) {
                # 2-group test: t-test or Wilcoxon
                levels_vec <- levels(df_trait$temp_group)
                comp <- paste(sort(levels_vec), collapse = " vs ")
                
                if (normality_met && variance_met || force_parametric) {
                  pval <- tryCatch(t.test(temp_trait ~ temp_group, data = df_trait)$p.value, error = function(e) NA)
                  method <- "t-test"
                } else {
                  pval <- tryCatch(wilcox.test(temp_trait ~ temp_group, data = df_trait)$p.value, error = function(e) NA)
                  method <- "Wilcoxon"
                }
                
                out <- data.frame(
                  Comparison = comp,
                  p.adj = signif(pval, 4),
                  Method = method,
                  stringsAsFactors = FALSE
                )
              } else {
                # 3+ groups: Tukey or Dunn
                if (normality_met && variance_met || force_parametric) {
                  fit <- aov(temp_trait ~ temp_group, data = df_trait)
                  out <- TukeyHSD(fit)[[1]] %>%
                    as.data.frame() %>%
                    rownames_to_column("Comparison") %>%
                    dplyr::select(Comparison, p.adj = `p adj`) %>%
                    mutate(Method = "ANOVA (Tukey)")
                } else {
                  out <- df_trait %>%
                    rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
                    mutate(Comparison = paste(pmin(group1, group2), "vs", pmax(group1, group2)),
                           Method = "Kruskal (Dunn)") %>%
                    dplyr::select(Comparison, p.adj = p.adj, Method)
                }
              }
              
              # Normalize and de-duplicate comparisons (e.g., B vs A → A vs B)
              out <- out %>%
                mutate(Comparison = sapply(strsplit(as.character(Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
                                           function(x) paste(sort(trimws(x)), collapse = " vs "))) %>%
                group_by(Comparison) %>%
                slice(1) %>%
                ungroup()
              
              
              names(out)[names(out) == "p.adj"] <- paste0(trait, "_p_adj")
              names(out)[names(out) == "Method"] <- paste0(trait, "_method")
              
              results_list[[trait]] <- out
            }
            
            if (length(results_list) == 0) {
              writeLines("No valid traits for summary.", file)
            } else {
              combined <- Reduce(function(x, y) full_join(x, y, by = "Comparison"), results_list)
              write.csv(combined, file, row.names = FALSE)
            }
          }
          
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
            if (ngroups < 2) {
              writeLines("Not enough groups to perform test.", file)
              return()
            }
            
            # Assumption checks
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
                  TukeyHSD(fit)[[1]] %>% as.data.frame() %>% rownames_to_column("Comparison")
                } else {
                  df_filtered %>%
                    rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
                    dplyr::mutate(
                      Comparison = paste(group1, "vs", group2), # Create the Comparison column
                      Method = "Kruskal (Dunn)"                 # Create the Method column with a fixed string
                    ) %>%
                    dplyr::select(Comparison, `p adj` = p.adj, Method) # Select the desired columns, renaming p.adj
                }
              }
            }, error = function(e) {
              data.frame(Message = paste("Error:", conditionMessage(e)))
            })
            
            write.csv(result, file, row.names = FALSE)
          },
          contentType = "text/csv"
        )

        observeEvent(input$run_analysis, {
        })
        
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
        
        # Helper function for pairwise.adonis
        pairwise.adonis_meristic <- function(x, factors, sim.method = 'euclidean', p.adjust.m ='bonferroni', permutations = 50000) {
          co = combn(unique(as.character(factors)),2)
          pairs = c()
          F.Model =c()
          R2 = c()
          p.value = c()
          
          total_pairs <- ncol(co)

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
              next
            }
            
            # Calculate distance matrix for the subsetted data
            x1 = vegan::vegdist(x_subset, method=sim.method)
            
            ad = vegan::adonis2(x1 ~ factors_subset, permutations = permutations);
            
            pairs = c(pairs, paste(grp1, 'vs', grp2));
            F.Model =c(F.Model, ad$F[1]);
            R2 = c(R2, ad$R2[1]);
            p.value =c(p.value, ad$`Pr(>F)`[1]);
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
            df_source <- data_r()
            df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
            req(df)
            
            group_col_name <- names(df)[1]
            
            # Get numeric traits data (all columns except the first one assumed to be numeric traits)
            traits_data_raw <- df %>% dplyr::select(where(is.numeric))
            # Ensure the group column is not treated as a trait if it's numeric
            if (group_col_name %in% names(traits_data_raw)) {
              traits_data_raw <- traits_data_raw %>% dplyr::select(-!!sym(group_col_name))
            }
            
            # Handle NAs by removing complete rows
            complete_cases_idx <- complete.cases(traits_data_raw, df[[group_col_name]])
            if (!any(complete_cases_idx)) {
              showNotification("No complete cases found for selected traits. PERMANOVA cannot be performed.", type = "error")
              permanova_main_results_r(NULL)
              permanova_pairwise_results_r(NULL)
              shinyjs::removeClass(id = "run_permanova", class = "module-active")
              return()
            }
            
            traits_data_clean <- traits_data_raw[complete_cases_idx, , drop = FALSE]
            species_data_clean <- df[[group_col_name]][complete_cases_idx]
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
            
            # Check if 'use_pca' input exists and is TRUE
            if (!is.null(input$use_pca) && input$use_pca) {
              incProgress(0.2, detail = "Performing PCA for PERMANOVA...")
              # Check for zero variance columns before PCA
              zero_var_cols <- sapply(permanova_data_input, function(x) var(x, na.rm = TRUE) == 0)
              if (any(zero_var_cols)) {
                showNotification(paste0("Warning: Traits with zero variance found (",
                                        paste(names(permanova_data_input)[zero_var_cols], collapse = ", "),
                                        "). These will be removed before PCA."), type = "warning", duration = 8)
                permanova_data_input <- permanova_data_input[, !zero_var_cols, drop = FALSE]
                if (ncol(permanova_data_input) < 2) { # PCA needs at least 2 variables
                  showNotification("After removing zero-variance traits, less than 2 traits remain. Cannot perform PCA. Please uncheck 'Use PCA Scores' or provide more variable traits.", type = "error", duration = 10)
                  permanova_main_results_r(NULL)
                  permanova_pairwise_results_r(NULL)
                  shinyjs::removeClass(id = "run_permanova", class = "module-active")
                  return()
                }
              }
              
              pca_result <- prcomp(permanova_data_input, scale. = TRUE, center = TRUE)
              # Use all principal components (scores) that have variance
              permanova_data_input <- pca_result$x[, apply(pca_result$x, 2, var) > 1e-9, drop = FALSE] # Filter out PCs with effectively zero variance
              analysis_description <- "PCA scores space"
              
              if (ncol(permanova_data_input) == 0) {
                showNotification("PCA resulted in no components with variance. PERMANOVA cannot be performed on PCA scores.", type = "error")
                permanova_main_results_r(NULL)
                permanova_pairwise_results_r(NULL)
                shinyjs::removeClass(id = "run_permanova", class = "module-active")
                return()
              }
            }
            
            incProgress(0.3, detail = "Calculating main PERMANOVA...")
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
            
            # Update progress and detail based on main result significance
            if (!is.null(main_result) && "Pr(>F)" %in% names(main_result) &&
                main_result$`Pr(>F)`[1] < 0.05 && n_distinct(species_data_clean) > 2) {
              incProgress(0.5, detail = "Main PERMANOVA significant. Running pairwise comparisons...")
              pairwise_result <- tryCatch({
                pairwise.adonis_meristic(x = permanova_data_input, 
                                         factors = species_data_clean,
                                         sim.method = input$permanova_distance_method,
                                         permutations = input$permanova_permutations)
              }, error = function(e) {
                showNotification(paste("Error running pairwise PERMANOVA:", e$message), type = "error")
                return(NULL)
              })
              permanova_pairwise_results_r(pairwise_result)
            } else {
              reason <- if(is.null(main_result)) "Main PERMANOVA failed" else if(main_result$`Pr(>F)`[1] >= 0.05) "Main PERMANOVA not significant" else "Less than 3 groups"
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
    observeEvent(input$main_tabs, {
      if (input$main_tabs == "PCAtest") {
        
        # Observe for PCAtest button click
        observeEvent(input$run_pcatest, {
          shinyjs::addClass(id = "run_pcatest", class = "module-active")
          
          withProgress(message = 'PCAtest Analysis in progress', value = 0, {
            incProgress(0.1, detail = "Preparing data for PCAtest...")
            df_source <- data_r()
            df_for_pcatest <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
            req(df_for_pcatest) 
            
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
              # Create connections to capture output
              output_con <- textConnection("output_lines", "w", local = TRUE)
              message_con <- textConnection("message_lines", "w", local = TRUE)
              
              # Redirect output and messages
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
                filtered_output <- full_output[start_index[1]:length(full_output)]
                filtered_output <- paste(filtered_output, collapse = "\n")
              } else {
                filtered_output <- paste(full_output, collapse = "\n")
              }
              
              list(result = result, output = filtered_output)
            }, error = function(e) {
              showNotification(paste("Error running PCAtest:", e$message), type = "error")
              return(NULL)
            }, finally = {
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
        
        # Trait contributions 
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