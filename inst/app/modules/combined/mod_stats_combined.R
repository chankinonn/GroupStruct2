
mod_inferential_ui_combined <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Inferential Statistics"),
    hr(),
    p(strong("Univariate")),
    p("Perform univariate analyses to determine which traits are (or are not) statistically different between groups. This module automatically detects the number of groups, tests for assumptions of normality and homogeneity of variance, and selects the appropriate statistical tests (t-test or Wilcoxon/Mann-Whitney if only two groups are detected, ANOVA or Kruskal-Wallis if >2 groups, followed by the appropriate ad hoc tests)."),
    br(),
    
    tabsetPanel(id = ns("main_tabs"), 
                # Univariate Tests Tab
                tabPanel("Univariate",
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
                         downloadButton(ns("download_all_summary"), "Download Summary of All Traits"),
                         hr(),
                         h5("Significant Traits by Pairwise Comparison:"),
                         uiOutput(ns("alpha_selector_ui")), 
                         uiOutput(ns("significant_traits_ui")), 
                         hr(),
                ),

    )
  )
}


mod_inferential_server_combined <- function(id, data_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pairwise_result_r <- reactiveVal(NULL)
    selected_trait <- reactiveVal(NULL)
    #permanova_main_results_r <- reactiveVal(NULL)
    #permanova_pairwise_results_r <- reactiveVal(NULL)
    #pcatest_results_r <- reactiveVal(NULL) # New reactive value for PCAtest results
    
    # Univariate Tests 
    observeEvent(input$main_tabs, {
      # Corrected tab name to match UI: "Univariate"
      req(input$main_tabs)
      if (input$main_tabs == "Univariate") {
        
        output$trait_buttons <- renderUI({
          # Safely access data
          df <- tryCatch({
            ds <- data_r()
            if (is.reactive(ds)) ds() else if (inherits(ds, "reactiveVal")) ds() else ds
          }, error = function(e) NULL)
          
          # VALIDATION - SAFE VERSION
          if (is.null(df)) return(p("Data not loaded yet"))
          if (!is.data.frame(df)) return(p("Invalid data format"))
          if (nrow(df) == 0) return(p("Data is empty"))  # Now safe
          
          # Rest of your original code...
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, names(df)[1])
          
          if (length(traits) == 0) return(p("No numeric traits found"))
          
          btns <- lapply(traits, function(trait) {
            active <- isTRUE(!is.null(selected_trait()) && selected_trait() == trait)
            actionButton(
              ns(paste0("btn_", trait)),
              label = trait,
              width = "150px",
              style = if (active) "background-color: #337ab7; color: white;" else ""
            )
          })
          
          tagList(btns)
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
          
          # Get all unique comparisons across traits
          all_comparisons <- unique(unlist(lapply(pairwise_list, function(df) df$Comparison)))
          combined_df <- data.frame(Comparison = all_comparisons, stringsAsFactors = FALSE)
          
          # Merge all trait-wise results into one combined_df
          for (trait in names(pairwise_list)) {
            df <- pairwise_list[[trait]]
            df <- df %>%
              dplyr::select(Comparison, p_value, Method) %>%
              dplyr::rename(
                !!paste0(trait, "_p_value") := p_value,
                !!paste0(trait, "_method") := Method
              )
            combined_df <- merge(combined_df, df, by = "Comparison", all.x = TRUE)
          }
          
          # Normalize comparison strings (e.g., "A vs B" == "B vs A")
          combined_df$NormComparison <- sapply(
            strsplit(combined_df$Comparison, "[-]|[ ]vs[ ]", perl = TRUE),
            function(x) paste(sort(trimws(x)), collapse = " vs ")
          )
          
          # Extract all columns ending with "_p_value"
          pval_cols <- grep("_p_value$", names(combined_df), value = TRUE)
          if (length(pval_cols) == 0) {
            return("No p-value columns found in the combined results.")
          }
          
          # Build significant traits per comparison
          sig_list <- lapply(unique(combined_df$NormComparison), function(norm_comp) {
            rows <- combined_df[combined_df$NormComparison == norm_comp, ]
            sig_traits <- c()
            
            for (col in pval_cols) {
              pval <- suppressWarnings(as.numeric(rows[[col]]))
              if (any(!is.na(pval) & pval < alpha)) {
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
            return(paste0("List of significant traits:\n\n", paste(sig_list, collapse = "\n")))
          }
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
        
        output$significant_traits <- renderText({
          significant_trait_summary()
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

      } 
    })
    
    
    return(data_r)
      
  })
}
