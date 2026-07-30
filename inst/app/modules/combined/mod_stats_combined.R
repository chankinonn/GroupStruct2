
mod_inferential_ui_combined <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Inferential Statistics (Mixed Data)"),
    p("This submodule performs inferential statistics on morphometric and meristic variables in a mixed dataset. These analyses are not applicable to categorical variables. Morphometric analyses are performed only on allometrically corrected variables from the Allometric Correction submodule. For meristic analyses, users must select which numeric variables should be treated as meristic traits."),
    hr(),
    tabsetPanel(id = ns("data_type_tabs"),
                tabPanel("Morphometric",
                         br(),
                         tags$div(
                           style = "background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 12px; margin-bottom: 20px;",
                           p(style = "margin: 0;",
                             strong("Notice:"),
                             "Inferential statistics will only be performed on allometrically corrected morphometric variables. The allometric correction must be run first with morphometric traits selected."
                           )
                         ),
                         br(),
                         tabsetPanel(id = ns("main_tabs"), 
                                     tabPanel("Univariate",
                                              br(), 
                                              h4("Univariate Analysis"),
                                              p("Perform univariate analyses to determine which trait means are (or are not) statistically different between groups. This module automatically detects the number of groups, tests for assumptions of normality and homogeneity of variance, and selects the appropriate statistical tests (t-test or Wilcoxon/Mann-Whitney if only 2 groups are detected, ANOVA or Kruskal-Wallis if >2 groups, followed by the appropriate ad hoc tests)."),
                                              
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
                                              h4("Significant Traits by Pairwise Comparison:"),
                                              uiOutput(ns("alpha_selector_ui")), 
                                              uiOutput(ns("significant_traits_ui")), 
                                              downloadButton(ns("download_significant_traits"), "Download Significant Traits"),
                                              hr(),
                                     ),
                                     
                                     
                                     tabPanel("Multivariate (PERMANOVA)",
                                              br(), 
                                              h4("PERMANOVA Analysis"),
                                              p("PERMANOVA (Permutational Multivariate Analysis of Variance) tests whether groups differ",
                                                "significantly in", strong("centroid position"), "in multivariate trait space — i.e. whether groups",
                                                "occupy different regions in their respective plotted space on average. A significant result (p < 0.05) indicates that at least one",
                                                "pair of groups differ, and the R² value quantifies the proportion of total multivariate variance",
                                                "explained by group membership (effect size)."),
                                              p("However, PERMANOVA is sensitive to heterogeneity in", strong("dispersion"),
                                                "(within-group spread of the data points around the centroid). If groups differ significantly in dispersion (heterogenous) — rather than in centroid placement",
                                                "— PERMANOVA may still return a significant result, making interpretation ambiguous.",
                                                "For this reason, a", strong("dispersion analysis (betadisper)"), "is run alongside PERMANOVA as an",
                                                "assumption check. Homogeneous dispersion (betadisper p > 0.05) supports a clean centroid-based",
                                                "interpretation of a significant PERMANOVA; heterogeneous dispersion (p < 0.05) warrants caution."),
                                              p("The tabs below present: (1) the overall PERMANOVA result, (2) pairwise PERMANOVA comparisons between groups,",
                                                "(3) dispersion analysis (betadisper), (4) pairwise dispersion comparisons (Tukey HSD on distances-to-centroid),",
                                                "(5) pairwise centroid distances as a magnitude measure, (6) a consolidated pairwise summary joining PERMANOVA and",
                                                "BetaDisper results, and (7) a joint interpretation guide."),
                                              
                                              fluidRow(
                                                column(6, numericInput(ns("permanova_permutations"), "Number of Permutations:", value = 10000, min = 100, step = 100))
                                                # column(6, selectInput(ns("permanova_distance_method"), "Distance Method:",
                                                #                       choices = c("euclidean", "manhattan", "bray", "jaccard", "altGower"),
                                                #                       selected = "euclidean"))
                                              ),
                                              
                                              actionButton(ns("run_permanova"), "Run PERMANOVA", icon = icon("play"), class = "btn-primary"),
                                              br(), br(),
                                              
                                              tabsetPanel(
                                                tabPanel("Main PERMANOVA",
                                                         br(),
                                                         verbatimTextOutput(ns("permanova_main_results"))
                                                ),
                                                tabPanel("Pairwise PERMANOVA",
                                                         br(),
                                                         p(em("Tests whether group ", strong("centroids"), " differ significantly in multivariate trait space.",
                                                              " Distinct from pairwise dispersion comparisons below, which test", strong("spread"),
                                                              " around centroids.")),
                                                         DTOutput(ns("permanova_pairwise_results"))
                                                ),
                                                tabPanel("BetaDisper",
                                                         br(),
                                                         p("Tests the homogeneity of multivariate dispersions using ",
                                                           code("vegan::betadisper()"), ". A significant result indicates groups differ in within-group spread,",
                                                           " an important assumption of PERMANOVA."),
                                                         verbatimTextOutput(ns("betadisper_results"))
                                                ),
                                                tabPanel("Pairwise Dispersion",
                                                         br(),
                                                         p(em(strong("Note:"), "Tukey HSD on within-group", strong("dispersion"), "— not a post-hoc test for group mean differences.")),
                                                         DTOutput(ns("betadisper_tukey_results"))
                                                ),
                                                tabPanel("Centroid Distances",
                                                         br(),
                                                         p("Euclidean distances between group centroids in PCoA space. Larger values indicate greater separation."),
                                                         DTOutput(ns("centroid_dist_results"))
                                                ),
                                                tabPanel("Consolidated Summary",
                                                         br(),
                                                         p("Joint view of pairwise PERMANOVA and BetaDisper results, sorted by R2 (descending)."),
                                                         DTOutput(ns("consolidated_pairwise"))
                                                )
                                              ),
                                              br(),
                                              downloadButton(ns("download_all_permanova"), "Download All PERMANOVA Tables"),
                                              hr(),
                                              
                                              h5("Interpreting Results Together"),
                                              
                                              p("PERMANOVA results should not be interpreted using significance alone. Evidence for group divergence",
                                                "should be evaluated jointly using PERMANOVA (p-value + R²), betadisper, and centroid separation. R²",
                                                "represents effect size (how much variation is explained by group identity), while betadisper",
                                                "evaluates whether dispersion differences may bias interpretation."),
                                              tags$p(
                                                style = "font-size: 0.9em;",
                                                strong("betadisper (dispersion check)"), br(),
                                                "Evaluates within-group spread:", br(),
                                                "— ", strong("Homogeneous"), "(p > 0.05): similar dispersion; PERMANOVA inference is reliable", br(),
                                                "— ", strong("Heterogeneous"), "(p < 0.05): unequal dispersion; may inflate or bias PERMANOVA results", br(),
                                                "Non-significant dispersion is preferred for clean interpretation."
                                              ),
                                              tags$table(
                                                class = "table table-bordered table-condensed",
                                                style = "font-size: 0.9em; margin-top: 10px;",
                                                tags$thead(
                                                  tags$tr(
                                                    tags$th("PERMANOVA"), tags$th("betadisper"), tags$th("R² (effect size)"), tags$th("Interpretation")
                                                  )
                                                ),
                                                tags$tbody(
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Non-significant"), tags$td("High"),
                                                    tags$td("Strong divergence — clear separation in multivariate space; best-case signal.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Non-significant"), tags$td("Moderate"),
                                                    tags$td("Moderate divergence — real but partial separation among groups.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Significant"), tags$td("High"),
                                                    tags$td("Strong signal but confounded by unequal dispersion; interpret with caution.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Significant"), tags$td("Low–Moderate"),
                                                    tags$td("Weak structural support; significance likely influenced by dispersion.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Non-significant"), tags$td("Non-significant"), tags$td("Low"),
                                                    tags$td("No evidence of group separation in multivariate space.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Non-significant"), tags$td("Significant"), tags$td("Low"),
                                                    tags$td("No centroid separation; differences driven by dispersion only.")
                                                  )
                                                )
                                              ),
                                              br(),
                                              hr(),
                                     )
                                     
                         ),
                ),
                tabPanel("Meristic",
                         br(),
                         tags$div(
                           style = "background-color: #d9edf7; border-left: 5px solid #31708f; padding: 12px; margin-bottom: 20px;",
                           p(style = "margin: 0;",
                             strong("Notice:"),
                             "Analyses will only be performed on the selected meristic traits. Univariate tests are performed on raw meristic data, while PERMANOVA is performed on centered and scaled meristic data."
                           )
                         ),
                         h4("Select Meristic Traits"),
                         p("Select the meristic variables from the mixed dataset to include in the analyses below."),
                         uiOutput(ns("meristic_trait_selector")),
                         hr(),
                         tabsetPanel(id = ns("meristic_main_tabs"),
                                     tabPanel("Univariate",
                                              br(),
                                              h4("Univariate Analysis"),
                                              p("Run univariate tests on one selected meristic trait at a time. Assumptions are checked automatically and the appropriate parametric or non-parametric test is selected."),
                                              div(style = "white-space: nowrap;",
                                                  checkboxInput(ns("meristic_force_parametric"),
                                                                label = HTML("<strong style='color: red;'>*Perform parametric tests even though assumptions are not met</strong>"),
                                                                value = FALSE)),
                                              uiOutput(ns("meristic_selected_trait_ui")),
                                              br(),
                                              textOutput(ns("meristic_trait_name")),
                                              verbatimTextOutput(ns("meristic_test_results")),
                                              br(),
                                              uiOutput(ns("meristic_select_trait_download")),
                                              br(),
                                              downloadButton(ns("download_meristic_all_summary"), "Download Summary of Selected Meristic Traits"),
                                              hr(),
                                              h5("Significant Traits by Pairwise Comparison:"),
                                              uiOutput(ns("meristic_alpha_selector_ui")),
                                              uiOutput(ns("meristic_significant_traits_ui")),
                                              downloadButton(ns("download_meristic_significant_traits"), "Download Significant Traits"),
                                              hr()
                                     ),
                                     tabPanel("Multivariate (PERMANOVA)",
                                              br(),
                                              h4("PERMANOVA Analysis"),
                                              p("PERMANOVA is performed on centered and scaled selected meristic traits."),
                                              fluidRow(
                                                column(6, numericInput(ns("meristic_permanova_permutations"), "Number of Permutations:", value = 10000, min = 100, step = 100))
                                              ),
                                              actionButton(ns("run_meristic_permanova"), "Run PERMANOVA", icon = icon("play"), class = "btn-primary"),
                                              br(), br(),
                                              tabsetPanel(
                                                tabPanel("Main PERMANOVA", br(), verbatimTextOutput(ns("meristic_permanova_main_results"))),
                                                tabPanel("Pairwise PERMANOVA", br(), DTOutput(ns("meristic_permanova_pairwise_results"))),
                                                tabPanel("BetaDisper", br(), verbatimTextOutput(ns("meristic_betadisper_results"))),
                                                tabPanel("Pairwise Dispersion", br(), DTOutput(ns("meristic_betadisper_tukey_results"))),
                                                tabPanel("Centroid Distances", br(), DTOutput(ns("meristic_centroid_dist_results"))),
                                                tabPanel("Consolidated Summary",
                                                         br(),
                                                         p("Joint view of pairwise PERMANOVA and BetaDisper results, sorted by R² (descending)."),
                                                         DTOutput(ns("meristic_consolidated_pairwise"))
                                                )
                                              ),
                                              br(),
                                              downloadButton(ns("download_meristic_all_permanova"), "Download All PERMANOVA Tables"),
                                              hr(),
                                              
                                              h5("Interpreting Results Together"),
                                              
                                              p("PERMANOVA results should not be interpreted using significance alone. Evidence for group divergence",
                                                "should be evaluated jointly using PERMANOVA (p-value + R²), betadisper, and centroid separation. R²",
                                                "represents effect size (how much variation is explained by group identity), while betadisper",
                                                "evaluates whether dispersion differences may bias interpretation."),
                                              tags$p(
                                                style = "font-size: 0.9em;",
                                                strong("betadisper (dispersion check)"), br(),
                                                "Evaluates within-group spread:", br(),
                                                "— ", strong("Homogeneous"), "(p > 0.05): similar dispersion; PERMANOVA inference is reliable", br(),
                                                "— ", strong("Heterogeneous"), "(p < 0.05): unequal dispersion; may inflate or bias PERMANOVA results", br(),
                                                "Non-significant dispersion is preferred for clean interpretation."
                                              ),
                                              tags$table(
                                                class = "table table-bordered table-condensed",
                                                style = "font-size: 0.9em; margin-top: 10px;",
                                                tags$thead(
                                                  tags$tr(
                                                    tags$th("PERMANOVA"), tags$th("betadisper"), tags$th("R² (effect size)"), tags$th("Interpretation")
                                                  )
                                                ),
                                                tags$tbody(
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Non-significant"), tags$td("High"),
                                                    tags$td("Strong divergence — clear separation in multivariate space; best-case signal.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Non-significant"), tags$td("Moderate"),
                                                    tags$td("Moderate divergence — real but partial separation among groups.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Significant"), tags$td("High"),
                                                    tags$td("Strong signal but confounded by unequal dispersion; interpret with caution.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Significant"), tags$td("Significant"), tags$td("Low–Moderate"),
                                                    tags$td("Weak structural support; significance likely influenced by dispersion.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Non-significant"), tags$td("Non-significant"), tags$td("Low"),
                                                    tags$td("No evidence of group separation in multivariate space.")
                                                  ),
                                                  tags$tr(
                                                    tags$td("Non-significant"), tags$td("Significant"), tags$td("Low"),
                                                    tags$td("No centroid separation; differences driven by dispersion only.")
                                                  )
                                                )
                                              ),
                                              br()
                                     )
                         )
                )
    )
  )
}


mod_inferential_server_combined <- function(id, data_r, corrected_traits_r = NULL, raw_data_r = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    get_reactive_value <- function(x) {
      if (is.null(x)) return(NULL)
      if (shiny::is.reactive(x)) return(x())
      if (inherits(x, "reactiveVal")) return(x())
      x
    }
    
    meristic_data_source_r <- reactive({
      raw_df <- get_reactive_value(raw_data_r)
      if (!is.null(raw_df) && nrow(raw_df) > 0) return(raw_df)
      get_reactive_value(data_r)
    })
    
    assess_univariate_assumptions <- function(df_filtered) {
      if (is.null(df_filtered) || nrow(df_filtered) == 0) {
        return(list(shapiro = NULL, levene = NULL, shapiro_ok = FALSE,
                    levene_ok = FALSE, zero_variance = TRUE, levene_p = NA_real_))
      }
      
      shapiro_df <- df_filtered %>%
        dplyr::group_by(temp_group) %>%
        dplyr::summarise(
          n = dplyr::n(),
          statistic = ifelse(n >= 3 && length(unique(temp_trait)) > 1,
                             tryCatch(unname(stats::shapiro.test(temp_trait)$statistic), error = function(e) NA_real_),
                             NA_real_),
          p = ifelse(n >= 3 && length(unique(temp_trait)) > 1,
                     tryCatch(stats::shapiro.test(temp_trait)$p.value, error = function(e) NA_real_),
                     NA_real_),
          message = dplyr::case_when(
            n < 3 ~ "Not enough data points (<3) for Shapiro-Wilk test",
            length(unique(temp_trait)) <= 1 ~ "Constant values in group, Shapiro-Wilk test not applicable",
            TRUE ~ NA_character_
          ),
          .groups = "drop"
        )
      
      valid_shapiro_p <- shapiro_df$p[!is.na(shapiro_df$p)]
      shapiro_ok <- length(valid_shapiro_p) > 0 && all(valid_shapiro_p > 0.05)
      
      levene <- tryCatch(car::leveneTest(temp_trait ~ temp_group, data = df_filtered), error = function(e) NULL)
      levene_p <- if (!is.null(levene)) as.numeric(levene[["Pr(>F)"]][1]) else NA_real_
      levene_ok <- !is.na(levene_p) && levene_p > 0.05
      
      zero_variance <- df_filtered %>%
        dplyr::group_by(temp_group) %>%
        dplyr::summarise(sd_val = stats::sd(temp_trait, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(is.na(sd_val) | sd_val == 0) %>%
        nrow() > 0
      
      list(shapiro = shapiro_df, levene = levene, shapiro_ok = shapiro_ok,
           levene_ok = levene_ok, zero_variance = zero_variance, levene_p = levene_p)
    }
    
    pairwise_univariate_for_trait <- function(df_trait, force_parametric = FALSE) {
      ngroups <- dplyr::n_distinct(df_trait$temp_group)
      if (ngroups < 2 || length(unique(df_trait$temp_trait)) <= 1) return(NULL)
      
      assumptions <- assess_univariate_assumptions(df_trait)
      use_parametric <- isTRUE(force_parametric) ||
        (assumptions$shapiro_ok && assumptions$levene_ok && !assumptions$zero_variance)
      
      if (ngroups == 2) {
        groups <- levels(droplevels(df_trait$temp_group))
        comp <- paste(sort(groups), collapse = " vs ")
        if (use_parametric) {
          pval <- tryCatch(stats::t.test(temp_trait ~ temp_group, data = df_trait)$p.value, error = function(e) NA_real_)
          method <- "t-test"
        } else {
          pval <- tryCatch(stats::wilcox.test(temp_trait ~ temp_group, data = df_trait, exact = FALSE)$p.value, error = function(e) NA_real_)
          method <- "Wilcoxon"
        }
        result <- data.frame(Comparison = comp, p_value = signif(pval, 4), Method = method, stringsAsFactors = FALSE)
      } else {
        if (use_parametric) {
          result <- tryCatch({
            fit <- stats::aov(temp_trait ~ temp_group, data = df_trait)
            stats::TukeyHSD(fit)[[1]] %>%
              as.data.frame() %>%
              tibble::rownames_to_column("Comparison") %>%
              dplyr::mutate(Method = "ANOVA (Tukey)") %>%
              dplyr::select(Comparison, p_value = `p adj`, Method)
          }, error = function(e) {
            data.frame(Comparison = paste0("Error: ", conditionMessage(e)), p_value = NA_real_, Method = "Error")
          })
        } else {
          result <- tryCatch({
            df_trait %>%
              rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
              dplyr::mutate(Comparison = paste(pmin(group1, group2), "vs", pmax(group1, group2)),
                            Method = "Kruskal (Dunn)") %>%
              dplyr::select(Comparison, p_value = p.adj, Method)
          }, error = function(e) {
            data.frame(Comparison = paste0("Error: ", conditionMessage(e)), p_value = NA_real_, Method = "Error")
          })
        }
      }
      
      result %>%
        dplyr::mutate(Comparison = sapply(strsplit(as.character(Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
                                          function(x) paste(sort(trimws(x)), collapse = " vs "))) %>%
        dplyr::group_by(Comparison) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    }
    
    univariate_summary_for_traits <- function(df, traits, group_col, force_parametric = FALSE) {
      if (is.null(df) || length(traits) == 0) return(NULL)
      out <- lapply(traits, function(trait) {
        df_trait <- df %>%
          dplyr::mutate(temp_trait = as.numeric(.data[[trait]]),
                        temp_group = factor(.data[[group_col]])) %>%
          dplyr::filter(!is.na(temp_trait), !is.na(temp_group))
        
        ngroups <- dplyr::n_distinct(df_trait$temp_group)
        if (ngroups < 2 || length(unique(df_trait$temp_trait)) <= 1) {
          return(data.frame(Trait = trait, N = nrow(df_trait), Groups = ngroups,
                            Normality = "Not tested", Shapiro_min_p = NA_real_,
                            Homogeneity = "Not tested", Levene_p = NA_real_,
                            Test = NA_character_, Post_hoc = NA_character_,
                            Message = "Not enough valid data", stringsAsFactors = FALSE))
        }
        
        assumptions <- assess_univariate_assumptions(df_trait)
        use_parametric <- isTRUE(force_parametric) ||
          (assumptions$shapiro_ok && assumptions$levene_ok && !assumptions$zero_variance)
        valid_shapiro_p <- assumptions$shapiro$p[!is.na(assumptions$shapiro$p)]
        shapiro_min_p <- if (length(valid_shapiro_p) > 0) min(valid_shapiro_p) else NA_real_
        
        if (ngroups == 2) {
          test <- if (use_parametric) "t-test" else "Wilcoxon/Mann-Whitney"
          post_hoc <- NA_character_
        } else {
          test <- if (use_parametric) "ANOVA" else "Kruskal-Wallis"
          post_hoc <- if (use_parametric) "Tukey HSD" else "Dunn"
        }
        
        data.frame(Trait = trait, N = nrow(df_trait), Groups = ngroups,
                   Normality = ifelse(assumptions$shapiro_ok, "Met", "Not met / not testable"),
                   Shapiro_min_p = shapiro_min_p,
                   Homogeneity = ifelse(assumptions$levene_ok, "Met", "Not met / not testable"),
                   Levene_p = assumptions$levene_p,
                   Test = test, Post_hoc = post_hoc,
                   Message = ifelse(isTRUE(force_parametric), "Parametric test forced by user", NA_character_),
                   stringsAsFactors = FALSE)
      })
      dplyr::bind_rows(out) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 5)))
    }
    
    pairwise_result_r <- reactiveVal(NULL)
    selected_trait <- reactiveVal(NULL)
    permanova_main_results_r <- reactiveVal(NULL)
    permanova_pairwise_results_r <- reactiveVal(NULL)
    betadisper_results_r <- reactiveVal(NULL)
    centroid_dist_r <- reactiveVal(NULL)
    
    consolidated_pairwise_r <- reactive({
      perm_df <- permanova_pairwise_results_r()
      bd_res  <- betadisper_results_r()
      if (is.null(perm_df) || is.null(bd_res)) return(NULL)
      if ("Message" %in% names(perm_df)) return(NULL)
      
      sig_stars <- function(p) dplyr::case_when(
        p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"
      )
      norm_key <- function(s) paste(sort(strsplit(s, "-", fixed = TRUE)[[1]]), collapse = "-")
      
      if (!is.null(bd_res$tukey)) {
        tukey_df <- as.data.frame(bd_res$tukey$group) %>%
          tibble::rownames_to_column("Comparison") %>%
          dplyr::rename(BD_diff = diff, BD_lower = lwr, BD_upper = upr, BD_p_adj = `p adj`)
        perm_df$key  <- sapply(perm_df$pairs,       norm_key)
        tukey_df$key <- sapply(tukey_df$Comparison, norm_key)
        merged <- dplyr::inner_join(perm_df, tukey_df, by = "key", suffix = c("", "_bd"))
        if (nrow(merged) == 0) return(NULL)
        merged %>%
          dplyr::mutate(PERMANOVA_sig = sig_stars(p.adjusted), BD_sig = sig_stars(BD_p_adj)) %>%
          dplyr::select(pairs, F.Model, R2, p.value, p.adjusted, PERMANOVA_sig,
                        BD_p_adj, BD_sig) %>%
          dplyr::rename(Comparison = pairs, `F` = F.Model, `PERMANOVA p` = p.value,
                        `PERMANOVA p.adj` = p.adjusted, `PERMANOVA sig.` = PERMANOVA_sig,
                        `Betadisper p (adj.)` = BD_p_adj, `Betadisper sig.` = BD_sig) %>%
          dplyr::arrange(dplyr::desc(R2)) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
      } else {
        gd <- bd_res$betadisper$group.distances
        bd_diff <- as.numeric(gd[2] - gd[1])
        bd_p    <- bd_res$permutest$tab$`Pr(>F)`[1]
        perm_df %>%
          dplyr::mutate(PERMANOVA_sig = sig_stars(p.adjusted),
                        BD_diff = bd_diff, BD_lower = NA_real_, BD_upper = NA_real_,
                        BD_p_adj = bd_p, BD_sig = sig_stars(bd_p)) %>%
          dplyr::select(pairs, F.Model, R2, p.value, p.adjusted, PERMANOVA_sig,
                        BD_p_adj, BD_sig) %>%
          dplyr::rename(Comparison = pairs, `F` = F.Model, `PERMANOVA p` = p.value,
                        `PERMANOVA p.adj` = p.adjusted, `PERMANOVA sig.` = PERMANOVA_sig,
                        `Betadisper p (adj.)` = BD_p_adj, `Betadisper sig.` = BD_sig) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
      }
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
          
          # Only traits explicitly selected for allometric correction are eligible
          corrected <- get_reactive_value(corrected_traits_r)
          if (is.null(corrected)) corrected <- character(0)
          traits <- intersect(traits, corrected)
          
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
            
            # Only traits explicitly selected for allometric correction are eligible
            corrected <- get_reactive_value(corrected_traits_r)
            if (is.null(corrected)) corrected <- character(0)
            traits <- intersect(traits, corrected)
            
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
                shapiro_list[[g]] <- data.frame(
                  temp_group = g,
                  variable = trait,
                  statistic = unname(test_result$statistic),
                  p = test_result$p.value,
                  message = NA_character_
                )
              } else {
                shapiro_list[[g]] <- data.frame(temp_group = g, variable = trait, statistic = NA, p = NA,
                                                message = "Shapiro-Wilk test failed")
              }
            }
          }
          
          if (length(shapiro_list) > 0) {
            return(dplyr::bind_rows(shapiro_list) %>% as_tibble())
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
        
        # Compute all pairwise p-value summaries for all corrected morphometric traits
        # Statistical logic and output format match the morphometric module exactly.
        all_pairwise_results <- reactive({
          df_source <- data_r()
          df <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          req(df)
          
          group_col <- names(df)[1]
          traits <- names(df)[sapply(df, is.numeric)]
          traits <- setdiff(traits, group_col)
          corrected <- get_reactive_value(corrected_traits_r)
          if (is.null(corrected)) corrected <- character(0)
          traits <- intersect(traits, corrected)
          
          results_list <- list()
          
          for (trait in traits) {
            df_trait <- df %>%
              mutate(temp_trait = as.numeric(.data[[trait]]),
                     temp_group = factor(.data[[group_col]])) %>%
              filter(!is.na(temp_trait))
            
            ngroups <- n_distinct(df_trait$temp_group)
            if (ngroups < 2) next
            
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
            return(dplyr::bind_rows(sig_list))
          }
        })
        
        
        output$download_all_summary <- downloadHandler(
          filename = function() {
            paste0("summary_all_traits_", Sys.Date(), ".csv")
          },
          content = function(file) {
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
    
    # Helper function for pairwise PERMANOVA using adonis2
    pairwise.adonis <- function(x, factors, sim.method = 'euclidean', p.adjust.m ='bonferroni', permutations = 50000) {
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
        if (ncol(x_subset) > 0 && all(apply(x_subset, 2, function(col) length(unique(col)) == 1))) {
          warning(paste("All traits in subset for", grp1, "vs", grp2, "are constant. Cannot compute distance matrix."))
          pairs = c(pairs, paste(grp1, grp2, sep = '-'));
          F.Model =c(F.Model, NA);
          R2 = c(R2, NA);
          p.value =c(p.value, NA);
          next
        }
        x1 = vegan::vegdist(x_subset, method=sim.method)
        ad = vegan::adonis2(x1 ~ factors_subset, permutations = permutations);
        pairs = c(pairs, paste(grp1, grp2, sep = '-'));
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
    
    # PERMANOVA
    observeEvent(input$main_tabs, {
      if (input$main_tabs == "Multivariate (PERMANOVA)") {
        
        # Observe for PERMANOVA
        observeEvent(input$run_permanova, {
          shinyjs::addClass(id = "run_permanova", class = "module-active")
          shinybusy::show_modal_spinner(
            spin = "fading-circle",
            text = "PERMANOVA is running — this may take several minutes..."
          )
          on.exit({
            shinybusy::remove_modal_spinner()
            shinyjs::removeClass(id = "run_permanova", class = "module-active")
          })
          
          req(data_r())
          df_source <- data_r()
          df_for_permanova <- if (is.reactive(df_source)) df_source() else if (inherits(df_source, "reactiveVal")) df_source() else df_source
          
          group_col_name <- names(df_for_permanova)[1]
          
          # Get only allometrically corrected morphometric traits for the Morphometric tab
          corrected_traits <- if (!is.null(corrected_traits_r)) {
            if (is.reactive(corrected_traits_r)) corrected_traits_r() else corrected_traits_r()
          } else {
            character(0)
          }
          corrected_traits <- intersect(corrected_traits, names(df_for_permanova))
          corrected_traits <- corrected_traits[sapply(df_for_permanova[corrected_traits], is.numeric)]
          
          if (length(corrected_traits) < 1) {
            showNotification("No allometrically corrected morphometric traits are available for PERMANOVA.", type = "error")
            permanova_main_results_r(NULL)
            permanova_pairwise_results_r(NULL)
            betadisper_results_r(NULL)
            centroid_dist_r(NULL)
            return()
          }
          
          traits_data_raw <- df_for_permanova[, corrected_traits, drop = FALSE]
          traits_data_raw <- as.data.frame(lapply(traits_data_raw, as.numeric))
          
          complete_cases_idx <- complete.cases(traits_data_raw, df_for_permanova[[group_col_name]])
          if (!any(complete_cases_idx)) {
            showNotification("No complete cases found for selected traits. PERMANOVA cannot be performed.", type = "error")
            permanova_main_results_r(NULL)
            permanova_pairwise_results_r(NULL)
            betadisper_results_r(NULL)
            centroid_dist_r(NULL)
            return()
          }
          
          traits_data_clean <- traits_data_raw[complete_cases_idx, , drop = FALSE]
          species_data_clean <- df_for_permanova[[group_col_name]][complete_cases_idx]
          species_data_clean <- droplevels(as.factor(species_data_clean))
          
          # Check for sufficient data after cleaning
          if (nrow(traits_data_clean) < 2 || ncol(traits_data_clean) < 1 || n_distinct(species_data_clean) < 2) {
            showNotification("Not enough complete data (rows < 2), numeric traits (columns < 1), or distinct groups (groups < 2) to perform PERMANOVA. Please check your data.", type = "error")
            permanova_main_results_r(NULL)
            permanova_pairwise_results_r(NULL)
            betadisper_results_r(NULL)
            centroid_dist_r(NULL)
            return()
          }
          
          # Check for constant values in traits_data_clean
          if (all(apply(traits_data_clean, 2, function(col) length(unique(col)) == 1))) {
            showNotification("All selected numeric traits have constant values. PERMANOVA cannot be performed on constant data.", type = "error")
            permanova_main_results_r(NULL)
            permanova_pairwise_results_r(NULL)
            betadisper_results_r(NULL)
            centroid_dist_r(NULL)
            return()
          }
          
          # Run PERMANOVA on allometrically corrected morphometric traits
          permanova_data_input <- traits_data_clean
          
          # Run main adonis2 (PERMANOVA)
          main_result <- tryCatch({
            vegan::adonis2(formula = permanova_data_input ~ species_data_clean,
                           permutations = input$permanova_permutations,
                           method = "euclidean")
          }, error = function(e) {
            showNotification(paste("Error running main PERMANOVA:", e$message), type = "error")
            permanova_main_results_r(NULL)
            permanova_pairwise_results_r(NULL)
            return(NULL)
          })
          
          permanova_main_results_r(main_result)
          
          pairwise_result <- tryCatch({
            pairwise.adonis(x = permanova_data_input,
                            factors = species_data_clean,
                            sim.method = "euclidean",
                            permutations = input$permanova_permutations)
          }, error = function(e) {
            showNotification(paste("Error running pairwise PERMANOVA:", e$message), type = "error")
            return(NULL)
          })
          permanova_pairwise_results_r(pairwise_result)
          
          # Betadisper — runs unconditionally as an assumption check for PERMANOVA
          betadisper_capture <- tryCatch({
            dist_mat <- vegan::vegdist(permanova_data_input, method = "euclidean")
            bd <- vegan::betadisper(dist_mat, species_data_clean)
            bd_permutest <- vegan::permutest(bd, permutations = input$permanova_permutations, pairwise = TRUE)
            bd_tukey <- if (nlevels(species_data_clean) > 2) TukeyHSD(bd) else NULL
            list(betadisper = bd, permutest = bd_permutest, tukey = bd_tukey)
          }, error = function(e) {
            showNotification(paste("Warning: betadisper could not be computed:", e$message), type = "warning")
            NULL
          })
          betadisper_results_r(betadisper_capture)
          
          # Centroid distances — pairwise Euclidean distances between group centroids in PCoA space
          if (!is.null(betadisper_capture)) {
            tryCatch({
              centroids <- betadisper_capture$betadisper$centroids
              centroid_dist_mat <- as.matrix(dist(centroids))
              centroid_dist_mat <- round(centroid_dist_mat, 4)
              centroid_dist_df <- as.data.frame(centroid_dist_mat)
              centroid_dist_df <- cbind(Group = rownames(centroid_dist_df), centroid_dist_df)
              rownames(centroid_dist_df) <- NULL
              centroid_dist_r(centroid_dist_df)
            }, error = function(e) {
              showNotification(paste("Warning: could not compute centroid distances:", e$message), type = "warning")
              centroid_dist_r(NULL)
            })
          } else {
            centroid_dist_r(NULL)
          }
          
          showNotification("PERMANOVA analysis complete. Results are displayed below.", type = "default", duration = 10)
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
        
        
        output$betadisper_results <- renderPrint({
          res <- betadisper_results_r()
          if (is.null(res)) { cat("Dispersion analysis not yet run.\n"); return() }
          cat("=== Multivariate Homogeneity of Group Dispersions (betadisper) ===\n\n")
          cat("Average distance to group centroid (within-group spread):\n")
          print(round(res$betadisper$group.distances, 4))
          cat("\n--- Permutation test (permutest) ---\n")
          cat("(Pairwise comparisons are shown in the Pairwise Dispersion tab)\n\n")
          print(res$permutest$tab)
        })
        
        output$betadisper_tukey_results <- renderDT({
          res <- betadisper_results_r()
          if (is.null(res) || is.null(res$tukey)) {
            return(datatable(data.frame(Message = "Pairwise dispersion comparisons require more than 2 groups."),
                             options = list(dom = 't', paging = FALSE, searching = FALSE)))
          }
          tukey_df <- as.data.frame(res$tukey$group) %>%
            tibble::rownames_to_column("Comparison") %>%
            dplyr::rename(diff = diff, lower = lwr, upper = upr, p_adj = `p adj`) %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
          datatable(tukey_df, options = list(dom = 't', paging = FALSE, searching = FALSE))
        })
        
        output$centroid_dist_results <- renderDT({
          df <- centroid_dist_r()
          if (is.null(df)) {
            return(datatable(data.frame(Message = "Centroid distances not available."),
                             options = list(dom = 't', paging = FALSE, searching = FALSE)))
          }
          datatable(df, options = list(dom = 't', paging = FALSE, searching = FALSE), rownames = FALSE)
        })
        
        output$consolidated_pairwise <- renderDT({
          df <- consolidated_pairwise_r()
          if (is.null(df) || nrow(df) == 0) {
            return(datatable(data.frame(Message = "Consolidated table unavailable. Run both PERMANOVA and BetaDisper first."),
                             options = list(dom = 't', paging = FALSE, searching = FALSE)))
          }
          datatable(df, rownames = FALSE, options = list(dom = 'tip', pageLength = 25, scrollX = TRUE))
        })
        
        output$download_all_permanova <- downloadHandler(
          filename = function() paste0("permanova_results_morphometric_", Sys.Date(), ".zip"),
          content = function(file) {
            tmpdir <- tempdir()
            files  <- c()
            perm_main <- permanova_main_results_r()
            if (!is.null(perm_main)) {
              f <- file.path(tmpdir, paste0("permanova_main_morphometric_", Sys.Date(), ".txt"))
              sink(f); print(perm_main); sink()
              files <- c(files, f)
            }
            perm_pair <- permanova_pairwise_results_r()
            if (!is.null(perm_pair)) {
              f <- file.path(tmpdir, paste0("permanova_pairwise_morphometric_", Sys.Date(), ".csv"))
              write.csv(perm_pair, f, row.names = FALSE)
              files <- c(files, f)
            }
            bd <- betadisper_results_r()
            if (!is.null(bd)) {
              f <- file.path(tmpdir, paste0("betadisper_morphometric_", Sys.Date(), ".txt"))
              sink(f)
              cat("=== Multivariate Homogeneity of Group Dispersions (betadisper) ===\n\n")
              cat("Average distance to group centroid:\n")
              print(round(bd$betadisper$group.distances, 4))
              cat("\n--- Permutation test ---\n")
              print(bd$permutest$tab)
              if (!is.null(bd$tukey)) { cat("\n--- Pairwise Tukey HSD ---\n"); print(bd$tukey) }
              sink()
              files <- c(files, f)
              if (!is.null(bd$tukey)) {
                f <- file.path(tmpdir, paste0("betadisper_pairwise_morphometric_", Sys.Date(), ".csv"))
                tukey_df <- as.data.frame(bd$tukey$group) %>%
                  tibble::rownames_to_column("Comparison") %>%
                  dplyr::rename(diff = diff, lower = lwr, upper = upr, p_adj = `p adj`)
                write.csv(tukey_df, f, row.names = FALSE)
                files <- c(files, f)
              }
            }
            df_cent <- centroid_dist_r()
            if (!is.null(df_cent)) {
              f <- file.path(tmpdir, paste0("centroid_distances_morphometric_", Sys.Date(), ".csv"))
              write.csv(df_cent, f, row.names = FALSE)
              files <- c(files, f)
            }
            df_cons <- consolidated_pairwise_r()
            if (!is.null(df_cons)) {
              f <- file.path(tmpdir, paste0("consolidated_pairwise_morphometric_", Sys.Date(), ".csv"))
              write.csv(df_cons, f, row.names = FALSE)
              files <- c(files, f)
            }
            zip(file, files, flags = "-j")
          }
        )
      } 
    }) 
    
    
    
    # ---------------- Meristic inferential statistics for selected traits ----------------
    selected_meristic_trait <- reactiveVal(NULL)
    meristic_main_results_r <- reactiveVal(NULL)
    meristic_pairwise_results_r <- reactiveVal(NULL)
    meristic_betadisper_results_r <- reactiveVal(NULL)
    meristic_centroid_dist_r <- reactiveVal(NULL)
    
    
    meristic_consolidated_pairwise_r <- reactive({
      perm_df <- meristic_pairwise_results_r()
      bd_res  <- meristic_betadisper_results_r()
      if (is.null(perm_df) || is.null(bd_res)) return(NULL)
      if ("Message" %in% names(perm_df)) return(NULL)
      sig_stars <- function(p) dplyr::case_when(
        p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns"
      )
      norm_key <- function(s) paste(sort(strsplit(s, "-", fixed = TRUE)[[1]]), collapse = "-")
      if (!is.null(bd_res$tukey)) {
        tukey_df <- as.data.frame(bd_res$tukey$group) %>%
          tibble::rownames_to_column("Comparison") %>%
          dplyr::rename(BD_diff = diff, BD_lower = lwr, BD_upper = upr, BD_p_adj = `p adj`)
        perm_df$key  <- sapply(perm_df$pairs,       norm_key)
        tukey_df$key <- sapply(tukey_df$Comparison, norm_key)
        merged <- dplyr::inner_join(perm_df, tukey_df, by = "key", suffix = c("", "_bd"))
        if (nrow(merged) == 0) return(NULL)
        merged %>%
          dplyr::mutate(PERMANOVA_sig = sig_stars(p.adjusted), BD_sig = sig_stars(BD_p_adj)) %>%
          dplyr::select(pairs, F.Model, R2, p.value, p.adjusted, PERMANOVA_sig,
                        BD_p_adj, BD_sig) %>%
          dplyr::rename(Comparison = pairs, `F` = F.Model, `PERMANOVA p` = p.value,
                        `PERMANOVA p.adj` = p.adjusted, `PERMANOVA sig.` = PERMANOVA_sig,
                        `Betadisper p (adj.)` = BD_p_adj, `Betadisper sig.` = BD_sig) %>%
          dplyr::arrange(dplyr::desc(R2)) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
      } else {
        gd <- bd_res$betadisper$group.distances
        bd_diff <- as.numeric(gd[2] - gd[1])
        bd_p    <- bd_res$permutest$tab$`Pr(>F)`[1]
        perm_df %>%
          dplyr::mutate(PERMANOVA_sig = sig_stars(p.adjusted),
                        BD_diff = bd_diff, BD_lower = NA_real_, BD_upper = NA_real_,
                        BD_p_adj = bd_p, BD_sig = sig_stars(bd_p)) %>%
          dplyr::select(pairs, F.Model, R2, p.value, p.adjusted, PERMANOVA_sig,
                        BD_p_adj, BD_sig) %>%
          dplyr::rename(Comparison = pairs, `F` = F.Model, `PERMANOVA p` = p.value,
                        `PERMANOVA p.adj` = p.adjusted, `PERMANOVA sig.` = PERMANOVA_sig,
                        `Betadisper p (adj.)` = BD_p_adj, `Betadisper sig.` = BD_sig) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
      }
    })
    
    meristic_numeric_traits_r <- reactive({
      df <- meristic_data_source_r()
      if (is.null(df) || nrow(df) == 0) return(character(0))
      setdiff(names(df)[sapply(df, is.numeric)], names(df)[1])
    })
    
    selected_meristic_traits_r <- reactive({
      traits <- input$meristic_traits
      if (is.null(traits)) traits <- character(0)
      intersect(traits, meristic_numeric_traits_r())
    })
    
    output$meristic_trait_selector <- renderUI({
      traits <- meristic_numeric_traits_r()
      if (length(traits) == 0) {
        return(p("No numeric variables were found in the mixed dataset."))
      }
      checkboxGroupInput(
        ns("meristic_traits"),
        label = "Select meristic traits:",
        choices = traits,
        selected = character(0),
        inline = TRUE
      )
    })
    
    # Use the same trait-button interface as the morphometric benchmark.
    output$meristic_selected_trait_ui <- renderUI({
      traits <- selected_meristic_traits_r()
      if (length(traits) == 0) {
        return(p("Select at least one meristic trait above."))
      }
      
      btns <- lapply(traits, function(trait) {
        active <- !is.null(selected_meristic_trait()) && selected_meristic_trait() == trait
        style <- if (active) "background-color: #337ab7; color: white;" else ""
        actionButton(
          ns(paste0("meristic_btn_", trait)),
          label = trait,
          width = "150px",
          style = style
        )
      })
      do.call(tagList, btns)
    })
    
    # Initialize the selected trait and create observers for the selected-trait buttons.
    observeEvent(selected_meristic_traits_r(), {
      traits <- selected_meristic_traits_r()
      
      if (length(traits) == 0) {
        selected_meristic_trait(NULL)
        return()
      }
      
      if (is.null(selected_meristic_trait()) ||
          !(selected_meristic_trait() %in% traits)) {
        selected_meristic_trait(traits[1])
      }
      
      lapply(traits, function(current_trait) {
        btn_id <- paste0("meristic_btn_", current_trait)
        observeEvent(input[[btn_id]], {
          selected_meristic_trait(current_trait)
        }, ignoreInit = TRUE)
      })
    }, ignoreNULL = FALSE)
    
    output$meristic_trait_name <- renderText({
      trait <- selected_meristic_trait()
      if (is.null(trait) || trait == "") {
        "Select a trait above to run inferential statistics."
      } else {
        paste("Running inferential statistics on meristic trait:", trait)
      }
    })
    
    output$meristic_select_trait_download <- renderUI({
      req(selected_meristic_trait())
      downloadButton(
        ns("download_meristic_summary"),
        label = paste0("Download Results for '", selected_meristic_trait(), "'")
      )
    })
    
    meristic_trait_data_r <- reactive({
      trait <- selected_meristic_trait()
      df <- meristic_data_source_r()
      if (is.null(df) || nrow(df) == 0 || is.null(trait) || !(trait %in% names(df))) return(NULL)
      group <- names(df)[1]
      df %>%
        dplyr::mutate(!!rlang::sym(group) := droplevels(as.factor(.data[[group]]))) %>%
        dplyr::mutate(temp_trait = as.numeric(.data[[trait]]),
                      temp_group = factor(.data[[group]])) %>%
        dplyr::filter(!is.na(temp_trait), !is.na(temp_group))
    })
    
    # Shapiro-Wilk results for the currently selected meristic trait
    meristic_shapiro_results_r <- reactive({
      df_filtered <- meristic_trait_data_r()
      trait <- selected_meristic_trait()
      if (is.null(df_filtered) || is.null(trait) || trait == "") return(NULL)
      
      shapiro_list <- list()
      groups <- levels(df_filtered$temp_group)
      
      for (g in groups) {
        group_data <- df_filtered %>% dplyr::filter(temp_group == g)
        if (nrow(group_data) < 3) {
          shapiro_list[[g]] <- data.frame(
            temp_group = g, variable = trait, statistic = NA, p = NA,
            message = "Not enough data points (<3) for Shapiro-Wilk test"
          )
        } else if (length(unique(group_data$temp_trait)) == 1) {
          shapiro_list[[g]] <- data.frame(
            temp_group = g, variable = trait, statistic = NA, p = NA,
            message = "Constant values in group, Shapiro-Wilk test not applicable"
          )
        } else {
          test_result <- tryCatch(
            stats::shapiro.test(group_data$temp_trait),
            error = function(e) {
              warning(paste0("Error in Shapiro-Wilk for group ", g, ": ", e$message))
              NULL
            }
          )
          if (!is.null(test_result)) {
            shapiro_list[[g]] <- data.frame(
              temp_group = g,
              variable = trait,
              statistic = unname(test_result$statistic),
              p = test_result$p.value,
              message = NA_character_
            )
          } else {
            shapiro_list[[g]] <- data.frame(
              temp_group = g, variable = trait, statistic = NA, p = NA,
              message = "Shapiro-Wilk test failed"
            )
          }
        }
      }
      
      if (length(shapiro_list) > 0) {
        dplyr::bind_rows(shapiro_list) %>% tibble::as_tibble()
      } else {
        NULL
      }
    })
    
    # Levene's test for the currently selected meristic trait
    meristic_levene_results_r <- reactive({
      df_filtered <- meristic_trait_data_r()
      trait <- selected_meristic_trait()
      if (is.null(df_filtered) || is.null(trait) || trait == "") return(NULL)
      
      if (length(unique(df_filtered$temp_trait)) == 1) {
        warning("Levene's test not applicable: Trait has constant values across all samples.")
        return(NULL)
      }
      
      tryCatch(
        car::leveneTest(temp_trait ~ temp_group, data = df_filtered),
        error = function(e) {
          warning("Error in Levene's test: ", e$message)
          NULL
        }
      )
    })
    
    # Display selected-trait results using the same workflow and format as the morphometric benchmark
    output$meristic_test_results <- renderPrint({
      df_filtered <- meristic_trait_data_r()
      trait <- selected_meristic_trait()
      
      if (is.null(df_filtered) || is.null(trait) || trait == "") {
        cat("Select meristic traits above, then choose one trait for univariate analysis.\n")
        return()
      }
      
      cat(sprintf("Meristic Trait '%s': %d samples.\n\n", trait, nrow(df_filtered)))
      cat("Sample sizes per group:\n")
      print(table(df_filtered$temp_group))
      
      ngroups <- dplyr::n_distinct(df_filtered$temp_group)
      if (ngroups < 2) {
        cat("\nNot enough groups to perform statistical test (requires at least 2 groups).\n")
        return()
      }
      
      zero_variance_groups <- df_filtered %>%
        dplyr::group_by(temp_group) %>%
        dplyr::summarise(sd_val = stats::sd(temp_trait, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(sd_val == 0)
      
      if (nrow(zero_variance_groups) > 0) {
        cat("\n⚠ Warning: Zero variance detected in the following groups for this trait, which may cause some tests to fail:\n")
        print(zero_variance_groups$temp_group)
      }
      
      if (length(unique(df_filtered$temp_trait)) == 1) {
        cat("\n⚠ Warning: Trait has constant values across all samples, statistical tests cannot be performed.\n")
        return()
      }
      
      cat("\n--- Testing Assumptions ---\n")
      cat("1) Normality per group: Shapiro-Wilk test\n")
      cat("2) Homogeneity of variance: Levene's test\n\n")
      
      shapiro <- meristic_shapiro_results_r()
      levene <- meristic_levene_results_r()
      
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
      
      valid_shapiro <- if (!is.null(shapiro)) shapiro$p[!is.na(shapiro$p)] else numeric(0)
      normality_met <- length(valid_shapiro) > 0 && all(valid_shapiro > 0.05)
      variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
      force_parametric <- isTRUE(input$meristic_force_parametric)
      
      cat(sprintf("\nAssumptions summary:\n - Normality met? %s\n - Homogeneity of variance met? %s\n",
                  ifelse(normality_met, "Yes", "No"),
                  ifelse(variance_met, "Yes", "No")))
      cat("\n--- Statistical Testing ---\n")
      
      tryCatch({
        current_trait_zero_variance <- nrow(zero_variance_groups) > 0
        if (ngroups == 2) {
          cat("Comparing 2 groups:\n")
          if ((normality_met && variance_met && !current_trait_zero_variance) || force_parametric) {
            if (force_parametric && (!normality_met || !variance_met || current_trait_zero_variance)) {
              cat("⚠ Forcing parametric test despite violated assumptions.\n\n")
            }
            cat("Performing parametric Student's t-test.\n\n")
            fit <- stats::t.test(temp_trait ~ temp_group, data = df_filtered)
            cat("t-test results:\n")
            print(fit)
          } else {
            cat("Assumptions violated or zero variance, performing non-parametric Wilcoxon test.\n\n")
            fit <- stats::wilcox.test(temp_trait ~ temp_group, data = df_filtered)
            cat("Wilcoxon test results:\n")
            print(fit)
          }
        } else {
          cat(sprintf("Comparing %d groups:\n", ngroups))
          if ((normality_met && variance_met && !current_trait_zero_variance) || force_parametric) {
            if (force_parametric && (!normality_met || !variance_met || current_trait_zero_variance)) {
              cat("⚠ Forcing parametric test despite violated assumptions.\n\n")
            }
            cat("Performing parametric ANOVA with Tukey HSD post-hoc.\n\n")
            fit <- stats::aov(temp_trait ~ temp_group, data = df_filtered)
            cat("ANOVA summary:\n")
            print(summary(fit))
            tukey <- stats::TukeyHSD(fit)[[1]] %>%
              as.data.frame() %>%
              tibble::rownames_to_column("Comparison")
            cat("\nTukey HSD post-hoc results:\n")
            print(tukey)
          } else {
            cat("Assumptions violated or zero variance, performing non-parametric Kruskal-Wallis test with Dunn post-hoc.\n\n")
            kw <- df_filtered %>% rstatix::kruskal_test(temp_trait ~ temp_group)
            dunn <- df_filtered %>%
              rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
              dplyr::mutate(Comparison = paste(group1, "vs", group2)) %>%
              dplyr::select(Comparison, dplyr::everything(), -group1, -group2)
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
    
    # Pairwise summaries for selected meristic traits, matching the morphometric benchmark
    meristic_all_pairwise_results <- reactive({
      df <- meristic_data_source_r()
      req(df)
      
      group_col <- names(df)[1]
      traits <- selected_meristic_traits_r()
      if (length(traits) == 0) return(list())
      
      results_list <- list()
      for (trait in traits) {
        df_trait <- df %>%
          dplyr::mutate(
            temp_trait = as.numeric(.data[[trait]]),
            temp_group = factor(.data[[group_col]])
          ) %>%
          dplyr::filter(!is.na(temp_trait))
        
        ngroups <- dplyr::n_distinct(df_trait$temp_group)
        if (ngroups < 2) next
        
        shapiro <- tryCatch(
          df_trait %>% dplyr::group_by(temp_group) %>% rstatix::shapiro_test(temp_trait),
          error = function(e) NULL
        )
        levene <- tryCatch(
          car::leveneTest(temp_trait ~ temp_group, data = df_trait),
          error = function(e) NULL
        )
        
        normality_met <- !is.null(shapiro) && all(shapiro$p > 0.05)
        variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
        force_parametric <- isTRUE(input$meristic_force_parametric)
        
        if (ngroups == 2) {
          groups <- levels(df_trait$temp_group)
          comp <- paste(sort(groups), collapse = " vs ")
          if ((normality_met && variance_met) || force_parametric) {
            pval <- tryCatch(stats::t.test(temp_trait ~ temp_group, data = df_trait)$p.value,
                             error = function(e) NA_real_)
            method <- "t-test"
          } else {
            pval <- tryCatch(stats::wilcox.test(temp_trait ~ temp_group, data = df_trait)$p.value,
                             error = function(e) NA_real_)
            method <- "Wilcoxon"
          }
          result <- data.frame(
            Comparison = comp,
            p_value = signif(pval, 4),
            Method = method,
            stringsAsFactors = FALSE
          )
        } else if ((normality_met && variance_met) || force_parametric) {
          fit <- stats::aov(temp_trait ~ temp_group, data = df_trait)
          result <- stats::TukeyHSD(fit)[[1]] %>%
            as.data.frame() %>%
            tibble::rownames_to_column("Comparison") %>%
            dplyr::mutate(Method = "ANOVA (Tukey)") %>%
            dplyr::select(Comparison, p_value = `p adj`, Method)
        } else {
          result <- df_trait %>%
            rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
            dplyr::mutate(
              Comparison = paste(pmin(group1, group2), "vs", pmax(group1, group2)),
              Method = "Kruskal (Dunn)"
            ) %>%
            dplyr::select(Comparison, p_value = p.adj, Method)
        }
        
        result <- result %>%
          dplyr::mutate(
            Comparison = sapply(
              strsplit(Comparison, "[-]|[ ]vs[ ]", perl = TRUE),
              function(x) paste(sort(trimws(x)), collapse = " vs ")
            )
          ) %>%
          dplyr::group_by(Comparison) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup()
        
        results_list[[trait]] <- result
      }
      results_list
    })
    
    meristic_significant_traits_text <- reactive({
      pairwise_list <- meristic_all_pairwise_results()
      req(pairwise_list)
      
      alpha <- as.numeric(input$meristic_alpha_level)
      if (is.null(alpha)) alpha <- 0.05
      
      all_comparisons <- unique(unlist(lapply(pairwise_list, function(df) df$Comparison)))
      combined_df <- data.frame(Comparison = all_comparisons, stringsAsFactors = FALSE)
      
      for (trait in names(pairwise_list)) {
        df <- pairwise_list[[trait]] %>%
          dplyr::select(Comparison, p_value, Method) %>%
          dplyr::rename(
            !!paste0(trait, "_p_value") := p_value,
            !!paste0(trait, "_method") := Method
          )
        combined_df <- merge(combined_df, df, by = "Comparison", all.x = TRUE)
      }
      
      pval_cols <- grep("_p_value$", names(combined_df), value = TRUE)
      if (length(pval_cols) == 0) return("No p-value columns found in the combined results.")
      
      combined_df$NormComparison <- sapply(
        strsplit(as.character(combined_df$Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
        function(x) paste(sort(trimws(x)), collapse = " vs ")
      )
      
      sig_list <- lapply(unique(combined_df$NormComparison), function(norm_comp) {
        rows <- combined_df[combined_df$NormComparison == norm_comp, ]
        sig_traits <- c()
        for (col in pval_cols) {
          pvals <- as.numeric(rows[[col]])
          if (any(!is.na(pvals) & pvals < alpha)) {
            sig_traits <- c(sig_traits, sub("_p_value$", "", col))
          }
        }
        if (length(sig_traits) > 0) paste0(norm_comp, ": ", paste(sig_traits, collapse = ", ")) else NULL
      })
      sig_list <- sig_list[!sapply(sig_list, is.null)]
      
      if (length(sig_list) == 0) {
        "No traits are significant at the selected alpha level."
      } else {
        paste0("List of significant traits:\n\n", paste(trimws(sig_list), collapse = "\n"))
      }
    })
    
    meristic_significant_traits_df <- reactive({
      pairwise_list <- meristic_all_pairwise_results()
      req(pairwise_list)
      
      alpha <- as.numeric(input$meristic_alpha_level)
      if (is.null(alpha)) alpha <- 0.05
      
      all_comparisons <- unique(unlist(lapply(pairwise_list, function(df) df$Comparison)))
      combined_df <- data.frame(Comparison = all_comparisons, stringsAsFactors = FALSE)
      
      for (trait in names(pairwise_list)) {
        df <- pairwise_list[[trait]] %>%
          dplyr::select(Comparison, p_value, Method) %>%
          dplyr::rename(
            !!paste0(trait, "_p_value") := p_value,
            !!paste0(trait, "_method") := Method
          )
        combined_df <- merge(combined_df, df, by = "Comparison", all.x = TRUE)
      }
      
      pval_cols <- grep("_p_value$", names(combined_df), value = TRUE)
      if (length(pval_cols) == 0) {
        return(data.frame(Comparison = "No p-value columns found", Significant_Traits = ""))
      }
      
      combined_df$NormComparison <- sapply(
        strsplit(as.character(combined_df$Comparison), "[-]|[ ]vs[ ]", perl = TRUE),
        function(x) paste(sort(trimws(x)), collapse = " vs ")
      )
      
      sig_list <- lapply(unique(combined_df$NormComparison), function(norm_comp) {
        rows <- combined_df[combined_df$NormComparison == norm_comp, ]
        sig_traits <- c()
        for (col in pval_cols) {
          pvals <- as.numeric(rows[[col]])
          if (any(!is.na(pvals) & pvals < alpha)) {
            sig_traits <- c(sig_traits, sub("_p_value$", "", col))
          }
        }
        if (length(sig_traits) > 0) {
          data.frame(
            Comparison = norm_comp,
            Significant_Traits = paste(sig_traits, collapse = ", "),
            stringsAsFactors = FALSE
          )
        } else NULL
      })
      sig_list <- sig_list[!sapply(sig_list, is.null)]
      
      if (length(sig_list) == 0) {
        data.frame(Comparison = "No significant traits", Significant_Traits = "", stringsAsFactors = FALSE)
      } else {
        dplyr::bind_rows(sig_list)
      }
    })
    
    output$download_meristic_all_summary <- downloadHandler(
      filename = function() paste0("summary_all_selected_meristic_traits_", Sys.Date(), ".csv"),
      content = function(file) {
        pairwise_list <- meristic_all_pairwise_results()
        if (is.null(pairwise_list) || length(pairwise_list) == 0) {
          writeLines("No valid results to export.", file)
          return()
        }
        
        all_comparisons <- sort(unique(unlist(lapply(pairwise_list, function(df) df$Comparison))))
        combined_df <- data.frame(Comparison = all_comparisons, stringsAsFactors = FALSE)
        
        for (trait in names(pairwise_list)) {
          df <- pairwise_list[[trait]][, c("Comparison", "p_value", "Method")]
          colnames(df)[2:3] <- c(paste0(trait, "_p_adj"), paste0(trait, "_method"))
          combined_df <- merge(combined_df, df, by = "Comparison", all.x = TRUE)
        }
        write.csv(combined_df, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    output$download_meristic_summary <- downloadHandler(
      filename = function() {
        trait <- selected_meristic_trait()
        if (is.null(trait) || trait == "") trait <- "no_trait_selected"
        paste0("univariate_results_", trait, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        trait <- selected_meristic_trait()
        if (is.null(trait) || trait == "") {
          writeLines("No trait selected.", file)
          return()
        }
        
        df <- meristic_data_source_r()
        req(df)
        group <- names(df)[1]
        df_filtered <- df %>%
          dplyr::mutate(
            !!rlang::sym(group) := droplevels(as.factor(.data[[group]])),
            temp_trait = as.numeric(.data[[trait]]),
            temp_group = factor(.data[[group]])
          ) %>%
          dplyr::filter(!is.na(temp_trait))
        
        ngroups <- dplyr::n_distinct(df_filtered$temp_group)
        if (ngroups < 2 || length(unique(df_filtered$temp_trait)) == 1) {
          writeLines("Not enough valid data to perform test.", file)
          return()
        }
        
        shapiro <- tryCatch(
          df_filtered %>% dplyr::group_by(temp_group) %>% rstatix::shapiro_test(temp_trait),
          error = function(e) NULL
        )
        levene <- tryCatch(
          car::leveneTest(temp_trait ~ temp_group, data = df_filtered),
          error = function(e) NULL
        )
        normality_met <- !is.null(shapiro) && all(shapiro$p > 0.05)
        variance_met <- !is.null(levene) && levene[["Pr(>F)"]][1] > 0.05
        force_parametric <- isTRUE(input$meristic_force_parametric)
        
        result <- tryCatch({
          if (ngroups == 2) {
            if ((normality_met && variance_met) || force_parametric) {
              broom::tidy(stats::t.test(temp_trait ~ temp_group, data = df_filtered))
            } else {
              broom::tidy(stats::wilcox.test(temp_trait ~ temp_group, data = df_filtered))
            }
          } else if ((normality_met && variance_met) || force_parametric) {
            fit <- stats::aov(temp_trait ~ temp_group, data = df_filtered)
            stats::TukeyHSD(fit)[[1]] %>%
              as.data.frame() %>%
              tibble::rownames_to_column("Comparison")
          } else {
            df_filtered %>%
              rstatix::dunn_test(temp_trait ~ temp_group, p.adjust.method = "bonferroni") %>%
              dplyr::mutate(
                Comparison = paste(group1, "vs", group2),
                Method = "Kruskal (Dunn)"
              ) %>%
              dplyr::select(Comparison, `p adj` = p.adj, Method)
          }
        }, error = function(e) data.frame(Message = paste("Error:", conditionMessage(e))))
        
        write.csv(result, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    output$meristic_alpha_selector_ui <- renderUI({
      radioButtons(
        ns("meristic_alpha_level"),
        label = strong("Select alpha level for significance"),
        choices = c("0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001),
        selected = 0.05,
        inline = TRUE
      )
    })
    
    output$meristic_significant_traits_ui <- renderUI({
      verbatimTextOutput(ns("meristic_significant_traits"))
    })
    
    output$meristic_significant_traits <- renderText({
      meristic_significant_traits_text()
    })
    
    output$download_meristic_significant_traits <- downloadHandler(
      filename = function() {
        alpha <- as.numeric(input$meristic_alpha_level)
        if (is.null(alpha)) alpha <- 0.05
        paste0("significant_traits_alpha_", alpha, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- meristic_significant_traits_df()
        req(df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    meristic_matrix_r <- reactive({
      traits <- selected_meristic_traits_r()
      df <- meristic_data_source_r()
      if (is.null(df) || nrow(df) == 0 || length(traits) < 1) return(NULL)
      group <- names(df)[1]
      mat <- as.data.frame(lapply(df[, traits, drop = FALSE], as.numeric))
      keep <- stats::complete.cases(mat, df[[group]])
      list(mat = mat[keep, , drop = FALSE], group = droplevels(as.factor(df[[group]][keep])))
    })
    
    observeEvent(input$run_meristic_permanova, {
      dat <- meristic_matrix_r()
      if (is.null(dat) || ncol(dat$mat) < 1 || nrow(dat$mat) < 2 || dplyr::n_distinct(dat$group) < 2) {
        showNotification("Select meristic traits with enough complete data and at least two groups before running PERMANOVA.", type = "error")
        return()
      }
      permanova_data_input <- dat$mat[, apply(dat$mat, 2, function(x) sd(x, na.rm = TRUE) > 0), drop = FALSE]
      if (ncol(permanova_data_input) < 1) {
        showNotification("Selected meristic traits are constant after filtering. PERMANOVA cannot be run.", type = "error")
        return()
      }
      permanova_data_input <- as.data.frame(scale(permanova_data_input))
      main_result <- tryCatch({
        vegan::adonis2(permanova_data_input ~ dat$group,
                       permutations = input$meristic_permanova_permutations,
                       method = "euclidean")
      }, error = function(e) {
        showNotification(paste("Error running meristic PERMANOVA:", e$message), type = "error")
        NULL
      })
      meristic_main_results_r(main_result)
      pairwise_result <- tryCatch({
        pairwise.adonis(x = permanova_data_input,
                        factors = dat$group,
                        sim.method = "euclidean",
                        permutations = input$meristic_permanova_permutations)
      }, error = function(e) {
        data.frame(Message = paste("Pairwise PERMANOVA could not be computed:", e$message))
      })
      meristic_pairwise_results_r(pairwise_result)
      bd_capture <- tryCatch({
        dist_mat <- vegan::vegdist(permanova_data_input, method = "euclidean")
        bd <- vegan::betadisper(dist_mat, dat$group)
        bd_permutest <- vegan::permutest(bd, permutations = input$meristic_permanova_permutations, pairwise = TRUE)
        bd_tukey <- if (nlevels(dat$group) > 2) TukeyHSD(bd) else NULL
        list(betadisper = bd, permutest = bd_permutest, tukey = bd_tukey)
      }, error = function(e) {
        showNotification(paste("Warning: meristic betadisper could not be computed:", e$message), type = "warning")
        NULL
      })
      meristic_betadisper_results_r(bd_capture)
      if (!is.null(bd_capture)) {
        cent <- tryCatch({
          cd <- round(as.matrix(dist(bd_capture$betadisper$centroids)), 4)
          data.frame(Group = rownames(cd), cd, row.names = NULL, check.names = FALSE)
        }, error = function(e) NULL)
        meristic_centroid_dist_r(cent)
      } else {
        meristic_centroid_dist_r(NULL)
      }
      showNotification("Meristic PERMANOVA analysis complete.", type = "default", duration = 10)
    })
    
    output$meristic_permanova_main_results <- renderPrint({
      if (is.null(meristic_main_results_r())) { cat("Meristic PERMANOVA has not been run yet.\n"); return() }
      print(meristic_main_results_r())
    })
    output$meristic_permanova_pairwise_results <- renderDT({
      df <- meristic_pairwise_results_r()
      if (is.null(df)) df <- data.frame(Message = "Meristic pairwise PERMANOVA has not been run yet.")
      datatable(df, options = list(dom = 'tip', pageLength = 25, scrollX = TRUE))
    })
    output$meristic_betadisper_results <- renderPrint({
      res <- meristic_betadisper_results_r()
      if (is.null(res)) { cat("Meristic dispersion analysis has not been run yet.\n"); return() }
      cat("=== Meristic Homogeneity of Group Dispersions (betadisper) ===\n\n")
      cat("Average distance to group centroid:\n")
      print(round(res$betadisper$group.distances, 4))
      cat("\n--- Permutation test ---\n")
      print(res$permutest$tab)
    })
    output$meristic_betadisper_tukey_results <- renderDT({
      res <- meristic_betadisper_results_r()
      if (is.null(res) || is.null(res$tukey)) {
        return(datatable(data.frame(Message = "Pairwise dispersion comparisons require more than 2 groups."),
                         options = list(dom = 't', paging = FALSE, searching = FALSE)))
      }
      tukey_df <- as.data.frame(res$tukey$group) %>%
        tibble::rownames_to_column("Comparison") %>%
        dplyr::rename(diff = diff, lower = lwr, upper = upr, p_adj = `p adj`) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
      datatable(tukey_df, options = list(dom = 't', paging = FALSE, searching = FALSE))
    })
    output$meristic_centroid_dist_results <- renderDT({
      df <- meristic_centroid_dist_r()
      if (is.null(df)) df <- data.frame(Message = "Meristic centroid distances are not available.")
      datatable(df, options = list(dom = 'tip', pageLength = 25, scrollX = TRUE), rownames = FALSE)
    })
    output$meristic_consolidated_pairwise <- renderDT({
      df <- meristic_consolidated_pairwise_r()
      if (is.null(df) || nrow(df) == 0) {
        return(datatable(data.frame(Message = "Consolidated table unavailable. Run both PERMANOVA and BetaDisper first."),
                         options = list(dom = 't', paging = FALSE, searching = FALSE)))
      }
      datatable(df, rownames = FALSE, options = list(dom = 'tip', pageLength = 25, scrollX = TRUE))
    })
    output$download_meristic_all_permanova <- downloadHandler(
      filename = function() paste0("permanova_results_meristic_", Sys.Date(), ".zip"),
      content = function(file) {
        tmpdir <- tempdir(); files <- c()
        perm_main <- meristic_main_results_r()
        if (!is.null(perm_main)) {
          f <- file.path(tmpdir, paste0("permanova_main_meristic_", Sys.Date(), ".txt"))
          sink(f); print(perm_main); sink(); files <- c(files, f)
        }
        perm_pair <- meristic_pairwise_results_r()
        if (!is.null(perm_pair)) {
          f <- file.path(tmpdir, paste0("permanova_pairwise_meristic_", Sys.Date(), ".csv"))
          write.csv(perm_pair, f, row.names = FALSE); files <- c(files, f)
        }
        bd <- meristic_betadisper_results_r()
        if (!is.null(bd)) {
          f <- file.path(tmpdir, paste0("betadisper_meristic_", Sys.Date(), ".txt"))
          sink(f)
          cat("=== Multivariate Homogeneity of Group Dispersions ===\n\n")
          print(round(bd$betadisper$group.distances, 4))
          cat("\n--- Permutation test ---\n"); print(bd$permutest$tab)
          if (!is.null(bd$tukey)) { cat("\n--- Tukey HSD ---\n"); print(bd$tukey) }
          sink(); files <- c(files, f)
          if (!is.null(bd$tukey)) {
            f <- file.path(tmpdir, paste0("betadisper_pairwise_meristic_", Sys.Date(), ".csv"))
            write.csv(as.data.frame(bd$tukey$group) %>% tibble::rownames_to_column("Comparison") %>%
                        dplyr::rename(diff=diff,lower=lwr,upper=upr,p_adj=`p adj`), f, row.names=FALSE)
            files <- c(files, f)
          }
        }
        df_cent <- meristic_centroid_dist_r()
        if (!is.null(df_cent)) {
          f <- file.path(tmpdir, paste0("centroid_distances_meristic_", Sys.Date(), ".csv"))
          write.csv(df_cent, f, row.names = FALSE); files <- c(files, f)
        }
        df_cons <- meristic_consolidated_pairwise_r()
        if (!is.null(df_cons)) {
          f <- file.path(tmpdir, paste0("consolidated_pairwise_meristic_", Sys.Date(), ".csv"))
          write.csv(df_cons, f, row.names = FALSE); files <- c(files, f)
        }
        if (length(files) == 0) { 
          f <- file.path(tmpdir, "no_meristic_permanova_results.txt")
          writeLines("No meristic PERMANOVA results available.", f)
          files <- c(files, f)
        }
        zip(file, files, flags = "-j")
      }
    )
    
    return(data_r)
  })
}
