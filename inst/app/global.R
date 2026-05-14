# global.R

# List all required packages for the entire application
required_packages <- c(
  "shiny", "DT", "dplyr", "ggplot2", "tidyr", "vegan", "viridis",
  "RColorBrewer", "rstatix", "car", "readr", "adegenet", "FactoMineR", "factoextra",
  "shinyjs", "colourpicker", "forcats", "purrr", "scales", "PCAtest",
  "openxlsx", "shinyWidgets", "ggthemes", "broom", "tibble",
  "htmltools", "stringr", "ggpubr", "ggrepel", "patchwork", "mclust", "conflicted", 
  "Boruta", "shinybusy", "plotly", "ggridges", "ape"
)

# Source Unified Data Input module
source("modules/mod_data_unified.R")

# Source Meristic modules
source("modules/meristic/mod_data.R")
source("modules/meristic/mod_summary.R")
source("modules/meristic/mod_stats.R")
source("modules/meristic/mod_visual.R")

# Source Morphometric modules
source("modules/morphometric/mod_data_morphometric.R")
source("modules/morphometric/mod_summary_morphometric.R")
source("modules/morphometric/mod_allometry_morphometric.R")
source("modules/morphometric/mod_stats_morphometric.R")
source("modules/morphometric/mod_species_delim.R")
source("modules/morphometric/mod_visual_morphometric.R")

# Source Combined modules
source("modules/combined/mod_data_combined.R")
source("modules/combined/mod_summary_combined.R")
source("modules/combined/mod_allometry_combined.R")
#source("modules/combined/mod_stats_combined.R")
source("modules/combined/mod_visual_combined.R")
source("modules/combined/mod_mfa.R")
source("modules/combined/mod_mfa_delim.R")

# Function to check for and install missing packages
check_and_install_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste0("Package '", pkg, "' not found. Attempting to install..."))
      
      if (pkg == "PCAtest") {
        if (!requireNamespace("devtools", quietly = TRUE)) {
          message("Installing 'devtools'...")
          install.packages("devtools", dependencies = TRUE)
          if (!requireNamespace("devtools", quietly = TRUE)) {
            stop("Failed to install 'devtools'. Please install it manually.")
          }
        }
        message("Installing 'PCAtest' from GitHub (arleyc/PCAtest)...")
        devtools::install_github("arleyc/PCAtest")
      } else {
        install.packages(pkg, dependencies = TRUE)
      }
      
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(paste0("Failed to install package '", pkg, "' — please install it manually."))
      }
    }
    library(pkg, character.only = TRUE)
  }
}

# Load required packages
check_and_install_packages(required_packages)

if (!exists(".groupstruct2_startup_done", envir = globalenv())) {
  cat("\n  All packages loaded. Starting app...\n\n")
  assign(".groupstruct2_startup_done", TRUE, envir = globalenv())
}

# Resolve conflicts using conflicted package
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer("select",          "dplyr",        quiet = TRUE)
  conflicted::conflict_prefer("filter",          "dplyr",        quiet = TRUE)
  conflicted::conflict_prefer("matches",         "dplyr",        quiet = TRUE)
  conflicted::conflict_prefer("rename",          "dplyr",        quiet = TRUE)
  conflicted::conflict_prefer("em",              "shiny",        quiet = TRUE)
  conflicted::conflict_prefer("count",           "dplyr",        quiet = TRUE)
  conflicted::conflict_prefer("map",             "purrr",        quiet = TRUE)
  conflicted::conflict_prefer("renderDataTable", "DT",           quiet = TRUE)
  conflicted::conflict_prefer("colourInput",     "colourpicker", quiet = TRUE)
}

# Define %||% operator (if not already defined)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Helper function for color scale 
get_color_scale <- function(palette_choices) {
  if (is.null(palette_choices)) {
    return(scales::hue_pal())
  } else if (is.function(palette_choices)) { # If a palette function is passed
    return(palette_choices)
  } else if (is.character(palette_choices) && length(palette_choices) > 1) {
    return(ggplot2::scale_color_manual(values = palette_choices))
  } else { # Default to hue_pal if no specific palette function/colors
    return(scales::hue_pal())
  }
}

MclustBayesFactorClassMerge <- function(data, 
                                        class, 
                                        modelType = "EDDA", 
                                        ...)
{
  ## Adapted from original code by: 
  # Luca Scruccca luca.scrucca@unipg.it
  
  data <- data.matrix(data)
  class <- cl <- as.factor(class)
  lclass <- lcl <- levels(class)
  nclass <- ncl <- nlevels(class)
  
  bestMod <- MclustDA(data = data, class = class, 
                      modelType = modelType, ...)
  BIC <- bestMod$bic
  K <- nclass
  combiClass <- list(lclass)
  combiM <- list(diag(K))
  
  while(ncl > 1)
  {
    allTuples <- combn(ncl, 2, simplify = FALSE)
    bic <- NULL
    for(j in 1:length(allTuples))
    {
      merge <- allTuples[[j]]
      # M <- combMat(ncl, merge[1], merge[2])
      ly <- lcl
      ly[merge] <- paste(lcl[merge], collapse = "-")
      y <- factor(cl, levels = lcl, labels = ly)
      mod <- MclustDA(data = data, class = y, 
                      modelType = modelType, ...)
      if(mod$bic > bestMod$bic) bestMod <- mod
      bic <- c(bic, mod$bic)
    }
    j <- which.max(bic)
    merge <- allTuples[[j]]
    M <- combMat(ncl, merge[1], merge[2])
    lcl[merge] <- paste(lcl[merge], collapse = "-")
    cl <- factor(cl, levels = levels(cl), labels = lcl)
    lcl <- levels(cl)
    ncl <- nlevels(cl)
    #
    BIC <- c(BIC, bic[j])
    combiM <- append(combiM, list(M))
    K <- c(K, ncl)
    combiClass <- append(combiClass, list(lcl))
  }
  #
  BIC_diff <- max(BIC) - BIC 
  BF <- exp(0.5*BIC_diff) 
  logMarLik <- 0.5*BIC 
  # Posterior model probability 
  # (assuming equal a priori model probs)
  post <- exp(logMarLik - mclust::logsumexp(logMarLik))
  
  tab <- data.frame("K" = K, "BIC" = BIC, 
                    "∆BIC = 2logBF" = BIC_diff, 
                    "BF = exp(∆BIC/2)" = BF, 
                    "PostMod" = post,
                    row.names = sapply(combiClass,
                                       paste0, 
                                       collapse = "|"),
                    check.names = FALSE)
  
  M <- vector(mode = "list", length = nclass)
  M[[1]] <- combiM[[1]]
  for(k in 2:nclass)
    M[[k]] <- combiM[[k]] %*% M[[k-1]]
  
  out <- list(tab = tab,
              k = which.max(BIC),
              modelType = modelType,
              combiM = combiM,
              M = M,
              combiClass = combiClass,
              class = class,
              bestMod = bestMod)
  return(out)
} 

### Fx to summarize results
summarize_class_merge <- function(result, digits = 3) {
  
  tab <- result$tab
  tab <- tab[order(tab$BIC, decreasing = TRUE), ]
  
  numeric_cols <- sapply(tab, is.numeric)
  tab[numeric_cols] <- lapply(tab[numeric_cols], round, digits = digits)
  
  lumped_pops <- rownames(tab)
  tab <- cbind("Lumped populations" = lumped_pops, tab)
  rownames(tab) <- NULL
  
  return(tab)
}

# Bayesian species delimitation function
# ── Phylogenetic hypothesis enumeration ───────────────────────────────────────
# Enumerates all valid monophyletic partitions from a rooted Newick species tree.
# Tip labels must exactly match OTU names in the dataset.
generate_all_phylo_hypotheses <- function(tree, species_col) {
  spec_vec     <- as.character(species_col)
  data_species <- unique(spec_vec)
  tree_tips    <- tree$tip.label
  
  only_in_tree <- setdiff(tree_tips, data_species)
  only_in_data <- setdiff(data_species, tree_tips)
  
  # Prune tips absent from data
  if (length(only_in_tree) > 0)
    tree <- ape::drop.tip(tree, only_in_tree)
  
  n_tips <- length(tree$tip.label)
  if (n_tips < 2)
    return(list(error = "Fewer than 2 tree tips matched the dataset OTU labels."))
  
  internal_nodes <- (n_tips + 1L):(n_tips + tree$Nnode)
  n_int          <- length(internal_nodes)
  node_tips      <- lapply(internal_nodes, function(nd)
    ape::extract.clade(tree, nd)$tip.label)
  
  anc <- matrix(FALSE, n_int, n_int)
  for (i in seq_len(n_int))
    for (j in seq_len(n_int))
      if (i != j) anc[i, j] <- all(node_tips[[j]] %in% node_tips[[i]])
  
  CAP_COMBOS    <- 20000L
  antichains    <- list()
  total_checked <- 0L
  capped        <- FALSE
  
  for (sz in seq_len(n_int)) {
    n_c <- choose(n_int, sz)
    if (total_checked + n_c > CAP_COMBOS) { capped <- TRUE; break }
    total_checked <- total_checked + n_c
    combos <- combn(n_int, sz, simplify = FALSE)
    for (cmb in combos) {
      ok <- TRUE
      if (sz > 1L) {
        for (a in seq_len(sz - 1L)) {
          for (b in (a + 1L):sz) {
            if (anc[cmb[a], cmb[b]] || anc[cmb[b], cmb[a]]) { ok <- FALSE; break }
          }
          if (!ok) break
        }
      }
      if (ok) antichains <- c(antichains, list(cmb))
    }
  }
  
  hyp_cols   <- list()
  disp_names <- character(0)
  k_vals     <- integer(0)
  
  k_full <- length(data_species)
  hyp_cols[["K_full"]] <- spec_vec
  disp_names["K_full"] <- paste0("K=", k_full, ": All split (original OTUs)")
  k_vals["K_full"]     <- k_full
  
  for (ac in antichains) {
    labels <- spec_vec
    lumped <- character(0)
    for (idx in ac) {
      grp    <- paste(sort(node_tips[[idx]]), collapse = "+")
      labels[spec_vec %in% node_tips[[idx]]] <- grp
      lumped <- c(lumped, grp)
    }
    k <- length(unique(labels))
    if (k < 2L) next
    
    key <- paste0("K", k, "_", paste(sort(lumped), collapse = "__"))
    if (!is.null(hyp_cols[[key]])) key <- paste0(key, "_", length(hyp_cols) + 1L)
    
    hyp_cols[[key]] <- labels
    disp_names[key] <- paste0("K=", k, ": ", paste(sort(unique(labels)), collapse = " | "))
    k_vals[key]     <- k
  }
  
  list(
    hyp_df        = as.data.frame(hyp_cols, stringsAsFactors = FALSE, check.names = FALSE),
    display_names = disp_names,
    k_values      = k_vals,
    only_in_tree  = only_in_tree,
    only_in_data  = only_in_data,
    tree_used     = tree,
    n_hypotheses  = length(hyp_cols),
    capped        = capped
  )
}
# ── End phylogenetic hypothesis enumeration ────────────────────────────────────

# ── Tree layout and phylogenetic hypothesis plot helpers ──────────────────────
# These are used by both mod_visual_combined and mod_visual_morphometric.

compute_tree_layout <- function(tree, type = "cladogram") {
  tree   <- ape::ladderize(tree)
  n_tips <- length(tree$tip.label)
  n_all  <- n_tips + tree$Nnode
  root   <- n_tips + 1L
  children_of <- vector("list", n_all)
  for (i in seq_len(nrow(tree$edge))) {
    p <- tree$edge[i, 1L]; ch <- tree$edge[i, 2L]
    children_of[[p]] <- c(children_of[[p]], ch)
  }
  x <- numeric(n_all)
  for (i in seq_len(nrow(tree$edge))) {
    p <- tree$edge[i, 1L]; ch <- tree$edge[i, 2L]
    bl <- if (type == "phylogram" && !is.null(tree$edge.length))
      tree$edge.length[i] else 1
    x[ch] <- x[p] + bl
  }
  if (type != "phylogram" || is.null(tree$edge.length))
    x[seq_len(n_tips)] <- max(x[seq_len(n_tips)])
  y <- numeric(n_all)
  tip_counter <- 0L
  stack <- root
  post_order <- integer(0)
  while (length(stack) > 0L) {
    nd <- stack[length(stack)]; stack <- stack[-length(stack)]
    post_order <- c(nd, post_order)
    ch <- children_of[[nd]]
    if (length(ch) == 0L) {
      tip_counter <- tip_counter + 1L; y[nd] <- tip_counter
    } else {
      stack <- c(stack, rev(ch))
    }
  }
  for (nd in post_order) {
    ch <- children_of[[nd]]
    if (length(ch) > 0L) y[nd] <- mean(y[ch])
  }
  tip_df <- data.frame(label = tree$tip.label,
                       x = x[seq_len(n_tips)], y = y[seq_len(n_tips)],
                       stringsAsFactors = FALSE)
  h_segs <- data.frame(x    = x[tree$edge[, 1L]], xend = x[tree$edge[, 2L]],
                       y    = y[tree$edge[, 2L]],  yend = y[tree$edge[, 2L]])
  v_segs <- do.call(rbind, lapply(unique(tree$edge[, 1L]), function(nd) {
    ch <- children_of[[nd]]
    data.frame(x = x[nd], xend = x[nd], y = min(y[ch]), yend = max(y[ch]))
  }))
  list(tip_df = tip_df, h_segs = h_segs, v_segs = v_segs,
       max_x = max(x), n_tips = n_tips, tree = tree)
}

build_phylo_hyp_plot <- function(sup_data, n_top, tree_type, tip_sz, rect_alpha) {
  tree         <- sup_data$tree_used
  hyp_df       <- sup_data$hyp_df
  ordered_keys <- sup_data$ordered_hyp_keys
  species_col  <- as.character(sup_data$species_col)
  summary_tbl  <- sup_data$summary_table
  if (is.null(tree) || is.null(hyp_df) || length(ordered_keys) == 0) return(NULL)
  layout  <- compute_tree_layout(tree, type = tree_type)
  tip_df  <- layout$tip_df; max_x <- layout$max_x; n_tips <- layout$n_tips
  n_top_actual <- min(n_top, length(ordered_keys))
  top_keys     <- head(ordered_keys, n_top_actual)
  sp_to_group_list <- lapply(top_keys, function(key) {
    col_labels <- as.character(hyp_df[[key]])
    setNames(vapply(tip_df$label, function(sp) {
      idx <- which(species_col == sp)
      if (length(idx) == 0L) return(sp)
      unique(col_labels[idx])[1L]
    }, character(1L)), tip_df$label)
  })
  qual_pal <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                "#A65628","#F781BF","#66C2A5","#FC8D62","#8DA0CB",
                "#E78AC3","#FFD92F")
  label_width <- max(nchar(tip_df$label)) * max_x * 0.022
  col_w   <- max_x * 0.10; col_gap <- max_x * 0.015
  col_x_ctr <- max_x + label_width + col_w / 2 +
    (seq_len(n_top_actual) - 1L) * (col_w + col_gap)
  rect_rows <- list()
  for (i in seq_len(n_top_actual)) {
    sp_grp <- sp_to_group_list[[i]]
    grp_sizes <- table(sp_grp[tip_df$label])
    lumped <- names(grp_sizes)[grp_sizes > 1L]
    colors <- setNames(qual_pal[seq_along(lumped)], lumped)
    for (grp in unique(sp_grp)) {
      tips_in <- tip_df$label[sp_grp[tip_df$label] == grp]
      ys <- tip_df$y[tip_df$label %in% tips_in]
      if (length(ys) == 0L) next
      rect_rows <- c(rect_rows, list(data.frame(
        xmin = col_x_ctr[i] - col_w / 2, xmax = col_x_ctr[i] + col_w / 2,
        ymin = min(ys) - 0.38, ymax = max(ys) + 0.38,
        fill = if (grp %in% lumped) colors[grp] else "#CCCCCC",
        stringsAsFactors = FALSE)))
    }
  }
  rect_df  <- if (length(rect_rows) > 0L) dplyr::bind_rows(rect_rows) else data.frame()
  k_col    <- if ("K" %in% names(summary_tbl)) "K" else NULL
  dbic_col <- grep("BIC", names(summary_tbl), value = TRUE)[2]
  hyp_col  <- "Hypothesis"
  k_vals   <- if (!is.null(k_col))
    summary_tbl[[k_col]][match(top_keys, summary_tbl[[hyp_col]])] else rep("?", n_top_actual)
  dbic_raw  <- if (!is.null(dbic_col))
    summary_tbl[[dbic_col]][match(top_keys, summary_tbl[[hyp_col]])] else rep(NA, n_top_actual)
  dbic_vals <- round(as.numeric(dbic_raw), 1)
  header_df <- data.frame(x = col_x_ctr, label  = paste0("K=", k_vals),
                          label2 = paste0("\u0394=", dbic_vals), stringsAsFactors = FALSE)
  rank_df   <- data.frame(x = col_x_ctr, label = paste0("#", seq_len(n_top_actual)),
                          stringsAsFactors = FALSE)
  x_max_plot <- max(col_x_ctr) + col_w / 2 + max_x * 0.01
  
  # Balanced spacing - reduced from original but enough to prevent overlap
  # Original was: rank at +2.1, K at +1.6, delta at +1.1 (0.5 spacing)
  # New spacing: 0.4 between rows
  rank_y_pos <- n_tips + 1.8    # Was 2.1
  k_y_pos    <- n_tips + 1.4    # Was 1.6  
  delta_y_pos <- n_tips + 1.0   # Was 1.1
  
  max_y_needed <- rank_y_pos + 0.5  # Small padding above highest text
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = layout$v_segs,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          color = "black", linewidth = 0.55) +
    ggplot2::geom_segment(data = layout$h_segs,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          color = "black", linewidth = 0.55) +
    ggplot2::geom_text(data = tip_df,
                       ggplot2::aes(x = max_x + max_x * 0.025, y = y, label = label),
                       hjust = 0, size = tip_sz, fontface = "italic") +
    ggplot2::geom_rect(data = rect_df,
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                    fill = fill),
                       color = "white", linewidth = 0.35, alpha = rect_alpha) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(data = rank_df,
                       ggplot2::aes(x = x, y = rank_y_pos, label = label),
                       size = 3.8, hjust = 0.5, fontface = "bold") +
    ggplot2::geom_text(data = header_df,
                       ggplot2::aes(x = x, y = k_y_pos, label = label),
                       size = 3.4, hjust = 0.5) +
    ggplot2::geom_text(data = header_df,
                       ggplot2::aes(x = x, y = delta_y_pos, label = label2),
                       size = 3.2, hjust = 0.5, color = "#444444") +
    # Moderate expansion - enough for the text but no extra
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0.5, 0.2)),
                                limits = c(NA, max_y_needed)) +
    ggplot2::coord_cartesian(xlim = c(-max_x * 0.02, x_max_plot), clip = "off") +
    ggplot2::theme_void() +
    # Small top margin to prevent text from being cut off
    ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 5, l = 15, unit = "pt"))
  
  if (tree_type == "phylogram" && !is.null(tree$edge.length)) {
    sb_len <- signif(max_x / 5, 1)
    p <- p +
      ggplot2::annotate("segment", x = 0, xend = sb_len, y = -0.5, yend = -0.5, linewidth = 0.5) +
      ggplot2::annotate("text", x = sb_len / 2, y = -0.9, label = as.character(sb_len), size = 2.5)
  }
  p
}


# ── End tree layout helpers ────────────────────────────────────────────────────

GMMBayesFactorTable <- function(..., prior = NULL) {
  stopifnot(requireNamespace("mclust", quietly = TRUE))
  stopifnot(packageVersion("mclust") >= "6.1")
  
  models <- list(...)
  model_names <- names(models) 
  
  stopifnot(all(sapply(models, function(mod) 
    inherits(mod, "MclustDA"))))
  classes <- sapply(models, function(mod) 
    paste0(levels(mod$class), 
           collapse = "|")) 
  M <- length(models)
  if(is.null(prior)) prior <- rep(1/M, M)
  prior <- prior/sum(prior)
  stopifnot("The sum of all the priors exceed 1"=
              length(prior) == M)
  
  K <- sapply(models, 
              function(mod) 
                nlevels(mod$class)) 
  BIC <- sapply(models, 
                function(mod) 
                  mod$bic)
  BIC_diff <- max(BIC) - BIC
  BF <- exp(0.5*BIC_diff) 
  logMarLik <- 0.5*BIC 
  
  post <- exp(logMarLik + log(prior) - 
                mclust::logsumexp(logMarLik + log(prior)))
  
  tab <- data.frame("Hypothesis" = model_names,  
                    "Taxonomic groupings" = classes,
                    "K" = K, "BIC" = BIC, 
                    "∆BIC = 2logBF" = round(BIC_diff,2), 
                    "BF = exp(∆BIC/2)" = round(BF,2), 
                    "PriorMod" = prior,
                    "PostMod" = post,
                    check.names = FALSE)
  row.names(tab) <- NULL
  return(tab)
}

# Helper function for GMM model interpretation
get_model_interpretation <- function(model_name) {
  interpretations <- c(
    "EII" = "Spherical, equal volume",
    "VII" = "Spherical, unequal volume",
    "EEI" = "Diagonal, equal volume and shape",
    "VEI" = "Diagonal, varying volume, equal shape",
    "EVI" = "Diagonal, equal volume, varying shape",
    "VVI" = "Diagonal, varying volume and shape",
    "EEE" = "Ellipsoidal, equal volume, shape, and orientation",
    "EVE" = "Ellipsoidal, equal volume and orientation",
    "VEE" = "Ellipsoidal, equal shape and orientation",
    "VVE" = "Ellipsoidal, equal orientation",
    "EEV" = "Ellipsoidal, equal volume and shape",
    "VEV" = "Ellipsoidal, equal shape",
    "EVV" = "Ellipsoidal, equal volume",
    "VVV" = "Ellipsoidal, varying volume, shape, and orientation"
  )
  
  # Use %||% or a simple fallback
  result <- interpretations[model_name]
  if (is.na(result)) {
    return("Unknown model type")
  } else {
    return(as.character(result))
  }
}

# Example datasets for landing page
example_meristic <- data.frame(
  Species = rep(c("Species A", "Species B", "Species C"), each = 3),
  Midbody_Scale = c(35L, 36L, 34L, 40L, 41L, 39L, 30L, 31L, 32L),
  Ventral_Scale = c(100L, 98L, 102L, 110L, 108L, 111L, 95L, 96L, 94L),
  Dorsal_Scale = c(45L, 46L, 44L, 50L, 52L, 51L, 40L, 41L, 39L),
  stringsAsFactors = FALSE
)

example_morphometric <- data.frame(
  Species = rep(c("Species A", "Species B", "Species C"), each = 3),
  Snout_vent_Length = c(85.5, 87.2, 86.0, 90.1, 89.5, 91.0, 80.0, 79.5, 81.2),
  Head_Length = c(25.2, 25.0, 24.8, 27.0, 26.8, 27.2, 23.0, 22.9, 23.1),
  Snout_Length = c(10.5, 10.7, 10.6, 11.0, 11.2, 10.9, 9.8, 9.7, 9.9),
  stringsAsFactors = FALSE
)

example_mixed <- data.frame(
  Species = rep(c("Species A", "Species B", "Species C"), each = 3),
  Midbody_Scale = c(35, 36, 34, 40, 41, 39, 30, 31, 32),
  Ventral_Scale = c(100, 98, 102, 110, 108, 111, 95, 96, 94),
  Snout_vent_Length = c(85.5, 87.2, 86.0, 90.1, 89.5, 91.0, 80.0, 79.5, 81.2),
  Head_Length = c(25.2, 25.0, 24.8, 27.0, 26.8, 27.2, 23.0, 22.9, 23.1),
  Stripes = rep(c("Yes", "No", "Yes"), 3),
  Eye_Color = rep(c("Brown", "Green", "Blue"), 3),
  stringsAsFactors = FALSE
)
