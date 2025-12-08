# global.R

# List all required packages for the entire application
required_packages <- c(
  "shiny", "DT", "dplyr", "ggplot2", "tidyr", "vegan", "viridis",
  "RColorBrewer", "rstatix", "car", "readr", "adegenet", "FactoMineR", "factoextra",
  "shinyjs", "colourpicker", "forcats", "purrr", "scales", "PCAtest",
  "openxlsx", "shinyWidgets", "ggthemes", "broom", "tibble",
  "htmltools", "stringr", "ggpubr", "ggrepel", "patchwork", "mclust", "conflicted", 
  "Boruta", "ggridges"
)

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

# Resolve conflicts using conflicted package
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer("select", "dplyr")
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflicts_prefer(dplyr::matches)
  conflicted::conflict_prefer("rename", "dplyr")
  conflicted::conflict_prefer("em", "shiny")
  conflicted::conflict_prefer("count", "dplyr")
  conflicted::conflict_prefer("map", "purrr")
  conflicted::conflict_prefer("renderDataTable", "DT")
  conflicted::conflict_prefer("colourInput", "colourpicker")
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

