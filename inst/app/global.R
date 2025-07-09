# global.R

# List all required packages for the entire application
required_packages <- c(
  "shiny", "DT", "dplyr", "ggplot2", "tidyr", "vegan", "viridis",
  "RColorBrewer", "rstatix", "car", "readr", "adegenet", "FactoMineR", "factoextra",
  "shinyjs", "colourpicker", "forcats", "purrr", "scales", "PCAtest",
  "openxlsx", "shinyWidgets", "ggthemes", "broom", "tibble",
  "htmltools", "stringr", "ggpubr", "ggrepel"
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

