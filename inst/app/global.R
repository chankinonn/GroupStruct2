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


