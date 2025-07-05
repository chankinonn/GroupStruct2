# GroupStruct2

**GroupStruct2** is a user-friendly Shiny application for analyzing numerical, categorical, or mixed (numerical + categorical) datasets for species diagnosis. GroupStruct2 features integrated outlier detection, automatic testing of statistical assumptions (e.g., normality, homogeneity of variance), and adaptive selection of appropriate statistical tests (e.g., parametric vs. non-parametric) to reduce systematic bias. It also incorporates allometric body-size correction from the original GroupStruct package, widely used dimension reduction methods (PCA and DAPC), and supports joint analysis of mixed data through multiple factor analysis (MFA).


---

## 🔧 Installation

You can install the development version of this package directly from GitHub using the `remotes` package:

```r
# Install remotes if needed
install.packages("remotes")

# Install GroupStruct2 from GitHub
remotes::install_github("chankinonn/GroupStruct2")

# Run GroupStruct2
remotes::install_github("chankinonn/GroupStruct2")
library(GroupStruct2)
groupstruct2()

# In case you run into any problems installing dependencies, here is the list:
required_packages <- c("shiny", "DT", "dplyr", "ggplot2", "tidyr", "vegan", "viridis",
  "RColorBrewer", "rstatix", "car", "readr", "adegenet", "FactoMineR", "factoextra",
  "shinyjs", "colourpicker", "forcats", "purrr", "scales", "PCAtest",
  "openxlsx", "shinyWidgets", "ggthemes", "broom", "tibble",
  "htmltools", "stringr", "ggpubr", "ggrepel")

# The PCAtest package needs to be installed via github
devtools::install_github("arleyc/PCAtest")