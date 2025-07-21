# GroupStruct2

**GroupStruct2** is a user-friendly Shiny application that provides statistical and visual support for species diagnosis. Although GroupStruct2 is optimized for species diagnosis, its analytical framework can be co-opted to infer group structure for any numeric or numeric + categorical dataset (biological or non-biological). GroupStruct2 combines robust statistical workflows with highly customizable, publication-ready visualizations based on the ggplot architecture, all within an intuitive graphical user interface that requires no coding experience beyond installing and launching the application.

---

## 🔧 Installation

GroupStruct2 is compatible with MacOS and Windows 10 but has not been tested on Windows 11. 

You can install the stable version of this package directly from GitHub using the `remotes` package:

```r
# Install remotes if needed
install.packages("remotes")

# Install GroupStruct2 from GitHub
remotes::install_github("chankinonn/GroupStruct2")

# Load and launch GroupStruct2
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
