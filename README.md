# GroupStruct2

## Latest Release

**v1.2.0** (April 2026) — Specimen ID tracking, interactive visualizations, and 3D plots.
See the [full changelog](https://github.com/chankinonn/GroupStruct2/releases/tag/v1.2.0) for details.

To update:
```r
devtools::install_github("chankinonn/GroupStruct2")
```

---

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
  "htmltools", "stringr", "ggpubr", "ggrepel", "patchwork", "mclust", "conflicted",
  "Boruta", "shinybusy", "plotly", "ggridges")

# The PCAtest package needs to be installed via GitHub
devtools::install_github("arleyc/PCAtest")
```

---

## 📋 Changelog

### v1.2.0 (April 2026)

**Specimen ID tracking**
- All three module families (meristic, morphometric, mixed) now support an optional specimen ID column (e.g. museum catalog numbers) in the first column of the input file
- Format is detected automatically based on column uniqueness — no manual selection required
- When specimen IDs are absent, sequential integers are assigned automatically so IDs are always available
- Specimen IDs appear in outlier detection reports, data previews, allometric correction tables, and downloaded files
- Allometric correction downloads include the SpecimenID column with row order preserved

**Interactive visualizations**
- Scatter, PCA, and DAPC plots across all module families now have an Interactive Mode toggle
- Hovering over points shows specimen ID, group, and axis values
- Interactive plots respond to color palette, plot theme, and font size settings
- Plots that do not support interactivity (boxplot, violin, ridge) remain static

**3D plots**
- 3D PCA tab added to the meristic and morphometric visualization modules
- 3D MFA Individuals tab added to the mixed data visualization module
- All 3D plots support hover tooltips with specimen IDs and support color palette selection

**Species delimitation**
- PCA clusters (unsupervised clustering) plot now has an Interactive Mode option with hover tooltips showing specimen ID, OTU, and cluster assignment
- Boruta, unsupervised clustering, supervised GMM, and Bayesian hypothesis testing analyses now use a modal busy spinner consistent with the PERMANOVA notification style
- PERMANOVA on MFA scores now shows a clear error notification if MFA has not been run first

**Bug fixes and consistency**
- Plot theme selector now correctly responds in interactive mode across all modules
- Fixed text aesthetic warning triggered by stat_ellipse and geom_smooth in interactive plots
- Fixed regex escape sequence error in MFA dimension extraction

### v1.1.0 (March 2026)

Visualization enhancements and PERMANOVA updates.

---

## 📦 Dependencies

GroupStruct2 depends on the following R packages. Most install automatically, but `PCAtest` must be installed manually from GitHub:

```r
# Required packages (installed automatically with GroupStruct2)
required_packages <- c("shiny", "DT", "dplyr", "ggplot2", "tidyr", "vegan", "viridis",
  "RColorBrewer", "rstatix", "car", "readr", "adegenet", "FactoMineR", "factoextra",
  "shinyjs", "colourpicker", "forcats", "purrr", "scales", "openxlsx", "shinyWidgets",
  "ggthemes", "broom", "tibble", "htmltools", "stringr", "ggpubr", "ggrepel",
  "patchwork", "mclust", "conflicted", "Boruta", "shinybusy", "plotly", "ggridges")

# Install PCAtest manually from GitHub
devtools::install_github("arleyc/PCAtest")
```
