# GroupStruct2

## Latest Release

**v1.3.1** (May 2026) — Additional MFA-based analyses; a centralized data input module that accommodates more flexible data formatting; improved visualizations; added inferential statistics submodule for Mixed Data.

See the [full changelog](https://github.com/chankinonn/GroupStruct2/releases/tag/v1.3.0) for details.

To update:
```r
install.packages("pak")
pak::pak("chankinonn/GroupStruct2")
```

---

**GroupStruct2** is a user-friendly Shiny application that provides statistical and visual support for species diagnosis. Although GroupStruct2 is optimized for species diagnosis, its analytical framework can be co-opted to infer group structure for any numeric or numeric + categorical dataset (biological or non-biological). GroupStruct2 combines robust statistical workflows with highly customizable, publication-ready visualizations based on the ggplot architecture, all within an intuitive graphical user interface that requires no coding experience beyond installing and launching the application.

---

## 🔧 Installation

GroupStruct2 is compatible with MacOS and Windows 10 but has not been tested on Windows 11.

You can install the stable version of this package directly from GitHub using the `pak` package:

```r
# Install remotes if needed
install.packages("pak")

# Install GroupStruct2 from GitHub
pak::pak("chankinonn/GroupStruct2")

# Load and launch GroupStruct2
library(GroupStruct2)
groupstruct2()

# In case you run into any problems installing dependencies, here is the list:
required_packages <- c("shiny", "DT", "dplyr", "ggplot2", "tidyr", "vegan", "viridis",
  "RColorBrewer", "rstatix", "car", "readr", "adegenet", "FactoMineR", "factoextra",
  "shinyjs", "colourpicker", "forcats", "purrr", "scales", "PCAtest",
  "openxlsx", "shinyWidgets", "ggthemes", "broom", "tibble",
  "htmltools", "stringr", "ggpubr", "ggrepel", "patchwork", "mclust", "conflicted",
  "Boruta", "shinybusy", "plotly", "ggridges", "ape")


```

---

## 📋 Changelog

### v1.3.1 (June 2026)

**Added Inferential Statistics to the Mix Data module**

### v1.3.0 (May 2026)

**Centralized data input module**
- Unified module for data input that is more flexible 
- Data can now include unique identifiers such as museum catalog numbers, clade labels, etc.
- Additional data columns that will not be analyzed (e.g. coordinates) can now be selected for exclusion

**DAMF (Discriminant Analysis of Multiple Factors)**
- New extended analysis available in the mixed data module
- Performs linear discriminant analysis on retained MFA dimensions, providing a supervised complement to the unsupervised MFA ordination (analogous to DAPC)
- Users select the number of MFA dimensions to retain via a variance threshold slider
- A sensitivity test available to determine the stability of results across 70–100% variance thresholds
- Results are consistent with the MFA individuals factor map, allowing direct visual comparison between unsupervised and supervised ordination spaces

**EDDA (Eigenvalue Decomposition Discriminant Analysis)**
- New extended analysis implementing Bayesian hypothesis testing via EDDA (Eigenvalue Decomposition Discriminant Analysis) on MFA scores
- Usupervised clustering analysis infers natural clusters in the data without relying on pre-determined groupings
- The optimal number of clusters is determined and ranked via BIC
- Correspondence between inferred clusters and pre-determined groupings are visualized as a heatmap in the Visualization module

**Topology-aware hypothesis testing**
- New extended analysis that constructs and compares all plausible monophyletic groupings based on a user-supplied reference tree
- Provides a phylogenetic context to hypothesis testing by only comparing plausible monophyletic partitions (based on the guide tree)

**Dispersion analysis for PERMANOVA**
- A dispersion analysis is now included to accompany the PERMANOVA
- PERMANOVA + Dispersion is automatically implemented together
- A consolidated table summarizes both analyses simultaneously
- All results can be downloaded as a compressed zip file

---

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

---

### v1.1.0 (March 2026)

Visualization enhancements and PERMANOVA updates.
