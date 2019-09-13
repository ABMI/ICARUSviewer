# ICARUS WINGS
Immune/Inflammatory Diseases CDM Augmentation for Research Union System (ICARUS) Web-based INfographic Service (WINGS)

# Getting Started
```r
install.packages("devtools")
devtools::install_github("cran/raster")
devtools::install_github("ohdsi/SqlRender")
devtools::install_github("ohdsi/DatabaseConnector")
devtools::install_github("ohdsi/DatabaseConnectorJars")
devtools::install_github("ohdsi/FeatureExtraction")
devtools::install_github("ohdsi/BigKnn")
devtools::install_github("ohdsi/OhdsiRTools")
devtools::install_github("ohdsi/OhdsiSharing")
devtools::install_github("ohdsi/PatientLevelPrediction")
devtools::install_github("ABMI/ICARUSviewer")

library(ICARUSviewer)

outputFolder <- 'S:/outputFolder'
options(fftempdir = 'S:/FFtemp')
Sys.setlocale(category = "LC_ALL", locale = "us")

WINGS()
```