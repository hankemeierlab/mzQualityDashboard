# README

## Introduction
This is the repository for the complementary dashboard for mzQuality, a tool for assessing metabolomics datasets using Quality Control samples.  

## Getting started

### Install R

- Install R (> 4.0) from [CRAN](https://cran.r-project.org/)
- Install RStudio from [RStudio](https://posit.co/download/rstudio-desktop/)

### Install mzQuality and necessary R packages
To install mzQualityDashboard and its dependencies, install the development version using the following code: 

```r
if (!"remotes" %in% installed.packages()) {
    install.packages("remotes", dependencies = TRUE)
}

if (!"mzQuality" %in% installed.packages()) {
    remotes::install_github("hankemeierlab/mzQuality", upgrade = "always")
}

if (!"mzQualityDashboard" %in% installed.packages()) {
    remotes::install_github("hankemeierlab/mzQualityDashboard", upgrade = "always")
}

```

Once installed, run the following lines of code to start the dashboard:

```r
library(mzQualityDashboard)
openDashboard()
```