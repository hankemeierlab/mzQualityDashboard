# README

## Introduction
This is the repository for mzQualityDashboard, a shiny application that runs 
in R. It is built on top of the [mzQuality package](https://github.com/hankemeierlab/mzQuality), which is a tool for assessing
metabolomics datasets using Quality Control samples. The dashboard
provides a user-friendly interface for visualizing and interpreting the
results of the mzQuality analysis. See our 
[preprint](https://www.biorxiv.org/content/10.1101/2025.01.22.633547v1)
for more information.

## Getting started

### Install R & Rstudio
If you have never used R before, you will need to install R and RStudio.
You can [install R from CRAN](https://cran.r-project.org/). Select your 
operating system and follow the installation instructions for the **base**
version.

Next is Rstudio, which is the recommended editor for writing and executing 
R code. You can find Rstudio at the [RStudio website](https://posit.co/download/rstudio-desktop/).
Select and download the desktop version for your operating system. Follow
the installation instructions before proceeding to the next step.

### Install mzQualityDashboard and necessary R packages
After installing R and RStudio, you will need to install the mzQualityDashboard 
package and its dependencies. We start by opening Rstudio and creating a new
file. In the top left corner, click on **File** > **New File** > **R Script**.

In the new file, copy and paste the code below. This code will check if the
necessary packages are installed, and if not, it will install them. To run
the script in Rstudio, select all lines and click on the **Run** button in the 
top right corner of the script editor, or press **Ctrl + A** followed by 
**Ctrl + Enter** (Windows/Linux) or **Cmd + A** & **Cmd + Enter** (Mac).

```r
pkgs <- installed.packages()
if (!"remotes" %in% pkgs) {
    # Needed to retrieve development packages from github
    install.packages("remotes", type = "binary")
}

if (!"BiocManager" %in% pkgs) {
    # Needed for some mzQuality dependencies
    install.packages("BiocManager", type = "binary")
}

if (!"GenomeInfoDbData" %in% pkgs) {
    # Required by some dependencies, but not automatically installed 
    BiocManager::install("GenomeInfoDbData")
}

if (!"mzQuality" %in% pkgs) {
    # Install mzQuality
    remotes::install_github("hankemeierlab/mzQuality",type = "binary")
}

if (!"mzQualityDashboard" %in% pkgs) {
    # Install the dashboard for mzQuality 
    remotes::install_github("hankemeierlab/mzQualityDashboard", type = "binary")
}
```

### Starting mzQualityDashboard
Once the installation of mzQuality has finished, the following lines of code
can be used to start the dashboard. It will open a new window in your web browser
with the dashboard interface. Each time you close the dashboard, you will need to
run the following code in Rstudio to open it again. 

```r
library(mzQualityDashboard)
openDashboard()
```

### Using mzQualityDashboard
