shiny.box_table <- function(title, type, width = 12, height = "80vh",
                            collapsed = FALSE) {

    shinydashboard::box(
        width = width, title = title, collapsible = TRUE, solidHeader = TRUE,
        collapsed = collapsed,
        div(style = "height: 3vh;") %>%
          helper(content = type, fade = TRUE, icon = "circle-question"),
        DT::dataTableOutput(type, height = height)
    )
}

shiny.box_plot <- function(title, type, height) {
    shinydashboard::box(
        solidHeader = TRUE, width = 12,
        collapsible = TRUE, title = title,
        div(style = "height: 3vh;") %>%
            helper(content = title, fade = TRUE, icon = "circle-question"),
        plotlyOutput(type, height = height)
    )
}

#' @title Box with controls for UI
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @noRd
shiny.box_controls <- function(inputs, title = "Controls", width = NULL) {
    if (is.null(width)) width <- as.integer(12 / length(inputs))
    tags <- tagList(lapply(inputs, function(x) column(width, x)))
    shinydashboard::box(
        solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        title = title,
        fluidRow(tags)
    )
}



shiny.sidebar <- function() {
    icon <- icon("angles-right")
    dashboardSidebar(sidebarMenu(
        id = "sidebar",
        menuItem("Create project",
            tabName = "home",
            icon = icon("file-arrow-up")
        ),
        menuItem("Select Data",
            tabName = "AM",
            icon = icon("circle-check")
        ),
        menuItem("Tables",
            tabName = "reports", icon = icon("table-list"),
            menuSubItem("Combined Data", "Combined", icon = icon),
            menuSubItem("Aliquot Data", "ColData", icon = icon),
            menuSubItem("Compound Data", "RowData", icon = icon),
            menuSubItem("Assay Data", "Assays", icon = icon),
            menuSubItem("Batch Correction", "batch_correction", icon = icon),
            menuSubItem("QC Metrics", "qcTable", icon = icon),
            menuSubItem("PCA Metrics", "pca_metrics", icon = icon),
            menuSubItem("RSD replicates", "RSD_replicates", icon = icon),
            menuSubItem("Effects", "Effects", icon = icon),
            menuSubItem("Coefficient Variations", "CV_Table", icon = icon),
            menuSubItem("Concentrations", "concentrations_tab", icon = icon),
            menuSubItem("Model Info", "model_tab", icon = icon),
            menuSubItem("Carry-Over effect", "carryover_tab", icon = icon)
        ),
        menuItem("Plots",
            tabName = "plots", id = "plot_id", icon = icon("image"),
            menuSubItem("Heatmap", tabName = "Heatmap_tab", icon = icon),
            menuSubItem("Aliquot", tabName = "Aliquots_tab", icon = icon),
            menuSubItem("Compound", tabName = "Compounds", icon = icon),
            menuSubItem("Compound Per Batch", tabName = "batchAssay_tab",
                        icon = icon),
            menuSubItem("PCA Plot", tabName = "PCA", icon = icon),
            menuSubItem("Batch Correction",
                tabName = "BatchBoxplot",
                icon = icon
            ),
            menuSubItem("Volcano Plot", tabName = "volcanoPlot_tab",
                        icon = icon),
            menuSubItem("RSDQCs", tabName = "Correlation_heatmap", icon = icon),
            menuSubItem("RSD Plot", tabName = "rsdPlot_tab", icon = icon),
            menuSubItem("QC Plot", tabName = "QCViolins", icon = icon),
            menuSubItem("Calibration Plot",
                tabName = "Calibrations",
                icon = icon
            ),
            menuSubItem("Calibration Model",
                tabName = "CalibrationModel",
                icon = icon
            ),
            menuSubItem("Coefficient Variation",
                tabName = "CV_Coefficients",
                icon = icon
            )
        ),
        menuItem("Download",
            tabName = "download",
            icon = icon("file-arrow-down")
        )
    ))
}

#' @title Shiny homepage
#' @importFrom utils packageVersion
shiny.homepage <- function() {
    version <- as.character(packageVersion("mzQuality"))
    versionType <- switch(stringr::str_count(version, "\\."),
                          "2" = "Stable",
                          "3" = "Nightly",
                          "4" = "Development"
    )
    fluidPage(
        fluidRow(
            column(3),
            column(
                6, div(
                    style = "text-align: center",
                    h2("Create a new mzQuality project"),
                    h4(sprintf("%s version: %s", versionType, version))
                ),
                helper(
                    content = "Project", fade = TRUE, icon = "circle-question",
                    box(
                        collapsible = TRUE, solidHeader = TRUE,
                        width = 12, title = "Basic",
                        textInput("project", "Name of the project",
                            placeholder = "mzQuality"
                        ),
                        fileInput("files", multiple = TRUE,
                            "Select your combined or batch file(s)",
                            accept = c(".txt", ".tsv")
                        ),
                        checkboxInput("useExamples", "Use example dataset",
                            value = FALSE
                        ),
                        checkboxInput(
                            "filterISTD",
                            value = TRUE,
                            "Remove Internal Standards (ISTDs) in Compounds"
                        ),
                        checkboxInput("filterSST", "Remove SSTs from Aliquots",
                            value = TRUE
                        ),
                        checkboxInput("showOutliers",
                            value = TRUE,
                            "Show popup of removed Aliquots and Compounds"
                        )
                    )
                )
            ),
            column(3)
        ),
        fluidRow(
            column(3),
            column(6, helper(
                content = "Additional", fade = TRUE, icon = "circle-question",
                box(
                    solidHeader = TRUE,
                    width = 12, title = "Advanced",
                    collapsible = TRUE, collapsed = TRUE,
                    checkboxInput(
                        inputId = "filterCalCompounds", value = FALSE,
                        label = "Use only compounds with known concentrations"
                    ),
                    fileInput("calFile", "Calibration file",
                        multiple = FALSE, accept = c(".txt", ".tsv")
                    ),
                    div(style = "margin-top: 1em;",
                        h4("Combined file options")),
                    textInput("colIndex", "Aliquot column",
                        placeholder = "aliquot"),
                    textInput("rowIndex", "Compound column",
                        placeholder = "compound"),
                    textInput("areaIndex", "Peak area column",
                        placeholder = "area"),
                    textInput("areaIsIndex", placeholder = "area_is",
                        "Peak area column of Internal Standard"
                    )
                )
            )),
            column(3)
        ),
        fluidRow(
            column(4),
            column(2, actionButton("submit", "Submit",
                width = "100%", style = "margin: 2%;"
            )),
            column(2, actionButton("refresh", "Clear",
                width = "100%", style = "margin: 2%;"
            )),
            column(4)
        )
    )
}

#' @title UI for shiny app
#' @importFrom plotly plotlyOutput
#' @importFrom shiny h2 tags textInput selectizeInput actionButton includeCSS
#' fluidPage tabsetPanel tabPanel column selectInput p downloadButton icon
#' fileInput fluidRow h4 sliderInput tagList uiOutput checkboxInput
#' radioButtons
#' @importFrom shinydashboard dashboardHeader menuItem menuSubItem sidebarMenu
#' dashboardBody tabItems tabItem dashboardPage dashboardSidebar box
#' @importFrom waiter useWaiter autoWaiter spin_loaders
#' @importFrom shinyhelper helper
#' @importFrom rhandsontable rHandsontableOutput
#' @noRd
shiny.ui <- function() {
    header <- dashboardHeader(title = "mzQuality")
    dashboardPage(
        title = "mzQuality", header = header, sidebar = shiny.sidebar(),
        body = dashboardBody(

            theme = bslib::bs_theme(version = 5, bootswatch = "slate"),
            tags$head(tags$style(HTML("
      .table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: rgba(200, 0, 0, 0.6) !important;
        }
      "))),

            useWaiter(),
            autoWaiter(
                html = div(spin_loaders(8, color = "black"),
                    h4("Loading..."),
                    style = "position: absolute; top: 30vh; right: 10vh;"
                ),
                color = transparent(0.7),
                fadeout = TRUE
            ),
            includeCSS(system.file("markup.css", package = "mzQuality")),
            tabItems(
                tabItem(
                    tabName = "home",
                    shiny.homepage()
                ),
                tabItem(
                    tabName = "AM",
                    shiny.box_controls(title = "QC Correction", list(
                        selectizeInput("qc_change", choices = c(),
                        "Select sample type for correction")
                        )
                    ),
                    shiny.box_table("Aliquots",
                        type = "aliquots", width = 12,
                        height = "50vh"
                    ),
                    shiny.box_table("Metabolites",
                        type = "compounds", width = 12,
                        height = "50vh"
                    ),
                    shiny.box_controls(title = "Internal Standards", list(
                        radioButtons("SelectIS", "Use IS",
                                     choices = c("Used", "Suggested"),
                                     selected = "Used", inline = TRUE
                        )
                    )),
                    shiny.box_table(
                        title = "Internal Standards",
                        type = "IStable",
                        width = 12,
                        height = "40vh"
                    )
                ),
                tabItem("Combined", shiny.box_table("Data", "combined")),
                tabItem("ColData", shiny.box_table("Data", "colData")),
                tabItem("RowData", shiny.box_table("Data", "rowData")),
                tabItem(
                    "Assays",
                    shiny.box_controls(list(
                        selectizeInput("assay_name",
                            label = "Assay",
                            choices = c()
                        ),
                        selectizeInput("assay_type",
                            label = "Aliquot type",
                            choices = c()
                        ),
                        selectizeInput("assay_batch", label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_table("Assay", "assay", height = "65vh"),
                ),
                tabItem(
                    "batch_correction",
                    shiny.box_table("Correction Factor", "batch_correction_table",
                        height = "80vh"
                    )
                ),
                tabItem(
                    "pca_metrics",
                    shiny.box_controls(list(
                        selectInput("pcaMetricsType", label = "Type",
                                    multiple = TRUE, choices = c()),
                        selectizeInput("pcaMetricsBatch", label = "Batch",
                                       choices = c())
                    )),
                    shiny.box_table("PCA Metrics", "pcaMetricsTable",
                                    height = "25vh")
                ),
                tabItem(
                    "qcTable",
                    shiny.box_controls(list(
                        selectizeInput("qc_table_type",
                            label = "QC type",
                            choices = c()
                        ),
                        selectizeInput("qc_corrected",
                            label = "Corrected",
                            choices = c("Raw", "Corrected")
                        )
                    )),
                    shiny.box_table("QC Metrics", "qc_table", height = "65vh"),
                ),
                tabItem(
                    "RT_shift",
                    shiny.box_controls(list(
                        selectizeInput("rt_type",
                            label = "Type",
                            choices = c("rt", "rt shift")
                        ),
                        selectizeInput("rt_istd",
                            label = "What",
                            choices = c("Compound", "Internal Standard")
                        ),
                        selectizeInput("rt_shift_type",
                            label = "Aliquot type",
                            choices = c()
                        ),
                        selectizeInput("rt_shift_batch",
                            label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_table("Rentention Time", "shift", height = "65vh")
                ),
                tabItem(
                    "CV_Table",
                    shiny.box_controls(list(
                        selectizeInput("cv_type",
                            label = "Aliquot type",
                            choices = c()
                        ),
                        selectizeInput("cv_corrected",
                            label = "Corrected",
                            choices = c("Raw", "Corrected")
                        )
                    )),
                    shiny.box_table("Coefficients of Variation", "coefficients",
                        height = "65vh"
                    )
                ),
                tabItem(
                    "Effects",
                    shiny.box_controls(list(
                        selectizeInput("effect_type",
                            label = "Aliquot type",
                            choices = c("BLANK", "PROC")
                        ),
                        selectizeInput("effect_batch", label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_table("Effect", "effect_table", height = "65vh")
                ),
                tabItem("Met_Info", shiny.box_table("Calibration", "met_info")),
                tabItem(
                    "RSD_replicates",
                    shiny.box_controls(list(
                        selectizeInput("replicate_type",
                            label = "Aliquot type",
                            choices = c()
                        ),
                        selectizeInput("replicate_batch",
                            label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_table("Relative Standard Deviation (RSD) of Replicates",
                        "replicates",
                        height = "65vh"
                    )
                ),
                tabItem(
                    tabName = "concentrations_tab",
                    shiny.box_controls(list(
                        selectizeInput("concentration_type",
                            label = "Aliquot type",
                            choices = c()
                        ),
                        selectizeInput("concentration_batch",
                            label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_table("Modelled concentrations", "concentrations",
                        height = "65vh"
                    )
                ),
                tabItem(
                  tabName = "model_tab",
                  shiny.box_controls(list(
                    selectizeInput("model_function", label = "Function",
                                   "choices" = c("residuals", "fitted.values", "effects"))
                  )),
                  shiny.box_table("Model Info", "model_table", height = "65vh")
                ),
                tabItem(
                    tabName = "carryover_tab",
                    shiny.box_table("Carry-Over effect", "carryOverTable",
                        height = "65vh")
                ),
                tabItem(
                    tabName = "rsdPlot_tab",
                    shiny.box_controls(list(
                        selectizeInput("rsd_type_qc",
                            label = "QC Type",
                            choices = c("SQC", "LQC")
                        ),
                        selectizeInput("rsd_assay", label = "Assay",
                            choices = c()
                        ),
                        sliderInput("rsd_number",
                            label = "Show",
                            min = 1, max = 1, value = 1
                        )
                    )),
                    shiny.box_plot("RSD Plot", "rsd_plot", "65vh")
                ),
                tabItem(
                    tabName = "Heatmap_tab",
                    shiny.box_controls(list(
                        selectizeInput("heatmap_assay", label = "Assay",
                            choices = c()
                        ),
                        selectInput("heatmap_type",
                            label = "Type", choices = c(),
                            multiple = TRUE
                        ),
                        selectizeInput("heatmap_batch", label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_plot("Heatmap", "heatmap", "65vh")
                ),
                tabItem(
                    tabName = "Aliquots_tab",
                    shiny.box_controls(list(
                        selectizeInput("sample_assay", label = "Assay",
                            choices = c()
                        ),
                        selectInput("sample_filtered",
                            label = "Type", choices = c(),
                            multiple = TRUE
                        ),
                        selectizeInput("sample_batch", label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_plot("Aliquot Plot", "sample_plot", "65vh")
                ),
                tabItem(
                    "Compounds",
                    shiny.box_controls(list(
                        selectizeInput("compound_metabolite",
                            label = "Compound",
                            choices = c()
                        ),
                        selectizeInput("compound_assay",
                            label = "Assay",
                            choices = c()
                        ),
                        selectInput("compound_filtered",
                            label = "Type",
                            choices = c(), multiple = TRUE
                        ),
                        selectInput("compound_batch",
                                    label = "Batch",
                                    choices = c(), multiple = TRUE
                        ),
                        selectInput("compound_lines",
                            label = "Guides",
                            choices = c(), multiple = TRUE
                        )
                    )),
                    shiny.box_plot("Compound Plot", "compound_plot", "65vh")
                ),
                tabItem(
                  "batchAssay_tab",
                  shiny.box_controls(list(
                    selectizeInput("batchAssayCompound",
                                   label = "Compound",
                                   choices = c()
                    ),
                    selectizeInput("batchAssay",
                                   label = "Assay",
                                   choices = c()
                    ),
                    selectizeInput("batchAssayType",
                                   label = "Type",
                                   choices = c())
                  )),
                  shiny.box_plot("Compound Per Batch", "batchAssayplot", "65vh")
                ),
                tabItem(
                  "volcanoPlot_tab",
                  shiny.box_controls(list(
                    selectizeInput("volcanoAssay", label = "Assay", choices = c()),
                    selectInput("volcanoType", label = "Type", choices = c(),
                                multiple = TRUE),
                    selectizeInput("volcanoBatch1", label = "Batch 1",  choices = c()),
                    selectizeInput("volcanoBatch2", label = "Batch 2", choices = c())
                  )),
                  shiny.box_plot("Volcano Plot", "volcanoPlot", "65vh")
                ),
                tabItem(
                    "PCA",
                    shiny.box_controls(list(
                        selectizeInput("pca_assay", label = "Assay",
                            choices = c()
                        ),
                        selectInput("pca_filtered",
                            label = "Type", choices = c(),
                            multiple = TRUE
                        ),
                        selectizeInput("pca_batch", label = "Batch",
                            choices = c()),
                        selectizeInput("pca_confidence",
                            label = "95% CI",
                            choices = c(TRUE, FALSE)
                        )
                    )),
                    shiny.box_plot("PCA Plot", "pca_plot", "65vh")
                ),
                tabItem(
                    "BatchBoxplot",
                    shiny.box_controls(list(
                        selectizeInput("batch_filtered",
                            label = "Type",
                            choices = c()
                        )
                    )),
                    shiny.box_plot("Batch Correction", "batch_boxplot", "65vh")
                ),
                tabItem(
                    "Correlation_heatmap",
                    shiny.box_plot(
                        "Internal Standard - Compound Corrected RSDQCs",
                        "correlation_heatmap", "80vh"
                    )
                ),
                tabItem(
                    "QCViolins",
                    shiny.box_controls(list(
                        selectizeInput("qc_assay",
                            label = "Area / Ratio",
                            choices = c()
                        ),
                        selectizeInput("qc_type",
                            label = "QC type",
                            choices = c("SQC", "LQC")
                        ),
                        selectizeInput("qc_batch", label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_plot("QC Distribution(s)", "badqc_plot", "65vh")
                ),
                tabItem(
                    "Calibrations",
                    shiny.box_controls(list(
                        selectizeInput("calibration_assay",
                            label = "Assay",
                            choices = c()
                        ),
                        selectizeInput("calibration_compound",
                            label = "Compound",
                            choices = c()
                        ),
                        selectizeInput("calibration_batch",
                            label = "Batch",
                            choices = c()
                        ),
                        selectInput("calibration_guides",
                            label = "Guides",
                            choices = c(), multiple = TRUE
                        )
                    )),
                    shiny.box_plot("Calibration Plot", "calibration_plot", "65vh")
                ),
                tabItem(
                    "CalibrationModel",
                    shiny.box_controls(list(
                        selectizeInput("linearCalibration_compound",
                            label = "Compound",
                            choices = c()
                        ),
                        selectInput("linearCalibration_type",
                                    label = "Type", multiple = TRUE,
                                    choices = c()
                        ),
                        selectizeInput("linearCalibration_batch",
                            label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_plot("Linear Calibration", "LinearCalibration", "55vh"),
                    shiny.box_table("Table of Calibration lines", "CalTable",
                                    height = "65vh", collapsed = TRUE)
                ),
                tabItem(
                    "CV_Coefficients",
                    shiny.box_controls(list(
                        selectizeInput("cv_plot_type", label = "Type",
                            choices = c()
                        ),
                        selectizeInput("cv_batch", label = "Batch",
                            choices = c()
                        )
                    )),
                    shiny.box_plot("Coefficients of Variation", "cv_plot", "65vh")
                ),
                tabItem(
                    tabName = "download",
                    fluidRow(
                        column(3),
                        column(
                            6,
                            div(
                                style = "text-align: center",
                                h2("Download Results")
                            ),
                            helper(
                                content = "Download", fade = TRUE,
                                icon = "circle-question",
                                box(
                                    width = 12, title = "Select Contents",
                                    checkboxInput("summary_report",
                                        "Summary Report", value = TRUE
                                    ),
                                    checkboxInput("compound_report",
                                        "Compound Report(s)",
                                        value = TRUE
                                    ),
                                    downloadButton("download_zip")
                                )
                            )
                        ),
                        column(3)
                    )
                )
            )
        )
    )
}
