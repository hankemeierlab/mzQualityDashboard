#' @title UI for the page that creates compound plots
#' @description
#' @details
#' @importFrom shiny fluidPage selectizeInput selectInput uiOutput div
#' @importFrom shinyhelper helper
#' @importFrom shinydashboard box
#' @noRd
.compoundPlotPage <- function() {
    fluidPage(
        .controlsBox(list(
            fluidRow(
                column(4, selectizeInput("compound_picked",
                    label = "Compound",
                    choices = c()
                )),
                column(4, selectizeInput("compound_assay",
                    label = "Assay", choices = c()
                )),
                column(4, selectInput("compound_types",
                    label = "Type", choices = c(), multiple = TRUE
                ))
            ),
            fluidRow(
                column(3, selectInput("compound_logscale",
                    label = "Log-Scale Y-Axis", choices = c(TRUE, FALSE)
                )),
                column(3, selectInput("compound_batch",
                    label = "Batch", choices = c(), multiple = TRUE
                )),
                column(3, selectInput("compound_trends",
                    label = "Trendlines",
                    choices = c(), multiple = TRUE
                )),
                column(3, selectInput(
                    "compound_columns", label = "Columns",
                    choices = c(1, 2), multiple = FALSE)
                )
            )
        )),
        box(
            title = "Compound Plot", solidHeader = TRUE,
            width = NULL, collapsible = TRUE,
            helper(
                div(style = "height: 3vh;"),
                content = "Compound Plot",
                fade = TRUE,
                icon = "circle-question"
            ),
            uiOutput("compound_plot_ui")
        )
    )
}

#' @title Page that creates the PCA view
#' @description
#' @details
#' @importFrom shiny fluidPage fluidRow column selectizeInput selectInput
#' @noRd
.pcaPlotPage <- function() {
    fluidPage(
        .controlsBox(list(
            fluidRow(
                column(3, selectizeInput("pca_assay",
                    label = "Assay",
                    choices = c()
                )),
                column(3, selectInput("pca_filtered",
                    label = "Type", choices = c(),
                    multiple = TRUE
                )),
                column(3, selectInput("pca_batch",
                    label = "Batch",
                    choices = c(), multiple = TRUE
                )),
                column(3, selectizeInput("pca_confidence",
                    label = "95% CI",
                    choices = c(TRUE, FALSE)
                ))
            )
        )),
        .plotBox("PCA Plot", "pca_plot", "65vh")
    )
}

#' @title Page that shows the RSDQCs for internal standards
#' @description
#' @details
#' @importFrom shiny fluidPage
#' @noRd
.rsdqcPlotPage <- function() {
    fluidPage(
        .plotBox(
            "Internal Standard - Compound Corrected RSDQCs",
            "ISheatmap",
            "80vh"
        )
    )
}

.qcPlotPage <- function() {
    fluidPage(
        .controlsBox(list(
            fluidRow(
                column(4, selectizeInput("qc_assay",
                    label = "Area / Ratio",
                    choices = c()
                )),
                column(4, selectizeInput("qc_type",
                    label = "Type",
                    choices = c()
                )),
                column(4, selectInput("qc_batch",
                    label = "Batch", multiple = TRUE,
                    choices = c()
                ))
            )
        )),
        .plotBox("QC Distribution(s)", "badqc_plot", "65vh")
    )
}

#' @title Page that creates the concentration plot
#' @description
#' @details
#' @importFrom shiny fluidPage fluidRow column selectizeInput selectInput
#' checkboxInput
#' @noRd
.concentrationPlotPage <- function() {
    fluidPage(
        .controlsBox(list(
            fluidRow(
                column(3, selectizeInput(
                    inputId = "concentrationCompound",
                    label = "Compound",
                    choices = c()
                )),
                column(3, selectInput(
                    inputId = "concentrationType",
                    label = "Type",
                    choices = c(),
                    multiple = TRUE
                )),
                column(2, selectizeInput(
                    inputId = "concentrationAssay",
                    label = "Assay",
                    choices = c("concentration", "concentration_corrected")
                )),
                column(2, selectInput(
                    inputId = "concentrationBatch",
                    label = "Batch",
                    choices = c(),
                    multiple = TRUE
                )),
                column(2, checkboxInput(
                    "concentrationPlotOnLine",
                    "Plot concentrations on Calibration Line"
                ))
            )
        )),
        fluidRow(
            column(12, .plotBox(
                "Calibration Model Plot",
                "concentrationPlot",
                "65vh"
            ))
        )
    )
}

#' @title Page that creates the page for creating the aliquot plot
#' @description
#' @details
#' @importFrom shiny fluidPage fluidRow column selectizeInput selectInput
#' uiOutput div
#' @importFrom shinyhelper helper
#' @importFrom shinydashboard box
#' @noRd
.aliquotPlotPage <- function() {
    fluidPage(
        .controlsBox(list(
            fluidRow(
                column(4, selectizeInput("sample_assay",
                    label = "Assay",
                    choices = c()
                )),
                column(4, selectInput("sample_filtered",
                    label = "Type", choices = c(),
                    multiple = TRUE
                )),
                column(4, selectInput("sample_batch",
                    label = "Batch",
                    choices = c(), multiple = TRUE
                ))
            )
        )),
        box(
            title = "Aliquot Plot",
            solidHeader = TRUE,
            width = NULL,
            collapsible = TRUE,

            helper(
                div(style = "height: 3vh;"),
                content = "Aliquot Plot",
                fade = TRUE,
                icon = "circle-question"
            ),
            uiOutput("sample_plot_ui")
        )
    )
}
