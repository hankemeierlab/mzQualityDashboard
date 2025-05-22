#' @title Download Page for exports
#' @description
#' @details
#' @returns
#' @importFrom shiny fluidPage fluidRow column selectizeInput checkboxInput
#' actionButton downloadButton
#' @importFrom shinyjs disabled
#' @noRd
.downloadPage <- function() {
    plots <- c("Aliquot", "Compound", "PCA", "RSDQC", "QC", "Concentrations")

    fluidPage(
        .controlsBox(
            title = "Download Options",
            inputs = list(
                fluidRow(
                    column(12, selectizeInput(
                        inputId = "downloadPlotPicker",
                        label = "Choose Plots",
                        choices = plots,
                        selected = plots,
                        multiple = TRUE
                    )),
                ),
                fluidRow(
                    column(12, selectizeInput(
                        inputId = "downloadAssayPicker",
                        label = "Choose Assays",
                        choices = c(),
                        selected = c(),
                        multiple = TRUE
                    ))
                ),
                fluidRow(
                    column(12, checkboxInput(
                        inputId = "summary_report",
                        label = "Summary Report",
                        value = TRUE
                    )),
                    column(12, checkboxInput(
                        inputId = "compound_report",
                        label = "Compound Report(s)",
                        value = FALSE
                    ))
                )
            ),
            footer = fluidPage(
                fluidRow(
                    column(1, actionButton(
                        inputId = "createZip", label = "Create Reports")
                    ),
                    column(1, disabled(downloadButton("download_zip")))
                )
            )
        )
    )
}
