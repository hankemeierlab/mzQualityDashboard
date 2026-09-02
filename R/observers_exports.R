#' @title Observers for creating reports and zip
#' @description
#' @details
#' @importFrom shiny observeEvent req showModal removeModal
#' @importFrom shinyjs enable
#' @importFrom zip zip
#' @importFrom mzQuality isValidExperiment createReports
#' @noRd
.observeReportCreation <- function(input, exp){

    observeEvent(input$createZip, {
        req(isValidExperiment(exp()))

        showModal(.generatingModal())
        on.exit(removeModal(), add = TRUE)

        createReports(
            folder = tempdir(),
            project = "placeholder",
            exp = exp(),
            makeSummaryReport = as.logical(input$summary_report),
            makeCompoundReport = as.logical(input$compound_report),
            summaryPlots = input$downloadPlotPicker,
            assays = input$downloadAssayPicker,
            backgroundPercent = input$backgroundSignal,
            cautionRSD = input$cautionRSD,
            nonReportableRSD = input$nonReportableRSD
        )

        output <- file.path(tempdir(), "placeholder")
        zip(
            zipfile = file.path(tempdir(), "mzQuality.zip"),
            files = list.files(output),
            root = output
        )

        enable("download_zip")
    })
}

#' @title observeExportAssays
#' @description
#' @details
#' @importFrom shiny req observe updateSelectInput
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment assayNames
#' @noRd
.observeExportAssays <- function(input, exp) {

    observe({
        req(isValidExperiment(exp()))

        updateSelectInput(
            inputId = "downloadAssayPicker",
            choices = assayNames(exp()),
            selected = c("ratio", "ratio_corrected")
        )

    })
}

#' @title observeDownloadZip
#' @description
#' @details
#' @importFrom shiny downloadHandler
#' @noRd
.observeDownloadZip <- function(input, output, exp) {

    output$download_zip <- downloadHandler(
        contentType = "application/zip",
        filename = function() {
            paste0("mzQuality_", Sys.Date(), ".zip")
        },
        content = function(file) {
            file.copy(file.path(tempdir(), "mzQuality.zip"), file)
        }
    )
}
