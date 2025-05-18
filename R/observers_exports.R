#' @title Observers for creating reports and zip
#' @description
#' @details
#' @importFrom shiny req
#' @importFrom methods is
#' @importFrom shinyjs enable disable
#' @importFrom zip zip
observeReportCreation <- function(input, exp){

    observeEvent(input$createZip, {
        req(is(exp(), "SummarizedExperiment"))

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
        disable("createZip")
    })
}

#' @title observeExportAssays
#' @description
#' @details
#' @importFrom shiny req updateSelectInput
#' @importFrom methods is
#' @importFrom shinyjs toggleState
#' @importFrom SummarizedExperiment assayNames
#' @noRd
observeExportAssays <- function(input, exp) {

    observe({
        req(is(exp(), "SummarizedExperiment"))

        updateSelectInput(
            inputId = "downloadAssayPicker",
            choices = assayNames(exp()),
            selected = c("ratio", "ratio_corrected")
        )

        state <- input$summary_report || input$compound_report

        toggleState("createZip", state)
        toggleState("download_zip", state)
    })
}

#' @title observeDownloadZip
#' @description
#' @details
#' @importFrom shiny downloadHandler
#' @importFrom shinyjs enable disable
#' @noRd
observeDownloadZip <- function(input, output, exp) {

    output$download_zip <- downloadHandler(
        contentType = "application/zip",
        filename = function() {
            paste0("mzQuality_", Sys.Date(), ".zip")
        },
        content = function(file) {
            disable("download_zip")
            enable("createZip")
            file.copy(file.path(tempdir(), "mzQuality.zip"), file)
        }
    )
}
