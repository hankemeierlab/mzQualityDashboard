#' @title Oberserver for submitting data
#' @importFrom mzQuality addConcentrations filterISTD filterSST
#' @importFrom shiny updateSelectInput observeEvent req
#' @importFrom utils read.delim
observeSubmitEvent <- function(session, input, aliquotDf, newExp){

    # Event when the submit button is clicked on the start screen
    observeEvent(input$submit, {

        localFileTest <- length(input$files$datapath) > 0
        exampleTest <- as.logical(input$useExampleData)
        addedConcentrations <- length(input$calFile) > 0

        req(localFileTest || exampleTest)

        waiter <- loadingScreen()
        waiter$show()
        if (exampleTest) {
            exp <- readRDS(system.file(package = "mzQuality", "data.RDS"))
        } else {
            combined <- submitDataEvent(session, input)
            exp <- buildExperimentEvent(session, input, combined)

            if (input$filterISTD) {
                exp <- filterISTD(exp, "STD")
            }
            if (input$filterSST) {
                exp <- filterSST(exp, "SST")
            }

            if (addedConcentrations) {
                conc <- read.delim(input$calFile$datapath, check.names = FALSE)
                exp <- addConcentrations(exp, conc)
            }
        }
        exp <- updateExperiment(input, exp, metadata(exp)$QC)

        updateSelectInput(session, "qc_change",
                          choices = unique(exp$type),
                          selected = metadata(exp)$QC
        )

        aliquotDf(createAliquotSelectionTable(exp))
        updateTabsetPanel(session, inputId = "sidebar", "selectedData")
        newExp(exp)
        waiter$hide()
    })
}
