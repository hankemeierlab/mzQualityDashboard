.outputLongTable <- function(input, output, exp) {
    # Combined Overall Table
    output$combined <- DT::renderDataTable({
        req(isValidExperiment(exp()))
        shinyWidgets::execute_safely(
            {
                input$compounds_rows_selected
                input$aliquots_rows_selected

                df <- expToCombined(
                    exp = exp(),
                    rowIndex = "compound",
                    colIndex = "aliquot"
                )

                df <- .combinedTable(df)
                return(df)
            },
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

#' @title Compound Details Table
#' @importFrom mzQuality isValidExperiment
#' @noRd
.outputRowData <- function(input, output, exp) {
    output$rowData <- DT::renderDataTable({
        req(isValidExperiment(exp()))

        shinyWidgets::execute_safely(
            expr = .rowDataTable(exp()[rowData(exp())$use, exp()$use]),
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

# Aliquot Details Table
#' @importFrom mzQuality isValidExperiment
.outputColData <- function(input, output, exp) {
    output$colData <- DT::renderDataTable({
        req(isValidExperiment(exp()))

        shinyWidgets::execute_safely(
            expr = .colDataTable(exp()[rowData(exp())$use, exp()$use]),
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

# Assay / Values Table
#' @importFrom mzQuality isValidExperiment
.outputAssayData <- function(input, output, exp) {
    output$assayData <- DT::renderDataTable({
        req(isValidExperiment(exp()))

        shinyWidgets::execute_safely(
            expr = .assayTable(input, exp()[rowData(exp())$use, exp()$use]),
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

