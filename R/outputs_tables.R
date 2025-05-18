outputLongTable <- function(input, output, exp) {
    # Combined Overall Table
    output$combined <- DT::renderDataTable({
        req(is(exp, "SummarizedExperiment"))
        shinyWidgets::execute_safely(
            {
                input$compounds_rows_selected
                input$aliquots_rows_selected

                df <- expToCombined(
                    exp = exp(),
                    rowIndex = "compound",
                    colIndex = "aliquot"
                )

                df <- combinedTable(df)
                return(df)
            },
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

# Compound Details Table
outputRowData <- function(input, output, exp) {
    output$rowData <- DT::renderDataTable({
        req(is(exp(), "SummarizedExperiment"))

        shinyWidgets::execute_safely(
            expr = rowDataTable(exp()[rowData(exp())$use, exp()$use]),
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

# Aliquot Details Table
outputColData <- function(input, output, exp) {
    output$colData <- DT::renderDataTable({
        req(is(exp(), "SummarizedExperiment"))

        shinyWidgets::execute_safely(
            expr = colDataTable(exp()[rowData(exp())$use, exp()$use]),
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

# Assay / Values Table
outputAssayData <- function(input, output, exp) {
    output$assayData <- DT::renderDataTable({
        req(is(exp(), "SummarizedExperiment"))

        shinyWidgets::execute_safely(
            expr = assayTable(input, exp()[rowData(exp())$use, exp()$use]),
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    })
}

# Table of Model Effects, R2, etc.
outputModelTable <- function(input, output, exp) {
    output$model_table <- DT::renderDataTable({
        exp <- exp()
        modelTable(input, exp[rowData(exp)$use, exp$use])
    })
}

