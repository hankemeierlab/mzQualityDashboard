#' @title Create the output for the currently picked Internal Standards
#' @description
#' @param output
#' @param exp
#' @importFrom DT renderDataTable
#' @importFrom mzQuality isValidExperiment
#' @noRd
.outputCurrentStandardsTable <- function(output, exp) {
    # Current Internal Standard table
    output$IsCurrentTable <- renderDataTable({
        req(isValidExperiment(exp()))
        .currentInternalStandardTable(exp())
    })
}

#' @title Create the output for the optionally new Internal Standards
#' @description
#' @param input
#' @param output
#' @param exp
#' @importFrom shiny req isolate
#' @importFrom DT renderDataTable formatRound
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment rowData
#' @noRd
.outputModifiedStandardsTable <- function(input, output, exp){

    # # Modify Internal Standard table
    output$IsModifyTable <- renderDataTable(server = FALSE, {

        # Build the internal standard table
        x <- isolate(exp())

        req(isValidExperiment(x))

        df <- .internalStandardTable(
            input = input,
            exp = x,
            selected = rowData(x)$compound_is
        )

        colNames <- c("Original RSDQC Corrected", "Suggested RSDQC Corrected")
        render <- .renderISTable(df, c(1, 2, 4))
        render <- formatRound(
            table = render,
            columns = colNames,
            dec.mark = ".",
            digits = 3
        )

        return(render)
    })
}
