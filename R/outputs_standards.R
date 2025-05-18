outputCurrentStandardsTable <- function(output, exp) {
    # Current Internal Standard table
    output$IsCurrentTable <- DT::renderDataTable({
        req(is(exp(), "SummarizedExperiment"))
        currentInternalStandardTable(exp())
    })
}

outputModifiedStandardsTable <- function(input, output, exp){

    # # Modify Internal Standard table
    output$IsModifyTable <- DT::renderDataTable(server = FALSE, {

        # Build the internal standard table
        x <- isolate(exp())
        df <- internalStandardTable(
            input = input,
            exp = x,
            selected = rowData(x)$compound_is
        )

        render <- renderISTable(df, c(1, 2, 4))
        DT::formatRound(
            render,
            columns = c("Original RSDQC Corrected", "Suggested RSDQC Corrected"),
            dec.mark = ".",
            digits = 3
        )
    })
}
