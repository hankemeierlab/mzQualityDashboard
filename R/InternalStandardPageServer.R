internalStandardPageServer <- function(input, output, internalStandards, exp) {
    manualSelection <- reactiveVal(FALSE)

    currentIsDf <- reactive({
        # # Force change when QC type has changed
        # input$qc_change

        input$compounds_rows_selected
        input$aliquots_rows_selected

        x <- exp()

        req(metadata(x)$hasIS)
        df <- rowData(x)
        df$compound <- rownames(df)

        df <- as.data.frame(df)[, c("compound", "rsdqc", "rsdqcCorrected")]


        rownames(df) <- seq_len(nrow(df))
        colnames(df) <- c("Compound", "RSDQC", "RSDQC Corrected")
        df
    })

    # Current Internal Standard table
    output$IsCurrentTable <- DT::renderDataTable({
        renderISTable(currentIsDf()) %>%
            DT::formatRound(columns = c("RSDQC", "RSDQC Corrected"), dec.mark = ".", digits = 3)
    })

    # # Modify Internal Standard table
    output$IsModifyTable <- DT::renderDataTable(server = FALSE, {
        # req(!manualSelection())
        # Trigger for when the current IS df has changed
        input$compounds_rows_selected
        input$aliquots_rows_selected



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


    observe({
        x <- exp()
        req(!is.null(x))
        comp_is <- lapply(seq_len(nrow(x)), function(i) input[[paste0("sel", i)]])

        req(sum(lengths(comp_is)) > 0)

        check <- unlist(comp_is) != rowData(x)$compound_is

        req(any(check))


        x <- replaceInternalStandards(x, unlist(comp_is))


        new <- doAnalysis(
            exp = x[which(check), ],
            removeOutliers = TRUE,
            doAll = TRUE,
            effectNaAsZero = input$effectNaAsZero
        )

        x[which(check), ] <- new

        internalStandards(rowData(x)$compound_is)
        exp(x)
        manualSelection(TRUE)
    })
}
