homepageServer <- function(session, input, output, aliquotDf, internalStandards, exp) {
    manualTrigger <- reactiveVal(FALSE)

    compoundDf <- reactive({
        # Use the aliquot df update as a trigger to
        # update the compounds, regardless of input$aliquot_rows_selected

        aliquotDf()
        input$aliquots_rows_selected
        internalStandards()
        experiment <- isolate(exp())

        req(!is.null(experiment))


        columns <- c(
            "compound", "rsdqc", "rsdqcCorrected",
            "backgroundSignal", sprintf("%sPresence", metadata(experiment)$QC), "use"
        )
        df <- rowData(experiment)

        print(df)

        df$compound <- rownames(df)
        df <- as.data.frame(df[, columns])
        rownames(df) <- seq_len(nrow(df))

        colnames(df) <- c(
            "Compound", "RSDQC", "RSDQC Corrected",
            "Background Signal", sprintf("Found in %s", metadata(experiment)$QC), "Use"
        )
        df
    })

    output$aliquots <- DT::renderDataTable({

        # fromBookmarkState <- length(aliquots_selected) > 0
        #
        # if (fromBookmarkState) {
        #     metadata(exp)$QC <- qc_selected
        #     selected <- aliquots_selected
        # } else {
        #     metadata(exp)$QC <- input$qc_change
        #     selected <- which(!exp$use)
        # }

        # x <- exp()
        # selected <- which(!x$use)

        # x <- identifyOutliers(x)

        df <- aliquotDf()

        renderTable(
            df = df,
            preSelect = which(!df$use),
            scrollY = 600,
            selectable = TRUE
        )

        # aliquotTable(x, select = selected)
    })

    output$compounds <- DT::renderDataTable({
        df <- compoundDf()
        experiment <- isolate(exp())


        render <- renderTable(
            df = df,
            preSelect = which(!df$Use),
            scrollY = 600,
            selectable = TRUE
        ) %>%
            DT::formatPercentage(
                columns = c(
                    "Background Signal", sprintf("Found in %s", metadata(experiment)$QC)
                ),
                digits = 2, dec.mark = "."
            ) %>%
            DT::formatRound(columns = c("RSDQC", "RSDQC Corrected"), dec.mark = ".", digits = 5)

        return(render)

        # x <- exp()
        # req(!is.null(x))
        #
        # useVector <- rep(TRUE, ncol(x))
        # useVector[input$aliquots_rows_selected] <- FALSE
        #
        # x$use <- useVector
        # x <- x %>%
        #     doAnalysis(
        #         doAll = TRUE,
        #         removeOutliers = TRUE,
        #         useWithinBatch = as.logical(input$useWithinBatch),
        #         backgroundPercentage = input$backgroundSignal,
        #         qcPercentage = input$qcPercentage,
        #         nonReportableRSD = input$nonReportableRSD
        #     )
    })

    observeEvent(input$aliquots_rows_selected, ignoreInit = TRUE, {
        # req(isolate(!trigger()))
        experiment <- isolate(exp())
        req(!is.null(experiment))
        useVector <- rep(TRUE, ncol(experiment))
        useVector[input$aliquots_rows_selected] <- FALSE

        experiment$use <- useVector
        experiment <- experiment %>%
            doAnalysis(
                doAll = TRUE,
                removeOutliers = TRUE,
                useWithinBatch = as.logical(input$useWithinBatch),
                backgroundPercentage = input$backgroundSignal,
                qcPercentage = input$qcPercentage,
                nonReportableRSD = input$nonReportableRSD,
                effectNaAsZero = input$effectNaAsZero
            )

        exp(experiment)
    })

    # observeEvent(input$aliquots_rows_selected, ignoreInit = TRUE, {
    #     output$compounds <- DT::renderDataTable({
    #
    #         shinyWidgets::execute_safely({
    #             useVector <- rep(TRUE, ncol(exp))
    #
    #             fromBookmarkState <- length(aliquots_selected) > 0
    #             if (fromBookmarkState) {
    #                 useVector[aliquots_selected] <- FALSE
    #             } else {
    #                 useVector[input$aliquots_rows_selected] <- FALSE
    #             }
    #
    #             exp$use <- useVector
    #             exp <- exp %>%
    #                 doAnalysis(
    #                     doAll = TRUE,
    #                     removeOutliers = TRUE,
    #                     useWithinBatch = as.logical(input$useWithinBatch),
    #                     backgroundPercentage = input$backgroundSignal,
    #                     qcPercentage = input$qcPercentage,
    #                     nonReportableRSD = input$nonReportableRSD
    #                 )
    #
    #
    #             exp <<- exp
    #             if (fromBookmarkState) {
    #                 selected <- compounds_selected
    #             } else {
    #                 selected <- which(!rowData(exp)$use)
    #             }
    #             compoundTable(exp, select = selected)
    #
    #         }, title = "Table Viewer Failed", message = "Could not create the table")
    #
    #     })
    #
    # })

    observeEvent(input$compounds_rows_selected, ignoreInit = TRUE, {
        x <- exp()
        req(!is.null(x))


        useVector <- rep(TRUE, nrow(x))
        useVector[input$compounds_rows_selected] <- FALSE
        rowData(x)$use <- useVector
        exp(x)
    })

    #     comps <- rownames(x)[useVector]
    #     updateSelectizeInput(session, "compound_metabolite", choices = comps, server = TRUE)
    #     updateSelectizeInput(session, "batchAssayCompound", choices = comps, server = TRUE)
    #     updateSelectizeInput(session, "calibration_compound", choices = comps, server = TRUE)
    #     #updateInputs(session, exp[useVector, exp$use])
    #
    #     type <- metadata(x)$concentration
    #     if (!is.null(type)) {
    #         knowns <- assay(x[, x$type == type & x$use], "concentration")
    #
    #         choices <- rownames(knowns)[rowSums(knowns == 0) != ncol(knowns)]
    #         choices <- choices[choices %in% compounds]
    #         updateSelectizeInput(inputId = "concentrationCompound", choices = choices, selected = choices[1])
    #     }
    # })
}
