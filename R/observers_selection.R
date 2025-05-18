observeAliquotTableSelection <- function(input, exp, compoundDf) {

    aliquotSelection <- debounce(
        reactive(input$aliquots_rows_selected),
        500
    )

    observeEvent(aliquotSelection(), ignoreInit = TRUE, {
        experiment <- isolate(exp())
        req(is(experiment, "SummarizedExperiment"))

        ## check if the selected rows are different from what is
        ## present in the use column of the experiment.
        useVector <- rep(TRUE, ncol(experiment))
        useVector[input$aliquots_rows_selected] <- FALSE
        req(any(experiment$use != useVector))

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
        compoundDf(createCompoundSelectionTable(experiment))
    })
}

observeAliquotTableUpdate <- function(exp, aliquotDf, compoundDf){

    observeEvent(aliquotDf(), {
        # Require that the experiment is available
        experiment <- isolate(exp())
        req(!is.null(experiment))
        df <- createCompoundSelectionTable(experiment)
        compoundDf(df)
    })
}

oberserveInternalStandardUpdate <- function(
        input, exp, internalStandards, compoundDf
) {
    observeEvent(internalStandards(), ignoreInit = TRUE, {
        x <- exp()
        req(is(x, "SummarizedExperiment"))
        df <- createCompoundSelectionTable(x)
        compoundDf(df)
    })
}

observeCompoundTableSelection <- function(input, exp) {
    observeEvent(input$compounds_rows_selected, ignoreInit = TRUE, {
        x <- isolate(exp())
        req(is(x, "SummarizedExperiment"))

        useVector <- rep(TRUE, nrow(x))
        useVector[input$compounds_rows_selected] <- FALSE
        rowData(x)$use <- useVector
        exp(x)
    })
}

observeQcSelection <- function(input, exp, aliquotDf){
    observeEvent(input$qc_change, ignoreInit = TRUE, {
        req(is(exp(), "SummarizedExperiment"))

        x <- exp()
        qcType <- input$qc_change
        if (qcType != metadata(x)$QC) {
            x <- updateExperiment(input, x, qcType)
            aliquotDf(createAliquotSelectionTable(x))
            exp(x)
        }
    })
}
