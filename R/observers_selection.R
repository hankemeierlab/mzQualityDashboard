#' @title Observers for Selection
#' @description
#' @param input Shiny input object
#' @param exp Reactive expression for the experiment
#' @param compoundDf Reactive expression for the compound data frame
#' @importFrom shiny debounce observeEvent reactive req
#' @noRd
.observeAliquotTableSelection <- function(input, exp, compoundDf) {

    aliquotSelection <- debounce(
        reactive(input$aliquots_rows_selected),
        500
    )

    observeEvent(aliquotSelection(), ignoreInit = TRUE, {
        experiment <- isolate(exp())
        req(isValidExperiment(experiment))

        ## check if the selected rows are different from what is
        ## present in the use column of the experiment.
        useVector <- rep(TRUE, ncol(experiment))
        useVector[input$aliquots_rows_selected] <- FALSE
        req(any(experiment$use != useVector))

        experiment$use <- useVector
        experiment <- doAnalysis(
                exp = experiment,
                doAll = TRUE,
                removeOutliers = TRUE,
                useWithinBatch = as.logical(input$useWithinBatch),
                backgroundPercentage = input$backgroundSignal,
                qcPercentage = input$qcPercentage,
                nonReportableRSD = input$nonReportableRSD,
                effectNaAsZero = input$effectNaAsZero
            )

        exp(experiment)
        compoundDf(.createCompoundSelectionTable(experiment))
    })
}

.observeAliquotTableUpdate <- function(exp, aliquotDf, compoundDf){

    observeEvent(aliquotDf(), {
        # Require that the experiment is available
        experiment <- isolate(exp())
        req(isValidExperiment(experiment))
        df <- .createCompoundSelectionTable(experiment)
        compoundDf(df)
    })
}

.oberserveInternalStandardUpdate <- function(
        input, exp, internalStandards, compoundDf
) {
    observeEvent(internalStandards(), ignoreInit = TRUE, {
        x <- exp()
        req(isValidExperiment(x))
        df <- .createCompoundSelectionTable(x)
        compoundDf(df)
    })
}

#' @title Observe the selection of the internal standards
#' @description
#' @param input Shiny input object
#' @param exp Reactive expression for the experiment
#' @importFrom shiny observeEvent req
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment rowData<-
#' @noRd
.observeCompoundTableSelection <- function(input, exp) {
    observeEvent(input$compounds_rows_selected, ignoreInit = TRUE, {
        x <- isolate(exp())
        req(isValidExperiment(x))

        useVector <- rep(TRUE, nrow(x))
        useVector[input$compounds_rows_selected] <- FALSE
        rowData(x)$use <- useVector
        exp(x)
    })
}

#' @importFrom mzQuality isValidExperiment
#' @noRd
.observeQcSelection <- function(input, exp, aliquotDf){
    observeEvent(input$qc_change, ignoreInit = TRUE, {
        req(isValidExperiment(exp()))

        x <- exp()
        qcType <- input$qc_change
        if (qcType != metadata(x)$QC) {
            x <- .updateExperiment(input, x, qcType)
            aliquotDf(.createAliquotSelectionTable(x))
            exp(x)
        }
    })
}
