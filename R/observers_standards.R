#' @title Observe the inputs for selecting internal standards
#' @description
#' @importFrom shiny observe req
#' @importFrom mzQuality isValidExperiment replaceInternalStandards
#' doAnalysis
#' @importFrom SummarizedExperiment rowData
#' @noRd
observeStandardSelection <- function(input, internalStandards, exp) {

    observe({
        x <- exp()
        req(isValidExperiment(x))

        comp_is <- lapply(seq_len(nrow(x)), function(i) {
            input[[paste0("sel", i)]]
        })
        req(sum(lengths(comp_is)) > 0)

        check <- unlist(comp_is) != rowData(x)$compound_is
        req(any(check))

        x <- replaceInternalStandards(x, unlist(comp_is))

        x[which(check), ] <-  doAnalysis(
            exp = x[which(check), ],
            removeOutliers = TRUE,
            doAll = TRUE,
            useWithinBatch = input$useWithinBatch,
            effectNaAsZero = input$effectNaAsZero
        )

        internalStandards(rowData(x)$compound_is)
        exp(x)
    })
}
