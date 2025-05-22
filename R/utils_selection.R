#' @title Construct a data.frame for the aliquot selection table
#' @description
#' @param exp
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment colData
createAliquotSelectionTable <- function(exp) {
    stopifnot(isValidExperiment(exp))

    df <- colData(exp)
    df$aliquot <- rownames(df)
    columns <- c("aliquot", "type", "datetime", "batch", "use")
    df <- as.data.frame(df[, columns])
    df$datetime <- as.character(df$datetime)
    rownames(df) <- seq_len(nrow(df))
    return(df)
}

#' @title Construct a data.frame for the compound selection table
#' @description
#' @param exp
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment rowData
createCompoundSelectionTable <- function(exp) {
    stopifnot(isValidExperiment(exp))

    qcColumn <- sprintf("%sPresence", metadata(exp)$QC)
    columns <- c(
        "compound", "rsdqc", "rsdqcCorrected",
        "backgroundSignal", qcColumn, "use"
    )

    df <- rowData(exp)
    df$compound <- rownames(df)
    df <- as.data.frame(df[, columns])

    colnames(df) <- c(
        "Compound", "RSDQC", "RSDQC Corrected", "Background Signal",
        "Found in Selected QC", "Use"
    )

    rownames(df) <- seq_len(nrow(df))
    return(df)
}

#' @title Update the experiment when the qcType has changed
#' @description
#' @param input
#' @param exp
#' @param qcType
#' @importFrom mzQuality isValidExperiment identifyOutliers
#' identifyMisInjections doAnalysis
#' @importFrom dplyr %>%
#' @importFrom S4Vectors metadata<-
updateExperiment <- function(input, exp, qcType){
    stopifnot(isValidExperiment(exp))

    metadata(exp)$QC <- qcType
    exp$use <- TRUE
    exp <- identifyOutliers(exp) %>%
        identifyMisInjections() %>%
        doAnalysis(
            doAll = TRUE,
            removeOutliers = TRUE,
            useWithinBatch = as.logical(input$useWithinBatch),
            backgroundPercentage = input$backgroundSignal,
            qcPercentage = input$qcPercentage,
            nonReportableRSD = input$nonReportableRSD,
            effectNaAsZero = input$effectNaAsZero
        )

    return(exp)
}
