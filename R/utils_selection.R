createAliquotSelectionTable <- function(exp, aliquotDf) {
    stopifnot(is(exp, "SummarizedExperiment"))

    df <- colData(exp)
    df$aliquot <- rownames(df)
    columns <- c("aliquot", "type", "datetime", "batch", "use")
    df <- as.data.frame(df[, columns])
    df$datetime <- as.character(df$datetime)
    rownames(df) <- seq_len(nrow(df))
    return(df)
}

createCompoundSelectionTable <- function(exp) {
    stopifnot(is(exp, "SummarizedExperiment"))

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

updateExperiment <- function(input, exp, qcType){
    stopifnot(is(exp, "SummarizedExperiment"))

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
