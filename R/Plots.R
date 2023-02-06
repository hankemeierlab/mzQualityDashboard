#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderAliquotPlot <- function(input, exp){
    req(!is.null(exp))

    p <- aliquotPlot(
        exp = exp[, exp$Type %in% input$sample_filtered],
        batch = input$sample_batch,
        assay = input$sample_assay
    )

    toPlotly(p)
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderCompoundPlot <- function(input, exp){
    req(!is.null(exp))

    exp <- exp[, exp$Type %in% input$compound_filtered &
                   exp$Batch %in% input$compound_batch]

    p <- compoundPlot(
        exp = exp,
        assay = input$compound_assay,
        compound = input$compound_metabolite,
        guides = input$compound_lines,
        useISTD = "ISTD" %in% input$compound_filtered
    )

    toPlotly(p)
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderViolinPlot <- function(input, exp){
    req(!is.null(exp))
    exp <- exp[, exp$Type == input$qc_type &
                   exp$Batch == input$qc_batch]

    p <- violinPlot(
        exp = exp,
        assay = input$qc_assay
    )

    toPlotly(p, dynamicTicks = FALSE)
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @param confidence
#' @importFrom shiny req
renderPcaPlot <- function(input, exp, confidence = 0.95){
    req(!is.null(exp))


    batches <- input$pca_batch
    if (batches == "All") {
        batches <- unique(exp$Batch)
    }

    exp <- exp[, exp$Type %in% input$pca_filtered &
                   exp$Batch %in% batches]

    p <- pcaPlot(
        exp = exp,
        assay = input$pca_assay,
        componentX = input$PCA_X,
        componentY = input$PCA_Y
    )

    # Add 95% Confidence Interval, relocate to mzQuality package
    if (as.logical(input$pca_confidence)) {
        p <- p + stat_ellipse(level = confidence)
    }

    # Convert to Plotly
    toPlotly(p)
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderBatchBoxPlot <- function(input, exp){
    req(!is.null(exp))
    exp <- exp[, exp$Type == input$batch_filtered]

    p <- batchCorrectionPlot(exp)

    return(toPlotly(p))
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderHeatMapPlot <- function(input, exp){
    req(!is.null(exp))
    exp <- exp[, exp$Batch %in% input$heatmap_batch &
                 exp$Type %in% input$heatmap_type]

    p <- heatmapPlot(
        exp = exp,
        assay = input$heatmap_assay
    )

    return(toPlotly(p))
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderBatchAssayPlot <- function(input, exp){
    req(!is.null(exp))
    exp <- exp[, exp$Type == input$batchAssayType]

    p <- batchAssayPlot(
        exp = exp,
        assay = input$batchAssay,
        compound = input$batchAssayCompound
    )

    return(toPlotly(p))
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderRsdqcPlot <- function(input, exp){
    req(metadata(exp)$hasIS & !is.null(exp))
    p <- rsdqcPlot(exp)

    return(toPlotly(p))
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderCalibrationPlot <- function(input, exp){
    req(!is.null(exp) & any(c("CAL", "ACAL") %in% exp$Type))

    p <- calibrationPlot(
        exp = exp,
        compound = input$calibration_compound,
        batch = input$calibration_batch,
        assay = input$calibration_assay,
        guides = input$calibration_guides
    )

    return(toPlotly(p))
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
#' @importFrom plotly highlight
renderRsdPlot <- function(input, exp){
    req(!is.null(exp))

    p <- rsdPlot(
        exp = exp,
        assay = input$rsd_assay,
        qc = input$rsd_type_qc,
        number = input$rsd_number
    )

    p <- toPlotly(p) %>%
        plotly::highlight(
            on = "plotly_hover",
            off = "plotly_doubleclick"
        )

    return(p)
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
#' @importFrom SummarizedExperiment assayNames
#' @importFrom S4Vectors metadata
renderModelPlot <- function(input, exp){
    req("Concentration" %in% assayNames(exp) & !is.null(exp))

    exp <- exp[, exp$Batch == input$linearCalibration_batch]

    plotLinearCalibration(
        exp = exp,
        compound = input$linearCalibration_compound,
        types = input$linearCalibration_type,
        calType = metadata(exp)$concentration
    )
}
