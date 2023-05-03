#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderAliquotPlot <- function(input, exp){
    req(!is.null(exp))

    p <- aliquotPlotNew(
        exp = exp,
        batch = input$sample_batch,
        assay = input$sample_assay,
        types = input$sample_filtered
    )

    N <- length(input$sample_batch)
    if (N > 1) {
        p <- facetPlot(p, ncol = round(N / 2))
    }

    suppressWarnings(plotly::ggplotly(p = p))
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

    library(ggplot2)
    p <- compoundPlotNew(
        exp = exp,
        assay = input$compound_assay,
        compound = input$compound_metabolite,
        batches = input$compound_batch,
        types = input$compound_filtered,
        withinTrend = TRUE,
        trendTypes = input$compound_trends,
    )
    N <- length(input$compound_batch)
    if (N > 1) {
        p <- facetPlot(p, ncol = round(N / 2))
    }

    toPlotly(p, dynamicTicks = FALSE)
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

    batches <- input$qc_batch
    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }

    p <- violinPlotNew(
        exp = exp,
        assay = input$qc_assay,
        batches = batches,
        types = input$qc_type,
        addMedian = TRUE,
        addConfidenceInterval = TRUE,
        withinTrend = FALSE
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

    p <- pcaPlotNew(
        exp = exp,
        assay = input$pca_assay,
        pc1 = 1, #input$PCA_X,
        pc2 = 2, #input$PCA_Y,
        batches = input$pca_batch,
        sampleAsBatch = TRUE,
        addConfidenceInterval = input$pca_confidence,
        types = input$pca_filtered,
        doLog = TRUE
    )
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
    exp <- exp[, exp$type == input$batch_filtered]

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
    exp <- exp[, exp$batch %in% input$heatmap_batch &
                 exp$type %in% input$heatmap_type]

    p <- mzQuality2:::heatmapPlot(
        exp = exp,
        assay = input$heatmap_assay,
        method = "interactive"
    )

    return(toPlotly(p))
}

renderConcentrationPlot <- function(input, exp){
    req(!is.null(exp) & "concentration" %in% assayNames(exp))


    p <- concentrationPlotNew(
        exp = exp,
        assay = input$concentrationAssay,
        compound = input$concentrationCompound,
        calType = "ACAL",
        batch = input$concentrationBatch,
        plotOnCalibrationLine = input$concentrationPlotOnLine,
        types = input$concentrationType
    )

    library(ggplot2)
    N <- length(input$concentrationBatch)
    if (N > 1) {
        p <- p %>%
            facetPlot(by = "batch", shareY = TRUE, shareX = TRUE, ncol = round(N / 2))
    }

    return(toPlotly(p, dynamicTicks = N == 1))
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
    exp <- exp[, exp$type == input$batchAssayType]

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
    p <- mzQuality2:::rsdqcPlot(exp, method = "interactive")
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
    req(!is.null(exp) & any(c("CAL", "ACAL") %in% exp$type))

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
renderModelPlot <- function(df, model, compound){



    #df <- df[complete.cases(df), ]
    # df$Outlier <- "normal"
    #
    # idx <- df$Type == calType
    # df$Outlier[idx] <- model$outliers[1:length(idx)]

    #ranges <- rowData(exp[compound, ])$LinearRangePerType

    #vals <- as.numeric(ranges[sprintf("Batch_%s_Type_%s", batch, types)])

    #subtitle <- sprintf("Batch: %s,  R2: %.3f", batch, r2[compound, batch])
    #ranges <- sprintf("Linear-Range: %s, Value: %.3f", types, vals)

    #ranges <- paste(ranges, collapse = "\n")

    #subtitle <- sprintf("%s\n%s", subtitle, ranges)

    df$concentration[df$type %in% 'SAMPLE'] <- 0


    lowerIntercept <- df$ratio[which(df$calno == 2)][1]
    upperIntercept <- df$ratio[which(df$calno == 6)][1]

    p <- ggplot(df, aes(x = .data$concentration, y = .data$ratio)) + #
        geom_point(aes(fill = .data$type), size = 2) + # shape = Outlier
        scale_fill_manual(values = c("red", "blue", "gray")) +
        scale_shape_manual(values = c(21, 24)) +
        ggplot2::geom_hline(yintercept = lowerIntercept, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = upperIntercept, linetype = "dashed") +
        ggtitle(compound) + #, subtitle) +
        geom_line(data = fortify(model), aes(x = .fitted, y = Assay)) +
        theme_minimal()


    toPlotly(p)

    # exp <- exp[, exp$Batch == input$linearCalibration_batch]
    #
    # plotLinearCalibration(
    #     exp = exp,
    #     compound = input$linearCalibration_compound,
    #     types = input$linearCalibration_type,
    #     calType = metadata(exp)$concentration
    # )
}
