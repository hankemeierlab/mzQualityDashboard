#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderAliquotPlot <- function(input, exp) {
    req(!is.null(exp))

    batches <- debounce(reactive({
        input$sample_batch
    }), 1000)()

    types <- debounce(reactive({
        input$sample_filtered
    }), 1000)()


    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }

    p <- aliquotPlot(
        exp = exp,
        batch = batches,
        assay = input$sample_assay,
        types = types
    )

    N <- length(batches)
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
renderCompoundPlot <- function(input, exp) {
    req(!is.null(exp))

    batches <- debounce(reactive({
        input$compound_batch
    }), 1000)()

    types <- debounce(reactive({
        input$compound_types
    }), 1000)()

    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }
    N <- length(batches)

    p <- compoundPlot(
        exp = exp,
        assay = input$compound_assay,
        compound = input$compound_metabolite,
        batches = as.character(batches),
        types = types,
        withinTrend = TRUE,
        trendTypes = input$compound_trends,
        logTransform = input$compound_logscale
    )


    if (N > 1) {
        p <- facetPlot(p, ncol = 1)
    }


    toPlotly(p, dynamicTicks = N == 1)
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
renderViolinPlot <- function(input, exp) {
    req(!is.null(exp))

    batches <- debounce(reactive({
        input$qc_batch
    }), 1000)()

    types <- debounce(reactive({
        input$qc_type
    }), 1000)()


    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }

    p <- violinPlot(
        exp = exp,
        assay = input$qc_assay,
        batches = batches,
        types = types,
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
renderPcaPlot <- function(input, exp, confidence = 0.95) {
    req(!is.null(exp))

    batches <- debounce(reactive({
        input$pca_batch
    }), 1000)()

    types <- debounce(reactive({
        input$pca_filtered
    }), 1000)()


    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }

    p <- pcaPlot(
        exp = exp,
        assay = input$pca_assay,
        pc1 = 1,
        pc2 = 2,
        batches = batches,
        sampleAsBatch = TRUE,
        addConfidenceInterval = input$pca_confidence,
        types = types,
        logTransform = TRUE
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
renderBatchBoxPlot <- function(input, exp) {
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
#' @importFrom mzQuality heatmapPlot
renderHeatMapPlot <- function(input, exp) {
    req(!is.null(exp))

    batches <- input$heatmap_batch
    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }

    exp <- exp[, exp$batch %in% batches &
        exp$type %in% input$heatmap_type]

    p <- heatmapPlot(
        exp = exp,
        assay = input$heatmap_assay
    )

    return(toPlotly(p))
}

renderConcentrationPlot <- function(input, exp) {
    req(!is.null(exp) & "concentration" %in% assayNames(exp))

    batches <- input$concentrationBatch
    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }

    p <- concentrationPlot(
        exp = exp,
        assay = "ratio_corrected",
        compound = input$concentrationCompound,
        calType = "ACAL",
        batch = batches,
        plotOnCalibrationLine = input$concentrationPlotOnLine,
        types = input$concentrationType
    )

    N <- length(batches)
    if (N > 1) {
        p <- p %>%
            facetPlot(by = "batch", shareY = TRUE,
                      shareX = TRUE, ncol = round(N / 2))
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
renderBatchAssayPlot <- function(input, exp) {
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
#' @importFrom mzQuality rsdqcPlot
renderRsdqcPlot <- function(input, exp) {
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
renderRsdPlot <- function(input, exp) {
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
#' @importFrom ggplot2 ggplot geom_point scale_fill_manual scale_shape_manual
#' geom_hline ggtitle geom_line theme_minimal
renderModelPlot <- function(df, model, compound) {
    df$concentration[df$type %in% "SAMPLE"] <- 0


    lowerIntercept <- df$ratio[which(df$calno == 2)][1]
    upperIntercept <- df$ratio[which(df$calno == 6)][1]

    p <- ggplot(df, aes(x = .data$concentration, y = .data$ratio)) + #
        geom_point(aes(fill = .data$type), size = 2) + # shape = Outlier
        scale_fill_manual(values = c("red", "blue", "gray")) +
        scale_shape_manual(values = c(21, 24)) +
        geom_hline(yintercept = lowerIntercept, linetype = "dashed") +
        geom_hline(yintercept = upperIntercept, linetype = "dashed") +
        ggtitle(compound) + # , subtitle) +
        geom_line(data = fortify(model), aes(x = .fitted, y = Assay)) +
        theme_minimal()


    toPlotly(p)
}
