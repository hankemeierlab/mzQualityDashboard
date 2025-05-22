#' Generate a concentration plot
#'
#' @description
#' Creates a concentration plot based on a SummarizedExperiment object
#' containing concentration data.
#' @details
#' This function creates a concentration plot using the `concentrationPlot`
#' function and converts it to a plotly object. If multiple batches are
#' specified, the plot is faceted by batch.
#' @return A plotly object representing the concentration plot
#' @param exp A SummarizedExperiment object with concentration data
#' @param batches Character vector of batch names to include in the plot, or
#'     "All" to include all batches
#' @param types Character vector specifying the types of concentration data to
#'     plot
#' @param plotOnCalibrationLine Logical indicating whether to plot points on
#'     the calibration line
#' @importFrom mzQuality concentrationPlot facetPlot
#' @importFrom S4Vectors metadata
#' @noRd
.getConcentrationPlot <- function(exp, batches, types, plotOnCalibrationLine) {

    if ("All" %in% batches) {
        batches <- unique(exp$batch)
    }
    N <- length(batches)

    p <- concentrationPlot(
        exp = exp,
        assay = "ratio_corrected",
        calType = metadata(exp)$concentration,
        batch = batches,
        plotOnCalibrationLine = plotOnCalibrationLine,
        types = types,
        removeOutliers = TRUE
    )
     
    p <- facetPlot(
        p, by = "batch", shareY = TRUE,
        shareX = TRUE, ncol = ceiling(N / 2)
    )

    return(.toPlotly(p, dynamicTicks = TRUE))
}

.getPlotAnnotations <- function(labels, columns = 2){
    labels <- unique(labels)
    N <- length(labels)
    columns <- as.integer(columns)
    cols <- max(ifelse(N < columns, N, columns))
    rows <- ceiling(N / cols)
    yPositions <- rev(rep(seq_len(rows) / rows, each = cols))

    xPositions <- 1 / cols * (seq_along(labels) - 1) + 0.5 / cols
    xPositions <- xPositions - floor(xPositions)

    annotations <- lapply(seq_along(labels), function(i) {

        list(
            x = xPositions[i],
            y = yPositions[i],
            font = list(size = 14),
            text = labels[i],
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom"
        )
    })

    annotations
}

#' @title Create a compound plot
#' @description
#' @details
#' @param exp
#' @param assay
#' @param batches
#' @param types
#' @param trendsTypes
#' @param columns
#' @param logTransform
#' @importFrom mzQuality compoundPlot isValidExperiment
#' @noRd
.getCompoundPlot <- function(
        exp, assay, batches, types, trendsTypes,
        columns = 2, logTransform = TRUE
) {

    if (!isValidExperiment(exp)) return(NULL)

    batches <- unique(batches)

    plotList <- lapply(batches, function(batchLabel) {
        compoundPlot(
            exp,
            compound = 1,
            batches = batchLabel,
            assay = assay,
            trendTypes = trendsTypes,
            types = types,
            addInternalStandards = "ISTD" %in% types,
            logTransform = logTransform
        )
    })

    annotations <- .getPlotAnnotations(
        labels = batches,
        columns = columns
    )
    .stackedPlotly(plotList, columns, annotations)
}

#' @title Retrieve the PCA plot
#' @description
#' @details
#' @returns
#' @param exp
#' @param assay
#' @param batches
#' @param types
#' @param confidence
#' @importFrom mzQuality isValidExperiment pcaPlot
#' @noRd
.getPcaPlot <- function(exp, assay, batches, types, confidence = 0.95) {

    if (!isValidExperiment(exp)) return(NULL)

    p <- pcaPlot(
        exp = exp,
        assay = assay,
        pc1 = 1,
        pc2 = 2,
        batches = batches,
        sampleAsBatch = TRUE,
        addConfidenceInterval = confidence,
        types = types,
        logTransform = TRUE
    )
    # Convert to Plotly
    .toPlotly(p)
}


#' @title Convert ggplot to plotly
#' @description This function can be used to convert any static plot
#' made with ggplot to an interactive plot with plotly. This is useful for
#' more complex plots, where panning and zooming may be needed.
#' @param plot ggplot2 object
#' @param dynamicTicks Logical indicating whether to use dynamic ticks
#' @returns Plotly object of the given ggplot
#' @importFrom plotly ggplotly layout
#' @noRd
.toPlotly <- function(plot, dynamicTicks = TRUE) {
    p <- ggplotly(
        p = plot,
        dynamicTicks = dynamicTicks,
        tooltip = c("text")
    )

    if (dynamicTicks) {
        p <- plotly::layout(
            p = p,
            yaxis = list(
                exponentformat = "E",
                margin = list(l = 0, r = 0, t = 0, b = 0)
            )
        )
    }
    # Return Plotly
    return(p)
}

#' @title Create stacked plotly subplots
#' @description
#' @param plotList
#' @param columns
#' @param annotations
#' @importFrom plotly subplot layout
#' @noRd
.stackedPlotly <- function(plotList, columns, annotations) {

    N <- length(plotList)
    columns <- as.integer(columns)
    rows <- ceiling(N / max(ifelse(N < columns, N, columns)))

    plotly::subplot(
        plotList, nrows = rows, shareY = TRUE, titleY = TRUE, titleX = TRUE,
        margin = c(0.02, 0.02, 0, 0.15 / (rows * 0.8))
    ) %>%
        plotly::layout(
            annotations = annotations,
            yaxis = list(exponentformat = "E"),
            legend = "legend"
        )
}
