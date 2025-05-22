#' @title Create a concentration plot in a Shiny application
#' @description
#' Sets up a plotly-based concentration plot in a Shiny application. This
#' function is designed to be called within a Shiny server function.
#' @details
#' This function renders a concentration plot using `getConcentrationPlot`. It
#' requires that the provided SummarizedExperiment object has a
#' "concentration" assay. The function uses `shinyWidgets::execute_safely` to
#' handle potential errors gracefully.
#' @param input The Shiny input object
#' @param output The Shiny output object
#' @param exp A reactive function that returns a SummarizedExperiment object
#'     containing concentration data
#' @importFrom plotly renderPlotly
#' @importFrom shinyWidgets execute_safely
.outputConcentrationPlot <- function(input, output, exp){
    output$concentrationPlot <- renderPlotly({
        x <- exp()
        req(isValidExperiment(x) && "concentration" %in% assayNames(x))
        execute_safely(
            expr = .getConcentrationPlot(
                exp = x[input$concentrationCompound, x$use],
                batches = input$concentrationBatch,
                plotOnCalibrationLine = input$concentrationPlotOnLine,
                types = input$concentrationType
            ),
            title = "Plot Failed",
            message = "Could not create the plot (No concentrations provided?)",
            include_error = FALSE
        )
    })
}

.outputCompoundPlot <- function(input, output, exp) {
    batches <- debounce(reactive(input$compound_batch), 500)
    types <- debounce(reactive(input$compound_types), 500)


    output$compoundPlotPreRender <- plotly::renderPlotly({

        x <- exp()
        req(isValidExperiment(x))
        shinyWidgets::execute_safely(
            {
                batches <- batches()
                types <- types()
                if ("All" %in% batches) {
                    batches <- unique(x$batch)
                }

                .getCompoundPlot(
                    exp = x[input$compound_picked, x$use],
                    assay = input$compound_assay,
                    batches = batches,
                    types = types,
                    trendsTypes = input$compound_trends,
                    columns = as.integer(input$compound_columns),
                    logTransform = as.logical(input$compound_logscale)
                )
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    })


    output$compound_plot_ui <- renderUI({
        x <- exp()
        req(isValidExperiment(x))
        b <- batches()

        if ("All" %in% b) {
            b <- unique(x$batch)
        }
        factor <- ceiling(length(b) / as.integer(input$compound_columns))

        height <- paste0(60 * factor, "vh")
        plotly::plotlyOutput("compoundPlotPreRender", height = height)
    })
}


#' @title Create an aliquot plot in a Shiny application
#' @description Creates an aliquot plot in a Shiny application
#' @details This function renders an aliquot plot using renderAliquotPlot
#' @param input The Shiny input object
#' @param output The Shiny output object
#' @param exp A reactive function that returns a SummarizedExperiment object
#' @importFrom plotly renderPlotly
#' @importFrom shinyWidgets execute_safely
#' @importFrom shiny debounce reactive req renderUI
#' @importFrom mzQuality aliquotPlot isValidExperiment
#' @noRd
.outputSamplePlot <- function(session, input, output, exp) {
    batches <- debounce(reactive(input$sample_batch), 500)
    types <- debounce(reactive(input$sample_filtered), 500)

    output$samplePlotPreRender <- plotly::renderPlotly({
        x <- exp()
        req(isValidExperiment(x))
        shinyWidgets::execute_safely({
                x <- x[rowData(x)$use, x$use]
                batches <- batches()
                types <- types()

                if ("All" %in% batches) {
                    batches <- unique(x$batch)
                }

                plotList <- lapply(batches, function(batchLabel) {
                    aliquotPlot(
                        exp = x,
                        batches = batchLabel,
                        assay = input$sample_assay,
                        types = types
                    )
                })

                annotations <- .getPlotAnnotations(
                    labels = batches,
                    columns = 1
                )
                .stackedPlotly(plotList, 1, annotations)
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    })

    output$sample_plot_ui <- renderUI({
        x <- exp()
        batches <- batches()
        if ("All" %in% batches) {
            batches <- unique(x$batch)
        }
        factor <- length(batches)
        height <- paste0(60 * factor, "vh")
        plotly::plotlyOutput("samplePlotPreRender", height = height)
    })
}

#' @title Create a QC violin plot in a Shiny application
#' @description Creates a QC violin plot in a Shiny application
#' @details This function renders a violin plot for QC samples
#' @param input The Shiny input object
#' @param output The Shiny output object
#' @param exp A reactive function that returns a SummarizedExperiment object
#' @importFrom plotly renderPlotly
#' @importFrom shinyWidgets execute_safely
#' @importFrom mzQuality violinPlot isValidExperiment
#' @noRd
.outputBadQcPlot <- function(input, output, exp) {
    output$badqc_plot <- plotly::renderPlotly({
        x <- exp()
        req(isValidExperiment(x))
        shinyWidgets::execute_safely({
                x <- x[rowData(x)$use, x$use]
                batches <- debounce(reactive({
                    input$qc_batch
                }), 1000)()

                types <- debounce(reactive({
                    input$qc_type
                }), 1000)()


                if ("All" %in% batches) {
                    batches <- unique(x$batch)
                }

                p <- violinPlot(
                    exp = x,
                    assay = input$qc_assay,
                    batches = batches,
                    types = types,
                    addMedian = TRUE,
                    addConfidenceInterval = TRUE,
                    withinTrend = FALSE
                )

                .toPlotly(p, dynamicTicks = FALSE)
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    })
}

#' @title Create a PCA plot in a Shiny application
#' @description Creates a principal component analysis plot
#' @details This function renders a PCA plot using renderPcaPlot
#' @param input The Shiny input object
#' @param output The Shiny output object
#' @param exp A reactive function that returns a SummarizedExperiment object
#' @importFrom plotly renderPlotly
#' @importFrom shinyWidgets execute_safely
#' @importFrom mzQuality pcaPlot isValidExperiment
#' @importFrom shiny debounce reactive req
#' @noRd
.outputPcaPlot <- function(input, output, exp) {
    output$pca_plot <- plotly::renderPlotly({
        x <- exp()
        req(isValidExperiment(x))
        shinyWidgets::execute_safely(
            {
                x <- x[rowData(x)$use, x$use]
                batches <- debounce(reactive({
                    batches <- input$pca_batch
                    if ("All" %in% batches) {
                        batches <- unique(x$batch)
                    }
                    batches
                }), 1000)()

                types <- debounce(reactive({
                    input$pca_filtered
                }), 1000)()

                .getPcaPlot(x, input$pca_assay, batches, types,
                    confidence = input$pca_confidence
                )
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    })
}

#' @title Create an RSDQC heatmap in a Shiny application
#' @description Creates a heatmap of relative standard deviation for QC samples
#' @details This function renders an RSDQC heatmap plot
#' @param input The Shiny input object
#' @param output The Shiny output object
#' @param exp A reactive function that returns a SummarizedExperiment object
#' @importFrom plotly renderPlotly
#' @importFrom shinyWidgets execute_safely
#' @importFrom mzQuality rsdqcPlot isValidExperiment
#' @noRd
.outputRsdqcHeatmap <- function(input, output, exp) {
    output$ISheatmap <- plotly::renderPlotly({
        x <- exp()
        req(isValidExperiment(x) && metadata(x)$hasIS)
        execute_safely(
            expr = .toPlotly(rsdqcPlot(x)),
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    })
}

