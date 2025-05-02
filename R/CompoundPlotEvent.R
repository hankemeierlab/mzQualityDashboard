#' @title Event for making a compound plot
#' @description
#' @importFrom shiny req
#' @importFrom shinyWidgets execute_safely
#' @importFrom plotly plotlyOutput
#' @importFrom mzQuality compoundPlot
#' @importFrom dplyr filter mutate arrange
#' @importFrom stats reorder
compoundPlotEvent <- function(input, output, newExp) {
    batches <- debounce(reactive({
        b <- input$compound_batch

        if ("All" %in% b) {
            b <- unique(isolate(newExp())$batch)
        }
        b
    }), 1000)

    types <- debounce(reactive({
        input$compound_types
    }), 1000)


    plot_height <- reactive({
        b <- batches()

        if ("All" %in% b) {
            b <- unique(newExp()$batch)
        }
        N <- length(b)

        paste0(60 * ceiling(N / as.integer(input$compound_columns)), "vh")
    })

    plotData <- reactive({
        expToCombined(newExp(), "compound", "aliquot") %>%
            filter(compound %in% input$compound_metabolite)
    })


    adjustedVisualData <- reactive({
        df <- plotData()

        withinTrend <- TRUE
        df <- df %>%
            mutate(group = paste(
                .data$type,
                .data[[ifelse(withinTrend, "batch", "type")]],
                ifelse(grepl("CAL", .data$type), .data$injection, 1),
                sep = "_"
            ))

        df <- df %>%
            arrange(.data$datetime) %>%
            mutate(aliquot = reorder(.data$aliquot, .data$injection_time))

        #df <- addTextBubble(isolate(newExp()), df, type = "aliquot")
        df
    })

    output$compound_plot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- isolate(newExp())

                req(!is.null(exp))
                exp <- exp[input$compound_metabolite, exp$use]

                b <- batches()
                N <- length(b)

                idx <- seq_len(N)


                plotList <- lapply(b, function(batchLabel) {

                    compoundPlot(
                        exp, compound = 1,
                        batches = batchLabel,
                        assay = input$compound_assay,
                        trendTypes = input$compound_trends,
                        types = types(),
                        addInternalStandards = "ISTD" %in% types(),
                        logTransform = input$compound_logscale
                    )
                })

                cols <- as.integer(input$compound_columns)
                if (length(idx) == 1) {
                    cols <- 1
                }

                cols <- max(ifelse(length(idx) < cols, length(idx), cols))
                rows <- ceiling(N / cols)

                yPositions <- rev(rep(seq_len(rows) / rows, each = cols))

                annotations <- lapply(idx, function(i) {
                    fraction <- 1 / cols * (i - 1) + 0.5 / cols
                    fraction <- fraction - floor(fraction)
                    list(
                        x = fraction,
                        y = yPositions[i],
                        font = list(size = 14),
                        text = paste("Batch:", i),
                        showarrow = FALSE,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "center",
                        yanchor = "bottom"
                    )
                })


                plotly::subplot(plotList, nrows = rows, shareY = TRUE, titleY = TRUE, titleX = TRUE, margin = c(0.02, 0.02, 0, 0.15 / (rows * 0.8))) %>%
                    plotly::layout(annotations = annotations, yaxis = list(exponentformat = "E"))
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )




    output$compound_plot_ui <- renderUI({
        plotly::plotlyOutput("compound_plot", height = plot_height())
    })
}
