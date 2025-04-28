#' @title Event for making a compound plot
#' @description
#' @importFrom shiny req
#' @importFrom shinyWidgets execute_safely
#' @importFrom plotly plotlyOutput
#' @importFrom mzQuality compoundPlot addTextBubble setSampleColors
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
        input$compound_filtered
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

        df <- addTextBubble(isolate(newExp()), df, type = "aliquot")
        df
    })

    colors <- reactive({
        types <- unique(plotData()$type)


        cols <- lapply(seq_along(types), function(i) {
            input[[paste0("colorpicker_", i)]]
        })

        names(cols) <- types
        setSampleColors(types, unlist(cols))
    })

    output$compound_plot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- isolate(newExp())
                exp <- exp[rowData(exp)$use, exp$use]

                data <- adjustedVisualData()

                req(!is.null(exp))

                b <- batches()
                N <- length(b)

                idx <- seq_len(N)


                plotList <- lapply(b, function(batchLabel) {
                    data <- data %>%
                        filter(.data$batch == batchLabel)

                    if (input$compound_assay == "area" & metadata(isolate(newExp()))$hasIS) {
                        data_is <- data
                        data_is$area <- data_is$area_is
                        data_is$type <- "ISTD"
                        data <- rbind(data, data_is)
                    }

                    p <- compoundPlot(data, input$compound_assay, input$compound_trends, colors())
                    p
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


    # # Compound Plot
    # output$compound_plot <- plotly::renderPlotly(
    #     shinyWidgets::execute_safely({
    #
    #         exp <- newExp()
    #         exp <- exp[rowData(exp)$use, exp$use]
    #
    #         req(!is.null(exp))
    #
    #
    #
    #         b <- batches()
    #         N <- length(b)
    #
    #         library(ggplot2)
    #
    #         idx <- seq_len(N)
    #         plotList <- lapply(idx, function(x){
    #             p <- compoundPlotNew(
    #                 exp = exp,
    #                 assay = input$compound_assay,
    #                 compound = input$compound_metabolite,
    #                 batches = as.character(x),
    #                 types = types(),
    #                 withinTrend = TRUE,
    #                 trendTypes = input$compound_trends,
    #                 doLog = input$compound_logscale,
    #                 smooth = TRUE
    #             )
    #             toPlotly(p, dynamicTicks = F)
    #         })
    #
    #
    #         cols <- as.integer(input$compound_columns)
    #         if (length(idx) == 1) {
    #             cols <- 1
    #         }
    #
    #         cols <- max(ifelse(length(idx) < cols, length(idx), cols))
    #         rows <- ceiling(N / cols)
    #
    #         yPositions <- rev(rep(seq_len(rows) / rows, each = cols))
    #
    #         annotations <- lapply(idx, function(i){
    #             fraction <- 1/cols * (i - 1) + 0.5 / cols
    #             fraction <- fraction - floor(fraction)
    #             list(x = fraction,
    #                  y = yPositions[i],
    #                  font = list(size = 14),
    #                  text = paste("Batch:", i),
    #                  showarrow = FALSE,
    #                  xref = "paper",
    #                  yref = "paper",
    #                  xanchor = "center",
    #                  yanchor = "bottom"
    #             )
    #         })
    #
    #
    #         plotly::subplot(plotList, nrows = rows, shareY = TRUE, titleY = TRUE, titleX = TRUE, margin = c(0.02, 0.02, 0, 0.15 / (rows * 0.8))) %>%
    #             plotly::layout(annotations = annotations, yaxis = list(exponentformat = "E"))
    #
    #     }, title = "Plot Failed", message = "Could not create the plot")
    #
    # )

    output$compound_plot_ui <- renderUI({
        plotly::plotlyOutput("compound_plot", height = plot_height())
    })
}
