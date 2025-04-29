#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom dplyr %>%
renderTable <- function(df, readonly = TRUE, rowHeaders = NULL,
                        preSelect = c(), scrollY = 500, selectable = FALSE,
                        editable = c()) {
    df <- as.data.frame(df)

    if (!is.null(rowHeaders)) {
        rows <- rowHeaders
    } else {
        rows <- FALSE
    }

    selection <- "none"
    if (selectable) {
        selection <- list(
            mode = "multiple",
            selected = as.vector(preSelect)
        )
    }

    if (length(editable) > 0) {
        editable <- list(
            target = "column",
            disable = list(
                columns = which(!colnames(df) %in% editable)
            )
        )
    } else {
        editable <- FALSE
    }


    table <- DT::datatable(
        style = "bootstrap4",
        data = df,
        escape = FALSE,
        selection = selection,
        rownames = rows,
        extensions = c("Scroller", "FixedColumns"),
        editable = editable,
        options = list(
            deferRender = TRUE,
            scrollY = scrollY,
            scroller = TRUE,
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1),
            columnDefs = list(list(className = "dt-left", targets = "_all"))
        )
    )



    return(table)
}

#' @title Convert ggplot to plotly
#' @description This function can be used to convert any static plot
#' made with ggplot to an interactive plot with plotly. This is useful for
#' more complex plots, where panning and zooming may be needed.
#' @param plot ggplot2 object
#' @returns Plotly object of the given ggplot
#' @importFrom plotly ggplotly layout
#' @export
#' @examples
#' # Read example dataset
#' exp <- readRDS(system.file(package = "mzQuality", "data.RDS"))
#'
#' # Perform analysis
#' exp <- doAnalysis(exp)
#'
#' # Create PCA plot
#' p <- pcaPlot(exp)
#'
#' # Make the PCA interactive
#' toPlotly(p)
toPlotly <- function(plot, dynamicTicks = TRUE) {
    p <- ggplotly(
        p = plot,
        dynamicTicks = dynamicTicks,
        tooltip = c("text")
    )

    if (dynamicTicks) {
        p <- plotly::layout(p,
            yaxis = list(
                exponentformat = "E",
                margin = list(l = 0, r = 0, t = 0, b = 0)
            )
        )
    }
    # Return Plotly
    return(p)
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @param assayName
#' @importFrom shiny req
createAssayTable <- function(exp, assayName) {
    req(!is.null(exp))
    as.data.frame(cbind(
        Compound = rownames(exp),
        round(assay(exp, assayName), 5)
    ))
}
