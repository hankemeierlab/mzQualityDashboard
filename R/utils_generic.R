#' @title Render a dataframe using Datatables
#' @description
#' @importFrom DT datatable
#' @noRd
.renderTable <- function(df, readonly = TRUE, rowHeaders = NULL,
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

    datatable(
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
}
