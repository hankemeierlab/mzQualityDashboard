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
