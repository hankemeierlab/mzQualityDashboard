#' @title
#' @description
#' @details
#' @returns
#' @param experiment
#' @param select
#' @importFrom shiny req
#' @importFrom mzQuality isValidExperiment
#' @noRd
.aliquotTable <- function(exp, select = which(!exp$use)) {
    req(isValidExperiment(exp))
    df <- colData(exp)
    df$aliquot <- rownames(df)
    columns <- c("aliquot", "type", "datetime", "batch", "use")
    df <- as.data.frame(df[, columns])
    df$datetime <- as.character(df$datetime)
    rownames(df) <- seq_len(nrow(df))

    render <- .renderTable(
        df = df,
        preSelect = select,
        scrollY = 600,
        selectable = TRUE
    )

    return(render)
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @param select
#' @importFrom shiny req
#' @importFrom mzQuality isValidExperiment
.compoundTable <- function(exp, select = which(!rowData(exp)$use)) {
    req(isValidExperiment(exp))

    columns <- c(
        "compound", "rsdqc", "rsdqcCorrected",
        "backgroundSignal", sprintf("%sPresence", metadata(exp)$QC),
        "matrixEffectFactor", "use"
    )
    df <- rowData(exp)

    df$compound <- rownames(df)
    df <- as.data.frame(df[, columns])

    rownames(df) <- seq_len(nrow(df))
    colnames(df) <- c(
        "Compound", "RSDQC", "RSDQC Corrected",
        "Background Signal", "Found in SQC", "Matrix Effect Factor", "Use"
    )


    render <- .renderTable(
        df = df,
        preSelect = select,
        scrollY = 600,
        selectable = TRUE
    )

    render <- formatPercentage(
        table = render,
        columns = c(
            "Background Signal", "Found in SQC",
            "Matrix Effect Factor"
        ),
        digits = 2,
        dec.mark = "."
    )


    render <- formatRound(
        table = render,
        columns = c("RSDQC", "RSDQC Corrected"),
        dec.mark = ".",
        digits = 3
    )


    return(render)
}

.currentInternalStandardTable <- function(exp) {
    req(isValidExperiment(exp))

    df <- rowData(exp)
    df$compound <- rownames(df)

    df <- as.data.frame(df)[, c("compound", "rsdqc", "rsdqcCorrected")]


    rownames(df) <- seq_len(nrow(df))
    colnames(df) <- c("Compound", "RSDQC", "RSDQC Corrected")

    table <- .renderISTable(df) %>%
    DT::formatRound(
        table = table,
        columns = c("RSDQC", "RSDQC Corrected"),
        dec.mark = ".",
        digits = 3
    )
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @param selected
#' @importFrom shiny req
#' @importFrom SummarizedExperiment rowData
#' @importFrom mzQuality isValidExperiment
.internalStandardTable <- function(input, exp, selected) {
    req(isValidExperiment(exp))
    df <- rowData(exp)
    if (!"compound_is" %in% colnames(df)) {
        return(NULL)
    }

    columns <- c(
        "compound", "compound_is", "rsdqcCorrected",
        "suggestedIS", "suggestedRSDQC"
    )

    df$compound <- rownames(df)

    df <- as.data.frame(df)[, columns]
    rownames(df) <- seq_len(nrow(df))

    choices <- sort(unique(df$compound_is))

    for (i in seq_len(nrow(df))) {
        id <- paste0("sel", i)
        df$currentIS[i] <- as.character(
            div(
                style = "margin-bottom:-20px; cursor: pointer;",
                class = "select-input",
                selectInput(
                    inputId = id,
                    label = NULL,
                    choices = choices,
                    selected = selected[i]
                )
            )
        )
    }

    colnames(df) <- c(
        "Compound", "Original IS", "Original RSDQC Corrected", "Suggested IS",
        "Suggested RSDQC Corrected", "Selected IS"
    )
    return(df)
}

.renderISTable <- function(df, charLimits = "_all") {
    render <- DT::datatable(
        data = df,
        style = "bootstrap4",
        extensions = c("Scroller", "FixedColumns"),
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(
            fixedHeader = TRUE,
            ordering = TRUE,
            scrollY = 900,
            scroller = TRUE,
            columnDefs = list(list(
                targets = charLimits, render = DT::JS("renderLongText")
            ))
        ),
        callback = DT::JS("datatableCallback()")
    )
    return(render)
}

#' @title
#' @description
#' @details
#' @returns
#' @param experiment
#' @importFrom shiny req
.combinedTable <- function(combined) {
    req(nrow(combined) > 0)

    render <- .renderTable(
        df = combined,
        scrollY = 1000
    )

    return(render)
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom SummarizedExperiment rowData
#' @importFrom mzQuality cleanDataframe
.rowDataTable <- function(exp) {
    req(isValidExperiment(exp))

    # Check if the rowData is a matrix
    data <- cleanDataframe(rowData(exp), onlyNumeric = FALSE)

    data <- cbind(
        Compound = rownames(exp),
        data
    )

    return(.renderTable(
        df = data,
        scrollY = 1000
    ))
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
.colDataTable <- function(exp) {
    req(isValidExperiment(exp))

    data <- cbind(
        Aliquot = colnames(exp),
        as.data.frame(colData(exp), )
    )

    return(.renderTable(
        df = data,
        scrollY = 1000
    ))
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment assay
.assayTable <- function(input, exp) {
    req(isValidExperiment(exp))
    assayTable <- exp[, exp$batch == input$assay_batch &
        exp$type == input$assay_type]

    table <- as.data.frame(cbind(
        Compound = rownames(exp),
        round(assay(assayTable, input$assay_name), 5)
    ))

    return(.renderTable(
        df = table,
        scrollY = 800
    ))
}

