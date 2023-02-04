#' @title
#' @description
#' @details
#' @returns
#' @param experiment
#' @importFrom shiny req
#' @importFrom SummarizedExperiment colData
aliquotTable <- function(experiment){
    req(!is.null(experiment))
    df <- colData(experiment)
    df$Aliquot <- rownames(df)
    columns <- c("Aliquot", "Type", "Datetime", "Batch", "Use")
    df <- as.data.frame(df[, columns])
    df$Datetime <- as.character(df$Datetime)
    rownames(df) <- 1:nrow(df)

    render <- renderTable(
        df = df,
        preSelect = which(!df$Use)
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
compoundTable <- function(exp, select = which(!rowData(exp)$Use)){
    req(!is.null(exp))

    columns <- c("Compound", "RSDQC", "RSDQC.Corrected", "backgroundSignal", "Use")
    df <- rowData(exp)
    df$Compound <- rownames(df)

    df <- as.data.frame(df[, columns])
    rownames(df) <- 1:nrow(df)

    render <- renderTable(
        df = df,
        preSelect = select,
        editable = TRUE
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
#' @importFrom rhandsontable hot_to_r
internalStandardTable <- function(input, exp) {


    columns <- c("Compound", "RSDQC.Corrected", "Compound_is", "SuggestedIS", "RSDQC.Suggested")
    df <- rowData(exp)
    df$Compound <- rownames(df)

    df <- as.data.frame(df[, columns])
    rownames(df) <- 1:nrow(df)
    df$RSDQC.Suggested <- round(df$RSDQC.Suggested, 3)

    colnames(df) <- c("Compound", "RSDQC", "IS Used", "IS Suggested", "RSDQC Suggested")


    render <- renderTable(
        df = df
    )

    return(render)


    # if (!is.null(IS_compounds())) {
    #     is_df <- data.frame(
    #         Used.IS = rowData(experiment)$Compound_is,
    #         Suggested.IS = unlist(rowData(experiment)$SuggestedIS),
    #         Selected.IS = as.vector(IS_compounds())
    #     )
    #
    #     source_col <- sort(unique(is_df$Used.IS))
    #     is_comps <- is_df$Selected.IS
    #     exp <- replaceIS(exp[, aliquots], is_comps)
    # }

    # comps <- cbind(comps, is_df)
    # hot_table(stretchH = "all", rhandsontable(comps,
    #                                           readOnly = FALSE
    # ) %>%
    #     hot_cols(
    #         columnSorting = TRUE, fixedColumnsLeft = 1,
    #         halign = "htLeft"
    #     ) %>%
    #     hot_col("Selected.IS",
    #             type = "dropdown",
    #             source = source_col
    #     ))
}

#' @title
#' @description
#' @details
#' @returns
#' @param experiment
#' @importFrom shiny req
combinedTable <- function(experiment){
    req(!is.null(experiment))
    df <- expToCombined(experiment)

    render <- renderTable(df = df, readonly = TRUE)

    return(render)
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom SummarizedExperiment rowData
rowDataTable <- function(exp){
    req(!is.null(exp))

    data <- cbind(
        Compound = rownames(exp),
        as.data.frame(rowData(exp))
    )

    return(renderTable(data))
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom SummarizedExperiment colData
colDataTable <- function(exp){
    req(!is.null(exp))

    data <- cbind(
        Aliquot = colnames(exp),
        as.data.frame(colData(exp))
    )

    return(renderTable(data))

}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
assayTable <- function(input, exp){
    req(!is.null(exp))
    assayTable <- exp[, exp$Batch == input$assay_batch &
                        exp$Type == input$assay_type]

    table <- createAssayTable(assayTable, input$assay_name)

    return(renderTable(table))
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment assay
batchCorrectionFactorTable <- function(exp){
    req(!is.null(exp))

    exp <- exp[, exp$Type == metadata(exp)$QC]

    exp <- exp[, which(!duplicated(exp$Batch))]

    df <- assay(exp, "Ratio") / assay(exp, "Ratio Corrected")

    colnames(df) <- paste("Batch", unique(exp$Batch))

    return(renderTable(df, rowHeaders = rownames(exp)))
}

modelPropertyTable <- function(input, exp){

    # DEFUNCT FUNCTION
    req(!is.null(exp) & !is.null(metadata(exp)$Models))
    func <- input$model_function
    x <- exp()
    models <- metadata(x)$Models
    res <- do.call(cbind, lapply(names(models), function(batchName){
        batchModels <- models[[batchName]]
        df <- do.call(rbind, lapply(batchModels, function(compoundModel){
            do.call(func, list(compoundModel$Model))
        }))
        colnames(df) <- colnames(x[, x$Type == metadata(x)$concentration &
                                       x$Batch == batchName])
        df
    }))
    render_table(cbind(Compound = rownames(res), round(res, 3)))
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
backGroundEffectTable <- function(input, exp){
    req(!is.null(exp))
    exp <- exp[, exp$Batch == input$effect_batch]

    data <- backgroundSignal(exp, input$effect_type)

    return(renderTable(data))
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom SummarizedExperiment rowData
carryOverTable <- function(exp){
    req("CarryOver" %in% colnames(rowData(exp)) & !is.null(exp))
    df <- as.data.frame(rowData(exp)$CarryOver)

    return(renderTable(df, rowHeaders = rownames(exp)))
}

rtShiftTable <- function(input, exp){

    # DEFUNCT

    req("Rt" %in% assayNames(exp) & !is.null(exp))
    istd <- ifelse(input$rt_istd == "Internal Standard", "_IS", "")
    assay <- sprintf("%s%s", input$rt_type, istd)

    exp <- exp[, exp$Batch == input$rt_shift_batch &
                   exp$Type == input$rt_shift_type]

    rt_istd <- switch(input$rt_istd,
                      Compound = rownames(exp),
                      `Internal Standard` = rowData(exp)$Compound_is
    )

    data <- as.data.frame(round(assay(exp, assay), 3)) %>%
        dplyr::distinct()

    return(renderTable(data, rowHeaders = rt_istd))
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom SummarizedExperiment colData assay
#' @importFrom stats setNames
replicateTable <- function(input, exp){
    req(!is.null(exp))
    if (!"Sample" %in% colnames(colData(exp))) return(NULL)


    x <- exp[, exp$Batch == input$replicate_batch &
                 exp$Type == nput$replicate_type]

    tab <- table(x$Sample)
    replicates <- names(tab[which(tab > 1)])
    replicates <- setNames(replicates, replicates)

    df <- do.call(cbind, lapply(replicates, function(name) {
        # Replace with RSDQC function? Also saves import
        df <- assay(x[, x$Sample == name], "Ratio Corrected")
        rowSds(df, na.rm = TRUE) / rowMeans(df, na.rm = TRUE) * 100
    }))

    return(renderTable(df, rowHeaders = rownames(exp)))
}
