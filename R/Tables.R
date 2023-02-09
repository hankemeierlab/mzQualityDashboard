#' @title
#' @description
#' @details
#' @returns
#' @param experiment
#' @importFrom shiny req
#' @importFrom SummarizedExperiment colData
aliquotTable <- function(experiment, select = which(!experiment$Use)){
    req(!is.null(experiment))
    df <- colData(experiment)
    df$Aliquot <- rownames(df)
    columns <- c("Aliquot", "Type", "Datetime", "Batch", "Use")
    df <- as.data.frame(df[, columns])
    df$Datetime <- as.character(df$Datetime)
    rownames(df) <- 1:nrow(df)

    render <- renderTable(
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
#' @importFrom shiny req
#' @importFrom SummarizedExperiment rowData
compoundTable <- function(exp, select = which(!rowData(exp)$Use)){
    req(!is.null(exp))

    columns <- c("Compound", "RSDQC", "RSDQC.Corrected",
                 "backgroundSignal", "qcPresence", "matrixEffectFactor", "Use")
    df <- rowData(exp)
    df$Compound <- rownames(df)

    df <- as.data.frame(df[, columns])
    rownames(df) <- 1:nrow(df)

    colnames(df) <- c("Compound", "RSDQC", "RSDQC Corrected",
                      "Background Signal", "Found in SQC", "Matrix Effect Factor", "Use")

    render <- renderTable(
        df = df,
        preSelect = select,
        scrollY = 600,
        selectable = TRUE
    ) %>%
        formatPercentage(columns = c("Background Signal", "Found in SQC",
                                     "Matrix Effect Factor"),
                         digits = 2, dec.mark = ".") %>%
        formatRound(columns = c("RSDQC",  "RSDQC Corrected"), dec.mark = ".")


    return(render)
}

currentInternalStandardTable <- function(exp){
  df <- rowData(exp)
  df$Compound <- rownames(df)
  columns <- c("Compound", "Compound_is", "RSDQC", "RSDQC.Corrected")

  df$Compound <- rownames(df)
  df <- as.data.frame(df[, columns])

  rownames(df) <- 1:nrow(df)
  colnames(df) <- c("Compound", "Current IS", "RSDQC", "RSDQC Corrected")

  renderISTable(df) %>%
    formatRound(columns = c("RSDQC", "RSDQC Corrected"), dec.mark = ".")
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom SummarizedExperiment rowData
#' @importFrom rhandsontable hot_to_r
internalStandardTable <- function(input, exp, selected) {


  df <- rowData(exp)

    if (!"Compound_is" %in% colnames(df)) return(NULL)

    columns <- c("Compound", "Compound_is", "RSDQC.Corrected", "SuggestedIS", "SuggestedRSDQC")


    df$Compound <- rownames(df)


    df <- as.data.frame(df[, columns])
    rownames(df) <- 1:nrow(df)

    choices <- sort(unique(df$Compound_is))

    for (i in 1:nrow(df)) {
        id <- paste0("sel", i)
        df$currentIS[i] <- as.character(
            div(style = "margin-bottom:-20px; cursor: pointer;",
                class = "select-input",
                selectInput(inputId = id,
                            label = NULL,
                            choices = choices,
                            selected = selected[i]
                )
            )
        )
    }



    colnames(df) <- c("Compound","Original IS", "Original RSDQC Corrected", "Suggested IS", "Suggested RSDQC Corrected", "Selected IS")
    return(
      renderISTable(df) %>%
      formatRound(columns = c("Original RSDQC Corrected", "Suggested RSDQC Corrected"), dec.mark = ".")
    )
}

renderISTable <- function(df){

  render <- DT::datatable(
    data = df,
    style = "bootstrap4",
    extensions = 'FixedHeader',
    escape = FALSE,
    selection = 'none',
    rownames = FALSE,
    options = list(
      paging = FALSE,
      fixedHeader = TRUE,
      #ordering = FALSE,
      ordering = TRUE,
      scrollY = 800,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )
  )

  return(render)

}

#' @title
#' @description
#' @details
#' @returns
#' @param experiment
#' @importFrom shiny req
combinedTable <- function(combined){
    req(nrow(combined) > 0)

    render <- renderTable(
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
rowDataTable <- function(exp){
    req(!is.null(exp))

    data <- cbind(
        Compound = rownames(exp),
        as.data.frame(rowData(exp))
    )

    return(renderTable(
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
#' @importFrom SummarizedExperiment colData
colDataTable <- function(exp){
  req(!is.null(exp))

  data <- cbind(
    Aliquot = colnames(exp),
    as.data.frame(colData(exp),)
  )

  return(renderTable(
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
assayTable <- function(input, exp){
    req(!is.null(exp))
    assayTable <- exp[, exp$Batch == input$assay_batch &
                        exp$Type == input$assay_type]

    table <- as.data.frame(cbind(
      Compound = rownames(exp),
      round(assay(assayTable, input$assay_name), 3)
    ))

    return(renderTable(
        df = table,
        scrollY = 800
    ))
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

