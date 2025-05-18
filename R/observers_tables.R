observeAssayTableInputs <- function(input, exp){
    observe({
        x <- exp()
        req(is(x, "SummarizedExperiment"))
        batches <- unique(x$batch)
        types <- sort(unique(x$type))

        assays <- assayNames(x)
        assays <- assays[!assays %in% "ACALRange"]

        updateSelectizeInput(
            inputId = "assay_batch",
            choices = c("All", batches),
            selected = batches[1]
        )

        updateSelectizeInput(
            inputId = "assay_type",
            choices = types,
            selected = types[1]
        )

        updateSelectizeInput(
            inputId = "assay_name",
            choices = assays,
            selected = "ratio_corrected"
        )
    })
}

