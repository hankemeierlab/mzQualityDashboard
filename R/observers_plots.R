#' @title Observe the inputs for the aliquot plot
#' @description
#' @importFrom shiny observe req updateSelectizeInput
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment assayNames
#' @noRd
observeAliquotPlotInputs <- function(input, exp) {

    observe({
        x <- exp()
        req(isValidExperiment(x))

        assays <- assayNames(x)
        assays <- assays[!assays %in% "ACALRange"]

        updateSelectizeInput(
            inputId = "sample_assay",
            choices = assays,
            selected = "ratio_corrected"
        )

        updateSelectizeInput(
            inputId = "sample_batch",
            choices = c("All", unique(x$batch)),
            selected = x$batch[1]
        )

        updateSelectizeInput(
            inputId = "sample_filtered",
            choices = unique(x$type),
            selected = unique(x$type)
        )
    })
}

#' @title Input observers for the compount plot
#' @description
#' @importFrom shiny observe req updateSelectInput
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment rowData
#' @noRd
observeCompoundPlotKeyInputs <- function(input, exp) {
    observeEvent(input$key_pressed, {
        if (input$sidebar == "Compounds") {
            x <- exp()
            req(isValidExperiment(x))

            allComps <- rownames(x)[rowData(x)$use]
            currentComp <- input$compound_picked
            idx <- which(allComps == currentComp)[1]

            if (input$key_pressed == "ArrowRight") {
                # Next compound
                idx <- min(length(allComps), idx + 1)
            } else if (input$key_pressed == "ArrowLeft") {
                # Previous compound
                idx <- max(1, idx - 1)
            }
            # Update the selected compound
            updateSelectInput(
                inputId = "compound_picked",
                choices = allComps,
                selected = allComps[idx]
            )
        }
    })
}

#' @title Observe the inputs for the violin plot
#' @description
#' @importFrom shiny observe req updateSelectizeInput updateSelectInput
#' @importFrom mzQuality isValidExperiment
#' @importFrom SummarizedExperiment assayNames
#' @importFrom S4Vectors metadata
observeViolinPlotInputs <- function(input, exp) {
    observe({
        x <- exp()
        req(isValidExperiment(x))

        types <- sort(unique(x$type))
        assays <- assayNames(x)
        assays <- assays[!assays %in% "ACALRange"]

        updateSelectizeInput(
            inputId = "qc_type",
            choices = types,
            selected = metadata(x)$QC
        )

        updateSelectInput(
            inputId = "qc_batch",
            choices = c("All", unique(x$batch)),
            selected = x$batch[1]
        )

        updateSelectizeInput(
            inputId = "qc_assay",
            choices = assays,
            selected = "ratio_corrected"
        )
    })
}

#' @title Observe the inputs for the concentration plot
#' @description
#' @importFrom shiny observe req updateSelectizeInput
#' @importFrom mzQuality isValidExperiment
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment assayNames rowData
#' @noRd
observeConcentrationPlotInputs <- function(input, exp) {
    observe({
        x <- exp()
        req(isValidExperiment(x))
        req("concentration" %in% assayNames(x))

        compIdx <- rowData(x)$use & rowData(x)$hasKnownConcentrations
        conc_comps <- rownames(x)[compIdx]

        updateSelectizeInput(
            inputId = "concentrationCompound",
            choices = conc_comps,
            selected = conc_comps[1]
        )

        updateSelectizeInput(
            inputId = "concentrationBatch",
            choices = c("All", unique(x$batch)),
            selected = x$batch[1]
        )

        updateSelectizeInput(
            inputId = "concentrationType",
            choices = c("All", unique(x$type)),
            selected = c(metadata(x)$QC, "SAMPLE")
        )

        updateSelectizeInput(
            inputId = "concentrationAssay",
            choices = c("All", assayNames(x)),
            selected = assayNames(x)[1]
        )
    })
}

#' @title Observe the inputs for the compound plot
#' @description
#' @importFrom shiny observe req updateSelectizeInput updateSelectInput
#' @importFrom mzQuality isValidExperiment
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment assayNames
#' @noRd
observeCompoundPlotInputs <- function(input, exp) {
    observe({
        x <- exp()
        req(isValidExperiment(x))

        comps <- rownames(x)[rowData(x)$use]
        batches <- sort(unique(x$batch))
        types <- sort(unique(x$type))
        qcType <- metadata(x)$QC
        assays <- assayNames(x)
        assays <- assays[!assays %in% "ACALRange"]

        updateSelectizeInput(
            inputId = "compound_picked",
            choices = comps,
            selected = comps[1],
            server = TRUE
        )

        updateSelectizeInput(
            inputId = "compound_batch",
            choices = c("All", batches),
            selected = batches[1]
        )

        updateSelectInput(
            inputId = "compound_types",
            choices = c(types, "ISTD"),
            selected = types
        )

        updateSelectInput(
            inputId = "compound_trends",
            choices = types,
            selected = qcType)

        updateSelectizeInput(
            inputId = "compound_assay",
            choices = assays,
            selected = "ratio_corrected"
        )
    })
}

#' @title Observe the inputs for the PCA plot
#' @description
#' @importFrom shiny observe req updateSelectizeInput updateSelectInput
#' @importFrom mzQuality isValidExperiment
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment assayNames
#' @noRd
observePcaPlotInputs <- function(input, exp) {
    observe({
        x <- exp()
        req(isValidExperiment(x))

        batches <- sort(unique(x$batch))
        types <- sort(unique(x$type))
        qcType <- metadata(x)$QC
        qcTypes <- unique(c(qcType, grep("QC", x$type, value = TRUE)))

        assays <- assayNames(x)
        assays <- assays[!assays %in% "ACALRange"]

        updateSelectizeInput(
            inputId = "pca_assay",
            choices = assays,
            selected = "ratio_corrected"
        )

        updateSelectInput(
            inputId = "pca_batch",
            choices = c("All", batches),
            selected = "All"
        )

        updateSelectInput(
            inputId = "pca_filtered",
            choices = types,
            selected = c(qcTypes, "SAMPLE")
        )
    })
}





