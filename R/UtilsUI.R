#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
#' @importFrom rhandsontable renderRHandsontable
showOutliers <- function(exp) {
    excl.aliquots <- colnames(exp)[which(!as.logical(exp$use))]
    rsdqc <- rowData(exp)$rsdqcCorrected
    excl.compounds <- names(which(!rsdqc < 30 | is.na(rsdqc)))

    df <- data.frame(
        Aliquots = excl.aliquots,
        colData(exp[, excl.aliquots])[, seq_len(3)]
    )
    cols <- c("rsdqc", "rsdqcCorrected")
    df2 <- data.frame(
        Compounds = excl.compounds,
        rowData(exp[excl.compounds, ])[, cols]
    )

    box1 <- box(
        collapsible = FALSE,
        solidHeader = TRUE,
        title = "Excluded Aliquots",
        renderRHandsontable(render_table(df))
    )

    box2 <- box(
        collapsible = FALSE, solidHeader = TRUE,
        title = "Excluded Compounds",
        renderRHandsontable(render_table(df2))
    )

    showModal(modalDialog(
        easyClose = TRUE, size = "l",
        fade = TRUE, box1, box2
    ))
}

#' @title Update the inputs from the server
#' @param session Current Rshiny session
#' @param exp SummarizedExperiment object
#' @importFrom shiny req updateSelectizeInput updateSelectInput
#' @noRd
updateInputs <- function(session, input, exp) {
    req(!is.null(exp))
    batches <- unique(exp$batch)
    types <- sort(unique(exp$type))
    qc <- metadata(exp)$QC
    qcTypes <- unique(c(qc, grep("QC", exp$type, value = TRUE)))


    comps <- rownames(exp)[rowData(exp)$use]

    updateSelectizeInput(session, "compound_metabolite", choices = comps, server = TRUE)
    updateSelectizeInput(session, "batchAssayCompound", choices = comps, server = TRUE)
    updateSelectizeInput(session, "calibration_compound", choices = comps, server = TRUE)

    print(rowData(exp))
    conc_comps <- rownames(exp)[rowData(exp)$use & rowData(exp)$hasCalculatedConcentrations]
    updateSelectizeInput(inputId = "concentrationCompound", choices = conc_comps, selected = conc_comps[1])



    batch_inputs <- c(
        "sample_batch", "correlation_batch", "pca_batch", "compound_batch", "calibration_batch",
        "heatmap_batch", "rt_shift_batch",
        "concentration_batch", "assay_batch", "qc_batch", "modelBatch",
        "replicate_batch", "effect_batch", "cv_batch", "concentrationBatch"
    )
    for (inp in batch_inputs) {
        updateSelectizeInput(session, inp,
            choices = c("All", batches),
            selected = batches[1]
        )
    }

    type_inputs <- c(
        "correlation_type", "rt_shift_type", "cv_type", "batchAssayType",
        "replicate_type", "assay_type", "concentrationType"
    )
    for (inp in type_inputs) {
        updateSelectizeInput(session,
            inp,
            choices = types
        )
    }



    updateSelectInput(session, "concentrationType",
        choices = types,
        selected = c(metadata(exp)$QC, "SAMPLE")
    )

    updateSelectInput(session, "pcaMetricsType",
        choices = types,
        selected = c(metadata(exp)$QC, "SAMPLE")
    )



    updateSelectInput(session, "compound_filtered",
        choices = c(types, "ISTD"),
        selected = types
    )
    updateSelectInput(session, "sample_filtered",
        choices = types,
        selected = types
    )
    updateSelectInput(session, "pca_filtered",
        choices = types,
        selected = c(qcTypes, "SAMPLE")
    )
    updateSelectInput(session, "batch_filtered",
        choices = types,
        selected = types
    )
    updateSelectInput(session, "heatmap_type",
        choices = types,
        selected = types
    )
    updateSelectInput(session, "volcanoType",
        choices = types,
        selected = types
    )


    updateSelectInput(session, "compound_trends", choices = types, selected = metadata(exp)$QC)

    updateSelectizeInput(session, "qc_type", choices = types, selected = metadata(exp)$QC)

    assays <- assayNames(exp)
    assays <- assays[!assays %in% "ACALRange"]


    default <- "ratio_corrected"
    idx <- c(
        "assay_name", "concentration_assay", "compound_assay", "sample_assay",
        "pca_assay", "heatmap_assay", "qc_assay", "rsd_assay", "volcanoAssay",
        "batchAssay", "calibration_assay", "linearCalibration_assay", "downloadAssayPicker"
    )

    for (id in idx) {
        select <- isolate(input[[id]])

        if (is.character(select) && !select %in% c(default, "")) {
            updateSelectizeInput(session, id, choices = assays, selected = select)
        } else {
            updateSelectizeInput(session, id, choices = assays, selected = default)
        }
    }





    updateSelectizeInput(session, "cv_plot_type",
        choices = qcTypes,
        selected = qc
    )
    updateSelectizeInput(session, "concentration_type",
        choices = types, selected = qc
    )
    updateSelectizeInput(session, "correlation_type",
        choices = qcTypes,
        selected = qc
    )
    updateSelectizeInput(session, "replicate_type",
        choices = qcTypes,
        selected = qc
    )
    updateSelectizeInput(session, "qc_table_type",
        choices = qcTypes,
        selected = qc
    )
    updateSelectizeInput(session, "rsd_type_qc",
        choices = qcTypes,
        selected = qc
    )

    updateSliderInput(session, "rsd_number",
        value = 5,
        min = 1, max = nrow(exp), step = 1
    )

    ops <- c(
        "95% CI", "Limit of Blank", "Limit of Detection",
        "Limit of Quantification"
    )
    updateSelectInput(session, "calibration_guides", choices = ops)

    batches <- unique(exp$batch)

    updateSelectInput(session, "linearCalibration_batch",
        choices = batches, selected = batches[1]
    )
    updateSelectInput(session, "pcaMetricsBatch",
        choices = batches, selected = batches[1]
    )


    updateSelectInput(session, "volcanoBatch1",
        choices = batches[1]
    )
    updateSelectInput(session, "volcanoBatch2",
        choices = ifelse(length(batches) > 1,
            batches[2], batches[1]
        )
    )
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom waiter Waiter transparent spin_loaders
#' @importFrom shiny div h4
loadingScreen <- function() {
    Waiter$new(
        color = transparent(0),
        hide_on_error = TRUE,
        fadeout = TRUE,
        html = div(
            style = "display: flex; align-items: center; justify-content: center;",
            waiter::spin_loaders(8, color = "black", style = "display: flex; align-items: center; justify-content: center;")
        )
    )
}
