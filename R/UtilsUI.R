#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
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
#' @importFrom SummarizedExperiment assays<-
#' @importFrom S4Vectors metadata
#' @importFrom shiny req updateSelectizeInput updateSelectInput
#' @noRd
updateInputs <- function(session, exp) {
    req(!is.null(exp))
    batches <- c("All", unique(exp$batch))
    types <- sort(unique(exp$type))
    qc <- metadata(exp)$QC
    qcTypes <- unique(c(qc, grep("QC", exp$type, value = TRUE)))
    batch_inputs <- c(
        "sample_batch", "correlation_batch",
        "heatmap_batch", "pca_batch", "rt_shift_batch",
        "concentration_batch", "assay_batch", "qc_batch",
        "replicate_batch", "effect_batch", "cv_batch", "concentrationBatch"
    )
    for (inp in batch_inputs) {
        updateSelectizeInput(session, inp,
                             choices = batches,
                             selected = batches[2]
        )
    }

    type_inputs <- c(
        "correlation_type", "rt_shift_type", "cv_type", "batchAssayType",
        "replicate_type", "assay_type", "concentration_type"
    )
    for (inp in type_inputs) {
        updateSelectizeInput(session,
                             inp,
                             choices = types
        )
    }
    updateSelectInput(session, "pcaMetricsType", choices = types,
                      selected = c(metadata(exp)$QC, "SAMPLE"))

    updateSelectInput(session, "linearCalibration_type", choices = types,
                      selected = c(metadata(exp)$concentration, "SAMPLE"))

    updateSelectizeInput(session, "calibration_batch",
                         choices = unique(exp$batch),
                         selected = unique(exp$batch)[1]
    )

    updateSelectizeInput(session, "compound_batch",
                         choices = unique(exp$batch),
                         selected = unique(exp$batch)[1]
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


    assays <- assayNames(exp)

    updateSelectizeInput(session, "assay_name",
                         choices = assays,
                         selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "concentration_assay",
                         choices = assays, selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "compound_assay",
                         choices = assays,
                         selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "sample_assay",
                         choices = assays,
                         selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "pca_assay",
                         choices = assays,
                         selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "heatmap_assay",
                         choices = assays,
                         selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "qc_assay",
                         choices = assays,
                         selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "rsd_assay",
                         choices = assays,
                         selected = "Ratio_Corrected"
    )

    updateSelectizeInput(session, "volcanoAssay",
                         choices = assays,
                         selected = "Ratio_Corrected")

    updateSelectizeInput(session, "batchAssay",
                         choices = assays,
                         selected = "Ratio_Corrected")
    updateSelectizeInput(session, "calibration_assay",
                         choices = assays, selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "linearCalibration_assay",
                         choices = assays, selected = "Ratio_Corrected"
    )
    updateSelectizeInput(session, "downloadAssayPicker",
                         choices = assays, selected = "Ratio_Corrected"
    )



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
    updateSelectizeInput(session, "qc_type",
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
    updateSelectInput(session, "compound_lines", choices = ops)
    updateSelectInput(session, "calibration_guides", choices = ops)

    batches <- unique(exp$batch)

    updateSelectInput(session, "linearCalibration_batch",
                      choices = batches, selected = batches[1]
    )
    updateSelectInput(session, "pcaMetricsBatch",
                      choices = c("All", batches), selected = batches[1]
    )


    updateSelectInput(session, "volcanoBatch1",
                      choices = batches[1]
    )
    updateSelectInput(session, "volcanoBatch2",
                      choices = ifelse(length(batches) > 1,
                                       batches[2], batches[1])
    )
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom waiter Waiter transparent spin_loaders
#' @importFrom shiny div h4
loadingScreen <- function(){
    Waiter$new(
        color = transparent(0),
        hide_on_error = TRUE,
        fadeout = TRUE,
        html = div(
            spin_loaders(8),
            h4("Loading...",
               style = "color: white; line-height: 1vh;")#,

            # style = "display: flex;
            #      width: 6vw;
            #      justify-content: space-between;
            #      align-items: self-start;
            #      align-content: center;"
        ),
    )
}
