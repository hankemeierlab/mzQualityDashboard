compoundPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(6, selectizeInput("compound_metabolite",
                                         label = "Compound",
                                         choices = c()
                )),
                column(6, selectizeInput("compound_assay",
                                         label = "Assay",
                                         choices = c()
                ))
            ),
            fluidRow(
                column(4, selectInput("compound_filtered",
                                      label = "Type",
                                      choices = c(), multiple = TRUE
                )),
                column(4, selectInput("compound_batch",
                                      label = "Batch",
                                      choices = c(), multiple = TRUE
                )),
                column(4, selectInput("compound_lines",
                                      label = "Guides",
                                      choices = c(), multiple = TRUE
                ))
            )
        )),
        shiny.box_plot("Compound Plot", "compound_plot", "65vh")
    )
}

pcaPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(6, selectizeInput("pca_assay", label = "Assay",
                                         choices = c()
                )),
                column(3, selectInput("pca_filtered",
                                      label = "Type", choices = c(),
                                      multiple = TRUE
                )),
                column(3, selectizeInput("pca_batch", label = "Batch",
                                         choices = c())
                )
            ),
            fluidRow(
                column(2, numericInput("PCA_X", "Component X-axis", value = 1, min = 1, step = 1)),
                column(2, numericInput("PCA_Y", "Component Y-axis", value = 2, min = 1, step = 1)),
                column(2, selectizeInput("pca_confidence",
                                         label = "95% CI",
                                         choices = c(TRUE, FALSE)
                ))
            )
        )),
        shiny.box_plot("PCA Plot", "pca_plot", "65vh")
    )
}

rsdqcPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(12, selectizeInput("rsdqc_type",
                           label = "QC Type",
                           choices = c()
                ))
            )
        )),
        shiny.box_plot(
            "Internal Standard - Compound Corrected RSDQCs",
            "correlation_heatmap", "80vh"
        )
    )
}

qcPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(4, selectizeInput("qc_assay",
                                         label = "Area / Ratio",
                                         choices = c()
                )),
                column(4, selectizeInput("qc_type",
                                         label = "QC type",
                                         choices = c("SQC", "LQC")
                )),
                column(4, selectizeInput("qc_batch", label = "Batch",
                                         choices = c()
                ))
            )
        )),
        shiny.box_plot("QC Distribution(s)", "badqc_plot", "65vh")
    )
}

calibrationPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(3, selectizeInput("calibration_assay",
                                         label = "Assay",
                                         choices = c()
                )),
                column(3, selectizeInput("calibration_compound",
                                         label = "Compound",
                                         choices = c()
                )),
                column(3, selectizeInput("calibration_batch",
                                         label = "Batch",
                                         choices = c()
                )),
                column(3, selectInput("calibration_guides",
                                      label = "Guides",
                                      choices = c(), multiple = TRUE
                ))
            )
        )),
        shiny.box_plot("Calibration Plot", "calibration_plot", "65vh")
    )
}

concentrationPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(3, selectizeInput(
                    inputId = "concentrationCompound",
                    label = "Compound",
                    choices = c()
                )),
                column(3, selectizeInput(
                    inputId = "concentrationAssay",
                    label = "Assay",
                    choices = c("Ratio", "Ratio Corrected")
                )),
                column(3, selectizeInput(
                    inputId = "concentrationBatch",
                    label = "Batch",
                    choices = c()
                )),
                column(3, selectInput(
                    inputId = "concentrationType",
                    label = "Type",
                    choices = c()
                ))
            ),
            fluidRow(
                column(3, selectizeInput(
                    inputId = "concentrationModel",
                    label = "Calibration Type",
                    choices = c("CAL", "ACAL")
                )),
                column(3, selectizeInput(
                    inputId = "concentrationWeighted",
                    label = "Use a weighted model (not implemented)",
                    choices = c(TRUE, FALSE)
                )),
                column(3, selectInput(
                    inputId = "concentrationAdjustment",
                    label = "Adjust Model",
                    multiple = FALSE,
                    choices = c("No Adjustment", "Subtract CAL0", "Force Through Origin")
                )),
                column(3, selectizeInput(
                    inputId = "concentrationOutliers",
                    label = "Remove Outliers",
                    choices = c(TRUE, FALSE)
                ))
            )
        )),
        fluidRow(
            column(4, shiny.box_table(
                "Data",
                "ConcentrationTable",
                height = "65vh"
            )),
            column(8, shiny.box_plot(
                "Calibration Model Plot",
                "LinearCalibration",
                "65vh")
            )
        )

    )
}

compoundPerBatchPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(4, selectizeInput("batchAssayCompound",
                                         label = "Compound",
                                         choices = c()
                )),
                column(4, selectizeInput("batchAssay",
                                         label = "Assay",
                                         choices = c()
                )),
                column(4, selectizeInput("batchAssayType",
                                         label = "Type",
                                         choices = c()
                ))
            )
        )),
        shiny.box_plot("Compound Per Batch", "batchAssayplot", "65vh")
    )
}

aliquotPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(4, selectizeInput("sample_assay", label = "Assay",
                                         choices = c()
                )),
                column(4, selectInput("sample_filtered",
                                      label = "Type", choices = c(),
                                      multiple = TRUE
                )),
                column(4, selectizeInput("sample_batch", label = "Batch",
                                         choices = c()
                ))
            )
        )),
        shiny.box_plot("Aliquot Plot", "sample_plot", "65vh")
    )
}

heatmapPlotPage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(4, selectizeInput("heatmap_assay", label = "Assay",
                                         choices = c()
                )),
                column(4, selectInput("heatmap_type",
                                      label = "Type", choices = c(),
                                      multiple = TRUE
                )),
                column(4, selectizeInput("heatmap_batch", label = "Batch",
                                         choices = c()
                ))
            )
        )),
        shiny.box_plot("Heatmap", "heatmap", "65vh")
    )
}
