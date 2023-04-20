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
                column(4, selectInput("compound_trends",
                                      label = "Trendlines",
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
                column(2, numericInput("PCA_Y", "Component Y-axis", value = 2, min = 2, step = 1)),
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
                                         label = "Type",
                                         choices = c()
                )),
                column(4, selectInput("qc_batch", label = "Batch", multiple = TRUE,
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
                column(3, selectInput(
                    inputId = "concentrationType",
                    label = "Type",
                    choices = c(),
                    multiple = TRUE
                )),
                column(2, selectizeInput(
                    inputId = "concentrationAssay",
                    label = "Assay",
                    choices = c("Concentration", "Concentration_Corrected")
                )),
                column(2, selectInput(
                    inputId = "concentrationBatch",
                    label = "Batch",
                    choices = c(),
                    multiple = TRUE
                )),
                column(2, checkboxInput("concentrationPlotOnLine", "Plot concentrations on Calibration Line"))

            )
        )),
        fluidRow(
            # column(4, shiny.box_table(
            #     "Data",
            #     "concentrationTable",
            #     height = "65vh"
            # )),
            column(12, shiny.box_plot(
                "Calibration Model Plot",
                "concentrationPlot",
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
