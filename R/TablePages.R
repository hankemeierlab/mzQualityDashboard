combinedTablePage <- function(){
    fluidPage(
        shiny.box_table("Data", "combined")
    )
}

sampleTablePage <- function(){
    fluidPage(
        shiny.box_table("Data", "colData")
    )
}

compoundTablePage <- function(){
    fluidPage(
        shiny.box_table("Data", "rowData")
    )
}

assayTablePage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(4, selectizeInput("assay_name",
                                         label = "Assay",
                                         choices = c()
                )),
                column(4, selectizeInput("assay_type",
                                         label = "Aliquot type",
                                         choices = c()
                )),
                column(4, selectizeInput("assay_batch", label = "Batch",
                                         choices = c()
                ))
            )
        )),
        shiny.box_table("Assay", "assayData", height = "65vh")
    )
}


qcTablePlot <- function(){
    fluidPage(
        shiny.box_controls(list(
            selectizeInput("qc_table_type",
                           label = "QC type",
                           choices = c()
            ),
            selectizeInput("qc_corrected",
                           label = "Corrected",
                           choices = c("Raw", "Corrected")
            )
        )),
        shiny.box_table("QC Metrics", "qc_table", height = "65vh")
    )
}

modelTablePage <- function(){
    fluidPage(
        shiny.box_controls(list(
            fluidRow(
                column(6, selectizeInput(
                    inputId = "model_function",
                    label = "Function",
                    choices = c("studentizedResiduals", "concentrationR2", "Outliers", "linearRanges", "calRatios"))),
                column(6, selectInput("modelBatch", label = "Batch", choices = c(), multiple = TRUE))
            )
        )),
        shiny.box_table("Model Info", "model_table", height = "65vh")
    )
}
