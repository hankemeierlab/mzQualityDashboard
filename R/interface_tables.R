combinedTablePage <- function() {
    fluidPage(
        tableBox("Data", "combined", helperMd = "combinedTable")
    )
}

sampleTablePage <- function() {
    fluidPage(
        tableBox("Data", "colData", helperMd = "colDataTable")
    )
}

compoundTablePage <- function() {
    fluidPage(
        tableBox("Data", "rowData", helperMd = "rowDataTable")
    )
}

assayTablePage <- function() {
    fluidPage(
        controlsBox(list(
            fluidRow(
                column(4, selectizeInput("assay_name",
                    label = "Assay",
                    choices = c()
                )),
                column(4, selectizeInput("assay_type",
                    label = "Aliquot type",
                    choices = c()
                )),
                column(4, selectizeInput("assay_batch",
                    label = "Batch",
                    choices = c()
                ))
            )
        )),
        tableBox("Assay", "assayData", height = "65vh", helperMd = "assayTable")
    )
}
