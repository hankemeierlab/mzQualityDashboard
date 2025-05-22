#' @title Page that shows all data into one table
#' @description
#' @importFrom shiny fluidPage
#' @noRd
.combinedTablePage <- function() {
    fluidPage(
        .tableBox("Data", "combined", helperMd = "combinedTable")
    )
}

#' @title Page that shows the sample data in a table
#' @description
#' @importFrom shiny fluidPage
#' @noRd
.sampleTablePage <- function() {
    fluidPage(
        .tableBox("Data", "colData", helperMd = "colDataTable")
    )
}

#' @title Page that shows all data of compounds in a table
#' @description
#' @importFrom shiny fluidPage
#' @noRd
.compoundTablePage <- function() {
    fluidPage(
        .tableBox("Data", "rowData", helperMd = "rowDataTable")
    )
}

#' @title Page that shows the assay tables
#' @description
#' @importFrom shiny fluidPage fluidRow column selectizeInput
#' @noRd
.assayTablePage <- function() {
    fluidPage(
        .controlsBox(list(
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
        .tableBox(
            "Assay",
            "assayData",
            height = "65vh",
            helperMd = "assayTable"
        )
    )
}
