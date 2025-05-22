#' @title Page that makes the qc, aliquot, and compound selection
#' @description
#' @details
#' @importFrom shiny fluidPage selectizeInput
#' @noRd
selectDataPage <- function() {
    fluidPage(
        controlsBox(
            title = "QC Correction",
            helperMd = "qcCorrection",
            inputs = list(
                selectizeInput(
                    "qc_change",
                    choices = c(),
                    label = "Select sample type for correction"
                )
            )
        ),
        tableBox(
            title = "Aliquots",
            type = "aliquots",
            helperMd = "selectAliquots",
            height = "50vh"
        ),
        tableBox(
            title = "Metabolites",
            type = "compounds",
            helperMd = "selectCompounds",
            height = "50vh"
        )
    )
}

#' @title Page the shows the internal standard selection tables
#' @description
#' @details
#' @importFrom shiny fluidPage fluidRow column
internalStandardPage <- function() {
    fluidPage(
        fluidRow(
            column(4, tableBox(
                title = "Current Internal Standards",
                type = "IsCurrentTable",
                height = "80vh",
                helperMd = "IsCurrentTable"
            )),
            column(8, tableBox(
                title = "Modify Internal Standards",
                type = "IsModifyTable",
                height = "80vh",
                helperMd = "IsModifyTable"
            ))
        )
    )
}
