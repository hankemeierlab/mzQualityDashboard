#' @title Shiny homepage
#' @importFrom shiny fluidPage fluidRow column textInput h2 h4 fileInput
#' checkboxInput numericInput actionButton
#' @importFrom shinyjs disabled
homePage <- function() {
    fluidPage(
        fluidRow(
            column(3),
            column(6, div(
                style = "text-align: center",
                h2("Create a new mzQuality project")
            )),
            column(3)
        ),
        fluidRow(
            column(3),
            column(6, controlsBox(
                inputs = homepageInputs(),
                helperMd = "input",
                title = NULL,
                footer = fluidRow(
                    column(12,
                           actionButton(inputId = "submit", label = "Submit")
                    )
                )
            ))
        )
    )
}

#' @title Inputs for the homepage
#' @importFrom shiny textInput h3 hr div fileInput checkboxInput
#' @noRd
homepageInputs <- function(){
    boxInputs <- list(
        textInput(
            inputId = "project", placeholder = "mzQuality",
            label =  "Name of the project",
        ),
        hr(),
        h3("Select your Data File"),
        div(style = "height: 1vh;"),
        fileInput(
            inputId = "files", multiple = TRUE, accept = c(".txt", ".tsv"),
            label = "Select your combined or batch file(s)"
        ),
        checkboxInput(inputId = "useExampleData", label = "Use Example Data"),
        div(style = "height: 1vh;"),
        hr(),
        h3("(Optional) Concentrations"),
        fileInput(
            inputId = "calFile", multiple = FALSE, accept = c(".txt", ".tsv"),
            label = "(Optional) Select your file with known concentrations"
        ),
        controlsBox(
            title = "Advanced Options",
            collapsible = TRUE,
            inputs = homepageAdvancedInputs()
        )
    )
}

#' @title Advanced inputs for the homepage
#' @importFrom shiny numericInput checkboxInput
#' @noRd
homepageAdvancedInputs <- function(){
    advancedInputs <- list(
        numericInput(
            inputId = "backgroundSignal",
            label = "Background Effect Threshold",
            min = 0, max = Inf, value = 40, step = 1
        ),
        numericInput(
            inputId = "cautionRSD",
            label = "Compound QC-RSD for Caution",
            min = 0, max = Inf, value = 15, step = 0.1
        ),
        numericInput(
            inputId = "nonReportableRSD",
            label = "Compound QC-RSD for non-reportable",
            min = 0, max = Inf, value = 30, step = 0.1
        ),
        numericInput(
            inputId = "qcPercentage",
            label = "Minimal percentage found in QCs",
            min = 0, max = 100, value = 80, step = 1
        ),
        checkboxInput(
            inputId = "filterISTD", value = TRUE,
            label = "Remove Internal Standards (ISTDs) in Compounds"
        ),
        checkboxInput(
            inputId = "filterSST", value = TRUE,
            label = "Remove SSTs from Aliquots"
        ),
        checkboxInput(
            inputId = "useWithinBatch", value = TRUE,
            label =  "Apply Within-Batch correction"

        ),
        checkboxInput(
            inputId = "effectNaAsZero", value = FALSE,
            label = "Replace NA with 0 for Background Signal Calculation"
        )
    )
}
