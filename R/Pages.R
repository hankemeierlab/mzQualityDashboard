#' @title Shiny homepage
#' @importFrom utils packageVersion
#' @importFrom shiny fluidPage fluidRow column textInput h2 h4 fileInput checkboxInput numericInput actionButton
#' @importFrom shinyjs disabled
#' @importFrom stringr str_count
homePage <- function() {
    version <- as.character(packageVersion("mzQualityDashboard"))
    versionType <- switch(str_count(version, "\\."),
        "2" = "Stable",
        "3" = "Nightly",
        "4" = "Development"
    )
    fluidPage(
        fluidRow(
            column(3),
            column(6, div(
                style = "text-align: center",
                h2("Create a new mzQuality project"),
                h4(sprintf("%s version: %s", versionType, version))
            )),
            column(3)
        ),
        fluidRow(
            column(3),
            column(6, shiny.box_controls(
                inputs = list(
                    textInput("project", "Name of the project",
                        placeholder = "mzQuality"
                    ),
                    hr(),
                    h3("Select your Data File"),
                    div(style = "height: 1vh;"),
                    fileInput("files",
                        multiple = TRUE,
                        "Select your combined or batch file(s)",
                        accept = c(".txt", ".tsv")
                    ),
                    checkboxInput("useExampleData", "Use Example Data",
                        value = FALSE
                    ),
                    div(style = "height: 1vh;"),
                    hr(),
                    h3("(Optional) Concentrations"),
                    fileInput(
                        inputId = "calFile",
                        label = "(Optional) Select your file with known concentrations",
                        multiple = FALSE,
                        accept = c(".txt", ".tsv")
                    ),
                    shiny.box_controls(title = "Advanced Options", collapsible = TRUE, inputs = list(
                        numericInput(
                            inputId = "backgroundSignal",
                            label = "Background Effect Threshold",
                            min = 0,
                            max = Inf,
                            value = 40,
                            step = 1
                        ),
                        numericInput(
                            inputId = "cautionRSD",
                            label = "Compound QC-RSD for Caution",
                            min = 0,
                            max = Inf,
                            value = 15,
                            step = 0.1
                        ),
                        numericInput(
                            inputId = "nonReportableRSD",
                            label = "Compound QC-RSD for non-reportable",
                            min = 0,
                            max = Inf,
                            value = 30,
                            step = 0.1
                        ),
                        numericInput(
                            inputId = "qcPercentage",
                            label = "Minimal percentage found in QCs",
                            min = 0,
                            max = 100,
                            value = 80,
                            step = 1
                        ),
                        checkboxInput(
                            "filterISTD",
                            value = TRUE,
                            "Remove Internal Standards (ISTDs) in Compounds"
                        ),
                        checkboxInput("filterSST", "Remove SSTs from Aliquots",
                            value = TRUE
                        ),
                        checkboxInput("useWithinBatch", "Apply Within-Batch correction",
                            value = FALSE
                        ),
                        checkboxInput("effectNaAsZero", "Replace NA with 0 for Background Signal Calculation", value = FALSE)
                    ))
                ),
                helperMd = "input",
                title = NULL,
                footer = fluidRow(
                    column(12, actionButton("submit", "Submit",
                        width = "60%", height = "10%",
                        style = "
                                        margin-left: 20%;
                                        margin-right: 20%;
                                        margin-top: 1%;
                                        margin-bottom: 1%;
                                        "
                    ))
                )
            ))
        )
    )
}

selectDataPage <- function() {
    fluidPage(
        shiny.box_controls(
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
        shiny.box_table(
            title = "Aliquots",
            type = "aliquots",
            helperMd = "selectAliquots",
            height = "50vh"
        ),
        shiny.box_table(
            title = "Metabolites",
            type = "compounds",
            helperMd = "selectCompounds",
            height = "50vh"
        )
    )
}

internalStandardPage <- function() {
    fluidPage(
        # fluidRow(
        #     column(12, shiny.box_controls(
        #         title = "Internal Standards",
        #         inputs = list(
        #             radioButtons(
        #                 inputId = "SelectIS",
        #                 label = "Use IS",
        #                 choices = c("Used", "Suggested"),
        #                 selected = "Used",
        #                 inline = TRUE
        #             )
        #     )))
        # ),
        fluidRow(
            column(4, shiny.box_table(
                title = "Current Internal Standards",
                type = "IsCurrentTable",
                height = "80vh",
                helperMd = "IsCurrentTable"
            )),
            column(8, shiny.box_table(
                title = "Modify Internal Standards",
                type = "IsModifyTable",
                height = "80vh",
                helperMd = "IsModifyTable"
            ))
        )
    )
}

downloadPage <- function() {
    fluidPage(
        shiny.box_controls(
            title = "Download Options",
            inputs = list(
                fluidRow(
                    column(12, selectizeInput(
                        inputId = "downloadPlotPicker",
                        label = "Choose Plots",
                        choices = c(
                            "Heatmap", "Aliquot", "Compound",
                            "Compound Per Batch", "PCA", "RSDQC",
                            "QC", "Calibration Plot",
                            "Concentrations"
                        ),
                        selected = c(
                            "Aliquot", "Compound", "PCA",
                            "RSDQC", "QC"
                        ),
                        multiple = TRUE
                    )),
                ),
                fluidRow(
                    column(12, selectizeInput(
                        inputId = "downloadAssayPicker",
                        label = "Choose Assays",
                        choices = c(),
                        selected = c(),
                        multiple = TRUE
                    ))
                ),
                fluidRow(
                    column(12, checkboxInput(
                        inputId = "summary_report",
                        label = "Summary Report",
                        value = TRUE
                    )),
                    column(12, checkboxInput(
                        inputId = "compound_report",
                        label = "Compound Report(s)",
                        value = TRUE
                    )) # ,
                    # column(12, checkboxInput(
                    #     inputId = "copyDataLake",
                    #     label = "Save a copy on the Data Lake",
                    #     value = TRUE
                    # ))
                )
            ),
            footer = fluidPage(
                fluidRow(
                    column(1, actionButton("createZip", label = "Create Reports")),
                    column(1, shinyjs::disabled(downloadButton("download_zip")))
                ),
                div(style = "height: 3em"),
                fluidRow(
                    column(1, bookmarkButton(label = "Get Shareable Link")),
                    column(1, checkboxInput("remoteLink", label = "Remote Link"))
                )
            )
        )
    )
}
