#' @title Shiny homepage
homePage <- function() {
    version <- as.character(utils::packageVersion("mzQuality2"))
    versionType <- switch(stringr::str_count(version, "\\."),
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
            column(6, shiny.box_controls(inputs = list(
                textInput("project", "Name of the project",
                          placeholder = "mzQuality"
                ),
                hr(),
                h3("Select your Data File"),
                div(style = "height: 1vh;"),

                shiny.box_controls(title = "Local", inputs = list(
                    fileInput("files", multiple = TRUE,
                              "Select your combined or batch file(s)",
                              accept = c(".txt", ".tsv")
                    )
                ), collapsible = TRUE),

                shiny.box_controls(title = "Data Lake", inputs = list(
                shinyjs::disabled(selectizeInput("dataLakeBuckets", label = "Bucket", choices = c())),
                 shinyjs::disabled(selectizeInput("dataLakeProjects", label = "Project", choices = c())),
                 shinyjs::disabled(selectizeInput("dataLakeExports", label = "File", choices = c()))
                ), collapsible = TRUE),

                div(style = "height: 1vh;"),
                hr(),
                h3("Additional Input"),

                fileInput(
                    inputId = "calFile",
                    label = "(Optional) Select your file with known concentrations",
                    multiple = FALSE,
                    accept = c(".txt", ".tsv")
                ),
                checkboxInput(
                    "filterISTD",
                    value = TRUE,
                    "Remove Internal Standards (ISTDs) in Compounds"
                ),
                checkboxInput("filterSST", "Remove SSTs from Aliquots",
                              value = TRUE
                )
            ),
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
        )))
    )
}

selectDataPage <- function(){
    fluidPage(
        shiny.box_controls(
            title = "QC Correction",
            inputs = list(

            selectizeInput(
                "qc_change",
                choices = c(),
                label = "Select sample type for correction"
            ))
        ),
        shiny.box_table(
            title = "Aliquots",
            type = "aliquots",
            height = "50vh"
        ),
        shiny.box_table(
            title = "Metabolites",
            type = "compounds",
            height = "50vh"
        )
    )
}

internalStandardPage <- function(){
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
                height = "80vh"
            )),
            column(8, shiny.box_table(
                title = "Modify Internal Standards",
                type = "IsModifyTable",
                height = "80vh"
            ))
        )
    )
}

downloadPage <- function(){
    fluidPage(
        shiny.box_controls(
            title = "Download Options",
            inputs = list(
                fluidRow(
                    column(12, selectizeInput(
                        inputId = "downloadPlotPicker",
                        label = "Choose Plots",
                        choices = c("Heatmap", "Aliquot", "Compound",
                                    "Compound Per Batch", "PCA", "RSDQC",
                                    "QC", "Calibration Plot",
                                    "Concentrations"
                        ),
                        selected = c("Aliquot", "Compound", "PCA",
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
                    )),
                    column(12, checkboxInput(
                        inputId = "copyDataLake",
                        label = "Save a copy on the Data Lake",
                        value = TRUE
                    ))
                )
            ),
            footer = fluidRow(
                actionButton("createZip", label = "Create Reports"),
                shinyjs::disabled(downloadButton("download_zip"))
            )
        )
    )
}
