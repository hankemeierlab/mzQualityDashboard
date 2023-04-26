#' @title Shiny homepage
homePage <- function() {
    version <- as.character(utils::packageVersion("mzQuality"))
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
                fileInput("files", multiple = TRUE,
                          "Select your combined or batch file(s)",
                          accept = c(".txt", ".tsv")
                ),
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
                column(2),
                column(4, actionButton("submit", "Submit",
                                       width = "100%", style = "margin: 2%;"
                )),
                column(4, actionButton("refresh", "Clear",
                                       width = "100%", style = "margin: 2%;"
                )),
                column(2)
            )
        ))
        )


                # helper(
                #     content = "Project",
                #     fade = TRUE,
                #     icon = "circle-question",
            # box(
            #     collapsible = TRUE,
            #     solidHeader = TRUE,
            #     title = "Basic",
            #     textInput("project", "Name of the project",
            #               placeholder = "mzQuality"
            #     ),
            #     fileInput("files", multiple = TRUE,
            #               "Select your combined or batch file(s)",
            #               accept = c(".txt", ".tsv")
            #     ),
            #
            # )),
            # column(3)
        # ),
        # fluidRow(
        #     column(3),
        #
        #     column(6, helper(
        #         content = "Additional",
        #         fade = TRUE,
        #         icon = "circle-question",
        #         box(
        #             title = "Advanced",
        #             solidHeader = TRUE,
        #             collapsible = TRUE,
        #             collapsed = TRUE,
        #             checkboxInput(
        #                 inputId = "filterCalCompounds",
        #                 value = FALSE,
        #                 label = "Use only compounds with known concentrations"
        #             ),
        #             fileInput(
        #                 inputId = "calFile",
        #                 label = "Calibration file",
        #                 multiple = FALSE,
        #                 accept = c(".txt", ".tsv")
        #             ),
        #             div(style = "margin-top: 1em;",
        #                 h4("Combined file options")),
        #             textInput("colIndex", "Aliquot column",
        #                       placeholder = "aliquot"),
        #             textInput("rowIndex", "Compound column",
        #                       placeholder = "compound"),
        #             textInput("areaIndex", "Peak area column",
        #                       placeholder = "area"),
        #             textInput("areaIsIndex", placeholder = "area_is",
        #                       "Peak area column of Internal Standard"
        #             )
        #         )
        #     )),
        #     column(3)
        # ),

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
        fluidRow(
            column(12, shiny.box_controls(
                title = "Internal Standards",
                inputs = list(
                    radioButtons(
                        inputId = "SelectIS",
                        label = "Use IS",
                        choices = c("Used", "Suggested"),
                        selected = "Used",
                        inline = TRUE
                    )
            )))
        ),
        fluidRow(
            column(4, shiny.box_table(
                title = "Current Internal Standards",
                type = "IsCurrentTable",
                height = "70vh"
            )),
            column(8, shiny.box_table(
                title = "Modify Internal Standards",
                type = "IsModifyTable",
                height = "70vh"
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
                                    "Compound Per Batch", "PCA Plot", "RSDQCs",
                                    "QC Plot", "Calibration Plot",
                                    "Concentrations"
                        ),
                        selected = c("Aliquot", "Compound", "PCA",
                                     "RSDQCs", "QC"
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
                    ))
                )
            ),
            footer = downloadButton("download_zip")
        )
    )
}
