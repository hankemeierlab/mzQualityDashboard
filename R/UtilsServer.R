#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom rhandsontable rhandsontable hot_cols hot_table
#' @importFrom DT datatable
#' @importFrom dplyr %>%
renderTable <- function(df, readonly = TRUE, rowHeaders = NULL,
                        preSelect = c()) {

    df <- as.data.frame(df)

    if (!is.null(rowHeaders)) {
        newCols <- c("Compound", colnames(df))
        df$Compound <- rowHeaders
        df <- df[, newCols]
    }

    table <- DT::datatable(style = "bootstrap4",
        data = df,
        escape = FALSE,
        selection = list(
            mode = "multiple", selected = preSelect
        ),
        extensions = "Scroller",
        options = list(
                       deferRender = TRUE,
                       scrollY = 500,
                       scroller = TRUE)#,
        # callback = JS("table.rows().every(function(i, tab, row) {
        #   var $this = $(this.node());
        #   $this.attr('id', this.data()[0]);
        #   $this.addClass('shiny-input-checkbox');
        # });
        # Shiny.unbindAll(table.table().node());
        # Shiny.bindAll(table.table().node());")
    )

    # table <- rhandsontable(
    #     data = df,
    #     readOnly = readonly,
    #     rowHeader = 1:nrow(df)
    # )
    #
    # table <- hot_table(table, stretchH = "all") %>%
    #     hot_cols(
    #         columnSorting = TRUE,
    #         fixedColumnsLeft = 1,
    #         halign = "htLeft"
    # )

    return(table)
}

#' @title Convert ggplot to plotly
#' @description This function can be used to convert any static plot
#' made with ggplot to an interactive plot with plotly. This is useful for
#' more complex plots, where panning and zooming may be needed.
#' @param plot ggplot2 object
#' @returns Plotly object of the given ggplot
#' @importFrom plotly ggplotly layout
#' @export
#' @examples
#' # Read example dataset
#' data <- read.delim(system.file(package = "mzQuality", "dataset.txt"))
#'
#' # Construct experiment
#' exp <- buildExperiment(
#'     data,
#'     rowIndex = "Compound",
#'     colIndex = "Aliquot",
#'     primaryAssay = "Area",
#'     secondaryAssay = "Area_is"
#' )
#'
#' # Perform analysis
#' exp <- doAnalysis(exp)
#'
#' # Create PCA plot
#' p <- pcaPlot(exp)
#'
#' # Make the PCA interactive
#' toPlotly(p)
toPlotly <- function(plot, dynamicTicks = TRUE) {
    if (dynamicTicks) {
        p <- ggplotly(plot, dynamicTicks = TRUE, tooltip = c("text"))
        p <- plotly::layout(p, yaxis = list(exponentformat = "E"))

    } else {
        p <- suppressWarnings(ggplotly(plot))
    }
    # Return Plotly
    return(p)
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @param assayName
#' @importFrom shiny req
#' @importFrom SummarizedExperiment assay
createAssayTable <- function(exp, assayName) {
    req(!is.null(exp))
    as.data.frame(cbind(
        Compound = rownames(exp),
        round(assay(exp, assayName), 3)
    ))
}

#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom shiny req
downloadZip <- function(fileOut, exp) {
    req(!is.null(exp))

    withProgress(message = "Generating output files...", {
        w$show()
        project <- ifelse(input$project == "", "mzQuality", input$project)
        datetime <- format(metadata(exp)$Date, "%Y%m%d_%H%M%S")
        output_folder <- file.path(getwd(), project, datetime)

        reports <- file.path(output_folder, "reports")
        plots <- file.path(output_folder, "plots")
        compounds <- file.path(plots, "compounds")

        dir.create(reports, recursive = TRUE, showWarnings = FALSE)
        dir.create(compounds, recursive = TRUE, showWarnings = FALSE)

        combFile <- file.path(output_folder, "combined.tsv")
        utils::write.table(expToCombined(experiment()),
                           file = combFile,
                           sep = "\t", row.names = FALSE
        )

        exportTables(exp, reports)

        x <- doAnalysis(experiment()[, colnames(exp)])
        file <- "Metabolites_samples_to_keep.xlsx"
        exportFilterWorkbook(x, file.path(reports, file))

        selected <- rownames(exp)
        na_rsdqc <- rownames(x)[which(is.na(rowData(x)$RSDQC.Corrected))]
        x <- x[c(selected, na_rsdqc), ]

        file <- "QC_corrected_sample_final_report.xlsx"
        exportExcel(x[, x$Type == "SAMPLE"], file.path(reports, file))

        file <- "QC_corrected_all_final_report.xlsx"
        exportExcel(x, file.path(reports, file))

        if (!is.null(IS_compounds())) {
            comps <- hot_to_r(input$compounds)
            is_df <- data.frame(
                Compound = rownames(experiment()),
                Used.IS = rowData(experiment())$Compound_is,
                Suggested.IS = rowData(experiment())$SuggestedIS,
                Selected.IS = comps$Selected.IS
            )

            utils::write.table(is_df,
                               file = file.path(reports, "Used_IS.tsv"),
                               sep = "\t", row.names = FALSE
            )
        }
    })

    if (input$summary_report) {
        withProgress(message = "Generating Summary Report...", {
            summaryReport(exp, plots)
            loginfo("Created Summary Report")

        })
    }

    if (input$compound_report) {
        loginfo("Creating Compound Reports")
        pboptions(type = "shiny", title = "Generating Compound Reports...")
        compoundReports(exp, compounds)
    }
    w$hide()

    zipFolder(fileOut, output_folder)
}
