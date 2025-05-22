#' @title Controller for the homepage
#' @description
#' @noRd
.homepageController <- function(session, input, output, aliquotDf, experiment) {
    observeSubmitEvent(session, input, aliquotDf, experiment)
}

#' @title Controller for the selection screen page
#' @description
#' @noRd
.selectionController <- function(
        input, output, aliquotDf, compoundDf, internalStandards, experiment
) {

    observeQcSelection(input, experiment, aliquotDf)
    observeAliquotTableUpdate(experiment, aliquotDf, compoundDf)
    oberserveInternalStandardUpdate(
        input, experiment, internalStandards, compoundDf
    )
    observeAliquotTableSelection(input, experiment, compoundDf)
    observeCompoundTableSelection(input, experiment)

    outputAliquotSelectionTable(output, aliquotDf)
    outputCompoundSelectionTable(output, compoundDf)


    observeStandardSelection(input, internalStandards, experiment)
    outputCurrentStandardsTable(output, experiment)
    outputModifiedStandardsTable(input, output, experiment)

}

#' @title Controller for the table pages
#' @description
#' @noRd
.tablePagesController <- function(input, output, experiment) {
    outputLongTable(input, output, experiment)
    outputRowData(input, output, experiment)
    outputColData(input, output, experiment)
    outputAssayData(input, output, experiment)
    observeAssayTableInputs(input, experiment)
}

#' @title Controller for the plot pages
#' @description
#' @noRd
.plotPagesController <- function(session, input, output, experiment) {

    observeConcentrationPlotInputs(input, experiment)
    outputConcentrationPlot(input, output, experiment)

    outputSamplePlot(session, input, output, experiment)
    observeAliquotPlotInputs(input, experiment)

    outputBadQcPlot(input, output, experiment)
    observeViolinPlotInputs(input, experiment)

    outputPcaPlot(input, output, experiment)
    observePcaPlotInputs(input, experiment)

    outputRsdqcHeatmap(input, output, experiment)

    observeCompoundPlotKeyInputs(input, experiment)
    outputCompoundPlot(input, output, experiment)
    observeCompoundPlotInputs(input, experiment)
}

#' @title Controller for the Export page
#' @description
#' @noRd
.exportPageController <- function(input, output, experiment) {
    observeExportAssays(input, experiment)
    observeReportCreation(input, experiment)
    observeDownloadZip(input, output, experiment)
}

