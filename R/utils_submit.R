#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @importFrom shiny req updateSelectInput
#' @importFrom mzQuality isValidExperiment readData
#' @noRd
.submitDataEvent <- function(input) {

    dirn <- dirname(input$files$datapath)
    files <- file.path(dirn, input$files$name)
    file.rename(input$files$datapath, files)

    return(readData(files))
}

#' @title Event for when the experiment needs to be constructed
#' @description
#' @param combined
#' @importFrom mzQuality buildExperiment
#' @noRd
.buildExperimentEvent <- function(combined) {
    qcValue <- "SAMPLE"

    qcCandidates <- grep("QC", combined$type, value = TRUE, ignore.case = TRUE)

    if (length(qcCandidates) > 0) {
        qcValue <- "SQC"
        if (!qcValue %in% combined$type) {
            qcValue <- qcCandidates[1]
        }
    }

    exp <- buildExperiment(
        df = combined,
        qc = qcValue,
    )

    return(exp)
}

