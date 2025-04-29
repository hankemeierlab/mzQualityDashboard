#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @importFrom shiny req updateSelectInput
submitDataEvent <- function(session, input) {
    if (length(input$files$datapath) == 0) {
        shinyalert::shinyalert("Please select your files")
    }



    dirn <- dirname(input$files$datapath)
    files <- file.path(dirn, input$files$name)
    file.rename(input$files$datapath, files)

    # Build a combined file using the file(s) given
    # Should convert to arrow reader for combined files

    return(readData(files))
}

buildExperimentEvent <- function(session, input, combined) {
    qcCandidates <- grep("QC", combined$type, value = TRUE, ignore.case = TRUE)
    qcValue <- "SQC"
    if (!"SQC" %in% combined$type) {
        qcValue <- qcCandidates[1]
    }


    updateSelectInput(session, "qc_change",
        choices = unique(qcCandidates),
        selected = qcValue
    )

    exp <- buildExperiment(
        df = combined,
        qc = qcValue,
    )

    return(exp)
}



#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param experiment
#' @importFrom shiny req updateSelectInput
qcChangeEvent <- function(input, experiment) {
    req(!is.null(experiment))

    metadata(experiment)$QC <- input$qc_change

    # experiment <- doAnalysis(experiment)
    #
    # if (as.logical(input$showOutliers)) showOutliers(exp)

    return(experiment)
}
