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

