#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param exp
#' @importFrom shiny req
compoundTableClick <- function(input, exp){
    req(!is.null(exp))

    comp_df <- hot_to_r(input$compounds)
    comps <- comp_df$compound[comp_df$use == 1]
    if (length(comps) == 0) {
        comps <- comp_df$compound
    }

    return(doAnalysis(exp, compounds = comps))
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @importFrom shiny req updateSelectInput
submitDataEvent <- function(session, input){

    if (length(input$files$datapath) == 0) {
        shinyalert::shinyalert("Please select your files")
    }

    req(length(input$files$datapath) > 0)


    dirn <- dirname(input$files$datapath)
    files <- file.path(dirn, input$files$name)
    file.rename(input$files$datapath, files)

    # Build a combined file using the file(s) given
    # Should convert to arrow reader for combined files

    combined <- buildCombined(files)
    return(combined)
}

buildExperimentEvent <- function(session, input, combined){

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


    if (input$filterISTD) {
        exp <- filterISTD(exp, "ISTD")
    }
    if (input$filterSST) {
        exp <- filterSST(exp, "SST")
    }

    exp <- doAnalysis(exp = exp, doAll = TRUE)

    addedConcentrations <- length(input$calFile) > 0
    if (addedConcentrations) {
        conc <- utils::read.delim(input$calFile$datapath, check.names = FALSE)
        exp <- addConcentrations(exp, conc, filterComps = FALSE)
    }

    return(exp)
}



#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param experiment
#' @importFrom shiny req updateSelectInput
qcChangeEvent <- function(input, experiment){
    req(!is.null(experiment))

    metadata(experiment)$QC <- input$qc_change

    # experiment <- doAnalysis(experiment)
    #
    # if (as.logical(input$showOutliers)) showOutliers(exp)

    return(experiment)
}

