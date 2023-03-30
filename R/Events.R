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

    if (length(input$calFile) > 0) {
        conc <- utils::read.delim(input$calFile$datapath, check.names = FALSE)
        exp <- mzQuality2:::addConcentrations2(exp, conc)
    }

    if (input$filterISTD) {
        exp <- filterISTD(exp, "ISTD")
    }
    if (input$filterSST) {
        exp <- filterSST(exp, "SST")
    }

    exp <- doAnalysis(exp = exp, doAll = TRUE)

    print(exp)
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

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param experiment
#' @importFrom shiny req
calTableClickEvent <- function(){

    # DEFUNCT

    req("Concentration" %in% assayNames(exp()) & !is.null(experiment()))

    exp <- experiment()
    x <- exp[, exp$batch == input$linearCalibration_batch &
                 exp$type == metadata(exp)$concentration]

    table <- hot_to_r(input$CalTable)
    comps <- table$compound
    table <- table[,-1]
    to_remove <- which(table == FALSE)

    if (length(to_remove) > 0) {
        assay(x, "Concentration")[to_remove] <- NA
    }

    to_add <- which(table == TRUE)
    if (length(to_add) > 0) {
        assay(x, "Concentration")[to_add] <- concentrations()[to_add]
    }
    if (length(to_remove) > 0 | length(to_add) > 0) {
        assay(exp, "Concentration")[rownames(x), colnames(x)] <- assay(x, "Concentration")
        experiment(exp)
    }
}

#' @title
#' @description
#' @details
#' @returns
#' @param input
#' @param experiment
#' @importFrom stats na.omit
updateExperiment <- function(input, experiment){

    req(!is.null(experiment) & !is.null(input$aliquots))
    aliquot_df <- hot_to_r(input$aliquots)
    comp_df <- hot_to_r(input$compounds)

    comps <- which(comp_df$use == 1)
    if (length(comps) == 0) {
        comps <- 1:nrow(experiment)
    }

    # Think this should be in an internal standard listener..
    # if (!is.null(IS_compounds())) {
    #     is_comps <- comp_df$Selected.IS
    #     x <- replaceIS(x[, aliq], is_comps)
    # }

    return(doAnalysis(
        experiment,
        aliquots = which(aliquot_df$use == 1),
        compounds = comps
    ))
}
