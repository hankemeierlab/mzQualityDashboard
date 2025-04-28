#' @title Shiny server function
#' @param input Input variable for incoming UI requests
#' @param output Output variable for updating the UI
#' @importFrom waiter Waiter spin_flower transparent
#' @importFrom sodium password_store
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyhelper observe_helpers
#' @importFrom shiny div h3 observeEvent updateSelectInput updateSelectizeInput
#' req updateTabsetPanel downloadHandler bindCache incProgress reactive
#' reactiveVal renderUI updateSliderInput withProgress showModal modalDialog
#' updateCheckboxInput
#' @importFrom shinyjs hide show runjs removeClass addClass enable disable
#' @importFrom shinyWidgets execute_safely
#' @importFrom plotly renderPlotly
#' @importFrom DT renderDataTable
#' @importFrom mzQuality identifyMisInjections doAnalysis expToCombined
#' @noRd
server <- function(input, output, session, useLogin = FALSE) {


    if (useLogin) {
        # dataframe that holds usernames, passwords and other user data
        user_base <- data.frame(
            user = c("user1", "user"),
            password = vapply(c("pass1", "mslab"), password_store, character(1)),
            permissions = c("admin", "standard"),
            name = c("User One", "User Two")
        )

        shinyjs::hide("menuHidden")

        # login status and info will be managed by shinyauthr module and stores here
        credentials <- callModule(shinyauthr::login, "login",
            data = user_base,
            user_col = user,
            pwd_col = password,
            sodium_hashed = TRUE,
            log_out = reactive(logout_init())
        )

        # logout status managed by shinyauthr module and stored here
        logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))

        # this opens or closes the sidebar on login/logout
        observe({
            if (credentials()$user_auth) {
                shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
                shinyjs::show("menuHidden")
                # move to pipeline page
            } else {
                shinyjs::addClass(selector = "body", class = "sidebar-collapse")
                shinyjs::hide("menuHidden")
            }
        })
    } else {
        shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        shinyjs::show("menuHidden")
    }

    # Initialize Variables and Settings ---------------------------------------
    shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")

    mbLimit <- 5000
    options(shiny.maxRequestSize = mbLimit * 1024^2)
    observe_helpers(help_dir = system.file("helppages", package = "mzQualityDashboard"))
    w <- loadingScreen()

    # Set the total exp and subset `exp`
    combined <- data.frame()
    rsdqcs <- data.frame()
    project <- reactiveVal()
    newExp <- reactiveVal()
    aliquots_selected <- c()
    compounds_selected <- c()
    qc_selected <- c()
    aliquotDf <- reactiveVal()
    internalStandards <- reactiveVal()


    # Experiment update -----------------------------------------------------

    observeEvent(input$qc_change, ignoreInit = TRUE, {
        exp <- newExp()
        if (input$qc_change != metadata(exp)$QC) {
            metadata(exp)$QC <- isolate(input$qc_change)
            exp$use <- TRUE
            exp <- identifyOutliers(exp) %>%
                identifyMisInjections() %>%
                doAnalysis(
                    doAll = TRUE, removeOutliers = TRUE,
                    useWithinBatch = as.logical(input$useWithinBatch),
                    backgroundPercentage = input$backgroundSignal,
                    qcPercentage = input$qcPercentage,
                    nonReportableRSD = input$nonReportableRSD,
                    effectNaAsZero = input$effectNaAsZero
                )
            # output$compounds <- DT::renderDataTable(compoundTable(exp, select = which(!rowData(exp)$use)))

            df <- colData(exp)
            df$aliquot <- rownames(df)
            columns <- c("aliquot", "type", "datetime", "batch", "use")
            df <- as.data.frame(df[, columns])
            df$datetime <- as.character(df$datetime)
            rownames(df) <- seq_len(nrow(df))
            aliquotDf(df)

            newExp(exp)
        }
    })

    homepageServer(session, input, output, aliquotDf, internalStandards, newExp)
    internalStandardPageServer(input, output, internalStandards, newExp)

    observe({
        exp <- newExp()
        req(!is.null(exp))
        updateInputs(session, input, exp[rowData(exp)$use, exp$use])
    })



    output$typeColorPickers <- renderUI({
        exp <- newExp()
        types <- unique(exp$type)



        do.call(tagList, lapply(seq_along(types), function(i) {
            id <- which(exp$type == types[i])[1]
            shinyWidgets::colorPickr(paste0("colorpicker_", i), label = sprintf("Color for %s", types[i]), selected = exp$color[id])
        }))
    })

    # stateId <- ""
    # onBookmark(function(state) {
    #     state$values$aliquots_selected <- input$aliquots_rows_selected
    #     state$values$compounds_selected <- input$compounds_rows_selected
    #     state$values$qc_selected <- input$qc_change
    #     state$values$exp <- exp
    #     if (input$remoteLink) {
    #         stateId <<- sprintf("127.0.0.1:8080/app/mzquality/?_state_id_=%s", basename(state$dir))
    #     } else {
    #         stateId <<- sprintf("mzquality.bmfl.nl/?_state_id_=%s", basename(state$dir))
    #     }
    #
    # })
    #
    # onBookmarked(function(state){
    #     showBookmarkUrlModal(stateId)
    # })
    #
    # onRestore(function(state){
    #     exp <<- state$values$exp
    #
    # })
    #
    # onRestored(function(state) {
    #     aliquots_selected <<- state$values$aliquots_selected
    #     compounds_selected <<- state$values$compounds_selected
    #     qc_selected <<- state$values$qc_selected
    #
    #     qcCandidates <- grep("QC", exp$type, value = TRUE, ignore.case = TRUE)
    #
    #     updateSelectInput(session, "qc_change",
    #                       choices = unique(qcCandidates),
    #                       selected = qc_selected
    #     )
    # })

    # Combined Overall Table
    output$combined <- DT::renderDataTable(
        shinyWidgets::execute_safely(
            {
                input$compounds_rows_selected
                input$aliquots_rows_selected

                combined <- expToCombined(exp = newExp(), rowIndex = "compound", colIndex = "aliquot")

                combinedTable(combined)
            },
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    )

    # Compound Details Table
    output$rowData <- DT::renderDataTable(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()
                rowDataTable(exp[rowData(exp)$use, exp$use])
            },
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    )

    # Aliquot Details Table
    output$colData <- DT::renderDataTable(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                colDataTable(exp[rowData(exp)$use, exp$use])
            },
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    )

    # Assay / Values Table
    output$assayData <- DT::renderDataTable(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                assayTable(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    )

    # Batch Effect Correction Factor Table
    output$batch_correction_table <- DT::renderDataTable({
        exp <- newExp()

        batchCorrectionFactorTable(exp[rowData(exp)$use, exp$use])
    })

    # Table of Model Effects, R2, etc.
    output$model_table <- DT::renderDataTable({
        exp <- newExp()

        modelTable(input, exp[rowData(exp)$use, exp$use])
    })

    # Table of Background effects
    output$effect_table <- DT::renderDataTable({
        exp <- newExp()

        backGroundEffectTable(input, exp[rowData(exp)$use, exp$use])
    })

    # Table of Carry Over Effect
    output$carryOverTable <- DT::renderDataTable({
        exp <- newExp()

        carryOverTable(exp[rowData(exp)$use, exp$use])
    })

    output$concentrationTable <- DT::renderDataTable(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                concentrationTable(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Table Viewer Failed",
            message = "Could not create the table"
        )
    )


    # Plots -------------------------------------------------------------------

    # Aliquot Plot
    output$sample_plot <- plotly::renderPlotly({
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderAliquotPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    })

    compoundPlotEvent(input, output, newExp)

    # QC Violin Plot
    output$badqc_plot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderViolinPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    # Principle Component Plot
    output$pca_plot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderPcaPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    # Batch Effect Box Plot
    output$batch_boxplot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderBatchBoxPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    # Heatmap Plot
    output$heatmap <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderHeatMapPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    # Batch Assay Plot
    output$batchAssayplot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderBatchAssayPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    # RSDQC Heatmap Plot
    output$correlation_heatmap <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderRsdqcPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    # (Academic) Calibration Plot
    output$calibration_plot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderCalibrationPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    # Relative Standard Deviation Plot
    output$rsd_plot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderRsdPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot"
        )
    )

    output$concentrationPlot <- plotly::renderPlotly(
        shinyWidgets::execute_safely(
            {
                exp <- newExp()

                renderConcentrationPlot(input, exp[rowData(exp)$use, exp$use])
            },
            title = "Plot Failed",
            message = "Could not create the plot (No concentrations provided?)",
            include_error = FALSE
        )
    )


    # Events ------------------------------------------------------------------

    observeEvent(input$key_pressed, {
        exp <- newExp()
        if (input$sidebar == "Compounds") {
            allComps <- rownames(exp)[rowData(exp)$use]
            currentComp <- input$compound_metabolite
            idx <- which(allComps == currentComp)[1]
            if (input$key_pressed == "ArrowRight") {
                updateSelectInput(session, inputId = "compound_metabolite", choices = allComps, selected = allComps[min(length(allComps), idx + 1)])
            } else if (input$key_pressed == "ArrowLeft") {
                updateSelectInput(session, inputId = "compound_metabolite", choices = allComps, selected = allComps[max(1, idx - 1)])
            }
        }
    })

    # Event when the submit button is clicked on the start screen
    observeEvent(input$submit, {
        localFileTest <- length(input$files$datapath) > 0
        exampleTest <- as.logical(input$useExampleData)

        filledExp <- any(dim(newExp()) > 0)
        req(localFileTest || exampleTest)
        w$show()


        proj <- ifelse(input$project == "", "mzQuality", input$project)
        timeFormat <- format(Sys.time(), "%d-%m-%Y_%H.%M.%S")

        project(sprintf("%s_%s", proj, timeFormat))

        if (!filledExp) {
            combined <- shinyWidgets::execute_safely(
                {
                    if (exampleTest) {
                        combined <- buildCombined(system.file("example.tsv", package = "mzQuality"))
                    } else if (length(input$files$datapath) > 0) {
                        combined <- submitDataEvent(session, input)
                    }
                    combined
                },
                message = "
        A problem occured with your data. Ensure that your aliquots are named correctly.
        If this problem still occurs, contact the developer"
            )

            newExp(buildExperimentEvent(session, input, combined))
        }


        df <- colData(newExp())
        df$aliquot <- rownames(df)
        columns <- c("aliquot", "type", "datetime", "batch", "use")
        df <- as.data.frame(df[, columns])
        df$datetime <- as.character(df$datetime)
        rownames(df) <- seq_len(nrow(df))
        aliquotDf(df)


        updateTabsetPanel(session, inputId = "sidebar", "selectedData")

        # if (all(newExp()$use)) {
        #     output$compounds <- DT::renderDataTable({
        #         shinyWidgets::execute_safely({
        #             compoundTable(newExp(), select = which(!rowData(newExp())$use))
        #         })
        #     })
        # }

        w$hide()
    })



    # Event triggered when refreshing the page
    observeEvent(input$refresh, {
        # exp(NULL)
        IS_compounds(NULL)
        newExp(NULL)
        updateCheckboxInput(session, "filterISTD", value = TRUE)
        updateCheckboxInput(session, "filterSST", value = TRUE)
        updateCheckboxInput(session, "showOutliers", value = TRUE)
        updateSelectizeInput(session, "qc_change", choices = c(), selected = "")
    })


    observeEvent(input$createZip, {
        exp <- newExp()
        req(!is.null(exp))


        input$compounds_rows_selected
        input$aliquots_rows_selected

        rowData(exp)$use <- TRUE
        rowData(exp)$use[input$compounds_rows_selected] <- FALSE
        exp$use <- TRUE
        exp$use[input$aliquots_rows_selected] <- FALSE



        withProgress(message = "Generating output files...", {
            w$show()

            downloadZip(
                project = project(),
                exp = exp,
                summaryReport = as.logical(input$summary_report),
                compoundReport = as.logical(input$compound_report),
                summaryPlots = input$downloadPlotPicker,
                copyDataLake = input$copyDataLake,
                assays = input$downloadAssayPicker,
                backgroundPercent = input$backgroundSignal,
                cautionRSD = input$cautionRSD,
                nonReportableRSD = input$nonReportableRSD
            )

            w$hide()
            shinyjs::enable("download_zip")
            shinyjs::disable("createZip")
        })
    })




    # Event triggered when the download button is clicked
    output$download_zip <- downloadHandler(
        contentType = "application/zip",
        filename = "mzQuality.zip",
        content = function(file) {
            w$show()
            shinyjs::disable("download_zip")
            zipFolder(file, file.path(getwd(), project()))
            shinyjs::enable("createZip")
            w$hide()
        }
    )

    session$onSessionEnded(function() {
        session$close()
    })
}

#' @title Run mzQuality in the browser
#' @param browser Should the browser be started?
#' @param host Which host should be used? Defaults to "127.0.0.0"
#' @param port which port number should be hosted
#' @returns Shiny application in a browser on port 3838
#' @importFrom shiny shinyApp
#' @export
#' @examples
#' # Don't run
#' if (FALSE) {
#'     openDashboard()
#' }
openDashboard <- function(browser = TRUE, host = "0.0.0.0", port = 3838,
                          mount = NA, useLogin = FALSE) {
    # library(mzQuality)
    # library(shinyWidgets)
    # library(shinyjs)

    if (!is.na(mount)) {
        setwd(mount)
    }

    opts <- list(
        host = host,
        port = port,
        launch.browser = browser
    )

    shinyApp(
        options = opts,
        ui = ui,
        server = function(input, output, session) {
            server(input, output, session, useLogin)
        }
    )
}

#' @rdname openDashboard
#' @export
shiny <- openDashboard
