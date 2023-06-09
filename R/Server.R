# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
    user = c("user1", "user"),
    password = sapply(c("pass1", "mslab"), sodium::password_store),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
)

#' @title Shiny server function
#' @param input Input variable for incoming UI requests
#' @param output Output variable for updating the UI
#' @importFrom waiter Waiter spin_flower transparent
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyhelper observe_helpers
#' @importFrom shiny div h3 observeEvent updateSelectInput updateSelectizeInput
#' req updateTabsetPanel downloadHandler bindCache incProgress reactive
#' reactiveVal renderUI updateSliderInput withProgress showModal modalDialog
#' updateCheckboxInput
#' @noRd
server <- function(input, output, session) {

    if (!is.na(Sys.getenv("DashboardMount", unset = NA))) {
        setwd(Sys.getenv("DashboardMount"))
    }

    if (Sys.getenv("useLogin")) {

        shinyjs::hide("menuHidden")

        # login status and info will be managed by shinyauthr module and stores here
        credentials <- callModule(shinyauthr::login, "login",
                                  data = user_base,
                                  user_col = user,
                                  pwd_col = password,
                                  sodium_hashed = TRUE,
                                  log_out = reactive(logout_init()))

        # logout status managed by shinyauthr module and stored here
        logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))

        # this opens or closes the sidebar on login/logout
        observe({
            if(credentials()$user_auth) {
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

  mbLimit <- 1000
  options(shiny.maxRequestSize = mbLimit * 1024^2)
  observe_helpers(help_dir = system.file("helppages", package = "mzQuality2"))
  w <- loadingScreen()

  # Set the total exp and subset `exp`
  combined <- data.frame()
  experiment <- SummarizedExperiment()
  rsdqcs <- data.frame()
  project <- reactiveVal()
  fs <- mzQuality2::getFileSystem(connection = "remote")

# DataLake ----------------------------------------------------------------

  if (as.logical(Sys.getenv("useDataLake", unset = FALSE))) {
    shinyjs::enable("dataLakeBuckets")
    updateSelectizeInput(inputId = "dataLakeBuckets", choices = c("", fs$ls()))
  }

  observeEvent(input$dataLakeBuckets, {
    req(as.logical(Sys.getenv("useDataLake", unset = FALSE)) & input$dataLakeBuckets != "")

    fs <<- mzQuality2::setFolder(folder = fs, projectName = "", bucket = input$dataLakeBuckets)
    updateSelectizeInput(inputId = "dataLakeProjects", choices = c("", fs$ls()))
    shinyjs::enable("dataLakeProjects")
  })

  observeEvent(input$dataLakeProjects, {
    req(as.logical(Sys.getenv("useDataLake", unset = FALSE)) & input$dataLakeProjects != "")

    fs <<- fs$cd(input$dataLakeProjects)$cd("Exports")
    exports <- grep("mzQuality", fs$ls(), value = TRUE)
    updateSelectizeInput(inputId = "dataLakeExports", choices = c("", exports))
    shinyjs::enable("dataLakeExports")

  })




# Experiment update -----------------------------------------------------

  exp <- reactive({



    metadata(experiment)$QC <- isolate(input$qc_change)

    is <- unlist(lapply(1:nrow(experiment), function(i) input[[ paste0("sel", i) ]] ))

    replacedIS <- FALSE
    if (all(!is.null(is)) & metadata(experiment)$hasIS) {
      replacedIS <- TRUE
      experiment <- replaceInternalStandards(experiment, is)
    }

    aliquots <- colnames(experiment)
    if (length(input$aliquots_rows_selected) > 0) {
      # Get the aliquots for the analysis
      aliquots <- aliquots[-input$aliquots_rows_selected]
    }

    # Do analysis
    x <- doAnalysis(
      exp = experiment,
      aliquots = aliquots,
      doAll = length(input$aliquots_rows_selected) == 0 | replacedIS
    )

    if ("concentration" %in% assayNames(x)) {
        x <- x %>%
            calculateConcentrations(type = "ACAL") %>%
            addBatchCorrectionAssay(assay = "concentration")
    }

    updateInputs(session, x)

    # # Return x
    return(x)
  })

  useCompounds <- reactive({
    x <- isolate(exp())
    selected <- input$compounds_rows_selected
    if (all(is.null(selected))) {
      comps <- rownames(x)
    } else {
      comps <- rownames(x)[-selected]
    }
    return(comps)
  })

# Tables ------------------------------------------------------------------

  # # Aliquot Selection Table
  output$aliquots <- DT::renderDataTable(server = TRUE, {
    metadata(experiment)$QC <- input$qc_change
    experiment <- identifyOutliers(experiment)
    aliquotTable(experiment)
  })



  # Current Internal Standard table
  output$IsCurrentTable <- DT::renderDataTable({
    req(metadata(exp())$hasIS)
    currentInternalStandardTable(
      exp = isolate(exp())
    )
  })



  # # Modify Internal Standard table
  output$IsModifyTable <- DT::renderDataTable(server = FALSE, {


    # Force change when QC type has changed
    input$qc_change

    # Build the internal standard table
    df <- internalStandardTable(
      input = input,
      exp = isolate(exp()),
      selected = rowData(experiment)$compound_is)

    render <- renderISTable(df, c(1, 2, 4))
    DT::formatRound(render, columns = c("Original RSDQC Corrected", "Suggested RSDQC Corrected"), dec.mark = ".", digits = 5)
  })

  output$compounds <- DT::renderDataTable({
    shinyWidgets::execute_safely({

        compounds <- rownames(exp())

        updateSelectizeInput(session, "compound_metabolite", choices = compounds, server = TRUE)
        updateSelectizeInput(session, "batchAssayCompound", choices = compounds, server = TRUE)
        updateSelectizeInput(session, "calibration_compound", choices = compounds, server = TRUE)

      compoundTable(exp(), select = which(!rowData(exp())$use))
    }, title = "Table Viewer Failed", message = "Could not create the table")

  })

  # Combined Overall Table
  output$combined <- DT::renderDataTable(
    shinyWidgets::execute_safely({
      combinedTable(combined)
    }, title = "Table Viewer Failed", message = "Could not create the table")

  )

  # Compound Details Table
  output$rowData <- DT::renderDataTable(
    shinyWidgets::execute_safely({
      rowDataTable(exp()[useCompounds(), ])
    }, title = "Table Viewer Failed", message = "Could not create the table")

  )

  # Aliquot Details Table
  output$colData <- DT::renderDataTable(
    shinyWidgets::execute_safely({
      colDataTable(exp()[useCompounds(), ])
    }, title = "Table Viewer Failed", message = "Could not create the table")

  )

  # Assay / Values Table
  output$assayData <- DT::renderDataTable(
    shinyWidgets::execute_safely({
      assayTable(input, exp()[useCompounds(), ])
    }, title = "Table Viewer Failed", message = "Could not create the table")

  )

  # Batch Effect Correction Factor Table
  output$batch_correction_table <- DT::renderDataTable(
      batchCorrectionFactorTable(exp()[useCompounds(), ])
  )

  # Table of Model Effects, R2, etc.
  output$model_table <- DT::renderDataTable(
      modelTable(input, exp()[useCompounds(), ])
  )

  # Table of Background effects
  output$effect_table <- DT::renderDataTable(
      backGroundEffectTable(input, exp()[useCompounds(), ])
  )

  # Table of Carry Over Effect
  output$carryOverTable <- DT::renderDataTable(
      carryOverTable(exp()[useCompounds(), ])
  )

  output$concentrationTable <- DT::renderDataTable(
    shinyWidgets::execute_safely({
      concentrationTable(input, exp()[useCompounds(), ])
    }, title = "Table Viewer Failed", message = "Could not create the table")

  )


# Plots -------------------------------------------------------------------

  # Aliquot Plot
  output$sample_plot <- plotly::renderPlotly({
    shinyWidgets::execute_safely({

      renderAliquotPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")
  })

  # Compound Plot
  output$compound_plot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderCompoundPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")

  )

  # QC Violin Plot
  output$badqc_plot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderViolinPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")

  )

  # Principle Component Plot
  output$pca_plot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderPcaPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")

  )

  # Batch Effect Box Plot
  output$batch_boxplot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderBatchBoxPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")
  )

  # Heatmap Plot
  output$heatmap <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderHeatMapPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")
  )

  # Batch Assay Plot
  output$batchAssayplot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderBatchAssayPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")
  )

  # RSDQC Heatmap Plot
  output$correlation_heatmap <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderRsdqcPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")
  )

  # (Academic) Calibration Plot
  output$calibration_plot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderCalibrationPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")
  )

  # Relative Standard Deviation Plot
  output$rsd_plot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderRsdPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot")
  )

  output$concentrationPlot <- plotly::renderPlotly(
    shinyWidgets::execute_safely({
      renderConcentrationPlot(input, exp()[useCompounds(), ])
    }, title = "Plot Failed", message = "Could not create the plot (No concentrations provided?)", include_error = FALSE)
  )


# Events ------------------------------------------------------------------



  # Event when the submit button is clicked on the start screen
  observeEvent(input$submit, {
    dataLakeTest <- input$dataLakeProjects != "" && input$dataLakeExports != ""
    localFileTest <- length(input$files$datapath) > 0
    req(localFileTest | dataLakeTest)
    w$show()


    proj <- ifelse(input$project == "", "mzQuality", input$project)
    timeFormat <- format(lubridate::now(), "%d-%m-%Y_%H.%M.%S")
    project(glue::glue("{project}_{timeFormat}", project = proj, timeFormat = timeFormat))

    if (dataLakeTest) {
      combined <<- buildCombined(fs$path(input$dataLakeExports))
    } else {
      shinyWidgets::execute_safely({
        combined <<- submitDataEvent(session, input)
      }, message = "
        A problem occured with your data. Ensure that your aliquots are named correctly.
        If this problem still occurs, contact the developer"
      )
    }

    experiment <<- buildExperimentEvent(session, input, combined)

    updateInputs(session, experiment)
    updateTabsetPanel(session, inputId = "sidebar", "selectedData")
    w$hide()
  })


  # Event when a compound has been clicked / changed
  # Need to look up how this works with datatables
  observeEvent(input$compounds_rows_selected, {
      req(!is.null(exp()))

      compounds <- useCompounds()

      updateSelectizeInput(session, "compound_metabolite", choices = compounds, server = TRUE)
      updateSelectizeInput(session, "batchAssayCompound", choices = compounds, server = TRUE)
      updateSelectizeInput(session, "calibration_compound", choices = compounds, server = TRUE)


      type <- metadata(exp())$concentration
      if (!is.null(type)) {
          knowns <- assay(exp()[, exp()$type == type], "concentration")

          choices <- rownames(knowns)[rowSums(knowns == 0) != ncol(knowns)]
          choices <- choices[choices %in% compounds]
          updateSelectizeInput(inputId = "concentrationCompound", choices = choices, selected = choices[1])
      }
  })



  # Event triggered when refreshing the page
  observeEvent(input$refresh, {
     # exp(NULL)
      IS_compounds(NULL)
      updateCheckboxInput(session, "filterISTD", value = TRUE)
      updateCheckboxInput(session, "filterSST", value = TRUE)
      updateCheckboxInput(session, "showOutliers", value = TRUE)
      updateSelectizeInput(session, "qc_change", choices = c(), selected = "")
  })


  observeEvent(input$createZip, {
      req(!is.null(exp()))


      withProgress(message = "Generating output files...", {
          w$show()

          downloadZip(
              project = project(),
              exp = exp(),
              fullExp = experiment,
              summaryReport = as.logical(input$summary_report),
              compoundReport = as.logical(input$compound_report),
              summaryPlots = input$downloadPlotPicker,
              copyDataLake = input$copyDataLake,
              assays = input$downloadAssayPicker
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
          shinyjs::enable("download_zip")
          shinyjs::disable("createZip")
          w$hide()

      }
  )

  session$onSessionEnded(function() {
      shiny::stopApp()
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
                          mount = NA, useLogin = FALSE, useDataLake = FALSE) {

  if (!is.na(mount)) {
    Sys.setenv("DashboardMount" = mount)
  }

  Sys.setenv("useDataLake" = useDataLake)
  Sys.setenv("useLogin" = useLogin)

  library(mzQuality2)
  shinyApp(ui, server, options = list(
    host = host,
    port = port,
    launch.browser = browser
  ))
}

#' @rdname openDashboard
#' @export
shiny <- openDashboard
