#' @title Shiny server function
#' @param input Input variable for incoming UI requests
#' @param output Output variable for updating the UI
#' @importFrom S4Vectors metadata<-
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom waiter Waiter spin_flower transparent
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyhelper observe_helpers
#' @importFrom rhandsontable renderRHandsontable hot_table
#' rhandsontable hot_cols hot_to_r hot_col
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom shinyalert shinyalert
#' @importFrom shiny div h3 observeEvent updateSelectInput updateSelectizeInput
#' req updateTabsetPanel downloadHandler bindCache incProgress reactive
#' reactiveVal renderUI updateSliderInput withProgress showModal modalDialog
#' updateCheckboxInput
#' @importFrom DT renderDataTable
#' @noRd
server <- function(input, output, session) {


# Initialize Variables and Settings ---------------------------------------

  mbLimit <- 500
  options(shiny.maxRequestSize = mbLimit * 1024^2)
  observe_helpers(help_dir = system.file("helppages", package = "mzQuality"))
  w <- loadingScreen()

  # Set the total exp and subset `exp`
  combined <- data.frame()
  experiment <- SummarizedExperiment()
  rsdqcs <- data.frame()

# Experiment update -----------------------------------------------------

  exp <- reactive({

    # rows <- input$aliquots_rows_selected
    # n <- rows[length(rows)]
    # test <- experiment$Type[n] %in% c("SQC", "BLANK")
    #
    # selected <- isolate(input$compounds_rows_selected)
    # if (is.null(selected)) {
    #   selected <- which(!rowData(isolate(exp()))$Use)
    # }

    metadata(experiment)$QC <- input$qc_change

    is <- unlist(lapply(1:nrow(experiment), function(i) input[[ paste0("sel", i) ]] ))
    if (all(!is.null(is))) {
      experiment <- mzQuality2::replaceInternalStandards(experiment, is)
    }

    if (length(input$aliquots_rows_selected) > 0) {
      # Get the aliquots for the analysis
      aliquots <- colnames(experiment)[-input$aliquots_rows_selected]

      # Do analysis
      x <- doAnalysis(
        exp = experiment,
        aliquots = aliquots
      )
    } else {
      x <- experiment
    }
    # # Return x
    return(x)
  })

# Tables ------------------------------------------------------------------

  # Current Internal Standard table
  output$IsCurrentTable <- renderDataTable({
    currentInternalStandardTable(
      exp = exp()
    )
  })

  # # Modify Internal Standard table
  output$IsModifyTable <- renderDataTable(server = TRUE, {
    # Force change when QC type has changed
    input$qc_change

    # Build the internal standard table
    internalStandardTable(
      input = isolate(input),
      exp = experiment,
      selected = rowData(experiment)$Compound_is)

  })

  output$compounds <- renderDataTable({
    compoundTable(exp(), select = which(!rowData(exp())$Use))
  })

  # Combined Overall Table
  output$combined <- renderDataTable(
      combinedTable(combined)
  )

  # Compound Details Table
  output$rowData <- renderDataTable(
      rowDataTable(exp()[-input$compounds_rows_selected, ])
  )

  # Aliquot Details Table
  output$colData <- renderDataTable(
      colDataTable(exp()[-input$compounds_rows_selected, ])
  )

  # Assay / Values Table
  output$assay <- renderDataTable(
      assayTable(input, exp()[-input$compounds_rows_selected, ])
  )

  # Batch Effect Correction Factor Table
  output$batch_correction_table <- renderDataTable(
      batchCorrectionFactorTable(exp()[-input$compounds_rows_selected, ])
  )

  # Table of Model Effects, R2, etc.
  output$model_table <- renderDataTable(
      modelPropertyTable(input, exp()[-input$compounds_rows_selected, ])
  )

  # Table of Background effects
  output$effect_table <- renderDataTable(
      backGroundEffectTable(input, exp()[-input$compounds_rows_selected, ])
  )

  # Table of Carry Over Effect
  output$carryOverTable <- renderDataTable(
      carryOverTable(exp()[-input$compounds_rows_selected, ])
  )

  # Table of RT Shift
  output$shift <- renderDataTable(
      rtShiftTable(input, exp()[-input$compounds_rows_selected, ])
  )

  # Table of Replicate RSDQCs
  output$replicates <- renderDataTable(
      replicateTable(input, exp()[-input$compounds_rows_selected, ])
  )

# Plots -------------------------------------------------------------------

  # Aliquot Plot
  output$sample_plot <- renderPlotly(
      renderAliquotPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # Compound Plot
  output$compound_plot <- renderPlotly(
      renderCompoundPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # QC Violin Plot
  output$badqc_plot <- renderPlotly(
      renderViolinPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # Principle Component Plot
  output$pca_plot <- renderPlotly(
      renderPcaPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # Batch Effect Box Plot
  output$batch_boxplot <- renderPlotly(
      renderBatchBoxPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # Heatmap Plot
  output$heatmap <- renderPlotly(
      renderHeatMapPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # Batch Assay Plot
  output$batchAssayplot <- renderPlotly(
      renderBatchAssayPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # RSDQC Heatmap Plot
  output$correlation_heatmap <- renderPlotly(
      renderRsdqcPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # (Academic) Calibration Plot
  output$calibration_plot <- renderPlotly(
      renderCalibrationPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # Relative Standard Deviation Plot
  output$rsd_plot <- renderPlotly(
      renderRsdPlot(input, exp()[-input$compounds_rows_selected, ])
  )

  # Linear Model Plot, should look into for model creation
  output$LinearCalibration <- renderPlotly(
      renderModelPlot(input, exp()[-input$compounds_rows_selected, ])
  )

# Events ------------------------------------------------------------------

  # Event when the submit button is clicked on the start screen
  observeEvent(input$submit, {
      w$show()
      combined <<- submitDataEvent(session, input)

      experiment <<- buildExperimentEvent(session, input, combined)

      # # Aliquot Selection Table
      output$aliquots <- renderDataTable(
        aliquotTable(experiment)
      )

      updateInputs(session, experiment)

      updateSelectizeInput(session, "compound_metabolite", choices = rownames(experiment))
      updateSelectizeInput(session, "batchAssayCompound", choices = rownames(experiment))
      updateSelectizeInput(session, "calibration_compound", choices = rownames(experiment))

      updateTabsetPanel(session, inputId = "sidebar", "AM")
      w$hide()
  })


  # Event when a compound has been clicked / changed
  # Need to look up how this works with datatables
  observeEvent(input$compounds_rows_selected, {
      req(!is.null(exp()))

      compounds <- rownames(exp())[-input$compounds_rows_selected]

      updateSelectizeInput(session, "compound_metabolite", choices = compounds)
      updateSelectizeInput(session, "batchAssayCompound", choices = compounds)
      updateSelectizeInput(session, "calibration_compound", choices = compounds)
  })

  observeEvent(input$sidebar, {
    if (input$sidebar == "IS" & nrow(rsdqcs) == 0) {
      experiment <<- mzQuality2:::calculateCorrectedRSDQCs2(exp())
    }
  })


  # Event triggered when refreshing the page
  observeEvent(input$refresh, {
     # exp(NULL)
      IS_compounds(NULL)
      updateCheckboxInput(session, "useExamples", value = FALSE)
      updateCheckboxInput(session, "filterISTD", value = TRUE)
      updateCheckboxInput(session, "filterSST", value = TRUE)
      updateCheckboxInput(session, "showOutliers", value = TRUE)
      updateSelectizeInput(session, "qc_change", choices = c(), selected = "")
  })

  observeEvent(input$CalTable, ignoreNULL = TRUE, ignoreInit = TRUE, {
    # DEFUNCT
  })

  # Event triggered when files are selected
  observeEvent(input$files, {
    #exp(NULL)
    updateSelectizeInput(session, "qc_change", choices = c(), selected = "")
    updateCheckboxInput(session, "useExamples", value = FALSE)
  })

  # Event triggered when the download button is clicked
  output$download_zip <- downloadHandler(
      contentType = "application/zip",
      filename = "mzQuality.zip",
      content = downloadZip
  )
}

#' @title Run mzQuality in the browser
#' @param browser Should the browser be started?
#' @param host Which host should be used? Defaults to "127.0.0.0"
#' @param port which port number should be hosted
#' @returns Shiny application in a browser on port 3838
#' @importFrom pbapply pboptions
#' @importFrom shiny shinyApp
#' @export
#' @examples
#' # Don't run
#' if (FALSE) {
#'     openDashboard()
#' }
openDashboard <- function(browser = TRUE, host = "0.0.0.0", port = 3838) {
  library(mzQuality2)
  pboptions(type = "shiny", title = "Loading...")
  shinyApp(shiny.ui, server, options = list(
    host = host,
    port = port,
    launch.browser = browser
  ))
}
