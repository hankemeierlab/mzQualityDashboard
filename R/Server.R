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
  exp <- SummarizedExperiment()

# Tables ------------------------------------------------------------------

  # # Aliquot Selection Table
  # output$aliquots <- renderRHandsontable(
  #     aliquotTable(exp)
  # )
  #
  # # Compound Selection Table
  # output$compounds <- renderRHandsontable(
  #     compoundTable(exp)
  # )

  # Combined Overall Table
  output$combined <- renderDataTable(
      combinedTable(exp)
  )

  # Compound Details Table
  output$rowData <- renderDataTable(
      rowDataTable(exp)
  )

  # Aliquot Details Table
  output$colData <- renderDataTable(
      colDataTable(exp)
  )

  # Assay / Values Table
  output$assay <- renderDataTable(
      assayTable(input, exp)
  )

  # Batch Effect Correction Factor Table
  output$batch_correction_table <- renderDataTable(
      batchCorrectionFactorTable(exp)
  )

  # Table of Model Effects, R2, etc.
  output$model_table <- renderDataTable(
      modelPropertyTable(input, exp)
  )

  # Table of Background effects
  output$effect_table <- renderDataTable(
      backGroundEffectTable(input, exp)
  )

  # Table of Carry Over Effect
  output$carryOverTable <- renderDataTable(
      carryOverTable(exp)
  )

  # Table of RT Shift
  output$shift <- renderDataTable(
      rtShiftTable(input, exp)
  )

  # Table of Replicate RSDQCs
  output$replicates <- renderDataTable(
      replicateTable(input, exp)
  )

# Plots -------------------------------------------------------------------

  # Aliquot Plot
  output$sample_plot <- renderPlotly(
      renderAliquotPlot(input, exp)
  )

  # Compound Plot
  output$compound_plot <- renderPlotly(
      renderCompoundPlot(input, exp)
  )

  # QC Violin Plot
  output$badqc_plot <- renderPlotly(
      renderViolinPlot(input, exp)
  )

  # Principle Component Plot
  output$pca_plot <- renderPlotly(
      renderPcaPlot(input, exp)
  )

  # Batch Effect Box Plot
  output$batch_boxplot <- renderPlotly(
      renderBatchBoxPlot(input, exp)
  )

  # Heatmap Plot
  output$heatmap <- renderPlotly(
      renderHeatMapPlot(input, exp)
  )

  # Batch Assay Plot
  output$batchAssayplot <- renderPlotly(
      renderBatchAssayPlot(input, exp)
  )

  # RSDQC Heatmap Plot
  output$correlation_heatmap <- renderPlotly(
      renderRsdqcPlot(input, exp)
  )

  # (Academic) Calibration Plot
  output$calibration_plot <- renderPlotly(
      renderCalibrationPlot(input, exp)
  )

  # Relative Standard Deviation Plot
  output$rsd_plot <- renderPlotly(
      renderRsdPlot(input, exp)
  )

  # Linear Model Plot, should look into for model creation
  output$LinearCalibration <- renderPlotly(
      renderModelPlot(input, exp)
  )

# Events ------------------------------------------------------------------

  # Event when the submit button is clicked on the start screen
  observeEvent(input$submit, {
      w$show()
      exp <<- submitDataEvent(session, input)

      updateInputs(session, exp)

      # Aliquot Selection Table
      output$aliquots <- renderDataTable(server = FALSE,
          aliquotTable(exp)
      )

      # Compound Selection Table
      output$compounds <- renderDataTable(
          compoundTable(exp)
      )

      output$IStable <- renderDataTable(
          internalStandardTable(input, exp)
      )

      updateTabsetPanel(session, inputId = "sidebar", "AM")
      w$hide()
  })



  observeEvent(input$aliquots_rows_selected, {
      req(!is.null(exp))

      output$compounds <- renderDataTable({
          aliquots <- colnames(exp)
          if (!is.null(input$aliquots_rows_selected)) {
              aliquots <- aliquots[-input$aliquots_rows_selected]
          }
          exp <- doAnalysis(exp, aliquots = aliquots)

          select <- unique(c(!rowData(exp)$Use, isolate(input$compounds_rows_selected)))

          compoundTable(exp, select = select)
      })
  })

  # Event when a compound has been clicked / changed
  # Need to look up how this works with datatables
  observeEvent(input$compounds, {
      req(!is.null(exp))

      # # Update tables whenever needed, update exp accordingly
      # # Hopefully this function does not retrigger..
      #exp <<- compoundTableClick(input, exp)
      #
      # comps <- rownames(exp())
      #
      # updateSelectizeInput(session, "compound_metabolite", choices = comps)
      # updateSelectizeInput(session, "batchAssayCompound", choices = comps)
      # updateSelectizeInput(session, "calibration_compound", choices = comps)

  })

  # Event triggered when the QC has been changed
  # observeEvent(input$qc_change, {
  #     exp(qcChangeEvent(input, exp()))
  # })


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
