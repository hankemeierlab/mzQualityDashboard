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
    # test <- experiment$type[n] %in% c("SQC", "BLANK")
    #
    # selected <- isolate(input$compounds_rows_selected)
    # if (is.null(selected)) {
    #   selected <- which(!rowData(isolate(exp()))$use)
    # }

    metadata(experiment)$QC <- isolate(input$qc_change)

    is <- unlist(lapply(1:nrow(experiment), function(i) input[[ paste0("sel", i) ]] ))
    if (all(!is.null(is))) {
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
      doAll = length(input$aliquots_rows_selected) == 0
    ) %>%
        calculateConcentrations(type = "ACAL") %>%
        addBatchCorrectionAssay(assay = "Concentration")

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
    currentInternalStandardTable(
      exp = exp()
    )
  })

  # # Modify Internal Standard table
  output$IsModifyTable <- DT::renderDataTable(server = TRUE, {
    # Force change when QC type has changed
    input$qc_change

    # Build the internal standard table
    internalStandardTable(
      input = isolate(input),
      exp = experiment,
      selected = rowData(experiment)$compound_is)

  })

  output$compounds <- DT::renderDataTable({
    compoundTable(exp(), select = which(!rowData(exp())$use))
  })

  # Combined Overall Table
  output$combined <- DT::renderDataTable(
      combinedTable(combined)
  )

  # Compound Details Table
  output$rowData <- DT::renderDataTable(
      rowDataTable(exp()[useCompounds(), ])
  )

  # Aliquot Details Table
  output$colData <- DT::renderDataTable(
      colDataTable(exp()[useCompounds(), ])
  )

  # Assay / Values Table
  output$assayData <- DT::renderDataTable(
      assayTable(input, exp()[useCompounds(), ])
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

  # Table of RT Shift
  output$shift <- DT::renderDataTable(
      rtShiftTable(input, exp()[useCompounds(), ])
  )

  # Table of Replicate RSDQCs
  output$replicates <- DT::renderDataTable(
      replicateTable(input, exp()[useCompounds(), ])
  )

  output$concentrationTable <- DT::renderDataTable(
      concentrationTable(input, exp()[useCompounds(), ])
  )


# Plots -------------------------------------------------------------------

  # Aliquot Plot
  output$sample_plot <- plotly::renderPlotly({
    renderAliquotPlot(input, exp()[useCompounds(), ])
  })

  # Compound Plot
  output$compound_plot <- plotly::renderPlotly(
      renderCompoundPlot(input, exp()[useCompounds(), ])
  )

  # QC Violin Plot
  output$badqc_plot <- plotly::renderPlotly(
      renderViolinPlot(input, exp()[useCompounds(), ])
  )

  # Principle Component Plot
  output$pca_plot <- plotly::renderPlotly(
      renderPcaPlot(input, exp()[useCompounds(), ])
  )

  # Batch Effect Box Plot
  output$batch_boxplot <- plotly::renderPlotly(
      renderBatchBoxPlot(input, exp()[useCompounds(), ])
  )

  # Heatmap Plot
  output$heatmap <- plotly::renderPlotly(
      renderHeatMapPlot(input, exp()[useCompounds(), ])
  )

  # Batch Assay Plot
  output$batchAssayplot <- plotly::renderPlotly(
      renderBatchAssayPlot(input, exp()[useCompounds(), ])
  )

  # RSDQC Heatmap Plot
  output$correlation_heatmap <- plotly::renderPlotly(
      renderRsdqcPlot(input, exp()[useCompounds(), ])
  )

  # (Academic) Calibration Plot
  output$calibration_plot <- plotly::renderPlotly(
      renderCalibrationPlot(input, exp()[useCompounds(), ])
  )

  # Relative Standard Deviation Plot
  output$rsd_plot <- plotly::renderPlotly(
      renderRsdPlot(input, exp()[useCompounds(), ])
  )



  output$concentrationPlot <- plotly::renderPlotly(
      renderConcentrationPlot(input, exp()[useCompounds(), ])
  )


# Events ------------------------------------------------------------------

  # Event when the submit button is clicked on the start screen
  observeEvent(input$submit, {
      w$show()
      combined <<- submitDataEvent(session, input)
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
          knowns <- assay(exp()[, exp()$type == type], "Concentration")

          choices <- rownames(knowns)[rowSums(knowns == 0) != ncol(knowns)]
          choices <- choices[choices %in% compounds]
          print(choices)
          print(knowns[choices, ])
          updateSelectizeInput(inputId = "concentrationCompound", choices = choices, selected = choices[1])
      }
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
      updateCheckboxInput(session, "filterISTD", value = TRUE)
      updateCheckboxInput(session, "filterSST", value = TRUE)
      updateCheckboxInput(session, "showOutliers", value = TRUE)
      updateSelectizeInput(session, "qc_change", choices = c(), selected = "")
  })

  observeEvent(input$CalTable, ignoreNULL = TRUE, ignoreInit = TRUE, {
    # DEFUNCT
  })

  # Event triggered when the download button is clicked
  output$download_zip <- downloadHandler(
      contentType = "application/zip",
      filename = "mzQuality.zip",
      content = function(x) {
          req(!is.null(exp))

          withProgress(message = "Generating output files...", {
              w$show()
              project <- ifelse(input$project == "", "mzQuality", input$project)
              zip <- downloadZip(project, exp(), x, experiment)
              w$hide()
              zip
          })
      }
  )
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
openDashboard <- function(browser = TRUE, host = "0.0.0.0", port = 3838) {
  library(mzQuality2)
  shinyApp(ui, server, options = list(
    host = host,
    port = port,
    launch.browser = browser
  ))
}
