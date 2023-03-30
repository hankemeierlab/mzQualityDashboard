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
    )

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
  output$aliquots <- renderDataTable(server = TRUE, {
    metadata(experiment)$QC <- input$qc_change
    experiment <- identifyOutliers(experiment)
    aliquotTable(experiment)
  })



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
      rowDataTable(exp()[useCompounds(), ])
  )

  # Aliquot Details Table
  output$colData <- renderDataTable(
      colDataTable(exp()[useCompounds(), ])
  )

  # Assay / Values Table
  output$assay <- renderDataTable(
      assayTable(input, exp()[useCompounds(), ])
  )

  # Batch Effect Correction Factor Table
  output$batch_correction_table <- renderDataTable(
      batchCorrectionFactorTable(exp()[useCompounds(), ])
  )

  # Table of Model Effects, R2, etc.
  output$model_table <- renderDataTable(
      modelPropertyTable(input, exp()[useCompounds(), ])
  )

  # Table of Background effects
  output$effect_table <- renderDataTable(
      backGroundEffectTable(input, exp()[useCompounds(), ])
  )

  # Table of Carry Over Effect
  output$carryOverTable <- renderDataTable(
      carryOverTable(exp()[useCompounds(), ])
  )

  # Table of RT Shift
  output$shift <- renderDataTable(
      rtShiftTable(input, exp()[useCompounds(), ])
  )

  # Table of Replicate RSDQCs
  output$replicates <- renderDataTable(
      replicateTable(input, exp()[useCompounds(), ])
  )


# Plots -------------------------------------------------------------------

  # Aliquot Plot
  output$sample_plot <- renderPlotly({
    renderAliquotPlot(input, exp()[useCompounds(), ])
  })

  # Compound Plot
  output$compound_plot <- renderPlotly(
      renderCompoundPlot(input, exp()[useCompounds(), ])
  )

  # QC Violin Plot
  output$badqc_plot <- renderPlotly(
      renderViolinPlot(input, exp()[useCompounds(), ])
  )

  # Principle Component Plot
  output$pca_plot <- renderPlotly(
      renderPcaPlot(input, exp()[useCompounds(), ])
  )

  # Batch Effect Box Plot
  output$batch_boxplot <- renderPlotly(
      renderBatchBoxPlot(input, exp()[useCompounds(), ])
  )

  # Heatmap Plot
  output$heatmap <- renderPlotly(
      renderHeatMapPlot(input, exp()[useCompounds(), ])
  )

  # Batch Assay Plot
  output$batchAssayplot <- renderPlotly(
      renderBatchAssayPlot(input, exp()[useCompounds(), ])
  )

  # RSDQC Heatmap Plot
  output$correlation_heatmap <- renderPlotly(
      renderRsdqcPlot(input, exp()[useCompounds(), ])
  )

  # (Academic) Calibration Plot
  output$calibration_plot <- renderPlotly(
      renderCalibrationPlot(input, exp()[useCompounds(), ])
  )

  # Relative Standard Deviation Plot
  output$rsd_plot <- renderPlotly(
      renderRsdPlot(input, exp()[useCompounds(), ])
  )


  # Linear Model Plot, should look into for model creation
  observe({
    if (input$sidebar == "concentrationPlot") {
      exp <- exp()[useCompounds(), ]
      req("Concentration" %in% assayNames(exp) & !is.null(exp))


      compound <- input$concentrationCompound
      calType <- input$concentrationModel
      batch <- input$concentrationBatch

      # modifiers <- input$concentrationAdjustment
      # print(modifiers)
      # if (length(modifiers) == 0){
      #   modifiers <- c()
      # }

      model <- setCompoundModel(
        exp = exp,
        assay = input$concentrationAssay,
        compound = compound,
        batch = batch,
        weighted = FALSE, #as.logical(input$concentrationWeighted),
        type = calType,
        modifiers = input$concentrationAdjustment,
        removeOutliers = as.logical(input$concentrationOutliers)
      )

      concentrations <- getCompoundConcentrations(
        exp = exp,
        model = model,
        batch = batch,
        compound = compound,
        assay = input$concentrationAssay,
        type = calType
      )

      m <- as.matrix(concentrations)
      assay(exp[compound, colnames(concentrations)], "Concentration", withDimnames = FALSE) <- m


      cals <- exp$Type == calType
      subset <- exp[compound, exp$Batch == batch & (exp$Type %in% "SAMPLE" | cals)]

      ratios <- as.vector(assay(subset, "Ratio"))
      conc <- as.vector(assay(subset, "Concentration"))


      idx <- as.integer(rownames(model$model))
      ratios[idx] <- model$model$Assay[idx]

      df <- data.frame(
        Aliquot = colnames(subset),
        Concentration = round(conc, 4),
        Ratio = round(ratios, 4),
        Type = subset$Type,
        Calno = subset$Calno
      )


      table <- datatable(df,
        escape = FALSE,
        extensions = c("Scroller"),
        options = list(
          scrollY = 600,
          scroller = TRUE
        )
      )

      output$ConcentrationTable <- renderDataTable(table)
      output$LinearCalibration <- renderPlotly(renderModelPlot(df, model, compound))

    }
  })



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

      cals <- !is.na(exp()$Calno)
      if (!is.null(metadata(exp())$concentration)) {
          a <- rowSums(is.na(assay(exp()[, cals], "Concentration"))) != sum(cals)
          choices <- names(which(a))
          choices <- choices[choices %in% compounds]
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
      content = function(x) downloadZip(x, experiment)
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
  shinyApp(ui, server, options = list(
    host = host,
    port = port,
    launch.browser = browser
  ))
}
