#' @title Template box for displaying tables
#' @description
#' @details
#' @returns
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom DT dataTableOutput
shiny.box_table <- function(title, type, height = "80vh") {

    return(box(
      title = title,
      width = NULL,
      collapsible = TRUE,
      solidHeader = TRUE,

        # Box items
        # Helper icon for help pages
        div(style = "height: 3vh;") %>%
          helper(content = type, fade = TRUE, icon = "circle-question"),

        # Actual Table
        DT::dataTableOutput(type, height = height)
    ))
}

#' @title Template box for displaying Plotly plots
#' @description
#' @details
#' @returns
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
shiny.box_plot <- function(title, type, height) {

  return(box(
    title = title,
    solidHeader = TRUE,
    width = NULL,
    collapsible = TRUE,

    # Box items
    # Helper icon for help pages
    div(style = "height: 3vh;") %>%
      helper(content = title, fade = TRUE, icon = "circle-question"),

    # Actual Plot
    plotlyOutput(type, height = height)
  ))
}

#' @title Box with controls for UI
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @noRd
shiny.box_controls <- function(inputs, title = "Controls", width = NULL,
                               height = NULL, footer = NULL,
                               helperMd = NULL) {

    if (!is.null(height)) {
      inputs <- div(
        style = sprintf("min-height: %s;", height),
        inputs
      )
    }

    shinydashboard::box(
      title = title,
      solidHeader = TRUE,
      width = NULL,
      footer = footer,


        # Box items
        # Helper icon for help pages
        div(style = "height: 3vh;") %>%
          helper(content = title, fade = TRUE, icon = "circle-question"),

        # Contents
        inputs
    )
}


sidebar <- function() {
    icon <- icon("angles-right")
    dashboardSidebar(sidebarMenu(
        id = "sidebar",
        menuItem("Create project",
            tabName = "home",
            icon = icon("file-arrow-up")
        ),
        menuItem("Select Data",
            tabName = "selectedData",
            icon = icon("circle-check")
        ),
        menuItem("Internal Standards",
                 tabName = "IS",
                 icon = icon("circle-check")
        ),
        menuItem("Tables", tabName = "reports", icon = icon("table-list"),
            menuSubItem("Combined Data", "Combined", icon = icon),
            menuSubItem("Aliquot Data", "ColData", icon = icon),
            menuSubItem("Compound Data", "RowData", icon = icon),
            menuSubItem("Assay Data", "Assays", icon = icon),
            menuSubItem("Concentrations", "concentrations_tab", icon = icon),
            menuSubItem("Model Info", "model_tab", icon = icon),
            menuSubItem("Carry-Over effect", "carryover_tab", icon = icon)
        ),
        menuItem("Plots", tabName = "plots", id = "plot_id", icon = icon("image"),
            menuSubItem("Heatmap", tabName = "Heatmap_tab", icon = icon),
            menuSubItem("Aliquot", tabName = "Aliquots_tab", icon = icon),
            menuSubItem("Compound", tabName = "Compounds", icon = icon),
            menuSubItem("Compound Per Batch", tabName = "batchAssay_tab", icon = icon),
            menuSubItem("PCA Plot", tabName = "PCA", icon = icon),
            menuSubItem("RSDQCs", tabName = "Correlation_heatmap", icon = icon),
            menuSubItem("QC Plot", tabName = "QCViolins", icon = icon),
            menuSubItem("Calibration Plot", tabName = "Calibrations", icon = icon),
            menuSubItem("Concentrations", tabName = "concentrationPlot", icon = icon)
        ),
        menuItem("Download", tabName = "download", icon = icon("file-arrow-down"))
    ))
}



#' @title UI for shiny app
#' @importFrom plotly plotlyOutput
#' @importFrom shiny h2 tags textInput selectizeInput actionButton includeCSS
#' fluidPage tabsetPanel tabPanel column selectInput p downloadButton icon
#' fileInput fluidRow h4 sliderInput tagList uiOutput checkboxInput
#' radioButtons
#' @importFrom shinydashboard dashboardHeader menuItem menuSubItem sidebarMenu
#' dashboardBody tabItems tabItem dashboardPage dashboardSidebar box
#' @importFrom waiter useWaiter autoWaiter spin_loaders
#' @importFrom shinyhelper helper
#' @importFrom rhandsontable rHandsontableOutput
#' @noRd
ui <- function() {
  header <- dashboardHeader(title = HTML("mzQuality<sup>2</sup>"))
  dashboardPage(
    title = "mzQuality",
    header = header,
    sidebar = sidebar(),
    body = dashboardBody(

      theme = bslib::bs_theme(version = 5, bootswatch = "slate"),
      tags$script(HTML("$('body').addClass('fixed');")),
      includeCSS(system.file("markup.css", package = "mzQualityDashboard")),

      useWaiter(),
      autoWaiter(
        color = transparent(0.7),
        fadeout = TRUE,
        html = div(
          spin_loaders(8, color = "black"),
          h4("Loading..."),
          style = "position: absolute;"
        )
      ),
      tabItems(
        tabItem(tabName = "home", homePage()),
        tabItem(tabName = "selectedData", selectDataPage()),
        tabItem(tabName = "IS", internalStandardPage()),
        tabItem(tabName = "Combined", combinedTablePage()),
        tabItem(tabName = "ColData", sampleTablePage()),
        tabItem(tabName = "RowData", compoundTablePage()),
        tabItem(tabName = "Assays", assayTablePage()),
        tabItem(tabName = "qcTable", qcTablePlot()),
        tabItem(tabName = "concentrations_tab", concentrationTablePage()),
        tabItem(tabName = "model_tab", modelTablePage()),
        tabItem(tabName = "Heatmap_tab", heatmapPlotPage()),
        tabItem(tabName = "Aliquots_tab", aliquotPlotPage()),
        tabItem(tabName = "Compounds", compoundPlotPage()),
        tabItem(tabName = "batchAssay_tab", compoundPerBatchPage()),
        tabItem(tabName = "PCA", pcaPlotPage()),
        tabItem(tabName = "Correlation_heatmap", rsdqcPlotPage()),
        tabItem(tabName = "QCViolins", qcPlotPage()),
        tabItem(tabName = "Calibrations", calibrationPlotPage()),
        tabItem(tabName = "concentrationPlot", concentrationPlotPage()),
        tabItem(tabName = "download", downloadPage())
      )
    )
  )
}
