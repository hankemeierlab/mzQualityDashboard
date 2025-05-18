#' @title UI for shiny app
#' @importFrom shiny h2 tags textInput selectizeInput actionButton includeCSS
#' fluidPage tabsetPanel tabPanel column selectInput p downloadButton icon
#' fileInput fluidRow h4 sliderInput tagList uiOutput checkboxInput
#' radioButtons
#' @importFrom shinydashboard dashboardHeader menuItem menuSubItem sidebarMenu
#' dashboardBody tabItems tabItem dashboardPage dashboardSidebar box
#' @importFrom waiter useWaiter autoWaiter spin_loaders
#' @importFrom shinyhelper helper
#' @noRd
ui <- function() {

  cssFile <- system.file("markup.css", package = "mzQualityDashboard")
  jsFile <- system.file("helpers.js", package = "mzQualityDashboard")
    dashboardPage(
        title = "mzQuality",
        header = dashboardHeader(title = "mzQuality"),
        sidebar = sidebar(),
        body = dashboardBody(
            includeCSS(cssFile),
            includeScript(jsFile),
            useShinyjs(),
            autoWaiter(
                color = transparent(0.7),
                fadeout = TRUE,
                html = div(
                    spin_loaders(8, color = "black")
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
                tabItem(tabName = "Aliquots_tab", aliquotPlotPage()),
                tabItem(tabName = "Compounds", compoundPlotPage()),
                tabItem(tabName = "PCA", pcaPlotPage()),
                tabItem(tabName = "ISheatmap", rsdqcPlotPage()),
                tabItem(tabName = "QCViolins", qcPlotPage()),
                tabItem(tabName = "concentrationPlot", concentrationPlotPage()),
                tabItem(tabName = "download", downloadPage())
            )
        )
    )
}
