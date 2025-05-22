#' @title UI for shiny app
#' @importFrom shiny includeCSS includeScript div
#' @importFrom shinydashboard dashboardHeader dashboardBody tabItems tabItem
#' dashboardPage
#' @importFrom waiter autoWaiter transparent spin_loaders
#' @importFrom shinyjs useShinyjs
#' @noRd
ui <- function() {

    cssFile <- system.file("markup.css", package = "mzQualityDashboard")
    jsFile <- system.file("helpers.js", package = "mzQualityDashboard")
    dashboardPage(
        title = "mzQuality",
        header = dashboardHeader(title = "mzQuality"),
        sidebar = .sidebar(),
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
                tabItem(tabName = "homePage", .homePage()),
                tabItem(tabName = "selectedData", .selectDataPage()),
                tabItem(tabName = "internalStandards", .internalStandardPage()),
                tabItem(tabName = "Combined", .combinedTablePage()),
                tabItem(tabName = "ColData", .sampleTablePage()),
                tabItem(tabName = "RowData", .compoundTablePage()),
                tabItem(tabName = "Assays", .assayTablePage()),
                tabItem(tabName = "Aliquots_tab", .aliquotPlotPage()),
                tabItem(tabName = "Compounds", .compoundPlotPage()),
                tabItem(tabName = "PCA", .pcaPlotPage()),
                tabItem(tabName = "rsdqcPage", .rsdqcPlotPage()),
                tabItem(tabName = "QCViolins", .qcPlotPage()),
                tabItem(
                    tabName = "concentrationPlot",
                    .concentrationPlotPage()
                ),
                tabItem(tabName = "download", .downloadPage())
            )
        )
    )
}
