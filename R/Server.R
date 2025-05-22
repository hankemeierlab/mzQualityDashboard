#' @title Shiny server function
#' @param input Input variable for incoming UI requests
#' @param output Output variable for updating the UI
#' @param session Session variable for managing the Shiny app
#' @importFrom shinyhelper observe_helpers
#' @importFrom shiny reactiveVal
#' @importFrom shinyjs hide show runjs removeClass addClass enable disable
#' @importFrom shinyWidgets execute_safely
#' @importFrom plotly renderPlotly
#' @importFrom DT renderDataTable
#' @importFrom mzQuality identifyMisInjections doAnalysis expToCombined
#' @importFrom zip zip
#' @noRd
server <- function(input, output, session) {

    # Ensure that users can upload large files, i.e. for large studies
    options(shiny.maxRequestSize = Inf)

    # Load the help pages
    observe_helpers(
        help_dir = system.file("helppages", package = "mzQualityDashboard")
    )

    # Set the reactive values that are needed between the pages
    experiment <- reactiveVal()
    aliquotDf <- reactiveVal()
    compoundDf <- reactiveVal()
    internalStandards <- reactiveVal()

    # Controllers for separating pages and their observers & outputs
    .homepageController(session, input, output, aliquotDf, experiment)

    .selectionController(
        input, output, aliquotDf, compoundDf, internalStandards, experiment
    )

    .tablePagesController(input, output, experiment)

    .plotPagesController(session, input, output, experiment)

    .exportPageController(input, output, experiment)
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

    opts <- list(
        host = host,
        port = port,
        launch.browser = browser
    )

    shinyApp(
        options = opts,
        ui = ui,
        server = server
    )
}
