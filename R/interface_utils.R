#' @title Template box for displaying tables
#' @description
#' @details
#' @returns
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom DT dataTableOutput
#' @importFrom shiny div
#' @noRd
.tableBox <- function(title, type, height = "80vh", helperMd = NULL) {
    return(box(
        title = title,
        width = NULL,
        collapsible = TRUE,
        solidHeader = TRUE,

        # Box items
        # Helper icon for help pages
        
        helper(
            div(style = "height: 3vh;"),
            content = helperMd, 
            fade = TRUE, 
            icon = "circle-question"
        ),

        # Actual Table
        dataTableOutput(type, height = height)
    ))
}

#' @title Template box for displaying Plotly plots
#' @description
#' @details
#' @returns
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom plotly plotlyOutput
#' @importFrom shiny div
#' @noRd
.plotBox <- function(title, type, height, helperMd = NULL) {
    return(box(
        title = title,
        solidHeader = TRUE,
        width = NULL,
        collapsible = TRUE,

        helper(
            div(style = "height: 3vh;"),
            content = helperMd, 
            fade = TRUE, 
            icon = "circle-question"
        ),

        # Actual Plot
        plotlyOutput(type, height = height)
    ))
}

#' @title Box with controls for UI
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @importFrom shiny div
#' @noRd
.controlsBox <- function(inputs, title = "Controls", width = NULL,
                        height = NULL, footer = NULL,
                        helperMd = NULL, collapsible = FALSE) {
                            
    if (!is.null(height)) {
        inputs <- div(
            style = sprintf("min-height: %s;", height),
            inputs
        )
    }

    box(
        title = title,
        solidHeader = TRUE,
        width = NULL,
        footer = footer,
        collapsible = collapsible,
        collapsed = collapsible,

        helper(
            div(style = "height: 3vh;"),
            content = helperMd,
            fade = TRUE,
            icon = "circle-question"
        ),
        inputs
    )
}

