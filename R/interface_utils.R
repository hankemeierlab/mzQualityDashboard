#' @title Template box for displaying tables
#' @description
#' @details
#' @returns
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
tableBox <- function(title, type, height = "80vh", helperMd = NULL) {
    return(box(
        title = title,
        width = NULL,
        collapsible = TRUE,
        solidHeader = TRUE,

        # Box items
        # Helper icon for help pages
        div(style = "height: 3vh;") %>%
            helper(content = helperMd, fade = TRUE, icon = "circle-question"),

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
plotBox <- function(title, type, height, helperMd = NULL) {
    return(box(
        title = title,
        solidHeader = TRUE,
        width = NULL,
        collapsible = TRUE,

        # Box items
        # Helper icon for help pages
        div(style = "height: 3vh;") %>%
            helper(content = helperMd, fade = TRUE, icon = "circle-question"),

        # Actual Plot
        plotly::plotlyOutput(type, height = height)
    ))
}

#' @title Box with controls for UI
#' @importFrom shinydashboard box
#' @importFrom shinyhelper helper
#' @noRd
controlsBox <- function(inputs, title = "Controls", width = NULL,
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

        # Box items
        # Helper icon for help pages
        div(style = "height: 3vh;") %>%
            helper(
                content = helperMd,
                fade = TRUE,
                icon = "circle-question"
            ),
        inputs
    )
}

