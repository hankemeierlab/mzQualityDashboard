#' @title Create a modal shown while reports are being generated
#' @description
#' @details
#' @returns
#' @importFrom waiter spin_loaders
#' @importFrom shiny modalDialog div h4 p tags
#' @noRd
.generatingModal <- function() {
    style <- "display: flex; align-items: center; justify-content: center;"
    centered <- ".modal-dialog { display: flex; align-items: center;
        justify-content: center; min-height: calc(100vh - 60px); }
        .modal-dialog .modal-content { width: 100%; min-height: 220px;
        display: flex; flex-direction: column; justify-content: center; }"

    return(modalDialog(
        title = NULL,
        footer = NULL,
        easyClose = FALSE,
        tags$style(centered),
        div(
            style = style,
            spin_loaders(
                id = 8,
                color = "black",
                style = style
            )
        ),
        h4(
            style = "text-align: center;",
            "Generating reports, please wait..."
        ),
        p(
            style = "text-align: center;",
            "The download will be ready as soon as this is complete."
        )
    ))
}

#' @title Create a waiter loading screen indicator
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom waiter Waiter transparent spin_loaders
#' @importFrom shiny div h4
#' @noRd
.loadingScreen <- function() {
    style <- "display: flex; align-items: center; justify-content: center;"
    Waiter$new(
        color = transparent(0),
        hide_on_error = TRUE,
        fadeout = TRUE,
        html = div(
            style = style,
            waiter::spin_loaders(
                id = 8,
                color = "black",
                style = style
            )
        )
    )
}
