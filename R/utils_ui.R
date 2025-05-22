#' @title Create a waiter loading screen indicator
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom waiter Waiter transparent spin_loaders
#' @importFrom shiny div h4
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
