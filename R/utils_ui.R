#' @title
#' @description
#' @details
#' @returns
#' @param exp
#' @importFrom waiter Waiter transparent spin_loaders
#' @importFrom shiny div h4
loadingScreen <- function() {
    Waiter$new(
        color = transparent(0),
        hide_on_error = TRUE,
        fadeout = TRUE,
        html = div(
            style = "display: flex; align-items: center; justify-content: center;",
            waiter::spin_loaders(8, color = "black", style = "display: flex; align-items: center; justify-content: center;")
        )
    )
}
