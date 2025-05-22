#' @title Sidebar menu
#' @description
#' Creates the sidebar menu for the dashboard.
#' @details
#' @importFrom shinydashboard dashboardSidebar menuItem menuSubItem sidebarMenu
#' @importFrom shiny icon
#' @noRd
sidebar <- function() {
    icon <- icon("angles-right", verifa_fa = FALSE)
    dashboardSidebar(
        collapsed = FALSE,
        sidebarMenu(
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
                     icon = icon("shuffle")
            ),
            menuItem("Tables",
                     tabName = "reports", icon = icon("table-list"),
                     menuSubItem("Combined Data", "Combined", icon = icon),
                     menuSubItem("Aliquot Data", "ColData", icon = icon),
                     menuSubItem("Compound Data", "RowData", icon = icon),
                     menuSubItem("Assay Data", "Assays", icon = icon)
            ),
            menuItem("Plots",
                     tabName = "plots", id = "plot_id", icon = icon("image"),
                     menuSubItem("Aliquot", tabName = "Aliquots_tab", icon = icon),
                     menuSubItem("Compound", tabName = "Compounds", icon = icon),
                     menuSubItem("PCA Plot", tabName = "PCA", icon = icon),
                     menuSubItem("RSDQCs", tabName = "ISheatmap", icon = icon),
                     menuSubItem("QC Plot", tabName = "QCViolins", icon = icon),
                     menuSubItem("Concentrations", tabName = "concentrationPlot", icon = icon)
            ),
            menuItem("Download", tabName = "download", icon = icon("file-arrow-down"))
        )
    )
}
