#' @title render the aliqut selection table
#' @description
#' @importFrom shiny req
#' @importFrom DT renderDataTable
#' @noRd
.outputAliquotSelectionTable <- function(output, aliquotDf) {
    output$aliquots <- renderDataTable({

        req(is.data.frame(aliquotDf()))
        req(nrow(aliquotDf()) > 0)

        df <- aliquotDf()

        renderTable(
            df = df,
            preSelect = which(!df$use),
            scrollY = 600,
            selectable = TRUE
        )
    })
}

#' @title render the compound selection table
#' @description
#' @importFrom shiny req
#' @importFrom DT renderDataTable formatPercentage formatRound
#' @noRd
.outputCompoundSelectionTable <- function(output, compoundDf) {
    output$compounds <- renderDataTable({

        df <- compoundDf()
        req(is.data.frame(df))
        req(nrow(df) > 0)

        render <- renderTable(
            df = df,
            preSelect = which(!df$Use),
            scrollY = 600,
            selectable = TRUE
        )

        render <- formatPercentage(
            table = render,
            columns = c(
                "Background Signal", "Found in Selected QC"
            ),
            digits = 2, dec.mark = "."
        )

        render <- formatRound(
            table = render,
            columns = c("RSDQC", "RSDQC Corrected"),
            dec.mark = ".",
            digits = 5
        )

        return(render)
    })
}

