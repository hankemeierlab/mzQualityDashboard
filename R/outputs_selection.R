outputAliquotSelectionTable <- function(output, aliquotDf) {
    output$aliquots <- DT::renderDataTable({

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

outputCompoundSelectionTable <- function(output, compoundDf) {
    output$compounds <- DT::renderDataTable({

        df <- compoundDf()
        req(is.data.frame(df))
        req(nrow(df) > 0)

        render <- renderTable(
            df = df,
            preSelect = which(!df$Use),
            scrollY = 600,
            selectable = TRUE
        ) %>%
            DT::formatPercentage(
                columns = c(
                    "Background Signal", "Found in Selected QC"
                ),
                digits = 2, dec.mark = "."
            ) %>%
            DT::formatRound(
                columns = c("RSDQC", "RSDQC Corrected"),
                dec.mark = ".",
                digits = 5
            )

        return(render)
    })
}

