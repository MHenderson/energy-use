#' bills_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bills_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("bills_plot"))
  )
}

#' bills_plot Server Functions
#'
#' @noRd
mod_bills_plot_server <- function(id, bills){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    X <- bills %>%
      dplyr::filter(bill_id != 4, fuel == "gas", date > "2019-07-01") %>%
      dplyr::mutate(
        actual_bill_s = sprintf("£%.2f", actual_bill)
      )

    output$bills_plot <- plotly::renderPlotly({
      X %>%
        ggplot2::ggplot(ggplot2::aes(date, actual_bill)) +
        ggplot2::geom_col(ggplot2::aes(fill = supplier)) +
        ggplot2::geom_text(ggplot2::aes(label = actual_bill_s), nudge_y = 5) +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = "", y = "GBP")
    })
  })
}

## To be copied in the UI
# mod_bills_plot_ui("bills_plot_ui_1")

## To be copied in the server
# mod_bills_plot_server("bills_plot_ui_1")
