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
mod_bills_plot_server <- function(id, billing, fuel_){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$bills_plot <- plotly::renderPlotly({
      billing %>%
        dplyr::filter(fuel == fuel_) %>%
        ggplot2::ggplot(ggplot2::aes(x = ymd, y = value/100)) +
        ggplot2::geom_line(alpha = .5) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "GBP")
    })
  })
}

## To be copied in the UI
# mod_bills_plot_ui("bills_plot_ui_1")

## To be copied in the server
# mod_bills_plot_server("bills_plot_ui_1")
