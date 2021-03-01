#' annual_cost_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_annual_cost_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("annual_cost_plot"))
  )
}

#' annual_cost_plot Server Functions
#'
#' @noRd
mod_annual_cost_plot_server <- function(id, annual_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    X <- annual_summary %>%
      dplyr::mutate(GBP = round(value/100, 2)) %>%
      dplyr::mutate(
        GBP = round(value/100, 2),
        GBP_s = sprintf("£%.2f", GBP)
      )
    output$annual_cost_plot <- plotly::renderPlotly({
     X %>%
        ggplot2::ggplot(ggplot2::aes(x = year, y = GBP)) +
        ggplot2::geom_bar(stat = "identity", width = 1) +
        ggplot2::geom_text(ggplot2::aes(label = GBP_s), nudge_y = 30) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "GBP") +
        ggplot2::ylim(c(0, 400)) +
        ggplot2::facet_wrap(~ fuel, ncol = 1, scales = "free_y")
    })
  })
}

## To be copied in the UI
# mod_annual_cost_plot_ui("annual_cost_plot_ui_1")

## To be copied in the server
# mod_annual_cost_plot_server("annual_cost_plot_ui_1")
