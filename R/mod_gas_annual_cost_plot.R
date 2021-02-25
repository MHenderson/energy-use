#' gas_annual_cost_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gas_annual_cost_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("gas_annual_cost_plot"))
  )
}

#' gas_annual_cost_plot Server Functions
#'
#' @noRd
mod_gas_annual_cost_plot_server <- function(id, annual_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$gas_annual_cost_plot <- renderPlot({
      annual_summary %>%
        dplyr::filter(fuel == "gas") %>%
        ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
        ggplot2::geom_line(alpha = .5) +
        ggplot2::geom_point()
    })
  })
}

## To be copied in the UI
# mod_gas_annual_cost_plot_ui("gas_annual_cost_plot_ui_1")

## To be copied in the server
# mod_gas_annual_cost_plot_server("gas_annual_cost_plot_ui_1")
