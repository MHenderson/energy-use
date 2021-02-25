#' electricity_annual_cost_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_electricity_annual_cost_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("electricity_annual_cost_plot"))
  )
}

#' electricity_annual_cost_plot Server Functions
#'
#' @noRd
mod_electricity_annual_cost_plot_server <- function(id, annual_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$electricity_annual_cost_plot <- renderPlot({
    annual_summary %>%
        dplyr::filter(fuel == "electricity") %>%
        ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
        ggplot2::geom_line(alpha = .5) +
        ggplot2::geom_point()
    })
  })
}

## To be copied in the UI
# mod_electricity_annual_cost_plot_ui("electricity_annual_cost_plot_ui_1")

## To be copied in the server
# mod_electricity_annual_cost_plot_server("electricity_annual_cost_plot_ui_1")
