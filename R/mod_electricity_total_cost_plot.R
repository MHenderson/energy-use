#' electricity_total_cost_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_electricity_total_cost_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("electricity_total_cost_plot"))
  )
}

#' electricity_total_cost_plot Server Functions
#'
#' @noRd
mod_electricity_total_cost_plot_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$electricity_total_cost_plot <- renderPlot({
      tidy_energy %>%
        dplyr::filter(fuel == "electricity", var == "total") %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = value, colour = supplier)) +
        ggplot2::geom_line()
    })
  })
}

## To be copied in the UI
# mod_electricity_total_cost_plot_ui("electricity_total_cost_plot_ui_1")

## To be copied in the server
# mod_electricity_total_cost_plot_server("electricity_total_cost_plot_ui_1")
