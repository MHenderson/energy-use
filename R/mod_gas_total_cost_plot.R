#' gas_total_cost_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gas_total_cost_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("gas_total_cost_plot"))
  )
}

#' gas_total_cost_plot Server Functions
#'
#' @noRd
mod_gas_total_cost_plot_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$gas_total_cost_plot <- renderPlot({
      tidy_energy %>%
        dplyr::filter(fuel == "electricity", var == "total") %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = value, colour = supplier)) +
        ggplot2::geom_line()
    })
  })
}

## To be copied in the UI
# mod_gas_total_cost_plot_ui("gas_total_cost_plot_ui_1")

## To be copied in the server
# mod_gas_total_cost_plot_server("gas_total_cost_plot_ui_1")
