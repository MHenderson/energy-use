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
    plotOutput(ns("annual_cost_plot"))
  )
}

#' annual_cost_plot Server Functions
#'
#' @noRd
mod_annual_cost_plot_server <- function(id, annual_summary, fuel_){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$annual_cost_plot <- renderPlot({
      annual_summary %>%
        dplyr::filter(fuel == fuel_) %>%
        ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
        ggplot2::geom_line(alpha = .5) +
        ggplot2::geom_point()
    })
  })
}

## To be copied in the UI
# mod_annual_cost_plot_ui("annual_cost_plot_ui_1")

## To be copied in the server
# mod_annual_cost_plot_server("annual_cost_plot_ui_1")