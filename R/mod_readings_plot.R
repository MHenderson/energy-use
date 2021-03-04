#' readings_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_readings_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("readings_plot"))
  )
}

#' readings_plot Server Functions
#'
#' @noRd
mod_readings_plot_server <- function(id, readings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$readings_plot <- plotly::renderPlotly({
      readings %>%
        ggplot2::ggplot(ggplot2::aes(x = time, y = reading)) +
        ggplot2::geom_smooth() +
        ggplot2::geom_point() +
        ggplot2::theme_minimal() +
        ggplot2::facet_wrap(~ fuel, ncol = 1, scales = "free_y")
    })
  })
}

## To be copied in the UI
# mod_readings_plot_ui("readings_plot_ui_1")

## To be copied in the server
# mod_readings_plot_server("readings_plot_ui_1")
