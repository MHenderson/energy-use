#' usage_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_usage_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("usage_plot"))
  )
}

#' usage_plot Server Functions
#'
#' @noRd
mod_usage_plot_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$usage_plot <- plotly::renderPlotly({
      tidy_energy %>%
        dplyr::filter(var == "kwh") %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = value, colour = supplier)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(ggplot2::aes(x = date, y = value, colour = "blue")) +
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = "", y = "kWh") +
        ggplot2::facet_wrap(~ fuel, ncol = 1, scales = "free_y")
    })
  })
}

## To be copied in the UI
# mod_usage_plot_ui("usage_plot_ui_1")

## To be copied in the server
# mod_usage_plot_server("usage_plot_ui_1")
