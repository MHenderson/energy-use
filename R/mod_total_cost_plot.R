#' total_cost_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_total_cost_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("total_cost_plot"))
  )
}

#' total_cost_plot Server Functions
#'
#' @noRd
mod_total_cost_plot_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$total_cost_plot <- plotly::renderPlotly({
      tidy_energy %>%
        dplyr::filter(var == "total") %>%
        dplyr::mutate(GBP = round(value/100, 2)) %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = GBP, colour = supplier)) +
        ggplot2::geom_area(ggplot2::aes(fill = supplier), alpha = .5) +
        ggplot2::geom_line() +
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = "", y = "GBP") +
        ggplot2::ylim(c(0, 600)) +
        ggplot2::facet_wrap(~ fuel, ncol = 1, scales = "free_y")
    })
  })
}

## To be copied in the UI
# mod_total_cost_plot_ui("total_cost_plot_ui_1")

## To be copied in the server
# mod_total_cost_plot_server("total_cost_plot_ui_1")
