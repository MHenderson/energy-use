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
    plotOutput(ns("usage_plot"))
  )
}

#' usage_plot Server Functions
#'
#' @noRd
mod_usage_plot_server <- function(id, tidy_energy, fuel_){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$usage_plot <- renderPlot({
      tidy_energy %>%
        dplyr::filter(fuel == fuel_, var == "kwh") %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = value, colour = supplier)) +
        ggplot2::geom_point()
    })
  })
}

## To be copied in the UI
# mod_usage_plot_ui("usage_plot_ui_1")

## To be copied in the server
# mod_usage_plot_server("usage_plot_ui_1")
