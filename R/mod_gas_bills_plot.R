#' gas_bills_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gas_bills_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("gas_bills_plot"))
  )
}

#' gas_bills_plot Server Functions
#'
#' @noRd
mod_gas_bills_plot_server <- function(id, billing){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$gas_bills_plot <- renderPlot({
      billing %>%
        dplyr::filter(fuel == "gas") %>%
        ggplot2::ggplot(ggplot2::aes(x = ymd, y = value)) +
        ggplot2::geom_line(alpha = .5) +
        ggplot2::geom_point()

    })
  })
}

## To be copied in the UI
# mod_gas_bills_plot_ui("gas_bills_plot_ui_1")

## To be copied in the server
# mod_gas_bills_plot_server("gas_bills_plot_ui_1")
