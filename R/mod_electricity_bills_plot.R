#' electricity_bills_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_electricity_bills_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("electricity_bills_plot"))
  )
}

#' electricity_bills_plot Server Functions
#'
#' @noRd
mod_electricity_bills_plot_server <- function(id, billing){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$electricity_bills_plot <- renderPlot({
      billing %>%
        dplyr::filter(fuel == "electricity") %>%
        ggplot2::ggplot(ggplot2::aes(x = ymd, y = value)) +
          ggplot2::geom_line(alpha = .5) +
          ggplot2::geom_point()

    })
  })
}

## To be copied in the UI
# mod_electricity_bills_plot_ui("electricity_bills_plot_ui_1")

## To be copied in the server
# mod_electricity_bills_plot_server("electricity_bills_plot_ui_1")
