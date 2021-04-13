#' cum_usage_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cum_usage_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    dygraphs::dygraphOutput(ns("cum_usage_plot"))
  )
}

#' cum_usage_plot Server Function
#'
#' @noRd
mod_cum_usage_plot_server <- function(id, tidy_energy, plot1vars){
  moduleServer( id, function(input, output, session){
  ns <- session$ns
  output$cum_usage_plot <- dygraphs::renderDygraph({
    q <- tidy_energy %>%
      dplyr::filter(var == plot1vars$var()) %>%
      dplyr::select(fuel, date, value) %>%
      tidyr::pivot_wider(names_from = fuel, values_from = value) %>%
      dplyr::mutate(
        gas = cumsum(gas),
        electricity = cumsum(electricity)
      )

    q <- as.data.frame(q)
    xq <- xts::xts(q[,-1], order.by = q[,1])

    dygraphs::dygraph(xq, group = "usage") %>%
      dygraphs::dyRangeSelector() %>%
      dygraphs::dyOptions(stepPlot = TRUE) %>%
      dygraphs::dySeries("gas", pointSize = 2, drawPoints = TRUE, strokeWidth = 0) %>%
      dygraphs::dySeries("electricity", pointSize = 2, drawPoints = TRUE, strokeWidth = 0)
  })
  })
}

## To be copied in the UI
# mod_cum_usage_plot_ui("cum_usage_plot_ui_1")

## To be copied in the server
# callModule(mod_cum_usage_plot_server, "cum_usage_plot_ui_1")

