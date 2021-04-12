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
    dygraphs::dygraphOutput(ns("usage_plot"))
  )
}

#' usage_plot Server Functions
#'
#' @noRd
mod_usage_plot_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$usage_plot <- dygraphs::renderDygraph({
      q <- tidy_energy %>%
        dplyr::filter(var == "kwh") %>%
        dplyr::select(fuel, date, value) %>%
        tidyr::pivot_wider(names_from = fuel, values_from = value)

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
# mod_usage_plot_ui("usage_plot_ui_1")

## To be copied in the server
# mod_usage_plot_server("usage_plot_ui_1")
