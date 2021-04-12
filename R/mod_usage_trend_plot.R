#' usage_trend_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_usage_trend_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    dygraphs::dygraphOutput(ns("usage_trend_plot"))
  )
}

#' usage_trend_plot Server Function
#'
#' @noRd
mod_usage_trend_plot_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$usage_trend_plot <- dygraphs::renderDygraph({
      q <- tidy_energy %>%
        dplyr::filter(var == "kwh") %>%
        dplyr::select(fuel, date, value) %>%
        tidyr::pivot_wider(names_from = fuel, values_from = value)

      q <- as.data.frame(q)
      xq <- xts::xts(q[,-1], order.by = q[,1])

      dygraphs::dygraph(xq, group = "usage") %>%
        dygraphs::dyRoller(rollPeriod = 7) %>%
        dygraphs::dyRangeSelector()
    })
  })
}

## To be copied in the UI
# mod_usage_trend_plot_ui("usage_trend_plot_ui_1")

## To be copied in the server
# callModule(mod_usage_trend_plot_server, "usage_trend_plot_ui_1")

