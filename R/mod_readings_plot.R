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
    dygraphs::dygraphOutput(ns("readings_plot"))
  )
}

#' readings_plot Server Functions
#'
#' @noRd
mod_readings_plot_server <- function(id, readings){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$readings_plot <- dygraphs::renderDygraph({
      readings$date <- as.Date(readings$time)

      q <- readings %>%
        dplyr::select(-time) %>%
        tidyr::pivot_wider(names_from = fuel, values_from = reading)


      q <- as.data.frame(q)
      xq <- xts::xts(q[,-1], order.by = q[,1])

      dygraphs::dygraph(xq, group = "usage") %>%
        dygraphs::dyRangeSelector() %>%
        dygraphs::dyOptions() %>%
        dygraphs::dySeries("gas", pointSize = 3, drawPoints = TRUE, strokeWidth = 1) %>%
        dygraphs::dySeries("electricity", pointSize = 3, drawPoints = TRUE, strokeWidth = 1)

    })
  })
}

## To be copied in the UI
# mod_readings_plot_ui("readings_plot_ui_1")

## To be copied in the server
# mod_readings_plot_server("readings_plot_ui_1")
