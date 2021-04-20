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
    shinycssloaders::withSpinner(dygraphs::dygraphOutput(ns("usage_plot")))
  )
}

#' usage_plot Server Functions
#'
#' @noRd
mod_usage_plot_server <- function(id, tidy_energy, plot1vars){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    output$usage_plot <- dygraphs::renderDygraph({

      q <- tidy_energy %>%
        dplyr::filter(var == plot1vars$var(), tarrif_id %in% plot1vars$tariff()) %>%
        dplyr::select(tarrif_id, fuel, date, value) %>%
        dplyr::mutate(
          id_fuel = paste0(fuel, "_", tarrif_id)
        ) %>%
        dplyr::select(id_fuel, date, value) %>%
        tidyr::pivot_wider(names_from = id_fuel, values_from = value)

      q <- as.data.frame(q)
      xq <- xts::xts(q[,-1], order.by = q[,1])

      dygraphs::dygraph(xq, group = "usage") %>%
        dygraphs::dyRangeSelector(dateWindow = c("2021-01-01", "2021-04-18")) %>%
        dygraphs::dyOptions(drawPoints = TRUE, strokeWidth = 0) %>%
        dygraphs::dyLegend(width = 300, hideOnMouseOut = FALSE) %>%
        dygraphs::dyAxis("y", label = plot1vars$var())

    })
  })
}

## To be copied in the UI
# mod_usage_plot_ui("usage_plot_ui_1")

## To be copied in the server
# mod_usage_plot_server("usage_plot_ui_1")
