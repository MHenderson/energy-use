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
    shinycssloaders::withSpinner(dygraphs::dygraphOutput(ns("cum_usage_plot")))
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
      dplyr::filter(
        var == plot1vars$var(),
        seq_id %in% c(plot1vars$tariff(), if(plot1vars$history()) c(5) else c()),
        fuel %in% plot1vars$fuel()
      ) %>%
      dplyr::select(seq_id, fuel, date, value) %>%
      dplyr::mutate(
        id_fuel = paste0(fuel, "_", seq_id)
      ) %>%
      dplyr::select(id_fuel, date, value) %>%
      dplyr::group_by(id_fuel) %>%
      dplyr::mutate(
        value = cumsum(value)
      ) %>%
      tidyr::pivot_wider(names_from = id_fuel, values_from = value)

    q <- as.data.frame(q)
    xq <- xts::xts(q[,-1], order.by = q[,1])

    dygraphs::dygraph(xq, group = "usage") %>%
      dygraphs::dyRangeSelector(dateWindow = c("2021-01-01", "2021-04-18")) %>%
      dygraphs::dyOptions(stepPlot = TRUE) %>%
      dygraphs::dyLegend(width = 400, hideOnMouseOut = FALSE) %>%
      dygraphs::dyAxis("y", label = plot1vars$var())

  })
  })
}

## To be copied in the UI
# mod_cum_usage_plot_ui("cum_usage_plot_ui_1")

## To be copied in the server
# callModule(mod_cum_usage_plot_server, "cum_usage_plot_ui_1")

