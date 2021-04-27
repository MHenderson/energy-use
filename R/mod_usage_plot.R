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

      X <- tibble::tribble(
        ~seq_id, ~code,
        1, "SP",
        2, "YE",
        3, "TE",
        4, "GS",
        5, ""
      )

      q <- tidy_energy %>%
        dplyr::filter(
          var == plot1vars$var(),
          seq_id %in% c(plot1vars$tariff(), if(plot1vars$history()) c(5) else c()),
          fuel %in% plot1vars$fuel()
        ) %>%
        dplyr::select(seq_id, fuel, date, value) %>%
        dplyr::left_join(X, by = "seq_id") %>%
        dplyr::mutate(
          fuel_code = substr(fuel, 1, 1),
          id_fuel = paste0(code, "_", fuel_code)
        ) %>%
        dplyr::select(id_fuel, date, value) %>%
        tidyr::pivot_wider(names_from = id_fuel, values_from = value)

      if(nrow(q) == 0) return()

      q <- tibble::as_tibble(q)
      xq <- xts::xts(q[,-1], order.by = q$date)

      p <- dygraphs::dygraph(xq, group = "usage") %>%
        dygraphs::dyRangeSelector(dateWindow = c(plot1vars$date()[1], plot1vars$date()[2])) %>%
        dygraphs::dyLegend(show = "follow") %>%
        dygraphs::dyAxis("y", label = plot1vars$var()) %>%
        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, strokeWidth = 0)

      if("_g" %in% colnames(q)) {
        p <- p %>%
          dygraphs::dySeries("_g", color = "#BDBDBD", pointShape = "plus", pointSize = 5)
      }

      if("_e" %in% colnames(q)) {
        p <- p %>%
          dygraphs::dySeries("_e", color = "#636363", pointShape = "plus", pointSize = 5)
      }

      p

    })
  })
}

## To be copied in the UI
# mod_usage_plot_ui("usage_plot_ui_1")

## To be copied in the server
# mod_usage_plot_server("usage_plot_ui_1")
