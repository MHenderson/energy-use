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
    shinycssloaders::withSpinner(dygraphs::dygraphOutput(ns("usage_trend_plot")))
  )
}

#' usage_trend_plot Server Function
#'
#' @noRd
mod_usage_trend_plot_server <- function(id, tidy_energy, plot1vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$usage_trend_plot <- dygraphs::renderDygraph({

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
        dplyr::left_join(X) %>%
        dplyr::mutate(
          fuel_code = substr(fuel, 1, 1),
          id_fuel = paste0(code, "_", fuel_code)
        ) %>%
        dplyr::select(id_fuel, date, value) %>%
        tidyr::pivot_wider(names_from = id_fuel, values_from = value)

      q <- as.data.frame(q)
      xq <- xts::xts(q[,-1], order.by = q[,1])

      p <- dygraphs::dygraph(xq, group = "usage") %>%
        dygraphs::dyRoller(rollPeriod = 7) %>%
        dygraphs::dyRangeSelector(dateWindow = c("2021-01-01", as.character(Sys.Date()))) %>%
        dygraphs::dyLegend(width = 300, hideOnMouseOut = FALSE) %>%
        dygraphs::dyAxis("y", label = plot1vars$var())

      if("_g" %in% colnames(q)) {
        p <- p %>%
          dygraphs::dySeries("_g", color = "#BDBDBD", strokePattern = "dashed")
      }

      if("_e" %in% colnames(q)) {
        p <- p %>%
          dygraphs::dySeries("_e", color = "#636363", strokePattern = "dashed")
      }

      p

    })
  })
}

## To be copied in the UI
# mod_usage_trend_plot_ui("usage_trend_plot_ui_1")

## To be copied in the server
# callModule(mod_usage_trend_plot_server, "usage_trend_plot_ui_1")

