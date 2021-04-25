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
      dplyr::group_by(id_fuel) %>%
      dplyr::mutate(
        value = cumsum(value)
      ) %>%
      tidyr::pivot_wider(names_from = id_fuel, values_from = value)

    q <- tibble::as_tibble(q)
    xq <- xts::xts(q[,-1], order.by = q$date)

    p <- dygraphs::dygraph(xq, group = "usage") %>%
      dygraphs::dyRangeSelector(dateWindow = c("2021-01-01", as.character(Sys.Date()))) %>%
      dygraphs::dyOptions(stepPlot = TRUE) %>%
      dygraphs::dyLegend(show = "follow") %>%
      dygraphs::dyAxis("y", label = plot1vars$var()) %>%
      dygraphs::dyHighlight(highlightSeriesBackgroundAlpha = 0.2)

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
# mod_cum_usage_plot_ui("cum_usage_plot_ui_1")

## To be copied in the server
# callModule(mod_cum_usage_plot_server, "cum_usage_plot_ui_1")

