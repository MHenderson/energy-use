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
    column(10,
      shinycssloaders::withSpinner(dygraphs::dygraphOutput(ns("cum_usage_plot")))
    ),
    column(2,
      shinycssloaders::withSpinner(uiOutput(ns("cum_usage_info")))
    )
  )
}

#' cum_usage_plot Server Function
#'
#' @noRd
mod_cum_usage_plot_server <- function(id, tidy_energy, plot1vars){
  moduleServer( id, function(input, output, session){
  ns <- session$ns

  output$cum_usage_info <- renderUI({

    gas_total <- reactive({
      tidy_energy %>%
        dplyr::filter(
          var == plot1vars$var(),
          seq_id %in% c(plot1vars$tariff(), if(plot1vars$history()) c(5) else c()),
          fuel %in% plot1vars$fuel()
        ) %>%
        dplyr::filter(
          date >= input$cum_usage_plot_date_window[1],
          date <= input$cum_usage_plot_date_window[2]
        ) %>%
        dplyr::select(seq_id, fuel, date, value) %>%
        dplyr::left_join(X, by = "seq_id") %>%
        dplyr::mutate(
          fuel_code = substr(fuel, 1, 1),
          id_fuel = paste0(code, "_", fuel_code)
        ) %>%
        dplyr::select(id_fuel, date, value) %>%
        dplyr::group_by(id_fuel) %>%
        dplyr::mutate(
          value = cumsum(value)
        ) %>%
        dplyr::filter(value == max(value)) %>%
        dplyr::filter(id_fuel == "_g") %>%
        dplyr::pull("value") %>%
        round(2)
    })

    electricity_total <- reactive({
      tidy_energy %>%
        dplyr::filter(
          var == plot1vars$var(),
          seq_id %in% c(plot1vars$tariff(), if(plot1vars$history()) c(5) else c()),
          fuel %in% plot1vars$fuel()
        ) %>%
        dplyr::filter(
          date >= input$cum_usage_plot_date_window[1],
          date <= input$cum_usage_plot_date_window[2]
        ) %>%
        dplyr::select(seq_id, fuel, date, value) %>%
        dplyr::left_join(X, by = "seq_id") %>%
        dplyr::mutate(
          fuel_code = substr(fuel, 1, 1),
          id_fuel = paste0(code, "_", fuel_code)
        ) %>%
        dplyr::select(id_fuel, date, value) %>%
        dplyr::group_by(id_fuel) %>%
        dplyr::mutate(
          value = cumsum(value)
        ) %>%
        dplyr::filter(value == max(value)) %>%
        dplyr::filter(id_fuel == "_e") %>%
        dplyr::pull("value") %>%
        round(2)
    })

    shinydashboardPlus::boxPad(
      color = "green",
      shinydashboardPlus::descriptionBlock(
        header = gas_total(),
        text = "Gas",
        rightBorder = FALSE,
        marginBottom = TRUE
      ),
      shinydashboardPlus::descriptionBlock(
        header = electricity_total(),
        text = "Electricity",
        rightBorder = FALSE,
        marginBottom = TRUE
      )
      ,
      shinydashboardPlus::descriptionBlock(
        header = gas_total() + electricity_total(),
        text = "Total",
        rightBorder = FALSE,
        marginBottom = TRUE
      )
    )
  })

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
      dplyr::left_join(X, by = "seq_id") %>%
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

    if(nrow(q) == 0) return()

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

