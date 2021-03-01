#' bills_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bills_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("bills_plot"))
  )
}

#' bills_plot Server Functions
#'
#' @noRd
mod_bills_plot_server <- function(id, billing){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    X <- billing %>%
      dplyr::mutate(
        GBP = round(value/100, 2),
        GBP_s = sprintf("£%.2f", GBP)
      )
    output$bills_plot <- plotly::renderPlotly({
      X %>%
        ggplot2::ggplot(ggplot2::aes(x = ymd, y = GBP)) +
        ggplot2::geom_line(alpha = .5) +
        ggplot2::geom_text(data = X %>% dplyr::filter(dplyr::row_number() %% 3 == 1), ggplot2::aes(label = GBP_s), nudge_y = 3) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "", y = "GBP") +
        ggplot2::ylim(c(0, 50)) +
        ggplot2::facet_wrap(~ fuel, ncol = 1, scales = "free_y")
    })
  })
}

## To be copied in the UI
# mod_bills_plot_ui("bills_plot_ui_1")

## To be copied in the server
# mod_bills_plot_server("bills_plot_ui_1")
