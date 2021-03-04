#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  mod_usage_plot_server("usage_plot_ui", tidy_energy)
  mod_total_cost_plot_server("total_cost_plot_ui", tidy_energy)

  mod_bills_plot_server("bills_plot_ui", bills)

  annual_summary <- tidy_energy %>%
    dplyr::filter(var == "cost") %>%
    dplyr::group_by(fuel, year) %>%
    dplyr::summarise(
      value = sum(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ymd = lubridate::ymd(paste(year, 12, 31, sep = "-")))

  mod_annual_cost_plot_server("annual_cost_plot_ui", annual_summary)

  mod_readings_plot_server("readings_plot_ui_1", readings)

}
