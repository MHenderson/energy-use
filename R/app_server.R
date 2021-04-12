#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  download.file("https://mjh-energy-data.netlify.app/data/tidy_energy.rds", destfile = "tidy_energy.rds")
  tidy_energy <- readRDS("tidy_energy.rds")
  mod_usage_plot_server("usage_plot_ui", tidy_energy)
  mod_total_cost_plot_server("total_cost_plot_ui", tidy_energy)

  download.file("https://mjh-energy-data.netlify.app/data/bills.rds", destfile = "bills.rds")
  bills <- readRDS("bills.rds")
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

  download.file("https://mjh-energy-data.netlify.app/data/readings.rds", destfile = "readings.rds")
  readings <- readRDS("readings.rds")
  mod_readings_plot_server("readings_plot_ui_1", readings)

  mod_usage_trend_plot_server("usage_trend_plot_ui_1", tidy_energy)

}
