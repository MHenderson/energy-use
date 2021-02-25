#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  energy_2019 <- readxl::read_xlsx("energy.xlsx", sheet = "2019")
  energy_2020 <- readxl::read_xlsx("energy.xlsx", sheet = "2020")
  energy_2021 <- readxl::read_xlsx("energy.xlsx", sheet = "2021")
  energy <- dplyr::bind_rows(energy_2019, energy_2020, energy_2021)
  tidy_energy <- prep_tidy_energy(energy)
  billing <- tidy_energy %>%
    dplyr::filter(var == "cost") %>%
    dplyr::group_by(fuel, year, month) %>%
    dplyr::summarise(
      value = sum(value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ymd = lubridate::ymd(paste(year, month, 1, sep = "-")))
  annual_summary <- tidy_energy %>%
    dplyr::filter(var == "cost") %>%
    dplyr::group_by(fuel, year) %>%
    dplyr::summarise(
      value = sum(value)
    ) %>%
    dplyr::ungroup()
  # List the first level callModules here
  mod_electricity_usage_yesterday_text_server("electricity_usage_yesterday_text_ui_1", tidy_energy)
  mod_electricity_usage_plot_server("electricity_usage_plot_ui_1", tidy_energy)
  mod_electricity_cost_yesterday_text_server("electricity_cost_yesterday_text_ui_1", tidy_energy)
  mod_electricity_total_cost_plot_server("electricity_total_cost_plot_ui_1", tidy_energy)
  mod_electricity_bills_plot_server("electricity_bills_plot_ui_1", billing)
  mod_electricity_annual_cost_plot_server("electricity_annual_cost_plot_ui_1", annual_summary)
}
