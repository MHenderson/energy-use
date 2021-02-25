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

  mod_annual_cost_plot_server("annual_cost_plot_ui_gas", annual_summary, "gas")
  mod_cost_yesterday_text_server("cost_yesterday_text_ui_gas", tidy_energy, "gas")
  mod_bills_plot_server("bills_plot_ui_gas", billing, "gas")
  mod_usage_plot_server("usage_plot_ui_gas", tidy_energy, "gas")
  mod_usage_yesterday_text_server("usage_yesterday_text_ui_gas", tidy_energy, "gas")
  mod_total_cost_plot_server("total_cost_plot_ui_gas", tidy_energy, "gas")

  mod_annual_cost_plot_server("annual_cost_plot_ui_electricity", annual_summary, "electricity")
  mod_cost_yesterday_text_server("cost_yesterday_text_ui_electricity", tidy_energy, "electricity")
  mod_bills_plot_server("bills_plot_ui_electricity", billing, "electricity")
  mod_usage_plot_server("usage_plot_ui_electricity", tidy_energy, "electricity")
  mod_usage_yesterday_text_server("usage_yesterday_text_ui_electricity", tidy_energy, "electricity")
  mod_total_cost_plot_server("total_cost_plot_ui_electricity", tidy_energy, "electricity")
}
