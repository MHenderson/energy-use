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
  # List the first level callModules here
  mod_electricity_yesterday_text_server("electricity_yesterday_text_ui_1", tidy_energy)
  mod_electricity_usage_plot_server("electricity_usage_plot_ui_1", tidy_energy)
}
