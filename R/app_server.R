#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  options(spinner.color="#0dc5c1")

  # download data
  download.file("https://mjh-energy-data.netlify.app/data/tidy_energy.rds", destfile = "tidy_energy.rds")
  download.file("https://mjh-energy-data.netlify.app/data/bills.rds", destfile = "bills.rds")
  download.file("https://mjh-energy-data.netlify.app/data/readings.rds", destfile = "readings.rds")

  # load data
  tidy_energy <- readRDS("tidy_energy.rds")
  bills <- readRDS("bills.rds")
  readings <- readRDS("readings.rds")

  # variable selection module
  plot1vars <- callModule(mod_var_select_server, "plot1_vars")

  # usage plot module
  mod_usage_plot_server("usage_plot_ui", tidy_energy, plot1vars)

  # trend plot module
  mod_usage_trend_plot_server("usage_trend_plot_ui_1", tidy_energy, plot1vars)

  # cumulative usage plot module
  mod_cum_usage_plot_server("cum_usage_plot_ui_1", tidy_energy, plot1vars)

}
