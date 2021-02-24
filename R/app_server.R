#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  #callModule(mod_electricity_yesterday_text_server, "mod_electricty_yesterday_text_1")
  mod_electricity_yesterday_text_server("electricity_yesterday_text_ui_1")
}
