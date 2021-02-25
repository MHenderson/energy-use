#' gas_cost_yesterday_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gas_cost_yesterday_text_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("gas_cost_yesterday"))
  )
}

#' gas_cost_yesterday_text Server Functions
#'
#' @noRd
mod_gas_cost_yesterday_text_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$gas_cost_yesterday <- renderText({
      tidy_energy %>%
        dplyr::filter(fuel == "gas", var == "kwh") %>%
        utils::tail(1) %>%
        dplyr::pull(value) %>%
        `/`(100) %>%
        round(2) %>%
        paste("GBP")
    })
  })
}

## To be copied in the UI
# mod_gas_cost_yesterday_text_ui("gas_cost_yesterday_text_ui_1")

## To be copied in the server
# mod_gas_cost_yesterday_text_server("gas_cost_yesterday_text_ui_1")
