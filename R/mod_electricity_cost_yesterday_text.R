#' electricity_cost_yesterday_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_electricity_cost_yesterday_text_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("electricity_cost_yesterday"))
  )
}

#' electricity_cost_yesterday_text Server Functions
#'
#' @noRd
mod_electricity_cost_yesterday_text_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$electricity_cost_yesterday <- renderText({
      tidy_energy %>%
        dplyr::filter(fuel == "electricity", var == "kwh") %>%
        utils::tail(1) %>%
        dplyr::pull(value) %>%
        `/`(100) %>%
        round(2) %>%
        paste("GBP")
    })
  })
}

## To be copied in the UI
# mod_electricity_cost_yesterday_text_ui("electricity_cost_yesterday_text_ui_1")

## To be copied in the server
# mod_electricity_cost_yesterday_text_server("electricity_cost_yesterday_text_ui_1")
