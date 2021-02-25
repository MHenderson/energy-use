#' electricity_yesterday_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_electricity_yesterday_text_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("electricity_yesterday"))
  )
}

#' electricity_yesterday_text Server Functions
#'
#' @noRd
mod_electricity_yesterday_text_server <- function(id, tidy_energy){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$electricity_yesterday <- renderText({
      tidy_energy %>%
        dplyr::filter(fuel == "electricity", var == "kwh") %>%
        utils::tail(1) %>%
        dplyr::pull(value) %>%
        paste("kwH")
    })
  })
}

## To be copied in the UI
# mod_electricity_yesterday_text_ui("electricity_yesterday_text_ui_1")

## To be copied in the server
# mod_electricity_yesterday_text_server("electricity_yesterday_text_ui_1")
