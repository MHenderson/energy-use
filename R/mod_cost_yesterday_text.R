#' cost_yesterday_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cost_yesterday_text_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("cost_yesterday"))
  )
}

#' cost_yesterday_text Server Functions
#'
#' @noRd
mod_cost_yesterday_text_server <- function(id, tidy_energy, fuel_){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$cost_yesterday <- renderText({
      tidy_energy %>%
        dplyr::filter(fuel == fuel_, var == "kwh") %>%
        utils::tail(1) %>%
        dplyr::pull(value) %>%
        `/`(100) %>%
        round(2) %>%
        paste("GBP")
    })
  })
}

## To be copied in the UI
# mod_cost_yesterday_text_ui("cost_yesterday_text_ui_1")

## To be copied in the server
# mod_cost_yesterday_text_server("cost_yesterday_text_ui_1")
