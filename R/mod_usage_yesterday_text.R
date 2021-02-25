#' usage_yesterday_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_usage_yesterday_text_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("usage_yesterday"))
  )
}

#' usage_yesterday_text Server Functions
#'
#' @noRd
mod_usage_yesterday_text_server <- function(id, tidy_energy, fuel_){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$usage_yesterday <- renderText({
      tidy_energy %>%
        dplyr::filter(fuel == fuel_, var == "kwh") %>%
        utils::tail(1) %>%
        dplyr::pull(value) %>%
        paste("kwH")
    })
  })
}

## To be copied in the UI
# mod_usage_yesterday_text_ui("usage_yesterday_text_ui_1")

## To be copied in the server
# mod_usage_yesterday_text_server("usage_yesterday_text_ui_1")
