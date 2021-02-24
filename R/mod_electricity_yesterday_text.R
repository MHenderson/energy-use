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
mod_electricity_yesterday_text_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$electricity_yesterday <- renderText({
      energy_2019 <- readxl::read_xlsx("energy.xlsx", sheet = "2019")
      energy_2020 <- readxl::read_xlsx("energy.xlsx", sheet = "2020")
      energy_2021 <- readxl::read_xlsx("energy.xlsx", sheet = "2021")
      energy <- dplyr::bind_rows(energy_2019, energy_2020, energy_2021)
      tidy_energy <- prep_tidy_energy(energy)
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
