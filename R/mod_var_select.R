#' var_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_var_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("var"), "Variable:",
                 c("kWh" = "kwh",
                   "GBP" = "cost"))
  )
}

#' var_select Server Function
#'
#' @noRd
mod_var_select_server <- function(input, output, session){
  ns <- session$ns
  return(
    list(
      var = reactive({ input$var })
    )
  )

}

## To be copied in the UI
# mod_var_select_ui("var_select_ui_1")

## To be copied in the server
# callModule(mod_var_select_server, "var_select_ui_1")

