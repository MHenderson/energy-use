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
    radioButtons(
      inputId = ns("var"),
        label = "Variable:",
      choices = c("kWh" = "kwh", "GBP" = "cost")
    ),
    checkboxGroupInput(
       inputId = ns("tariff"),
         label = "Tariff",
       choices = list(1, 2, 3, 4),
      selected = 1
    ),
    checkboxGroupInput(
      inputId = ns("fuel"),
      label = "Fuel",
      choices = list("Gas" = "gas", "Electricity" = "electricity"),
      selected = "gas"
    )
  )
}

#' var_select Server Function
#'
#' @noRd
mod_var_select_server <- function(input, output, session){
  ns <- session$ns
  return(
    list(
         var = reactive({ input$var }),
      tariff = reactive({ input$tariff }),
      fuel = reactive({ input$fuel })
    )
  )

}

## To be copied in the UI
# mod_var_select_ui("var_select_ui_1")

## To be copied in the server
# callModule(mod_var_select_server, "var_select_ui_1")

