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
      choices = c("kWh" = "kwh", "GBP" = "cost"),
      selected = "cost"
    ),
    shinyWidgets::pickerInput(
      inputId = ns("tariff"),
      label = "Tariff",
      choices = list(
        "SP: Scottish Power (Energy Exclusive)" = 1,
        "YE: Yorkshire Energy (Green Ribblehead)" = 2,
        "TE: Tonik Energy (Go Green)" = 3,
        "GS: Green Star Energy (Green)" = 4
      ),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE,
      selected = c()
    ),
    shinyWidgets::pickerInput(
      inputId = ns("fuel"),
      label = "Fuel",
      choices = list("Gas" = "gas", "Electricity" = "electricity"),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE,
      selected = c("gas", "electricity")
    ),
    radioButtons(
      inputId = ns("history"),
      label = "Show history:",
      choices = c(TRUE, FALSE)
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
     history = reactive({ input$history }),
        fuel = reactive({ input$fuel })
    )
  )

}

## To be copied in the UI
# mod_var_select_ui("var_select_ui_1")

## To be copied in the server
# callModule(mod_var_select_server, "var_select_ui_1")

