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
    shinyWidgets::awesomeRadio(
      inputId = ns("var"),
      label = h4("Select variable:"),
      choices = c("kWh" = "kwh", "GBP" = "cost"),
      selected = "cost",
      inline = TRUE
    ),
    shinyWidgets::awesomeRadio(
      inputId = ns("history"),
      label = h4("Show history?"),
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE,
      inline = TRUE
    ),
    shinyWidgets::pickerInput(
      inputId = ns("tariff"),
      label = h4("Tariff"),
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
      label = h4("Fuel"),
      choices = list("Gas" = "gas", "Electricity" = "electricity"),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE,
      selected = c("gas", "electricity")
    ),
    dateRangeInput(
      inputId = ns("dates"),
      label = h4("Date range"),
      start = "2021-01-01",
      end = as.character(Sys.Date()),
      min = "2019-07-16",
      max = Sys.Date()
    ),
    shinyWidgets::pickerInput(
      inputId = ns("preset"),
      label = h4("Interval presets"),
      choices = list(
        "none" = 1,
        "last 7 days" = 7,
        "last 30 days" = 30,
        "last 90 Days" = 90,
        "2021" = 2021,
        "2020" = 2020,
        "2019" = 2019
      ),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = FALSE,
      selected = c("gas", "electricity")
    )
  )
}

#' var_select Server Function
#'
#' @noRd
mod_var_select_server <- function(input, output, session){
  ns <- session$ns

  observe({

    preset <- as.numeric(req(input$preset))

    if(preset %in% c(7, 30, 90)) {
      updateDateRangeInput(
        session,
        "dates",
        start = Sys.Date() - preset,
        end = Sys.Date()
      )
    }

    if(preset == "2019") {
      updateDateRangeInput(
        session,
        "dates",
        start = "2019-07-16",
        end = "2019-12-31"
      )
    }

    if(preset == "2020") {
      updateDateRangeInput(
        session,
        "dates",
        start = "2020-01-01",
        end = "2020-12-31"
      )
    }

    if(preset == "2021") {
      updateDateRangeInput(
        session,
        "dates",
        start = "2021-01-01",
        end = Sys.Date()
      )
    }

    if(preset == 1) {
      updateDateRangeInput(
        session,
        "dates",
        start = "2019-07-16",
        end = Sys.Date()
      )
    }

  })

  return(
    list(
         var = reactive({ input$var }),
      tariff = reactive({ input$tariff }),
     history = reactive({ input$history }),
        fuel = reactive({ input$fuel }),
       dates = reactive({ input$dates }),
      preset = reactive({ input$preset })
    )
  )

}

## To be copied in the UI
# mod_var_select_ui("var_select_ui_1")

## To be copied in the server
# callModule(mod_var_select_server, "var_select_ui_1")

