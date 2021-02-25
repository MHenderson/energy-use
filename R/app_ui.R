#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # List the first level UI elements here
      shinydashboardPlus::dashboardPage(
        options = list(sidebarExpandOnHover = TRUE),
        header = shinydashboardPlus::dashboardHeader(),
        sidebar = shinydashboardPlus::dashboardSidebar(
          minified = TRUE,
          collapsed = TRUE,
          shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Gas", tabName = "gas", icon = icon("fire")),
            shinydashboard::menuItem("Electricity", tabName = "electricity", icon = icon("bolt"))
          )
        ),
        body = shinydashboard::dashboardBody(
          shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "electricity",
              fluidRow(
                shinydashboardPlus::box(
                  mod_usage_yesterday_text_ui("usage_yesterday_text_ui_electricity"),
                  width = 2, title = "Usage (yesterday)"
                ),
                shinydashboardPlus::box(
                  mod_cost_yesterday_text_ui("cost_yesterday_text_ui_electricity"),
                  width = 2, title = "Cost (yesterday)"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_usage_plot_ui("usage_plot_ui_electricity"),
                  width = 12, title = "Usage"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_electricity_total_cost_plot_ui("electricity_total_cost_plot_ui_1"),
                  width = 12, title = "Total cost"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_bills_plot_ui("bills_plot_ui_electricity"),
                  width = 12, title = "Bills"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_annual_cost_plot_ui("annual_cost_plot_ui_electricity"),
                  width = 12, title = "Annual cost"
                )
              )
            ),
            shinydashboard::tabItem(tabName = "gas",
              fluidRow(
                shinydashboardPlus::box(
                  mod_usage_yesterday_text_ui("usage_yesterday_text_ui_gas"),
                  width = 2, title = "Usage (yesterday)"
                ),
                shinydashboardPlus::box(
                  mod_cost_yesterday_text_ui("cost_yesterday_text_ui_gas"),
                  width = 2, title = "Cost (yesterday)"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_usage_plot_ui("usage_plot_ui_gas"),
                  width = 12, title = "Usage"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_gas_total_cost_plot_ui("gas_total_cost_plot_ui_1"),
                  width = 12, title = "Total cost"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_bills_plot_ui("bills_plot_ui_gas"),
                  width = 12, title = "Bills"
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  mod_annual_cost_plot_ui("annual_cost_plot_ui_gas"),
                  width = 12, title = "Annual cost"
                )
              )
            )
          )

        ),
        controlbar = shinydashboardPlus::dashboardControlbar(),
        title = "My Energy Use"
      )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'energyuse'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

