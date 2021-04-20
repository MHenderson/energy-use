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
          shinydashboard::sidebarMenu(
            mod_var_select_ui("plot1_vars")
          ),
          minified = TRUE,
          collapsed = FALSE
        ),
        body = shinydashboard::dashboardBody(
              fluidRow(
                column(width = 12,
                  fluidRow(
                    shinydashboardPlus::box(
                      mod_usage_plot_ui("usage_plot_ui"),
                      width = 12, title = "Readings"
                    )
                  ),
                  fluidRow(
                    shinydashboardPlus::box(
                      mod_usage_trend_plot_ui("usage_trend_plot_ui_1"),
                      width = 12, title = "Trend"
                    )
                  ),
                  fluidRow(
                    shinydashboardPlus::box(
                      mod_cum_usage_plot_ui("cum_usage_plot_ui_1"),
                      width = 12, title = "Total"
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

