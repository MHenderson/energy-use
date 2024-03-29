# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "dplyr" )
usethis::use_package( "dygraphs" )
usethis::use_package( "ggplot2" )
usethis::use_package( "googlesheets4" )
usethis::use_package( "lubridate" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package( "simputation" )
usethis::use_package( "plotly" )
usethis::use_package( "readxl" )
usethis::use_package( "xts" )

## Add modules ----
## Create a module infrastructure in R/
#golem::add_module( name = "electricity_cost_yesterday_text" ) # Name of the module
#golem::add_module( name = "electricity_usage_yesterday_text" ) # Name of the module
#golem::add_module( name = "electricity_usage_plot" ) # Name of the module
#golem::add_module( name = "electricity_total_cost_plot" ) # Name of the module
#golem::add_module( name = "electricity_bills_plot" ) # Name of the module
#golem::add_module( name = "electricity_annual_cost_plot" ) # Name of the module
#golem::add_module( name = "gas_cost_yesterday_text" ) # Name of the module
#golem::add_module( name = "gas_usage_yesterday_text" ) # Name of the module
#golem::add_module( name = "gas_usage_plot" ) # Name of the module
#golem::add_module( name = "gas_total_cost_plot" ) # Name of the module
#golem::add_module( name = "gas_bills_plot" ) # Name of the module
#golem::add_module( name = "gas_annual_cost_plot" ) # Name of the module

golem::add_module( name = "annual_cost_plot" ) # Name of the module
golem::add_module( name = "bills_plot" ) # Name of the module
golem::add_module( name = "total_cost_plot" ) # Name of the module
golem::add_module( name = "usage_plot" ) # Name of the module
golem::add_module( name = "readings_plot" ) # Name of the module
golem::add_module( name = "usage_trend_plot" ) # Name of the module
golem::add_module( name = "var_select" ) # Name of the module
golem::add_module( name = "cum_usage_plot" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" )
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "energy", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("energyuse")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

