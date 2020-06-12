#' This is an example shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial

library(shiny)
library('shinydashboard')

header <- dashboardHeader(
  disable = T
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Starting Information',
             menuSubItem('Global Options', tabName = 'go'),
             menuSubItem('Hospital Data', tabName = 'hosp'),
             menuSubItem('Inventory', tabName = 'inv'),
             menuSubItem('Orders & Tiers', tabName = 'ot'),
             menuSubItem('Item Classifications', tabName = 'itemclass')
             ),
    menuItem('Make Allocations',
             menuSubItem('Validate Inputs', tabName = 'valid'),
             menuSubItem('Allocate', tabName = 'allocate')
             )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'go', h2("Global Options")),
    tabItem(tabName = 'hosp', h2('Hospital Data')),
    tabItem(tabName = 'inv', h2('Inventory')),
    tabItem(tabName = 'ot', h2('Orders & Tiers')),
    tabItem(tabName = 'itemclass', h2('Item Classifications')),
    tabItem(tabName = 'valid', h2('Validate Inputs')),
    tabItem(tabName = 'allocate', h2('Run Allocations'))
  )
)

ui = dashboardPage(header, sidebar, body)


# ui = fluidPage(
#   titlePanel("Example Shiny App"),
#   sidebarLayout(
# 
#     sidebarPanel(
#       selectInput(
#         inputId = "dataset",
#         label = "Choose a dataset",
#         choices = c("rock", "pressure", "cars", "iris")),
# 
#       numericInput(
#         inputId = "obs",
#         label = "Number of observations to view:",
#         value = 10)
#     ),
# 
#     mainPanel(
#       tags$div(sprintf("Global Variable Value: %s", GLOBAL_VAR)),
#       verbatimTextOutput("dataSummary"),
#       tableOutput("dataView")
#     )
#   )
# )
