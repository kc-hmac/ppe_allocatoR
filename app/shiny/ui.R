#' This is an example shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial
header <- dashboardHeader(
  disable = T
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Starting Information',
             menuSubItem('Global Options', tabName = 'go'),
             menuSubItem('Orders & Tiers', tabName = 'ot'),
             menuSubItem('Hospital Data', tabName = 'hosp'),
             menuSubItem('Inventory', tabName = 'inv'),
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
    tabItem(tabName = 'go',
            h2("Global Options"),
            dateInput('cycle_date', label = 'Please input the cycle date', value = Sys.Date() - (wday(Sys.Date())+1) ),
            textInput('workdir', label = 'Please enter the base working directory path.', value = 'C:/Users/dcasey/Documents/test'),
            actionButton('go_validate', 'Create/load folder structure'),
            textOutput('workfolder')),
    tabItem(tabName = 'ot',
            h2('Orders & Tiers'),
            fileInput('ot_t1', 'Tier 1 aggregated requests'),
            fileInput('ot_t2', "Tier 2+ aggregated requests"),
            textInput('ot_ver', 'Order & Tiers version', 1),
            radioButtons('ot_use_prev', 'Carry forward previous version?', choices = c("Yes" = TRUE, 'No' = FALSE), selected = 'No'),
            textInput('ot_prev_ver', 'Order & tiers previous version', NULL),
            fileInput('ot_prev_sum', 'Summary of the previous week (asum)'),
            fileInput('ot_dump', 'WebEOC data dump'),
            fileInput('ot_add', 'Optional: additional orders'),
            actionButton('ot_run', 'Create order and tier sheets'),
            textOutput('t_o')),
    tabItem(tabName = 'hosp', h2('Hospital Data')),
    tabItem(tabName = 'inv', h2('Inventory')),
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
