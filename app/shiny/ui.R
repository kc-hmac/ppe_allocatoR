#' This is an example shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial
print('run ui')
source('./startup.R')
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
             menuSubItem('Item Classifications', tabName = 'itemclass'),
             startExpanded = TRUE
             ),
    menuItem('Make Allocations',
             #menuSubItem('Validate Inputs', tabName = 'valid'),
             menuSubItem('Other inputs', tabName = 'other_inputs'),
             menuSubItem('Allocate', tabName = 'allocate'),
             menuSubItem('Session Info', tabName = 'sessionInfo'),
             startExpanded = TRUE
             )
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'go',
            textOutput('wd'),
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
    tabItem(tabName = 'hosp',
            h2('Hospital Data'),
            fileInput('hosp_inv','Hospital PPE Inventory'), 
            fileInput('hosp_covid', 'Hospital COVID counts'),
            actionButton('hosp_go', 'Create hospital datasheet'),
            textOutput('hosp_o')),
    tabItem(tabName = 'inv',
            h2('Inventory'),
            textInput('inv_new_ver', 'New Inventory Version', value = 1),
            fileInput('inv_new', 'New Inventory Dataset'),
            fileInput('inv_old', 'Old Inventory Classifications'),
            actionButton('inv_go', 'Prepare inventory'),
            textOutput('inv_o')),
    tabItem(tabName = 'itemclass',
            h2('Item Classifications'),
            fileInput('ic_old', 'Existing Item Classifications'),
            fileInput('ic_new', 'Orders for this cycle'),
            actionButton('ic_go', 'Make new classifications sheet'),
            textOutput('ic_o')),
    tabItem(tabName = 'other_inputs',
            h2('Other Inputs'),
            fileInput('linelist', 'Linelist'),
            fileInput('cw', 'Crosswalk'),
            fileInput('replacements', 'Replacement Instructions'),
            fileInput('acrciq', 'ACRC/IQ Occupancy'),
            fileInput('chgs', 'PPE Changes by type'),
            actionButton('oi_go', 'Upload/Transfer files'),
            textOutput('oi_o')),
    tabItem(tabName = 'valid', h2('Validate Inputs')),
    tabItem(tabName = 'allocate',
            h2('Run Allocations'),
            textInput('cycle_v', 'Cycle Version', 1),
            textInput('inv_v', 'Inventory Version',1),
            textInput('ot_v', 'Orders & Tiers Version', 1),
            textInput('runtiers', 'Tiers to run (semi-colon seperated list)', '0; 1; 2; 3; 1.5; 1.75'),
            textInput('sized', 'Item types to distribute by size (semi-colon seperated list)',
                      'non-latex gloves; scrub pants; scrub pants; latex gloves; N95; coveralls'),
            textInput('ignore_me', 'Item types to not distribute (semi-colon seperated list)', ""),
            numericInput('holdback_frac', 'Inventory Distribute %', 95, 0, 100),
            textInput('hosp_thresh', 'Hospital Days of Supply Threshold', Inf),
            actionButton('make_allocs', "Run Allocations"),
            textOutput('alloc_response')
            ),
    tabItem(tabName = 'sessionInfo',
            textOutput('sesh_deets'),
            textOutput('Rhome'))
  )
)

ui = dashboardPage(header, sidebar, body)

