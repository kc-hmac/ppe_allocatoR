library('shiny')
library('shinydashboard')
library('openxlsx')
library('data.table')
library('tools')

dir2look = ifelse(basename(getwd()) == 'shiny', 'helpers/', './app/shiny/helpers/')
for(fff in list.files(dir2look, full.names = TRUE)) source(fff) 