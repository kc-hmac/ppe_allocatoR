print(getwd())
source('startup.R')
server = function(input, output, session) {
  
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })
  
  cache = reactiveValues()
  
  #build the folder when the button is clicked
  output$workfolder = renderText({
    req(input$go_validate)
    
    isolate({
      req(input$cycle_date, input$workdir)
      ret <- build_folder_structure(input$cycle_date, input$workdir)
      cache$workdir <- ret
    })
    
  })
  
  #Create the orders and tier files
  output$t_o <- renderText({
    req(input$ot_run, cache$workdir, input$cycle_date)
    isolate({
      
      order_and_tiers(fold = cache$workdir, date = input$cycle_date, t1 = input$ot_t1$datapath,
                      t2 = input$ot_t2$datapath, order_v = input$ot_ver, load_from_previous = input$ot_use_prev,
                      prev_v = input$ot_prev_ver, previous_week = input$ot_prev_sum$datapath,
                      dump = input$ot_dump$datapath, add_fp = input$ot_add$datapath)
    })
  })
  
  
}
