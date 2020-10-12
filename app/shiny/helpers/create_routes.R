#' Prepare the hospital data for analysis
#' @param fold directory path- location of the working directory
#' @param date cycle date
#' @param cycle_version numeric/character- version of the cycle. Usually increments up from 1. Failing to use a new value can result in files being overwritten (which is sometimes desirable)
#' @param ordersandtiers_version numeric/character- version of the orders and tiers files to use
create_routes = function(fold, 
                        date,
                        cycle_version,
                        ordersandtiers_version){
  # debug
  # fold = 'C:/Users/alibrown/Documents/ppe/distribute_8-28-2020'
  # date = as.Date(c("2020-08-28"))
  # cycle_version = '2'
  # ordersandtiers_version = '2'
  
  template = file.path('./templates/template_order_87.xlsx')
  #routes by region
  regions = c('north_seattle_shoreline','bellevue','sw_king_county','east_king_county','renton','south_seattle_downtown','se_king_county')
  
  #Load cycle information
  cycle_day = mday(date)
  cycle_mo = month(date)
  cycle_v = cycle_version
  
  ot_v = ordersandtiers_version
  suffix = paste0('_', cycle_mo,cycle_day,'_v', cycle_v)
  output = file.path(fold, suffix)
  out_excel_by_region = file.path(output, paste0('picklist', suffix,'_region_', regions, '.xlsx'))
  
  tiering = setDT(load_spreadsheet(file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))))
  tiering = tiering[, c('wa_num','region')]

  wide_pl_region = setDT(load_spreadsheet(file.path(output, paste0('picklist_wide', suffix,'.csv'))))
  # merge wide picklist with latest tiers
  pl_wide_region = merge(wide_pl_region, tiering, all.x = T, by.x = 'order_ids', by.y = 'wa_num')
  
  #The plan
  plan_route_picklists <- drake_plan(
    out_xl_region = target(save_region_picklist(pl_wide_region, !!template, file_out(a), r), transform = map(r = !!regions, a = !!out_excel_by_region, .id = r)),
  )
  
  make(plan_route_picklists)
  print(paste0('Picklists created in ',file.path(fold),'/',suffix))
} 
