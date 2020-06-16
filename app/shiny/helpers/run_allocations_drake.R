run_allocations_drake <- function(plan_source,
                      fold, 
                      date,
                      cycle_version, 
                      inventory_version,
                      ordersandtiers_version,
                      runtiers,
                      sized_items,
                      ignore_me,
                      standardize_chinook,
                      holdback_frac,
                      hosp_supply){
  #Load cycle information
  cycle_day = mday(date)
  cycle_mo = month(date)
  cycle_v = cycle_version
  inv_v = inventory_version
  ot_v = ordersandtiers_version
  suffix = paste0('_', cycle_mo,cycle_day,'_v', cycle_v)
  output = fold
  
  #construct cache
  dir.create(file.path(output, suffix))
  
  if (!dir.exists(file.path(output, suffix, '.drake')))
    invisible(new_cache(path = file.path(output, suffix, '.drake')))
  
  cache = drake_cache(file.path(output, suffix, '.drake'))
  
  #file paths
  template = file.path('./templates/template_order_51.xlsx')
  tiering = file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))
  orders = file.path(fold, paste0('order_list_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))
  item_class = file.path(fold, "item_classifications.csv")
  replacements = file.path(fold, 'replacements.xlsx')
  inv_fp = file.path(fold, 'inv', paste0('inventory_',cycle_mo, cycle_day, '_', inv_v,'.csv'))
  
  
  
}