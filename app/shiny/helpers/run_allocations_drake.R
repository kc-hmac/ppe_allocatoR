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
  
  #governing variables
  ltcf_categories = c('snf + alf', 'afh', 'supported living', 'alf', 'snf', 'ltcf')
  
  #Inputs! file paths
  template = file.path('./templates/template_order_51.xlsx')
  tiering = file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))
  orders = file.path(fold, paste0('order_list_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))
  item_class = file.path(fold, "item_classifications.csv")
  replacements = file.path(fold, 'replacements.xlsx')
  inv_fp = file.path(fold, 'inv', paste0('inventory_',cycle_mo, cycle_day, '_', inv_v,'.csv'))
  hosp = file.path(fold, 'hospital_data.csv')
  residents <- file.path('templates', 'ltcf_licensed_comprehensive.csv')
  cases = file.path(templates, 'linelist.csv')
  beds <- residents
  
  acrciq <- file.path(fold, 'acrciq.xlsx')
  if(!file.exists(acrciq)) acrciq <- file.path(fold, 'acrciq.csv')
  chgs <- file.path(fold, 'chgs.xlsx')
  if(!file.exists(acrciq)) chgs <- file.path(fold, 'chgs.csv')
  
  #Outputs
  fillable = file.path(output, paste0('asum_fillable', suffix,'.csv'))
  considered = file.path(output, paste0('asum_consider', suffix,'.csv'))
  allords = file.path(output, paste0('asum_all', suffix,'.csv'))
  lefts = file.path(output, paste0('leftovers', suffix,'.csv'))
  oot_excel = file.path(output, paste0('picklist', suffix,'.xlsx'))
  out_excel_by_tier = file.path(output, paste0('picklist', suffix,'_tier_', runtiers, '.xlsx'))
  oot_wide = file.path(output, paste0('picklist_wide', suffix,'.csv'))
  oot_dr = file.path(output, paste0('distribution_report', suffix,'.csv'))
  oot_no_1 = file.path(output, paste0('no_allocation_wanum', suffix,'.csv'))
  oot_no_2 = file.path(output, paste0('no_allocation_orders', suffix,'.csv'))
  oot_gowns = file.path(output, paste0('unfilled_gowns', suffix,'.csv'))
  oot_ltcf_1 = file.path(output, paste0('ltcf_allocations', suffix,'.csv'))
  oot_ltcf_2 = file.path(output, paste0('ltcf_allocations_sum', suffix,'.csv'))
  oot_ltcf_3 = file.path(output, paste0('ltcf_allocations_factype', suffix,'.csv'))
  oot_hosp_1 = file.path(output, paste0('hosp_allocations', suffix,'.csv'))
  oot_hosp_2 = file.path(output, paste0('hosp_allocations_sum', suffix,'.csv'))
  sum_cycle = file.path(output, paste0('sum_cycle_valid', suffix,'.csv'))
  
  r_make('run_allocations.R')
  
  
}