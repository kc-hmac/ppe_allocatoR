

hosp = file.path(base, paste0("hospital_", hosp_date, '.csv')) #inferred
ltcf_categories = c('snf + alf', 'afh', 'supported living', 'alf', 'snf', 'ltcf')

residents = "C:/Users/dcasey/ppe/ltcf/ltcf_long_list.xlsx" #templates
cases = paste0("J:/dc_ll/Linelist_Facility_data_", ll_date, ".csv") #inferred
cw = list.files('C:/Users/dcasey/ppe/ltcf/', pattern = 'DBID-DSHS', full.names = T) #inferred
cw = cw[which(grepl(cw_date, cw))] #inferred
stopifnot(length(cw)==1) #inferred
beds = "C:/Users/dcasey/ppe/ltcf/ltcf_licensed_comprehensive.csv" #templates
acrciq = file.path(base, "acrc_iq.xlsx") #inferred
chgs = file.path(base, "type_chg.xlsx") #inferred

sized_items = c('non-latex gloves', 'scrub pants', 'scrub pants', 'latex gloves', 'N95', 'coveralls') #input
ignore_me = "" #input
standardize_chinook_addresses = TRUE #input
holdback_frac = .95 #input
hosp_supply = Inf #28

#Pull from cache
for (fff in list.files('C:/Users/dcasey/Documents/code/ppe/drake_functions/', '\\.R$', full.names = T)) {
  source(fff)
}

#fps
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
#if the output file doesn't exist, create it.
if (!dir.exists(file.path(cloc, '.drake')))
  invisible(new_cache(path = file.path(cloc, '.drake')))

cache = drake_cache(file.path(cloc, '.drake'))

plan <- drake_plan(

  #check in user written functions
  funkys = compile_user_functions(),

  #load ppe and do initial tiering
  ppe = target(load_ppe_requests(file_in(!!orders), file_in(!!item_class), file_in(!!tiering), !!sized_items)),

  #addresses
  ads = target(load_ppe_requests(file_in(!!orders), file_in(!!item_class), file_in(!!tiering), !!sized_items, TRUE)),

  #load inventory
  inv = target(load_inventory(file_in(!!inv_fp), !!sized_items, !!holdback_frac)),

  #load and format hospital data
  hospital = target(load_hospital_data(file_in(!!hosp), !!hosp_supply)),

  #load and format ltcf data
  ltcf = target(load_ltcf_data(ppe, !!ltcf_categories, file_in(!!residents), file_in(!!beds), file_in(!!cases), file_in(!!cw))),

  #create weights
  wt = target(create_weights(ppe, hospital, ltcf, file_in(!!acrciq), file_in(!!chgs))),

  #determine what orders to fill
  #also adjusts ltcfs into tier 1 and tier 1.5
  orders = target(order_filler(ppe, inv, ltcf, hospital, !!runtiers, ignore_items = !!ignore_me, inv_mismatch = FALSE)),

  #get where requests and inventory don't match
  mismatch = target(order_filler(ppe, inv, ltcf, hospital, !!runtiers, ignore_items = !!ignore_me, inv_mismatch =  TRUE)),

  #allocate and assign
  allocations = target(assign_and_allocate(orders, inv, wt,ltcf_categories = !!ltcf_categories, !!replacement_file)),

  #confirm allocations don't overallocate and create leftovers summary
  leftovers = target(find_leftovers(inv, allocations)),

  write_left = target(write.csv(leftovers, row.names = F,
                                file_out(!!lefts))),

  #construct full agency summary
  sum_full = target(agency_summary(allocations, type = 'all',
                                   outpath = file_out(!!allords),
                                   tiers = !!runtiers)),

  #construct summary by type and item category

  #create a pick list and write (wide format) and add delivery info
  pl_wide = target(create_wide_pl(allocations, ads, !!standardize_chinook_addresses)),

  #write to excel
  out_excel = target(save_picklist(pl_wide, !!template, file_out(!!oot_excel))),

  out_xl_tier = target(save_picklist(pl_wide, !!template, file_out(a), t), transform = map(t = !!runtiers, a = !!out_excel_by_tier, .id = t)),

  #write out wide picklist
  out_wide = target(write.csv(pl_wide, file_out(!!oot_wide), row.names = F)),

  #write out distribution report
  out_dr = target(write.csv(distribution_report(allocations, inv), row.names = F, file = !!oot_dr)),

  #write out no orders
  no_orders = target(no_order(sum_full, file_out(!!oot_no_1), file_out(!!oot_no_2))),

  #unfilled gowns
  unfil_gowns = target(write.csv(sum_full[fill_me == 1 & percent_filled<100 & item_type == 'gowns'], row.names = F, file = file_out(!!oot_gowns))),

  ltcf_sum = target(make_ltcf_summary(sum_full,file_in(!!residents), file_out(!!oot_ltcf_1), file_out(!!oot_ltcf_2),file_out(!!oot_ltcf_3))),
  
  hosp_sum = target(make_type_summary(sum_full,'hospital', file_out(!!oot_hosp_1), file_out(!!oot_hosp_2))),
  
  overall = target(write.csv(summarize_cycle(sum_full),file = !!sum_cycle, row.names = F ))
  
)

a = drake_config(plan, cache_log_file = 'C:/Users/dcasey/Documents/code/ppe/cache_log.csv',
                 cache = cache)
a
