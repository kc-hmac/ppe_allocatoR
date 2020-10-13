#' Prepare the hospital data for analysis
#' @param fold directory path- location of the working directory
#' @param date cycle date
#' @param ordersandtiers_version numeric/character- version of the orders and tiers files to use
qa_orders = function(fold,
                    date,
                    ordersandtiers_version){

  #Load cycle information
  cycle_day = mday(date)
  cycle_mo = month(date)

  ot_v = ordersandtiers_version
  suffix = paste0('_', cycle_mo,cycle_day)
  output = file.path(fold)
  out_qa_order = file.path(output, paste0('qa_orders', suffix,'.csv'))

  items = setDT(load_spreadsheet(file.path(fold, paste0('item_classifications.csv'))))
  items[, item := trimws(trimws(item, whitespace = "[\\h\\v]"))]

  orders = setDT(load_spreadsheet(file.path(fold, paste0('order_list_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))))
  orders = orders[, c('wa_num','item_requested','requested','email','POC')]
  orders[, item_requested := trimws(trimws(item_requested, whitespace = "[\\h\\v]"))]

  tiering = setDT(load_spreadsheet(file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))))
  tiering = select (tiering,-c(logs_lnum,logs_type,logs_tier))

  # join item classification to orders
  order_items = merge(orders, items, all.x = T, by.x = 'item_requested', by.y = 'item')

  # join orders to tiers
  qa_file = merge(order_items, tiering, all.x = T, by = 'wa_num')

  # wa_num	item_requested	requested	email	POC	item_type	size	agency	address	zip	region	lnum	type	newname	notes	current.tier	priority
  qa_file = select (qa_file, c(wa_num,agency,type,item_type,requested,current.tier,priority,region,zip,address,email,POC,lnum,newname))

  #Validation 1: Check there are no NAs in: Type, Tier, Region
  if(!all(!is.na(tiering[, type]))){
    stop(paste('The following agencies are missing type:',
               paste0(tiering[is.na(type), agency], collapse = ', ')))
  }
  if(!all(!is.na(tiering[, current.tier]))){
    stop(paste('The following agencies are missing current.tier:',
               paste0(tiering[is.na(current.tier), agency], collapse = ', ')))
  }
  if(!all(!is.na(tiering[, region]))){
    stop(paste('The following agencies are missing region:',
               paste0(tiering[is.na(region), agency], collapse = ', ')))
  }

  #Validation 2: Check that Priority is not NA for Tier 2s
  tier_2_missing_priority = filter(tiering, current.tier == 2) %>%
    filter(is.na(priority))
  if(nrow(tier_2_missing_priority)){
    stop(paste('The following agencies set as Tier 2 are missing Priority value:',
               paste0(tier_2_missing_priority$agency, collapse = ', ')))
  }

  #Validation 3: Check that acrc/iq, ems new name values
  need_new_names = tiering %>%
    filter(type == "acrc/iq" | type == "ems") %>%
    filter(is.na(newname))
  if(nrow(need_new_names)){
    stop(paste('The following ACRC/IQ or EMS agencies are missing standardized name:',
               paste0(need_new_names$agency, collapse = ', ')))
  }

  ##Validation 4: Check that LTCFs have license and is unique
  ltcfs = tiering %>%
    filter(type == "ltcf")

  ltcfs_missing_license = ltcfs %>%
    filter(is.na(lnum))

  if(nrow(ltcfs_missing_license)){
    stop(paste('The following agencies classified as LTCF are missing license number:',
               paste0(ltcfs_missing_license$agency, collapse = ', ')))
  }

  ##Validation 5: Check that LTCFs license is unique
  license_counts = ltcfs %>%
    group_by(lnum) %>%
    summarize(license_nums=n())

  dupes = filter(license_counts, license_nums>1)
  if(nrow(dupes)){
    stop(paste('The following licenses are duplicated in the Tiers file:',
               paste0(dupes$lnum, collapse = ', ')))
  }

  #Validation 6: Check that License # exists in 2 LTCF files used in Allocation Process
  # TODO: extract licenses including multiples and check ltcf_licensed_comprehensive.csv and ltcf_long_list.xlsx

  #print full
  write.csv(qa_file,file = out_qa_order, row.names = F, na = '')
  print(paste0('QA file created ',out_qa_order))
}
