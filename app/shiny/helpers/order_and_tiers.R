#largely based of dist_helpers/prep_order_sheet.R
order_and_tiers = function(fold, date, t1, t2, order_v, load_from_previous, prev_v,
                           previous_week, dump, add_fp){
  
  cycle_mo = month(date)
  cycle_day = mday(date)
  
  #Read in the orders
  t1 = load_spreadsheet(t1)
  t2 = load_spreadsheet(t2)
  dump = load_spreadsheet(dump)
  previous_week = load_spreadsheet(previous_week)
  if(!missing(add_fp) && !is.null(add_fp)) add = load_spreadsheet(add_fp)
  
  
  stopifnot(all(names(t1) == names(t2)))
  
  orders = rbind(t1, t2, fill = T)
  
  if(exists('add') && inherits(add, 'data.table')){
    stopifnot(all(names(add) %in% names(orders)))
    orders = rbind(orders, add, fill = TRUE)
  }
  
  
  #standardize orders
  setnames(orders, c("King.County.Tracking.Number", "Agency.Name", "Facility.Type", 
                     "License./.ID", "Item.Requested.(Paste.item.requested.from.thecity,.tribal.resource.request.form.here)", 
                     "Enter.the.total.quantity.of.items.(Each).needed.(Paste.quantity.requested.here)", 
                     "Tier.Number", "POC", "POC.Email", "POCPhone", "Delivery.Address", "Requestor.Email"),
           c('wa_num', 'agency', 'logs_type', '_lnum', 'item_requested', 'requested', 'logs_tier', 'poc', 'email', 'phone', 'address', 'email2')
  )
  orders = orders[, .(wa_num, agency, logs_type, `_lnum`, item_requested, requested, logs_tier)]
  orders[, wa_num := trimws(wa_num, whitespace = "[\\h\\v]")]
  
  #fix addresses in the orders
  dump = dump[county == 'King']
  dump[, wa_num := trimws(rndNum, whitespace = "[\\h\\v]")]
  dump = dump[wa_num %in% orders[, wa_num]]
  dump = dump[, .(wa_num, address, email = ReqSiteEmail, phone = ReqSitePhone, POC = ReqSitePOC, status = ReqStatus)]
  dump[, address := gsub('\t', "", address, fixed = T)]
  dump = unique(dump)
  
  stopifnot('Wa nums in orders missing from dump' = all(unique(orders[, wa_num]) %in% dump[, wa_num]))
  
  dump = dump[wa_num %in% orders[, wa_num]]
  
  #For reasons I can't fathom, webeoc allows multiple discrete requests to have the same tracking number.
  #The next several lines check for that and use a few hueristics to determine whether that is a problem
  multi_dump = dump[, .N, wa_num][N>1]
  
  if(nrow(multi_dump)>0){
    
    dump = dump[!(wa_num %in% multi_dump[, wa_num] & !status %in% "Order in Process")]
    
    md_2 = dump[, .N, wa_num][N>1]
    if(nrow(md_2) > 0){
      stop(paste0('Error: tracking numbers (wa_num) are not unique per request: ', paste0(md_2[, wa_num], collapse = ', ')))
    }else{
      warning(paste0('WA nums were not unique to a request, but filtering on status made them unique. Worth double checking: ', paste0(multi_dump[, wa_num], collapse = ', ') ))
    }
  }
  
  start_n = nrow(orders)
  orders[, n_r_start := .N, wa_num]
  orders =merge(orders, dump, all.x = T, by = 'wa_num')
  orders[, n_r_end := .N, wa_num]
  end_n = nrow(orders)
  
  if(end_n != start_n){
    stop(end_n - start_n)
  }
  
  
  #create tiering file
  tiers = orders[, .(wa_num, agency, address, lnum = `_lnum`, type = "",
                     newname = "", notes = "", current.tier = "",
                     priority = "", logs_type, logs_tier)]
  tiers = unique(tiers)
  
  if(start_n != end_n){
    stop('WEBEOC PROBABLY FARTED DUPLICATE IDs AGAIN')
  }else{
    
    ttt = unique(tiers[ ,.(wa_num, agency, address, lnum = lnum, type = "", newname = "", 
                           notes = "", current.tier = "", priority = "", logs_lnum = lnum, logs_type, logs_tier)])
    
    if(load_from_previous){
      old = load_spreadsheet(paste0(fold, 'tiers_', cycle_mo, cycle_day, '_', prev_v, '.xlsx'))
      
      ttt = rbind(old, ttt[!wa_num %in% old[, wa_num]])
    }
    
    
    #check previous week
    prev_ids = trimws(unlist(strsplit(previous_week[, order_ids], split = ',', fixed = T)), whitespace = '[\\h\\v]')
    dupes = intersect(ttt[, wa_num], prev_ids)
    
    write.xlsx(orders, file.path(fold, paste0('order_list_', cycle_mo, cycle_day,'_', order_v, '.xlsx')))
    
    write.xlsx(ttt, file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', order_v, '.xlsx')))
    write.csv(ttt, file.path(fold, paste0('tiers_raw.csv')), row.names = F, na = "")
    
    if(length(dupes)>1) warning(paste0(dupes, collapse = ', '))
    
  }
  
  return(paste0('Tier File: ', file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', order_v, '.xlsx'))))
  
  
}
