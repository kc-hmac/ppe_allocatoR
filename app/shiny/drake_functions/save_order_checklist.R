
save_order_checklist= function(pl){
  
  pl = pl[,.(agency, order_ids, Region, Address)]
  
  # sort by region, agency
  setorder(pl, Region, agency)
  
  # tidy up column names
  setnames(pl, c('Agency', 'Order IDs', 'Route', 'Address'))
  
  # add Delivered column
  pl$Delivered = ""
  
  return(pl)
}
