prep_inventory = function(fold, date, old, new, version){
  cycle_mo = month(date)
  cycle_day = mday(date)
  
  old = load_spreadsheet(old)
  
  #try reading as an xml
  hold = try(read_xml(new), silent = TRUE)
  if(!inherits(hold, 'try-error')){
    new = hold
    new = as_list(new)
    
    #extract the table
    new = new[['Workbook']][['Worksheet']][['Table']]
    
    #find the row where the data starts
    extract_rowdata = function(x){
      dat = unlist(lapply(x, function(y) y$Data[[1]]))
      return(dat)
    }
    
    dat = lapply(new, extract_rowdata)
    
    #find the header row
    header =which(vapply(dat, function(x){
      if(length(x)>0){
        return(x[1] %in% 'Item')
      }else{
        FALSE
      }}, T))
    
    stopifnot(length(header) == 1)
    
    datarows =which(vapply(dat, function(x){
      if(length(x)>0){
        if(x[1] %in% '1424') return(TRUE)
        
        return(substr(x[1],1,3) == 'C19')
        
      }else{
        FALSE
      }}, T))
    
    #construct dataset
    dt = lapply(datarows, function(x) as.data.table(t(dat[[x]])))
    dt = rbindlist(dt, fill = T )
    setnames(dt, dat[[header]][seq_len(ncol(dt))])
    
  }else{
    dt = setDT(read_excel(new))
    setnames(dt, unlist(dt[4,]))
    dt = dt[5:nrow(dt)]
    dt = dt[1:(min(which(is.na(dt[, Item])))-1)]
  }
  new = copy(dt)
  
  new = merge(new, old[, .(each_per_su, item_type, size, Item)], all.x = T, by = 'Item')
  new[, ship_u := `Qty Avbl`]
  
  write.csv(new, file.path(fold, 'inv', paste0('inventory_',cycle_mo, cycle_day, '_', version,'.csv')), row.names = F, na = "")
  return(file.path(fold, 'inv', paste0('inventory_',cycle_mo, cycle_day, '_', version,'.csv')))
}