#' Create a sheet for classifications of items.
#' @param fold file path of the output/working folder
#' @param date date of the cycle
#' @param old file path to the old classification list
#' @param new file path to the new orders
#' 
prep_item_classifications = function(fold, date, old, new){
  new = load_spreadsheet(new)
  old = load_spreadsheet(old)
  
  old[, item := trimws(trimws(item, whitespace = "[\\h\\v]"))]
  new[, item := trimws(trimws(item_requested, whitespace = "[\\h\\v]"))]
  
  new = unique(new[, .(item)])
  new = rbind(old, new[!item %in% old[, item]], fill = T)
  
  cycle_mo = month(date)
  cycle_day = mday(date)
  
  out = file.path(fold, 'item_classifications.csv')
  
  write.csv(unique(new), row.names = F, file = out, na = "")
  
  return(out)
  
}
