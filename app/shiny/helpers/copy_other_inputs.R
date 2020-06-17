copy_other_inputs <- function(fold, linelist, crosswalk, replace, acrciq, chgs){
  file.copy(crosswalk, file.path(fold, paste0('crosswalk.', file_ext(crosswalk))))
  file.copy(replace, file.path(fold, paste0('replacements.', file_ext(replace))))
  file.copy(acrciq, file.path(fold, paste0('acrciq.', file_ext(acrciq))))
  file.copy(chgs, file.path(fold, paste0('chgs.', file_ext(chgs))))
  
  #the line list can sometimes have sensitive data so this to reduce the chance of data leakage
  linelist = load_spreadsheet(linelist)
  linelist = linelist[, .(DBID, `Res Cnt Death`, `Res Cnt Hsp`,
                 `Res Cnt Sym`, `Res Test Pos`,`Classification Value`)]
  
  write.csv(linelist, row.names = F, file.path(fold, 'linelist.csv'))
  
  return('Files copied/stored')
  
  
}