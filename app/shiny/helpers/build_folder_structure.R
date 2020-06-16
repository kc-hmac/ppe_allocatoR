#' Build the folder structure for a new round of stuff
#' 
#' 
#' 
build_folder_structure = function(date, base_folder){
  
  stopifnot('Directory does not exist' = dir.exists(base_folder))

  folder_name = paste0('distribute_',month(date),'-',mday(date),'-', year(date))
  
  if(file.path(dirname(base_folder), folder_name) == base_folder){
    fold = file.path(dirname(base_folder), folder_name)
  }else{
    fold = file.path(base_folder, folder_name)
  }
  
  dir.create(fold)
  dir.create(file.path(fold, 'inv'))
  
  return(fold)

}