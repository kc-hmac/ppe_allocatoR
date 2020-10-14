#' Review the LTCF data for QA analysis prior to allocations
#' @param fold directory path- location of the working directory
#' @param date cycle date
#' @param ordersandtiers_version numeric/character- version of the tiers files to use
#' @param open_confirmed_investigations filepath to Open and Confirmed Investigations
qa_ltcfs = function(fold,
                    date,
                    ordersandtiers_version,
                    open_confirmed_investigations){
  investigations = setDT(load_spreadsheet(open_confirmed_investigations))
  #force character license for join
  investigations[, LicenseNumber := as.character(LicenseNumber)]

  #Load cycle information
  cycle_day = mday(date)
  cycle_mo = month(date)

  #Create current tier file path
  ot_v = ordersandtiers_version
  suffix = paste0('_', cycle_mo,cycle_day)
  output = file.path(fold)
  out_qa_order = file.path(output, paste0('qa_ltcfs', suffix,'.csv'))

  tiering = setDT(load_spreadsheet(file.path(fold, paste0('tiers_', cycle_mo, cycle_day, '_', ot_v, '.xlsx'))))

  # filter for LTCFs only
  ltcfs = tiering[type == 'ltcf']

  # capture warnings that may require manual fix
  warnings = list('None')

  #Validation 1: Check Cases file to warn about missing licenses for Active LTCF/Supported Living entities that won't be joined to Tier file
  case_missing_licenses = filter(investigations, status == "Active") %>%
    filter(facility_type == "LTCF" | facility_type =="SUPPORTED LIVING") %>%
    filter(is.na(LicenseNumber))

  if(nrow(case_missing_licenses)>0){
    warning_message <- paste('The following agencies classified as LTCF or SUPPORTED LIVING are missing license numbers from Active Cases file:',
                             paste0(case_missing_licenses$facility_name, collapse = ', '))
    warning(warning_message)
    warnings[[1]] <- warning_message
  }

  #Output merged Tiers and Cases for manual/viz inspection
  ltcfs = filter(ltcfs, lnum !="")

  # join item classification to orders
  ltcfs = setDT(merge(ltcfs, investigations, all = T, by.x = 'lnum', by.y = 'LicenseNumber'))

  # create a field with agency or name to use to sort for missing license in investigations file
  ltcfs[is.na(agency), agency_or_facility_name := facility_name]
  ltcfs[is.na(facility_name), agency_or_facility_name := agency]

  qa_file = select(ltcfs, c(agency_or_facility_name,wa_num,lnum,agency,address.x,dbid,facility_name,address.y,facility_type,status,last_onset,notification_date,record_id,wdrs_obx))

  #print full
  write.csv(qa_file,file = out_qa_order, row.names = F, na = '')
  print(paste0('QA file created ',out_qa_order, '; Warning: ',warnings[[1]]))
}
