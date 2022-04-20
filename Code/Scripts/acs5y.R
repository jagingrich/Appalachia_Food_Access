#necessary packages
library(readxl)
library(tidyverse)
library(stringr)

#reference
#https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/5_year/ACS_2005-2009_SF_Tech_Doc.pdf

#function
acs5y <- function(year = NULL, state = NULL, cenunit = NULL, table = NULL, error = F, list_files = F){
  h <- getwd()
  setwd(h)
  #missing inputs in function
  #default is to list data tables, but not detail
  list_detail <- F
  if(!is.null(year) & !is.null(state) & !is.null(cenunit) & list_files != T){
    list_files <- F #year, state, or census unit, default to not list data tables
  }
  if(is.null(state)|is.null(cenunit)){
    list_files <- T #no state or census unit, default to list data tables, overriding input
  }
  if(is.null(year)){
    list_files <- T #no year, default to list data tables, overriding input
    year <- 2019 #pulling from most recent year (2019)
  }
  if(is.null(table)){
    list_files <- T #no table input, default to list data tables
    list_detail <- F #if you don't specify a table, you get the table list
  }
  if(!is.null(table) & list_files == T){ #if you specify a table and input list_files = T
    list_detail <- T #you get the table details
  }
  
  #census units
  if(!is.null(cenunit)){
    cenunit = toupper(cenunit)
    cenunit2 <- cenunit
    if("TRACT" %in% cenunit){
      cenunit2 <- c("COUNTY", cenunit)
    }
    if("BLKGRP" %in% cenunit){
      cenunit2 <- c("COUNTY", "TRACT", cenunit)
    }
    if("COUSUB" %in% cenunit){
      cenunit2 <- c("COUNTY", cenunit)
    }
  
    #summary level
    sumlevel <- data.frame(SUMLEVEL = c("020","030","040","050","060","067","140","150","160","170","230","250","251","252","254","310","314","330","335","350","355","500","610","620","860","950","960","970"),
                           DESC = c("Region", "Division", "State", "State-County", "State-County-County Subdivision", "State-County-County Subdivision-Subminor Civil Division", "State-County-Census Tract", "State-County-Census Tract-Block Group", "State-Place", "State-Consolidated City", "State-Alaska Native Regional Corporation", "American Indian Area/Alaska Native Area/Hawaiian Home Land", "American Indian Area-Tribal Subdivision/Remainder", "American Indian Area/Alaska Native Area (Reservation or Statistical Entity Only)", "American Indian Area (Off-Reservation Trust Land Only)/Hawaiian Home Land", "Metropolitan Statistical Area/Micropolitan Statistical Area", "Metropolitan Statistical Area-Metropolitan Division", "Combined Statistical Area", "Combined New England City and Town Area", "New England City and Town Area", "New England City and Town Area (NECTA)-NECTA Division", "Current Congressional District", "State-State Legislative District (Upper Chamber)", "State-State Legislative District (Lower Chamber)", "5-Digit ZIP code Tabulation Area", "State-School District (Elementary)/Remainder", "State-School District (Secondary)/Remainder", "State-School District (Unified)/Remainder"),
                           ABBR = c("REGION", "DIVISION", "STATE", "COUNTY", "COUSUB", "SUBMCD", "TRACT", "BLKGRP", "PLACE", "CONCIT", "ANRC", "AIANHH", "AITSCE", "AITS", "AIHHTLI", "CBSA", "METDIV", "CSA", "CNECTA", "NECTA", "NECTADIV", "CDCURR", "SLDU","SLDL", "ZCTA5", "SDELM", "SDSEC", "SDUNI")) %>%
      mutate(across(where(is.factor), as.character))
    sumlevel <- sumlevel %>% filter(ABBR == cenunit) %>% pull(SUMLEVEL)
  }
  
  #State name and abbreviation
  if(!is.null(state)){
    state <- str_to_title(state)
    
    st_abb <- data.frame(ST = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District Of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"), 
                         ST_ABB = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
    st_abb = tolower(as.character(st_abb %>% filter(ST == state) %>% pull(ST_ABB)))
  
    state <- gsub(" ", "", state)
  }
  
  #files from ACS FTP
  temp <- tempfile()
  temp2 <- tempfile()
  temp3 <- tempfile()
  temp4 <- tempfile()

  #Sequence table reference
  if(year >= 2013 & year < 2018){
    download.file(paste0("https://www2.census.gov/programs-surveys/acs/summary_file/", year, "/documentation/user_tools/ACS_5yr_Seq_Table_Number_Lookup.xls"), temp3)
    list <- read_excel(temp3)
  }
  if(year < 2013){
    download.file(paste0("https://www2.census.gov/programs-surveys/acs/summary_file/", year, "/documentation/5_year/user_tools/Sequence_Number_and_Table_Number_Lookup.xls"), temp3)
    list <- read_excel(temp3)
  }
  if(year == 2018){
    download.file(paste0("https://www2.census.gov/programs-surveys/acs/summary_file/", year, "/documentation/user_tools/ACS_5yr_Seq_Table_Number_Lookup.csv"), temp3)
    list <- read.csv(temp3)
  }
  if(year >= 2019){
    download.file(paste0("https://www2.census.gov/programs-surveys/acs/summary_file/", year, "/documentation/user_tools/ACS_5yr_Seq_Table_Number_Lookup.xlsx"), temp3)
    list <- read_excel(temp3)
  }
  unlink(temp3)
  colnames(list) <- c("FileID", "TableID", "SequenceNumber", "LineNumber", "StartPosition", "TotalCellsinTable", "TotalCellsinSequence", "TableTitle", "SubjectArea")
  
  #List files?
  if(list_files == T & list_detail == F){ #All tables
    list <- list %>% filter(SubjectArea != "") %>%
      dplyr::select(c(TableID, TableTitle, SubjectArea))
    return(list)
  }
  if(list_files == T & list_detail == T){ #Specific table
    list_name <- list %>% filter(TableID == unlist(strsplit(table, "_"))[1]) %>%
      filter(SubjectArea != "") %>% pull(TableTitle)
    list <- list %>% filter(TableID == unlist(strsplit(table, "_"))[1]) %>%
      filter(LineNumber != "") %>%
      mutate(TableName = list_name) %>%
      mutate(LineNumber = ifelse(nchar(as.character(LineNumber)) == 1, paste0("00", as.character(LineNumber)),
                                 ifelse(nchar(as.character(LineNumber)) == 2, paste0("0", as.character(LineNumber)), as.character(LineNumber)))) %>%
      mutate(TableID = paste0(TableID, "_", LineNumber)) %>%
      dplyr::select(c(TableID, TableName, TableTitle))
    return(list)
  }
  
  #data tables!
  if(list_files == F){
    list <- list %>%
      dplyr::select(c(TableID, SequenceNumber, TableTitle, SubjectArea))
    seq <- unique(list %>% filter(TableID == unlist(strsplit(table, "_"))[1]) %>% pull(SequenceNumber))
    seq2 <- ifelse(nchar(as.character(seq)) == 1, paste0("00", as.character(seq)),
              ifelse(nchar(as.character(seq)) == 2, paste0("0", as.character(seq)), as.character(seq)))

    #file template
    if(year != 2010){
      download.file(paste0("https://www2.census.gov/programs-surveys/acs/summary_file/", year, "/data/", year, "_5yr_Summary_FileTemplates.zip"), temp2)
    }
    if(year == 2010){
      download.file(paste0("https://www2.census.gov/programs-surveys/acs/summary_file/", year, "/data/", year, "_5yr_SummaryFileTemplates.zip"), temp2)
    }	
    
    #column headers
    if(year >= 2018){
      head <- read_excel(unzip(temp2, paste0("seq", seq, ".xlsx"), exdir = temp4))
    }
    if(year < 2016 & year != 2011 & year != 2014 & year != 2016){
      head <- read_excel(unzip(temp2, paste0("Seq", seq, ".xls"), exdir = temp4))
    }
    if(year == 2011){
      head <- read_excel(unzip(temp2, paste0("Seq0", seq2, ".xls"), exdir = temp4))
    }
    if(year == 2014){
      head <- read_excel(unzip(temp2, paste0("seq/Seq", seq, ".xls"), exdir = temp4))
    }
    if(year == 2016){
      head <- read_excel(unzip(temp2, paste0("templates/Seq", seq, ".xls"), exdir = temp4))
    }
    if(year == 2017){
      head <- read_excel(unzip(temp2, paste0("xls_temp/seq", seq, ".xlsx"), exdir = temp4))
    }
    unlink(temp2) #remove temp files
    unlink(temp4) #remove temp files
    
    #ACS data
    fn <- ifelse(cenunit %in% c("TRACT", "BLKGRP"), paste0(state, "_Tracts_Block_Groups_Only.zip"), paste0(state, "_All_Geographies_Not_Tracts_Block_Groups.zip"))
    download.file(paste0("https://www2.census.gov/programs-surveys/acs/summary_file/", year, "/data/5_year_by_state/", fn),temp)
    
    #parsing out the individual data tables
    #geography file
    geog <- read.fwf(unz(temp, paste0("g", year,"5", st_abb, ".txt")), colClasses = "character", header=F, widths = c(6,2,3,2,7,1,1,1,2,2,3,5,5,6,1,5,4,5,1,3,5,5,5,3,5,1,1,5,3,5,5,5,2,3,3,6,3,5,5,5,5,5,1,1,6,5,5,5,40)) #,1000,6,1,44)) dropping columns from name onward, because of problematic characters in names
    colnames(geog) <- c("FILEID", "STUSAB", "SUMLEVEL", "COMPONENT", "LOGRECNO", "US", "REGION", "DIVISION", "STATECE", "STATE", "COUNTY", "COUSUB", "PLACE", "TRACT", "BLKGRP", "CONCIT", "AIANHH", "AIANHHFP", "AIHHTLI", "AITSCE", "AITS", "ANRC", "CBSA", "CSA", "METDIV", "MACC", "MEMI", "NECTA", "CNECTA", "NECTADIV", "UA", "BLANK1", "CDCURR", "SLDU","SLDL", 
                        "BLANK2", "BLANK3", "ZTCA5", "SUBMCD", "SDELM", "SDSEC", "SDUNI", "UR", "PCI", "BLANK5", "BLANK6", "PUMA5", "BLANK7", "GEOID") #,"NAME", "BTTR", "BTRG", "BLANK8") dropping colnames from "NAME" onward
    geog <- geog %>% dplyr::select(c("FILEID", "STUSAB", "SUMLEVEL", "COMPONENT", "LOGRECNO", "GEOID", "STATE", all_of(cenunit2))) %>% #, "NAME")) %>%
      mutate_if(is.character, str_trim) %>%
      filter(SUMLEVEL %in% sumlevel) %>%
      mutate(LOGRECNO = as.numeric(LOGRECNO))

    if(error == F){ #Estimates
      est <- read.table(unz(temp, paste0("e", year, "5", st_abb, "0", seq2, "000.txt")), sep=",", header=F) 
      colnames(est) <- colnames(head)
      est <- est %>% mutate_at(vars(starts_with(paste0(table, "_"))), as.character) %>% 
        mutate_at(vars(starts_with(paste0(table, "_"))), as.numeric)
      est <- merge(geog, est %>% dplyr::select(-c(FILEID, STUSAB)), by="LOGRECNO") %>%
        mutate(YEAR = year, FILETYPE = "ESTIMATE") %>%
        dplyr::select(GEOID, STUSAB, STATE, all_of(cenunit2), YEAR, FILETYPE, starts_with(paste0(table, "_")))
    return(est)
    }
  
    if(error == T){ #Margin of error
      moe <- read.table(unz(temp, paste0("m", year, "5", st_abb, "0", seq2, "000.txt")), sep=",", header=F) 
      colnames(moe) <- colnames(head)
      moe <- moe %>% mutate_at(vars(starts_with(paste0(table, "_"))), as.character) %>% 
        mutate_at(vars(starts_with(paste0(table, "_"))), as.numeric)
      moe <- merge(geog, moe %>% dplyr::select(-c(FILEID, STUSAB)), by="LOGRECNO") %>%
        mutate(YEAR = year, FILETYPE = "ERROR") %>%
        dplyr::select(GEOID, STUSAB, STATE, all_of(cenunit2), YEAR, FILETYPE, starts_with(paste0(table, "_")))
    return(moe)
    }
    unlink(temp) #remove temp files
  }
}
