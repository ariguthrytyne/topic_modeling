# ============================================================================================ #
#                                                                                              #
#  The script below helps us to identify ad extract cross referenced resolutions. Following    #
#  strategy will be applied:                                                                   #
#                                                                                              #
#      1- extract required resolution data                                                     #
#                                                                                              #
#      2- perform initial cleaning  of the resolution_id column                                #
#                                                                                              #
#      3- detect all matches of potential cross reference resolutions                          #
#                                                                                              #
#      4- perform final cleaning                                                               #
#                                                                                              #
#      5- ...                                                                                  #
#                                                                                              #
#                                                                                              #
#     Input Parameters:                                                                        #
#     ----------------                                                                         #
#                                                                                              #
#        (1) no specific input required expect db/es connection parameters                     #
#                                                                                              #
#                                                                                              #
#     Output Parameters:                                                                       #
#     -----------------                                                                        #
#      ...                                                                                     #
#                                                                                              #
# ============================================================================================ #




#======================
# (1) PREPARATION STEPS ####
#====================== 


# -----------------------------  
# (11) LOADING REQUIRED PACKAGES ####
# -----------------------------

library(tidyverse)
library(rvest)
library(stringr)
library(xml2)
library(dplyr)
library(magrittr)
library(data.table)
library(pdftools)
library(stringi)
library(XML)
library(fulltext)
library(lubridate)
library(sjmisc)


# -------------------------  
# (12) LOADING REQUIRED DATA ####
# -------------------------

message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext_init_2.rds')

# removing resolutions having missing full text
ind <- which( (is.na(resolution_data_fulltext$ResolutionFullText) == TRUE)| (resolution_data_fulltext$ResolutionFullText == ""))
resolution_data_fulltext <- resolution_data_fulltext[-ind, ]




#=====================
# (2) DATA PREPARATION ####
#===================== 


# ---------------------------------  
# (21) PERFORMING REQUIRED CLEANING  ####
# ---------------------------------

# getting number of rows of the data set
nbr_rows <- nrow(resolution_data_fulltext)
stemmed_res_hyper_links <- rep(0, nrow(resolution_data_fulltext))


# resolutions with square parentheses
res_with_square_parentheses <- resolution_data_fulltext$Hyperlink %>% grep('\\[.*?\\]', ., value = TRUE)

# removing square parentheses from the 
for(k in 1:nbr_rows){
  print(paste0("running iteration number: ", k))
  
  tryCatch({
    
    if((resolution_data_fulltext$Hyperlink[k] %in% res_with_square_parentheses) == TRUE) {
      
      temp3 <- substr(resolution_data_fulltext$Hyperlink[k],1,nchar(resolution_data_fulltext$Hyperlink[k])-3)
      
    }
    else if (grepl("^[[:alpha:]]",str_sub(resolution_data_fulltext$Hyperlink[k], -1)) == TRUE) {
      temp3 <- substr(resolution_data_fulltext$Hyperlink[k],1,nchar(resolution_data_fulltext$Hyperlink[k])-1)
      
      
    }
    else {
      temp3 <- resolution_data_fulltext$Hyperlink[k]
      
    }
    stemmed_res_hyper_links[k] <- temp3
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}  
resolution_data_fulltext$ResStemmedIDs <- stemmed_res_hyper_links




#=====================
# (3) DATA PREPARATION ####
#===================== 


# -------------------------------------------  
# (31) RETRIEVING CROSS REFERENCED RESOLUTIONS  ####
# -------------------------------------------

# initializing result vector
res_cross_references <- rep(NA, nrow(resolution_data_fulltext))

# get number of available data-rows
nbr_rows <- nrow(resolution_data_fulltext)

# retrieving cross-referenced resolutions
for(j in 1:nbr_rows){
  
  print(paste0("running iteration number: ", j))
  tryCatch({
    
    # do not consider the NAs
    if(!is.na(resolution_data_fulltext$ResStemmedIDs[j])){
      
      # get resolution full text and resolution id
      res_text <- resolution_data_fulltext$ResolutionFullText[j]
      res_id <- gsub(" ", "", resolution_data_fulltext$ResStemmedIDs[j]) #
      
      # counting number of characters contained in the res_id-string
      nchar_res_id <- nchar(res_id)
      
      # get location of the resolution pattern
      res_pattern <- "\\d{2,}/\\d{1,}.*?"
      
      # extracting matching strings from the resolution (letter/digit/digit)
      matches <- str_extract_all(res_text, res_pattern) 
      
      # getting location of the matches string
      location_matches <- stringr::str_locate_all(res_text, pattern = res_pattern)
      
      # converting the matrix to data frame for my easier subletting because i want the end
      all_as_df <- as.data.frame(location_matches)
      
      # subsetting only the end positions as they are the ones i want to concatenate to
      only_end <- all_as_df$end
      
      # subsetting then start positions as ist also want to concatenate to it and see start of my string.
      only_start <- all_as_df$start
      
      # subtract number to start position to get substring
      only_start_subtract <- only_start - 6
      # adding a number  to all the end positions (only_end) in order to get the other substring i want to concatenate
      # only_end_add_3 <- only_end + (nchar_res_id - 6 + 1)
      only_end_add <- only_end + 10 # maximal length of the res_id string
      
      # add only_end_add  and only_start_add to df all_as_df 
      all_as_df$start_sub <- only_start_subtract
      all_as_df$end_add <- only_end_add

      # getting the extended res_id strings
      check_it <- str_sub(res_text, all_as_df$start, all_as_df$end_add)
      check_it_ohne_add <- str_sub(res_text, all_as_df$start, all_as_df$end)
      
      
      # the parts below not sure we need anymore
      ##############################################################################################
      
      ################################################################################################
      
      
      # ideally use an if function for the remaining 8
      # idea: use the line without gsub above and find location of empty space. 
      # subtract -1 from this position and then get the string we want
      
      
      # identifying cross-reference resolutions
      length(check_it_ohne_add)
      
      #cross_ref_res_ids2 <- sjmisc::str_contains(x = check_it_ohne_add, pattern = res_id, ignore.case = TRUE)
      cross_ref_res_ids <- stringr::str_detect(string = res_id , pattern = check_it_ohne_add)
      
      
      # idea: 
      
      # temp_1 <- check_it[cross_ref_res_ids == TRUE]
      # temp_1_json <-  rjson::toJSON(temp_1)
      
      temp <- check_it_ohne_add[cross_ref_res_ids == FALSE]
      
      
      temp_json <-  rjson::toJSON(unique(temp))
      
      res_cross_references[j] <- temp_json
      
      # remove pages match e.g 1/13, ...etc, by matching with page numbers
      #cleaned_temp <- grepl(pattern = resolution_data_fulltext$ReolutionNumberofPages[j], x= temp)
      
      
    }
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} 


# ---------------------------------------  
# (32) REMOVING STRINGS WRONGLY EXTRACTED ####
# ---------------------------------------

# extracting digit stemm of resolution_id
reg_exp <- regex("A/RES/", ignore_case = TRUE)
all_res <- gsub(reg_exp,"", resolution_data_fulltext$ResStemmedID)
all_res_unique <- unique(all_res)
res_cross_ref_clean <- rep(NA, nbr_rows)

# cleaning calculated cross references vector
for(j in 1:nbr_rows){
  
  # get selected resolution
  res_id1 <- resolution_data_fulltext$Hyperlink[j]
  res_id2 <- resolution_data_fulltext$ResStemmedIDs[j]
  
  # get list of potential cross references resolutions for the selected resolution
  temp <- rjson::fromJSON(res_cross_references[j])
  
  # removing non valid resolutions from temp
  temp_clean <- intersect(temp, all_res_unique)
  temp_clean_json <-  rjson::toJSON(unique(temp_clean))
  
  res_cross_ref_clean[j] <- temp_clean_json
}


# ------------------------------------------  
# (33) SAVING PERMANENTLY CALCULATED RESULTS  ####
# ------------------------------------------

# adding created column to the data frame
resolution_data_fulltext$ResolutionCrossReferences <- res_cross_ref_clean

# saving extended resolution_data as rds-file
 saveRDS(resolution_data_fulltext, file="./data/resolution_data_fulltext.rds")
 




