# -----------------------------  
# (1) LOADING REQUIRED PACKAGES ####
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
# (2) LOADING REQUIRED DATA ####
# -------------------------

message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext.rds')

# removing resolutions having missing full text
ind <- which( (is.na(resolution_data_fulltext$ResolutionFullText) == TRUE)| (resolution_data_fulltext$ResolutionFullText == ""))
resolution_data_fulltext <- resolution_data_fulltext[-ind, ]


# -------------------------------  
# (3) PERFORMING INITIAL CLEANING  ####
# -------------------------------

# getting number of rows of the data set
numrows_data <- nrow(resolution_data_fulltext)
stemmed_res_hyper_links <- rep(0, nrow(resolution_data_fulltext))


# resolutions with square parentheses
res_with_square_parentheses <- resolution_data_fulltext$Hyperlink %>% grep('\\[.*?\\]', ., value = TRUE)

# performing required cleaning
for(k in 1:numrows_data){
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



# -------------------------------------------  
# (4) RETRIEVING CROSS REFERENCED RESOLUTIONS  ####
# -------------------------------------------

# # removing resolutions with no resolution text
# resolution_data_fulltext <-  resolution_data_fulltext %>%
#   filter(is.na(ResolutionFullText)  == FALSE)

# initializing result vector
res_cross_references <- rep(NA, nrow(resolution_data_fulltext))

# get number of available data-rows
numrows_data <- nrow(resolution_data_fulltext)


# retrieving cross referenced resolutions
for(j in 1:numrows_data){
  
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
      res_pattern  <- "A/RES/"
      res_pattern2 <- "\\d{2,}/\\d{1,}.*?"
      
      # if there is any matches with any of patterns, do the following:
      # letter/digit/digit
      matches <- str_extract_all(res_text, res_pattern2) 
      # print(matches)
      location_matches <- stringr::str_locate_all(res_text, pattern = res_pattern2)
      # print(location_matches)
      
      
      # location_matches <- stringr::str_locate_all(res_text, res_pattern)
      
      # converting the matrix to data frame for my easier subsetting because i want the end
      all_as_df <- as.data.frame(location_matches)
      
      
      # subsetting only the end positions as they are the ones i want to concatenate to
      only_end <- all_as_df$end
      
      # subsetting then start positions as i aslso want to concatenate to it and see start of my string.
      only_start <- all_as_df$start
      
      # subtract number to start position to get substring
      only_start_subtract <- only_start - 6
      # adding a number  to all the end positions (only_end) in order to get the other substring i want to concatenate
      # only_end_add_3 <- only_end + (nchar_res_id - 6 + 1)
      only_end_add <- only_end + 10 # maximal length of the res_id string
      
      # add only_end_add  and only_start_add to df all_as_df 
      all_as_df$end_add <- only_end_add
      all_as_df$start_sub <- only_start_subtract
      
      # getting the extended res_id strings
      check_it <- str_sub(res_text, all_as_df$start, all_as_df$end_add)
      check_it_ohne_add <- str_sub(res_text, all_as_df$start, all_as_df$end)
      #check_it <- gsub(" ","", str_sub(res_text, all_as_df$start, all_as_df$end_add))
      
      
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



# ---------------------------  
# (4) SAVING REQUIRED RESULTS  ####
# ---------------------------

# adding result to the resolution data frame
resolution_data_fulltext$res_cross_references <- res_cross_references

# extracting digit stemm of resolution_id
reg_exp <- regex("A/RES/", ignore_case = TRUE)
temp <- gsub(reg_exp,"", resolution_data_fulltext$ResStemmedID)
unique_res_di_digits <- unique(temp)
res_cross_ref_clean <- rep(NA, numrows_data)

# cleaning calculated cross references vector
for(j in 1:numrows_data){
  
  # get selected resolution
  res_id1 <- resolution_data_fulltext$Hyperlink[j]
  res_id2 <- resolution_data_fulltext$ResStemmedIDs[j]
  res_id3 <- resolution_data_fulltext$Res_id_digits[j]
  
  # get list of potential cross references resolutions for the selected resolution
  temp <- rjson::fromJSON(resolution_data_fulltext$res_cross_references[j])
  
  # removing non valid resolutions from temp
  temp_clean <- intersect(temp, unique_res_di_digits)
  temp_clean_json <-  rjson::toJSON(unique(temp_clean))
  
  res_cross_ref_clean[j] <- temp_clean_json
  
}







# # saving extended resolution_data as rds-file
 saveRDS(resolution_data_fulltext, "ResolutionFullText_cross_refs_digit.rds")


# ##########checks
# 
# # first remove those with cross refs empty
# 
# res_data_cross_ref_empty  <- resolution_data_fulltext[ind, ] # 39 obs.
# resolution_data_fulltext <- resolution_data_fulltext[-ind, ] # now 3958
# set.seed(3458945)
# 
# sample1 <- resolution_data_fulltext[sample(nrow(resolution_data_fulltext), 15), ]


# --------------------------  
# (5) REMOVING WRONG RESULTS  ####
# --------------------------

# getting list of all available resolutions
available_res <- resolution_cross_ref$Hyperlink

# removing common resolution-pattern
available_res2 <- gsub("A/RES/", "", available_res)

# removing "[Letter]" pattern
k <- 5
test <- grepl("^[[:alpha:]]",str_sub(available_res2[k], -1) == TRUE) 


