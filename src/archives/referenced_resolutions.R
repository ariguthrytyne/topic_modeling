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


# -------------------------------  
# (3) PERFORMING INITIAL CLEANING  ####
# -------------------------------


# -------------------------------------------------------------------------------------------------------------
# in the first for loop below, idea was to make all res IDs the same so that i could pattern-match them better 
# in the second for loop. three cases: if a loop has square parentheses, if it just has a letter at end with no
# square parentheses and just the normal ID

# at the end of the second loop, there is a column added to res full text called cross-references which should
# contain the other resolutions if any.
# finally res-full text is saved as rds file.
# -------------------------------------------------------------------------------------------------------------


# getting number of rows of the data set
numrows_data <- nrow(resolution_data_fulltext)
stemmed_res_hyper_links <- rep(0, nrow(resolution_data_fulltext))


# resolutions with square parentheses
res_with_square_parentheses <- resolution_data_fulltext$Hyperlink %>% grep('\\[.*?\\]', ., value = TRUE)

# performing required cleaning
for(k in 1:nrow(resolution_data_fulltext)){
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


# initializing result vector
res_cross_references <- rep(NA, nrow(resolution_data_fulltext))

# removing resolutions with no resolution text
resolution_data_fulltext <-  resolution_data_fulltext %>%
  filter(is.na(ResolutionFullText)  == FALSE)

# get number of available data-rows
numrows_data <- nrow(resolution_data_fulltext)

# retrieving cross referenced resolutions
for(j in 1:numrows_data){
  
  print(paste0("running iteration number: ", j))
  tryCatch({
    
    # this will need to be removed once need to consider all documents
    if(!is.na(resolution_data_fulltext$ResStemmedIDs[j])){
    
      # get resolution full text and resolution id
      res_text <- resolution_data_fulltext$ResolutionFullText[j]
      res_id <- gsub(" ", "", resolution_data_fulltext$ResStemmedIDs[j]) #
      
      # counting number of characters contained in the res_id-string
      nchar_res_id <- nchar(res_id)

      # get location of the resolution pattern
      res_pattern  <- "A/RES/"
      location_matches <- stringr::str_locate_all(res_text, res_pattern)
      
      # converting the matrix to data frame for my easier subsetting because i want the end
      all_as_df <- as.data.frame(location_matches)
      
      # subsetting only the end positions as they are the ones i want to concatenate to
      only_end <- all_as_df$end
      
      # adding a number  to all the end positions (only_end) in order to get the other substring i want to concatenate
      # only_end_add_3 <- only_end + (nchar_res_id - 6 + 1)
      only_end_add <- only_end + 10 # maximal length of tghe res_id string

      # add only_end_add_3 to df all_as_df 
      all_as_df$end_add <- only_end_add
      
      # getting the extended res_id strings
      check_it <- str_sub(res_text, all_as_df$start, all_as_df$end_add)
      #check_it <- gsub(" ","", str_sub(res_text, all_as_df$start, all_as_df$end_add))

      if(length(check_it) > 0){
        for(i in 1:length(check_it)){
          x <- check_it[i]
          temp <- as.data.frame(str_locate_all(x, " "))
          if(nrow(temp) > 0){
            x <- substr(x, 1, temp$end-1)
            check_it[i] <- x 
          }
        }
      }
   
      # ideally use an if function for the remaining 8
      # idea: use the line without gsub above and find location of empty space. 
      # subtract -1 from this position and then get the string we want
      
  
      # identifying cross-reference resolutions
      #cross_ref_res_ids <- sjmisc::str_contains(x = check_it, pattern = res_id, ignore.case = TRUE)
      cross_ref_res_ids <- stringr::str_detect(string = check_it, pattern = res_id)
      
      
      # idea: 
      
      # temp_1 <- check_it[cross_ref_res_ids == TRUE]
      # temp_1_json <-  rjson::toJSON(temp_1)
      
      temp <- check_it[cross_ref_res_ids == FALSE]
      temp_json <-  rjson::toJSON(temp)
      
      res_cross_references[j] <- temp_json
      
    }
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} 


# -----------------------------------------
# some checks
ind <-  which(res_cross_references != "[]")
ind
length(ind)
j <- 1354

check <- resolution_data_fulltext %>%
  filter(Hyperlink == "A/RES/ES-10/16")
# -----------------------------------------

subset <- 
# ---------------------------  
# (4) SAVING REQUIRED RESULTS  ####
# ---------------------------

# adding result to the resolution data frame
resolution_data_fulltext$res_cross_references <- res_cross_references

# # saving extended resolution_data as rds-file
# saveRDS(resolution_data_fulltext, "ResolutionFullText_cross_refs.rds")





