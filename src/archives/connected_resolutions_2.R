# loading required packages
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


# get required resolution data 
message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext.rds')

# getting number of rows of the data set
numrows_data <- nrow(resolution_data_fulltext)
numrows_data <-  5
resolution_data_fulltext$connectedResolutions <- rep(NA, nrow(resolution_data_fulltext))

# initializing result vectors
res_cross_references <- rep(NA, nrow(resolution_data_fulltext))

# # identifying resolutions ids containing [A], ...
# res_id_brackets <- resolution_data_fulltext$Hyperlink %>% grep('\\[.*?\\]', ., value = TRUE) # 83 Hyperlinks have same document
# View(resolutions_same_doc)

j <- 1

for(j in 1:numrows_data){
  
  print(paste0("running iteration number: ", j))
  tryCatch({
    
    # this will need to be removed once need to consider all documents
    if(!is.na(resolution_data_fulltext$ResolutionFullText[j])){
    
      # get resolution full text and resolution id
      res_text <- resolution_data_fulltext$ResolutionFullText[j]
      res_id <- resolution_data_fulltext$Hyperlink[j]
      
      # counting number of characters contained in the res_id-string
      nchar_res_id <- nchar(res_id)

      # get location of the resolution pattern
      res_pattern  <- "A/RES/"
      location_matches <- stringr::str_locate_all(res_text, res_pattern)
      
      # converting the matrix to data frame for my easier subsetting because i want the end
      all_as_df <- as.data.frame(location_matches)
      
      # subsetting only the end positions as they are the ones i want to concatenate to
      only_end <- all_as_df$end
      
      # adding 3 to all the end positions (only_end) in order to get the other substring i want to concatenate
      # only_end_add_3 <- only_end + (nchar_res_id - 6 + 1)
      only_end_add_3 <- only_end + 10 # maximal length of tghe res_id string

      # add only_end_add_3 to df all_as_df 
      all_as_df$end_add_3 <- only_end_add_3
      
      # getting the extended res_id strings
      check_it <- gsub(" ","", str_sub(res_text, all_as_df$start, all_as_df$end_add_3))
      
      # identifying cross-reference resolutions
      cross_ref_res_ids <- sjmisc::str_contains(x = check_it, pattern = res_id, ignore.case = TRUE)
      cross_ref_res_ids <- stringr::str_detect(string = check_it, pattern = res_id)
      
      temp <- check_it[cross_ref_res_ids == FALSE]
      temp_json <-  rjson::toJSON(temp)
      
      res_cross_references[j] <- temp_json
      
    }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} 




# add only_end_add_3 to df all_as_df 
all_as_df$end_add_3 <- only_end_add_3

# now to get the new end positions from only_end_add_3 in the resolution text (get_text) together with the start and end positions
see_if_works <- rep(NA, 5)

for(t in 1:5){
  print(paste0("running iteration number: ", t))
  #print(get_text[t])
  print(all_as_df$start[t] )
  
  print(all_as_df$end_add_3[t])
  see_if_works[t] <- str_sub(get_text[t], all_as_df$start[t], all_as_df$end_add_3[t])
  print(see_if_works[t])
  
  
}

unlist(gregexpr(pattern ='2',"the2quickbrownfoxeswere2tired"))
titles_r <- get_title
get_text


mtitle <- grepl(get_title[j], get_text, fixed = TRUE ) 