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


# get required resolution data 
message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext.rds')


resolution_data_6 <- resolution_data_fulltext_df 
num_iterations = 5

numrows_data <- nrow(resolution_data_6)
resolution_data_6$connectedResolutions <- rep(NA, nrow(resolution_data_6))
# res_fulltext_4 <- rep(NA, numrows_data)  # n_rows <- 10
# res_nbr_pages_4 <- rep(NA, numrows_data)
# unique_en_pdf <- rep(TRUE, numrows_data)

# k <- 1
# 
# 
# for(k in 1:num_iterations){
#   
#   print(paste0("running iteration number: ", k))
#   
#   tryCatch({
#     
#     # get the url of eh considered res
#     url_global_4 <- resolution_data_6$URL[k]
#     
#     # reading the html of the given resolution into R
#     html_r_4 <- read_html(url_global_4) %>%
#       html_nodes('a') %>%
#       html_attr('href') %>%
#       grep('EN.pdf', ., value=TRUE)
#     
#     
#     if(length(html_r_4) == 1){
#       
#       # constructing the full pdf-url
#       base_url_4 <- "https://digitallibrary.un.org"
#       url_pdf_4 <- paste0(base_url_4, html_r_4)
#       
#       # reading the pdf-content into R
#       en_pdf_data_4 <- pdf_text(url_pdf_4)
#       
#       # storing the number of pages of the pdf-doc
#       res_nbr_pages_4[k] <- length(en_pdf_data_4) 
#       
#       # cleaning the loaded text data by removing multiple spaces, tabs
#       temp_4 <- stringr::str_squish(en_pdf_data_4) 
#       
#       # merging the text from all available pages
#       temp_4 <- paste(temp_4, collapse = ' ')
#       
#       res_fulltext_4[k] <- temp_4
#       
#       
#     }else{
#       unique_en_pdf_4[k] <- FALSE
#     }
#     
#   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#   
# }

resolution_data_fulltext <-  resolution_data_6
#resolution_data_fulltext$resolution_data_fulltext <-  res_fulltext_4

empty_text <- is.na(resolution_data_fulltext$ResolutionFullText) == FALSE

is_title_in_doc <- rep(NA, nrow(resolution_data_fulltext))
location_of_title <- rep(NA, nrow(resolution_data_fulltext))
is_ID_in_doc <- rep(NA, nrow(resolution_data_fulltext))
is_blah_in_doc <- rep(NA, nrow(resolution_data_fulltext))
location_matches <- rep(NA, nrow(resolution_data_fulltext))

for(j in 1:num_iterations){
  print(paste0("running iteration number: ", j))
  tryCatch({
    
    # this will need to be removed once need to consider all documents
    if(!is.na(resolution_data_fulltext$ResolutionFullText[j])){
    
      get_title <- resolution_data_fulltext$TitleofResolution[j]
      print(get_title)
      
      get_text <- resolution_data_fulltext$ResolutionFullText[j]
      #print(get_text)
      
      print(resolution_data_fulltext$Hyperlink[j])
      
      # checking if the title is in the document
      #is_title_in_doc[j] <- sjmisc::str_contains(x = get_text, pattern = get_title[j], 
                                                 #ignore.case = TRUE)
      #print(is_title_in_doc[j])
      # getting the title locations in the document
      #location_of_title[j] <- unlist(gregexpr(pattern = resolution_data_fulltext$TitleofResolution[j], 
                                         #resolution_data_fulltext$ResolutionFullText[j]))
      #print(location_of_title[j])
      
      # checking if ID is in document
      #is_ID_in_doc[j] <- sjmisc::str_contains(x = get_text[j], pattern = "A/RES/", ignore.case = TRUE)
      #print(is_ID_in_doc[j]) 
      
      
      #is_blah_in_doc[j] <- sjmisc::str_contains(x = get_text[j], pattern ="A/RES/53", ignore.case = TRUE)
      #print(is_blah_in_doc[j])
      
      location_matches <- stringr::str_locate_all(get_text, "A/RES/")
      print(location_matches)
      
      # all <- do.call(rbind, location_matches)
      # print(all)
      
      # converting the matrix to data frame for my easier subsetting because i want the end
      all_as_df <- as.data.frame(all)
      print(all_as_df)
      
      # subsetting only the end positions as they are the ones i want to concatenate to
      only_end <- all_as_df$end
      print(only_end)
      
      # adding 3 to all the end positions (only_end) in order to get the other substring i want to concatenate
      only_end_add_3 <- only_end + 6
      print(only_end_add_3)
      
      # add only_end_add_3 to df all_as_df 
      all_as_df$end_add_3 <- only_end_add_3
      print(all_as_df$end_add_3)
    }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} 
#getting all location matches with start and end positions as matrix 
all <- do.call(rbind, location_matches)

# converting the matrix to data frame for my easier subsetting
all_as_df <- as.data.frame(all)

# subsetting only the end positions as they are the ones i want to concatenate to
only_end <- all_as_df[, 2]

# adding 3 to all the end positions (only_end) in order to get the other substring i want to concatenate
only_end_add_3 <- only_end + 6

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