#==========================
# (6) RECCURENT RESOLUTIONS ####
#==========================


# ------------------------------------------------------------------------------------
# similar titles ? or more precisely: similar resolution_full_texts?
# recurrent resolutions are those that are regularly discusses during
# the plenum un-sessions:
#
# In this first phase we will explore the similarity between the resolutions based
#
# Objectives of our analysis:
#
#  1- create column containing for each resolution the list of related resolutions
#
#  2- create lda/lsa model
#
#  2- create a network-analysis graph (one resolution connected to her children --> )
#
#  3-  
#
#  4- 
#
#
# --- steps --
#
#  1- load resolution data 
#
#  2- order resolutions by date   
#
#  3- get number of resolutions per date(/month/year) --> DONE !!
#
#  4- select a single resolution and find all related resolutions
#
#  5- 
#
#  5- draw the network
# -----------------------------------------------------------------------------


# ------------------------------  
# (61) LOADING REQUIRED PACKAGES ####
# ------------------------------

library(magrittr)
library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(doParallel)
library(foreach)
library(future)
library(future.apply)
library(tictoc)
library(rjson)


# --------------------------  
# (62) LOADING REQUIRED DATA ####
# --------------------------

# getting the required data (resolution data) from a local db (connection issue with )
message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext.rds')

# converting VoteDate to date format
resolution_data_fulltext$VoteDate <- lubridate::as_date(resolution_data_fulltext$VoteDate)

# # ordering resolution data frame by VoteDate
# resolution_data_fulltext <- resolution_data_fulltext %>%
#   arrange(VoteDate)

# geeting subset of teh data set
#resolution_data_fulltext_subset  <- resolution_data_fulltext[2001:2500, ]
resolution_data_fulltext_subset  <- resolution_data_fulltext[2001:2005, ]

# # getting some recurrent resolutions
# ind <- which(resolution_data_fulltext$recurrent_res_y_n == 1)
# length(ind)
# i <-  ind[1005]


# -------------------------------------------------------  
# (63) CREATING A FUNCTION TO CALCULATE SIMILARITY SCORES ####
# -------------------------------------------------------

# creating a function to calculate similarity scores a single resolution
calculate_similarity_score <- function(j){
  
  # getting the title of the other resolution
  res_other_title <- temp_df$TitleofResolution[j]
  res_other_id <- temp_df$Hyperlink[j]
  
  # generating appropriate tokens
  dfmat <- quanteda::corpus(c(res_title, res_other_title)) %>%
    tokens(remove_punct=TRUE) %>%
    tokens_wordstem(language="en") %>%
    tokens_remove(stopwords("en")) %>%
    tokens_tolower()  %>%   
    dfm()
  
  # evaluating the similarity between the two titles
  sim <- quanteda.textstats::textstat_simil(dfmat['text1',] , dfmat['text2',], method = "cosine", margin = "documents")
  
  # returning required values
  return(as.numeric(sim))
}


# ----------------------------------------------  
# (64) CLASSIFYING RECURRENT RESOLUTIONS (furrr) ####
# ----------------------------------------------

# initializing required parameters
nbr_rows <- nrow(resolution_data_fulltext_subset)
#nbr_rows <- 10

recurrent_res_n <- rep(NA, nbr_rows)
res_id_children <-  rep(NA, nbr_rows)

# # recurrent_res <- rep(NA, nbr_rows)
# recurrent_list <- vector(mode = "list", length = nbr_rows)
# recurrent_


# recurrent_resolutions_children <- function(i){
ptm <- proc.time()
tic()
for (i in 1: nbr_rows){
  
  # print current status
  print(paste0("running iteration number: ", i))
  
  # selecting a single resolution
  res_id <<- resolution_data_fulltext_subset$Hyperlink[i]         # "A/RES/55/49" # "A/RES/62/180"
  res_title <<- resolution_data_fulltext_subset$TitleofResolution[i]
  
  # print("Inside recurrent_resolutions")
  # print(environment())
  # print(ls())
  
  # excluding current resolution from the pool of resolutions to be compared with
  temp_df <<-  resolution_data_fulltext %>%
    filter(Hyperlink != res_id)
  
  # defining vectors to contain results of similarity scores calculation
  nbr_rows2 <<- nrow(temp_df)
  #nbr_rows2 <- 1000
  indices_scores <<- 1:nbr_rows2
  similarity_scores <<- rep(0, nbr_rows2)
  
  
  # calculating required similarity scores  --> 51.93 sec per 
  plan(multisession)
  tic()
  similarity_scores <-  furrr::future_map_dbl(indices_scores, calculate_similarity_score, .progress = TRUE)
  toc()
  
  # decide if the resolution is recurrent or not
  ind <- which(similarity_scores > 0.6)  # we will work with 0.6 as threshold
  temp_class <- ifelse(length(ind) > 0, 1,0)
  temp_children <- temp_df$Hyperlink[ind] 
  
  # storing results into a json-element
  recurrent_res_n[i] <- temp_class
  res_id_children[i] <- toJSON(temp_children)
  
}
toc()
print(proc.time()-ptm)


# adding the created vector to the data frame
resolution_data_fulltext_subset$recurrent_res_n <- recurrent_res_n
resolution_data_fulltext_subset$res_id_children <- res_id_children


# saving a permanent version of the dataset
saveRDS(resolution_data_fulltext_subset, file = "./data/resolution_data_fulltext_recurrent_child_2001_2500.rds")
temp <- readRDS(file='./data/resolution_data_fulltext_recurrent_child_2001_2500.rds')





