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


# ----------------------------------------------------------  
# (64) CREATING A FUNCTION TO CLASSIFY RECURRENT RESOLUTIONS ####
# ----------------------------------------------------------

recurrent_resolutions <- function(i){
  
  # selecting a single resolution
  res_id <<- resolution_data_fulltext$Hyperlink[i]         # "A/RES/55/49" # "A/RES/62/180"
  res_title <<- resolution_data_fulltext$TitleofResolution[i]
  
  # excluding current resolution from the pool of resolutions to be compared with
  temp_df <<-  resolution_data_fulltext %>%
    filter(Hyperlink != res_id)

  # defining vectors to contain results of similarity scores calculation
  nbr_rows2 <<- nrow(temp_df)
  indices_scores <<- 1:nbr_rows2
  similarity_scores <<- rep(0, nbr_rows2)

  # # calculating required similarity scores  --> 100 sec in average per (parallel version)
  # plan(multisession)
  # tic()
  # similarity_scores <-  furrr::future_map_dbl(indices_scores, calculate_similarity_score, .progress = TRUE)
  # toc()

  # # calculating required similarity scores  --> 51.93 sec per (sequential version)
  # tic()
  # for(t in 1:nbr_rows2){
  #   similarity_scores[t] <- calculate_similarity_score(t)
  # }
  # toc()
  
  # calculating required similarity scores  --> 51.93 sec per (sequential version)
  tic()
  similarity_scores  <- vapply(indices_scores, calculate_similarity_score, numeric(1))
  toc()
  
  # decide if the resolution is recurrent or not
  ind <- which(similarity_scores > 0.6)  # we will work with 0.6 as threshold
  temp_class <- ifelse(length(ind) > 0, 1,0)
  temp_children <- temp_df$Hyperlink[ind] 
  
  # storing results into a json-element
  recurrent_class <- temp_class
  children_res <- toJSON(temp_children)

  # return required information
  return(list(recurrent_class, children_res))

}


# ----------------------------------------------  
# (65) CLASSIFYING RECURRENT RESOLUTIONS (furrr) ####
# ----------------------------------------------

# initializing required parameters
nbr_rows <- nrow(resolution_data_fulltext)
nbr_rows <- 7000 # it should be something like 30 mins
indices_res <- 6001:nbr_rows
#recurrent_list <- vector(mode = "list", nbr_rows)


# getting the required classes
future::plan(multisession)
ptm <- proc.time()
recurrent_list <- furrr::future_map(indices_res, recurrent_resolutions, .progress=TRUE)
print(proc.time()-ptm)

# extracting calculated information
recurrent_res_y_n <- as.vector(unlist(purrr::map(recurrent_list, 1)))
res_id_children <- as.vector(unlist(purrr::map(recurrent_list, 2)))

# creating a data frame to contain all calculated results
temp_df <- data.frame(recurrent_res_y_n, res_id_children)
temp_df$cnt_var <- indices_res


temp_df_6001_7000 <- temp_df

# temp_df <- temp_df[0,]

# Adding  the calculated rows to the existing recurrent_res_data frame
temp_df_initial <- readRDS(file="./recurrent_res/reccurent_res_df.rds")
reccurent_res_df <- rbind(temp_df_initial, temp_df)

# saving created objects as RData
saveRDS(reccurent_res_df, file="./recurrent_res/reccurent_res_df.rds")

# saving created objects as RData
saveRDS(temp_df_6001_7000, file="./recurrent_res/temp_df_6001_7000.rds")


temp_df_6001_7000b        

# -------------------------------------------------------------------------------------------
# temp <- readRDS(file="./recurrent_res/reccuren_res_df.rds")
# 
# res_id_children_1_1000 <- res_id_children
# recurrent_res_n_1_1000 <- recurrent_res_n
# 
# 
# # saving created objects as RData
# saveRDS(res_id_children_1_1000, file="./recurrent_res/res_id_children_1_1000.rds")
# saveRDS(recurrent_res_n_1_1000, file="./recurrent_res/recurrent_res_n_1_1000.rds")
# 
# 
# # load saved information
# recurrent_res_y_n <- temp1 <- readRDS(file="./recurrent_res/recurrent_res_n_1_1000.rds")
# res_id_children <- temp2 <- readRDS(file="./recurrent_res/res_id_children_1_1000.rds")
# -------------------------------------------------------------------------------------------

# # check
# i <- 1258
# i <- 3502
# i <- 6425
# 
# tic()
# test <- recurrent_resolutions(i)
# toc()
# 
# 
# # load saved information
# recurrent_res_y_n <- readRDS(file="./recurrent_res/recurrent_res_n_1_1000.rds")
# res_id_children <- readRDS(file="./recurrent_res/res_id_children_1_1000.rds")
# reccurent_res_df <- data.frame(recurrent_res_y_n, res_id_children)
# reccurent_res_df$cnt_var <- indices_res
# 
# 
# # add id_var to the resolution_data_fulltext data frame
# cnt_var <- 1:nrow(resolution_data_fulltext)
# resolution_data_fulltext$cnt_var <- cnt_var
# 
# saveRDS(resolution_data_fulltext, file="./data/resolution_data_fulltext.rds")


