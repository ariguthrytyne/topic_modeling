#==========================
# (6) RECCURENT RESOLUTIONS ####
#==========================

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
#  2- create a network-analysis graph (one resolution connected to her children --> )
#
#  3-  
#
#  4- 
#
#
# --- steps --
#
#  1- load data 
#
#  2- order resolutions by date   
#
#  3- get number of resolutions per date(/month/year)
#
#  4- select a single resolution and find all related resolutions
#
#  5- draw the network


# --------------------------  
# (61) LOADING REQUIRED DATA ####
# --------------------------

# getting the required data (resolution data) from a local db (connection issue with )
message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext.rds')

# converting VoteDate to date format
resolution_data_fulltext$VoteDate <- lubridate::as_date(resolution_data_fulltext$VoteDate)

# ordering resolution data frame by VoteDate
resolution_data_fulltext <- resolution_data_fulltext %>%
  arrange(VoteDate)

# initializing result-vector
similar_resolutions <- rep(NA, p)

# looping among all resolution
nbr_rows <- nrow(resolution_data_fulltext)
i <- 25 # test case

for(i in 1:nbr_rows){
  
  # selecting a single resolution
  res_id <- resolution_data_fulltext$Hyperlink[i]         # "A/RES/55/49" # "A/RES/62/180"

  selected_res <- resolution_data_fulltext$Hyperlink[ind]
  title_res <- resolution_data_fulltext$TitleofResolution[ind]

  title_similarities <- rep(0, nbr_rows)
  
  # calculating similarities with all other resolutions
  ptm <- proc.time()
  for(j in 1:nbr_rows){
    
    # getting the title of the other resolution
    title_other_res <- res_data_subset$TitleofResolution[j]
    
    # evaluating similarities between the titles
    title_similarities[j] <- RecordLinkage::levenshteinSim(title_res, title_other_res)
    #title_similarities[j] <- RecordLinkage::jarowinkler(title_res, title_other_res)
    
  }
  print(proc.time()-ptm)
  
  
  
  
  
  
  
  
  
}


# boxplot.stats(title_similarities)$stats
# lower_bound <- quantile(title_similarities, 0.01)
# lower_bound
# upper_bound <- quantile(title_similarities, 0.992)
# upper_bound
# 
# # outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)
# # dat[outlier_ind, ]
# 
# title_similarities.sort <- sort(title_similarities, decreasing = TRUE)
# upper_bound <- 0.75
# 
# # getting all similar resolutions
# ind_similar <- which(title_similarities > upper_bound)
# #titles_similar <- res_data_subset$TitleofResolution[ind_similar]
# res_similar <- res_data_subset$Hyperlink[ind_similar]
# res_similar <- res_similar[!(res_similar %in% selected_res)]
# 
# title_similarities.sort
# temp_dataset <- res_data_subset[ind_similar, ]
# View(temp_dataset)
