# The following file contains the script used within the project TMKx 
# for the development of a Topic Model for Text Classification and Category 
# identification (NLP)
#
#
# Change History
# Date			  Developer				Action
# 22.08.2022  RT/TM         Initial Creation

#' Main function of the project TMKx. Following activities will be performed:
#'
#'  1- Data Loading & Data Exploration
#' 
#'  2- Text Pre-processing: 
#'        a- Corpus, 
#'        b- Document Term Matrix
#'        c- Stop words removal
#'        d- Stemming
#'        e- Lemmatization
#'        f- Normalization
#'        g- feature hashing
#'       
#'  3- Modeling 
#'        a- Latent Dirichlet Allocation (Unsupervised Learning)
#'        b- n-grams
#'        c- Regression (Supervised Learning)
#'        
#'  
#'  4- Visualization
#'      a- LDAVis
#'      b- Word Cloud
#'      c- Tag Cloud
#'      d- Slope Chart
#'      e- Sankey Chart
#'      
#'  
#'  5- deployment in production environment 
#'      a- Tests
#'      b- Dockerization
#'      c- Model Maintenance
#'      e- AWS-container & Instance
#'      
#' The project structure is designed to store temporary results into 
#' dedicated directories.
#'
#' @param path_exchange \code{String} path of the exchange directory where all
#' temporary results will be stored and in the R-environment
#' Environment tmkx.
#' @param path_input \code{String} path of the input-directory where the final
#' results will be stored.
#' @param loglevel \code{String} indicates which notifications will be logged
#' and stored into the Log-file. Following characteristics are possible:
#' 'DEBUG', 'INFO', 'WARNING', 'ERROR'
#' @param state_error \code{String} that the status level indicates in case 
#' of an error
#' @param logfile_path \code{String} with the path under which the log file 
#' will stored.
#' @param logfile_prefix \code{String} with the Prefix of of the Log-file
#' 
#' @return Execution Status
#' @family tmkx_segmentierung
#' @export
 

# 1. preparing environment ----

# 1.1 clearing current R-environment
rm(list = ls(all.names = TRUE))

# 1.2 constructing useful paths (data, codes, ..)
root <- getwd()
path.data <- file.path(root, "data", "raw")

# 1.3 loading required packages
library(magrittr)
library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(future)
library(future.apply)
library(tm)
library(NLP)
library(stringr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(dplyr) # fun1(x1, x2, x3)
library(textstem)
library(tictoc)
library(SnowballC) # fun1(x1, x2, x3, x4)
library(wordcloud)
library(textstem)
library(caret)
library(stringi)

# 2. loading resolution data  ----

# 2.1 creating path to the required data 
path.res <- file.path(path.data, "resolution_data_fulltext.rds")

message("--- loading all available resolution full text data : ---")
res_data <- readRDS(file = path.res)

# fist high level exploration
str(res_data)

# 3. exploring resolution data  ----

# authoring countries
auth_countries <- as.data.frame(table(res_data$AuthoringCountries))
View(auth_countries)

# counting number of authoring countries per resolutions 
count_authoring_cntry <- length(strsplit(res_data$AuthoringCountries[5], 
                                         ",")[[1]])
count_authoring_cntry

auth_vect <-res_data$AuthoringCountries  

count_fun <- function(x){
  y <- length(strsplit(x, ",")[[1]])
  return(y)
}
total_count <- unlist(lapply(auth_vect, count_fun))

res_data$nbr_authoringcountries <- total_count

# visualizing th distribution of number of authoring countries per resolution
hist(res_data$nbr_authoringcountries, breaks = 20, ylim = c(0, 1000))

freq_nbr_auth <- as.data.frame(prop.table(table(res_data$nbr_authoringcountries, 
                                                useNA = "always")))
View(freq_nbr_auth)

# visualizing the number of pages of resolutions
hist(res_data$ReolutionNumberofPages, breaks = 80) # 

freq_nbr_pages <- as.data.frame(prop.table(table(res_data$ReolutionNumberofPages, 
                                                useNA = "always")))
View(freq_nbr_pages)

# exploration res_y_n 8recurrent resolutions)
table(res_data$recurrent_res_y_n, useNA = "always")

# recurrent resolutions can be used to validate the topics generated 
# by our LDA-model. Assumption: connected resolutions should have 
# similar topics. It means it is also necessary to identify connected 
# resolutions something like Res_12 --> (Res18, res96, Res458)
# 23.08.2023 (RT/TM)


# exploring voting_date and year
table(res_data$YEAR)


# 1995 1998 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 
# 2    15   198  226  224  188  206  160  189  179  193  184  199  193  195  192  195  203  201  210  223 
# 2019 2020 
# 218   14 

# findings
#
# we will keep data from 2000 to 2019
# number of data for the years 1995, 1998 and 2020 not "sufficient"
# only few records compared to the other years --> to be removed


# 4. pre-processing res data  ----

