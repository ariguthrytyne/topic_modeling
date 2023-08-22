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
library(dplyr)
library(textstem)
library(tictoc)
library(SnowballC)
library(wordcloud)
library(textstem)
library(caret)



# 2. loading resolution data  ----

# 2.1 creating path to the required data 
path.res <- file.path(path.data, "resolution_data_fulltext.rds")

message("--- loading all available resolution full text data : ---")
res_data <- readRDS(file = path.res)

# fist high level exploration
str(res_data)

# 3. exploring resolution data  ----
