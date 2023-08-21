# The following script explores how Python can be used together with R and 
# RStudio. 
# RTStudio uses the R-package "reticulate" to in
#
#
# Change History
# Date			  Developer				Action
# 14.01.2022  Thierry Monthe  Initial Creation

#' Main function of the project TMKx. Following activities will be performed:
#'
#'  1- Data Loading & Data Exploration
#' 
#'  2- Text Preprocessing: 
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
#'      a-Tests
#'      b- Dockerization
#'      c- Model Maintenance
#'      e- AWS-container & Instance
#'      
#' The project structure is designed to store temporary results into a 
#' dedicated directory and 
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


