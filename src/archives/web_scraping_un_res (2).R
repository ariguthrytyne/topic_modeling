# ============================================================================================ #
#                                                                                              #
#  The script described below contain various activities related to the UNVOPA-Project         #
#  Following activities will be performed:                                                     #
#                                                                                              #
#      1- loading resolution_data from the SAP-database                                        #
#                                                                                              #
#      2- scraping the full text of each single resolution and                                #
#                                                                                              #
#      3- storing the resolution full text into a new variable                                 #
#                                                                                              #
#      4- Indexing the resolution_data into elastic search engine                              #
#                                                                                              #
#      5- ...                                                                                  #
#                                                                                              #
#                                                                                              #
#     Input Parameters:                                                                        #
#     ----------------                                                                         #
#                                                                                              #
#        (1) no specific input required expect db/es connection parameters                     #
#                                                                                              #
#                                                                                              #
#     Output Parameters:                                                                       #
#     -----------------                                                                        #
#      ...                                                                                     #
#                                                                                              #
# ============================================================================================ #



#======================
# (0) PREPARATION STEPS ####
#======================  


# loading required libraries
library(rJava)
library(DBI)
library(RJDBC)
library(pool)
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


#=======================================
# (1) LOADING/INSPECTING RESOLUTION_DATA ####
#=======================================

# ****************************************************************
# The resolution_data were previously loaded from our SAP_database
# ****************************************************************

# getting the required data (resolution data) from a local db (connection issue with )
message("--- loading all available resolution data : ---")
resolution_data <- readRDS(file='./data/resolution_data.rds')
View(resolution_data)

# removing rows having missing url
ind <- which(is.na(resolution_data$URL) == TRUE)
length(ind) # 15

# displaying resolutions having missing url
resolution_data_missing_url  <- resolution_data[ind, ]
View(resolution_data_missing_url)

# getting only resolutions with given urls
resolution_data  <- resolution_data[-ind, ]


#===================================================================
# (2) WEB_SCRAPING RESOLUTION FULLTEXT FROM THE GIVEN RESOLUTION_URL ####
#===================================================================

# initializing helpers variables (results, ...)
n_rows <- nrow(resolution_data)
res_fulltext <- rep(NA, n_rows)  # n_rows <- 10
res_nbr_pages <- rep(NA, n_rows)
unique_en_pdf <- rep(TRUE, n_rows)

# SCRAPING the urls and loading the full text into R 
ptm <- proc.time()
for(pos in 1:n_rows){
  
  print(paste0("running iteration number: ", pos))
  
  tryCatch({
    
    # get the url of eh considered res
    url_global <- resolution_data$URL[pos]
    
    # reading the html of the given resolution into R
    html_r <- read_html(url_global) %>%
      html_nodes('a') %>%
      html_attr('href') %>%
      grep('EN.pdf', ., value=TRUE)
    
    
    if(length(html_r) == 1){
      
      # constructing the full pdf-url
      base_url <- "https://digitallibrary.un.org"
      url_pdf <- paste0(base_url, html_r)
      
      # reading the pdf-content into R
      en_pdf_data <- pdf_text(url_pdf)
      
      # storing the number of pages of the pdf-doc
      res_nbr_pages[pos] <- length(en_pdf_data) 
      
      # cleaning the loaded text data by removing multiple spaces, tabs
      temp <- stringr::str_squish(en_pdf_data)
      
      # merging the text from all available pages
      temp <- paste(temp, collapse = ' ')
      
      res_fulltext[pos] <- temp 
      
    }else{
      unique_en_pdf[pos] <- FALSE
    }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}
print(proc.time()-ptm)

# Adding the extracted texts to the resolution data frame
resolution_data_fulltext <-  resolution_data
resolution_data_fulltext$ResolutionFullText <-  res_fulltext
resolution_data_fulltext$ReolutionNumberofPages <- res_nbr_pages
resolution_data_fulltext$ResolutionUniqueEnglishVersion <-  unique_en_pdf
View(resolution_data_fulltext)

# saving a permanent version of the dataset
#saveRDS(resolution_data_fulltext, file = "./data/resolution_data_fulltext.rds")


#==================================================
# (3) 1st QUALITY CHECK OF THE RESOLUTION_FULL_TEXT  ####
#==================================================

# loading resolution_data containing the full text
resolution_data_fulltext <- readRDS(file = "./data/resolution_data_fulltext.rds")

# checking data quality (not filled text content)
ind <-which(is.na(resolution_data_fulltext$ResolutionFullText) == TRUE)
length(ind) # 10 

# having a look at the resolutions with missing text
resolution_data_fulltext_missing_text <- resolution_data_fulltext[ind, ]
View(resolution_data_fulltext_missing_text)

# 3: https://digitallibrary.un.org/record/3838695?ln=en --> "The record has been deleted."
# 4: https://digitallibrary.un.org/record/3846445?ln=en --> "The record has been deleted."
# 5: https://digitallibrary.un.org/record/3848290?ln=en --> "The record has been deleted."
# 6: https://digitallibrary.un.org/record/3848629?ln=en --> "The record has been deleted."
# 7: https://digitallibrary.un.org/record/3847706?ln=en --> "The record has been deleted."
# 8: https://digitallibrary.un.org/record/697747?ln=en  --> "Der Dateityp Gzip-Archiv (application/gzip) wird nicht unterstützt
# 10: https://digitallibrary.un.org/record/3848286?ln=en --> nur DE-Version available

# 1, 2, 9: pdfs available --> werden aus bis jetzt ungeklärten Gründen nicht gefunden  (have to check why ???)

# removing the resolutions having missing full text
resolution_data_fulltext_complete <- resolution_data_fulltext[-ind, ] # 99.4% of available data


#=====================================================
# (4) 1st EXPLORATION OF THE RESOLUTION_DATA_FULL_TEXT ####
#=====================================================

# -----------------------------------  
# (41) AVAILABLE RESOLUTIONS PER YEAR ####
# -----------------------------------

# check  available resolutions per year
avaialble_res_per_year <- as.data.frame(table(resolution_data_fulltext_complete$YEAR, useNA = "always"))
View(avaialble_res_per_year)


# displaying of number of resolutions per year
df <- resolution_data_fulltext_complete
ggplot(data=df, aes(YEAR, fill=YEAR)) + 
  geom_histogram(bins = 23,
                 col="black", 
                 fill="blue", 
                 binwidth = 1,
                 alpha=0.2) + 
  labs(title="Number of Resolutions per year", x="Year", y="Nbr. of Resolutions") + 
  ylim(c(0,250)) +
  theme_minimal() +
  theme(plot.title=element_text(size=14,
                                #face="bold",
                                family="Comic San MS",
                                color="black",
                                hjust=0.5,
                                lineheight=1.2),  # title
        panel.background = element_rect(fill = 'white'),  # Backgroung color white gray97, gray95
        plot.subtitle=element_text(size=9,
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),  # subtitle
        plot.caption=element_text(size=12),  # caption
        axis.title.x=element_text(vjust= 2,
                                  #hjust = -15,
                                  size=12),  # X axis title
        axis.title.y=element_text(size=12,
                                  vjust= 3),  # Y axis title
        axis.text.x=element_text(size=8,
                                 hjust=1,
                                 vjust = 1,
                                 angle = 45),  # X axis text position (h = horizontal)
        axis.text.y=element_text(size=9,
                                 hjust = 1,
                                 vjust = 1)
  ) # end theme      


# ------------------------------------- 
# (42) RESOLUTION_ID IN RESOLUTION_TEXT  ####
# -------------------------------------

# Is the resolution_id unique ?
(length(unique(resolution_data_fulltext_complete$Hyperlink)) == length(resolution_data_fulltext_complete$Hyperlink)  )
# TRUE

# Is the Resolution_Id in the Resolution_full_text ??
n_rows <- nrow(df)
total_nbr_position <- rep(NA, n_rows)
first_position <- rep(NA, n_rows)

ptm  <- proc.time()
for(i in 1:n_rows){
  
  print(paste0("running iteration number: ", i))
  
  # get the res_id and res_text
  res_id <- df$Hyperlink[i]
  res_text <- df$ResolutionFullText[i]
  res_url <- df$URL[i]
  
  temp <- gregexpr(pattern = res_id, res_text, fixed=TRUE)
  first_position[i] <- temp[[1]][1] # 16
  total_nbr_position[i] <-  ifelse(first_position[i] > 0, length(as.character(temp[[1]])), 0)
  
}
print(proc.time()-ptm)

# looking at the distributions (first occurrence / total number of occurrences)
first_pos_df <- as.data.frame(table(first_position)) # tell us something about the document structure
View(first_pos_df)

nbr_of_pos_df <- as.data.frame(table(total_nbr_position))
View(nbr_of_pos_df)

# inspecting resolutions for which the res_id was not found in the res_full_text
ind <- which(first_position == -1)
length(ind)

# sample checks 
pos <- 1442
res_id <- resolution_data_fulltext_complete$Hyperlink[pos]
res_id

res_url <- resolution_data_fulltext_complete$URL[pos]
res_url


# ---------------------------------  
# (43) RESOLUTIONS: NUMBER OF PAGES  ####
# ---------------------------------

# distribution number of pages per document
summary(resolution_data_fulltext_complete$ReolutionNumberofPages)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   3.000   4.608   5.000  58.000 

# average number of resolution per document per year (displaying)
df <- resolution_data_fulltext_complete

ggplot(df, aes(YEAR, ReolutionNumberofPages)) + 
  geom_boxplot(aes(group = cut_width(YEAR, 0.25)), fill="white", colour="#3366FF", outlier.colour = "red", outlier.shape = 1) +
  labs(title="Nbr of resolution pages by year",x="Year of publication", y = "number of pages")  


#=====================================================
# (5) COMPARING STRUCTURES OF THE RESOLUTION-DOCUMENTS ####
#=====================================================

