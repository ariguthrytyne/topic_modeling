library(magrittr)
library(dplyr)
library(pdftools)
library(rvest)
library(stringr)
library(tm)
library(utils)

message("--- loading all available resolution full text data : ---")
resolution_data_fulltext_df <- readRDS(file='./data/resolution_data_fulltext.rds')
head(resolution_data_fulltext_df)


# get original resolution-data
resolution_data_2 <- readRDS(file='./data/resolution_data.rds')
head(resolution_data_2)
class(resolution_data_2)

ind <- which(is.na(resolution_data_2$URL) == TRUE)
length(ind) # 15

resolution_data_missing_url_2  <- resolution_data_2[ind, ]
View(resolution_data_missing_url_2)

# getting only resolutions with given urls
resolution_data_2  <- resolution_data_2[-ind, ]
View(resolution_data_2)

# resolutions with same document but just different sections
resolutions_same_doc <- resolution_data_2$Hyperlink %>% grep('\\[.*?\\]', ., value = TRUE) # 83 Hyperlinks have same document
View(resolutions_same_doc)


# getting the row numbers of resolution_texts with different sections
res_doc_same <- which(resolution_data_2$Hyperlink %in% resolutions_same_doc) # row numbers for documemts with same document
resolution_data_same_doc <- resolution_data_2[res_doc_same, ]

# sorting res_id according suffixes ([A], [B], ...)
resolution_data_same_doc<- resolution_data_same_doc %>%
  arrange(Hyperlink)

View(resolution_data_same_doc)

