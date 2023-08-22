#================================
# (1) DATA PRE-PROCESSING FOR LDA ####
#================================


# ------------------------------  
# (10) LOADING REQUIRED PACKAGES ####
# ------------------------------

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


# --------------------------  
# (11) LOADING REQUIRED DATA ####
# --------------------------

# getting the required data (resolution data) from a local db (connection issue with )
message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext.rds')

# converting VoteDate to date format
resolution_data_fulltext$VoteDate <- lubridate::as_date(resolution_data_fulltext$VoteDate)

# missing values analysis (column: full_text)
missing_full_text <- which(is.na(resolution_data_fulltext$ResolutionFullText) == TRUE)
missing_full_text
# [1]   15   68 1267 1307 1315 1317 1339 2571 3572 3606 # 10 resolutions with no full text removed

# removing observation with missing text
resolution_data_fulltext <- resolution_data_fulltext[-missing_full_text, ]
dsn_orig <- resolution_data_fulltext

# removing start sentence --> irrelevant for the 
reg_exp <- regex("^United Nations A/RES/\\d{1,}/\\d{1,} Distr.: General General Assembly", ignore_case = TRUE)
resolution_data_fulltext$ResolutionFullText <- gsub(reg_exp,"", resolution_data_fulltext$ResolutionFullText)

# temp <- resolution_data_fulltext$ResolutionFullText
# temp2 <- gsub(reg_exp,"", temp)

# removing start sentence --> irrelevant for the  
reg_exp <- regex("^United Nations A/RES/\\d{1,}/\\d{1,} General Assembly Distr.: General", ignore_case = TRUE)
resolution_data_fulltext$ResolutionFullText <- gsub(reg_exp,"", resolution_data_fulltext$ResolutionFullText)

# removing resolution id string from the text
reg_exp <- regex("A/RES/\\d{1,}/\\d{1,}", ignore_case = TRUE)
resolution_data_fulltext$ResolutionFullText  <- gsub(reg_exp,"", resolution_data_fulltext$ResolutionFullText )

# removing start sentence --> irrelevant for the  
reg_exp <- regex("^United Nations", ignore_case = TRUE)
resolution_data_fulltext$ResolutionFullText <- gsub(reg_exp,"", resolution_data_fulltext$ResolutionFullText)

# removing start sentence --> irrelevant for the  
reg_exp <- regex("General Assembly Distr.: General" , ignore_case = TRUE)
resolution_data_fulltext$ResolutionFullText <- gsub(reg_exp,"", resolution_data_fulltext$ResolutionFullText)

# removing start sentence --> irrelevant for the analysis
reg_exp <- regex("General Assembly", ignore_case = TRUE)
resolution_data_fulltext$ResolutionFullText <- gsub(reg_exp,"", resolution_data_fulltext$ResolutionFullText)



# -------------------------------------  
# (12) CREATING TRAIN AND TEST DATASETS ####
# -------------------------------------

# adding an id-variable to the data frame
vitual_id <- 1:nrow(resolution_data_fulltext)
resolution_data_fulltext$id <- vitual_id

# setting seed to generate a reproducible random sampling
set.seed(05042014)

# creating training data as 80% of the dataset
random_sample <- caret::createDataPartition(resolution_data_fulltext$id, p = 0.8, list = FALSE)

# generating training dataset from the random_sample
random_sample <-  1:nrow(resolution_data_fulltext) # modeling using the entire data set (remove it later)
training_dataset <- resolution_data_fulltext[random_sample, ]
nbr_docs <- nrow(training_dataset)

# generating testing dataset
testing_dataset <- resolution_data_fulltext[-random_sample, ]



# ---------------------------------------------------------------------  
# (13) CREATING A DATA FRAME CONTAINING REQUIRED FULL TEXTS AND DOC_IDS ####
# ---------------------------------------------------------------------

# extracting all required full texts from the training dataset
tic()
for(i in 1:nbr_docs){
  form <-  parse(text = paste("document_",i, " <- training_dataset$ResolutionFullText[",i, "]", sep=""))
  eval(form)
}
toc()


#combining all loaded full_texts
df_list <- mget(ls(pattern = "^document_.*"))
res_text <- as.vector(Reduce(cbind, df_list))

# creating document id vector to relate ids to their respective full texts
doc_id <- paste0(rep("d_", nbr_docs), 1:nbr_docs)

# creating a data frame containing the both character vectors
res_fulltext_df <- data.frame(doc_id=character(nbr_docs), text=character(nbr_docs))

res_fulltext_df$doc_id <- doc_id
res_fulltext_df$text <- res_text

# removing unnecessary documents
tic()
for(i in 1:nbr_docs){
  form <-  parse(text = paste("rm('document_",i, "')", sep=""))
  eval(form)
}
toc()
# 0.19 sec elapsed

# cleaning garbage collector
gc()



# ----------------------------------------------  
# (14) CREATING A CORPUS BASED ON THE FULL TEXTS ####
# ----------------------------------------------

corpus_res <- tm::Corpus(tm::VectorSource(res_fulltext_df$text))
corpus_res

# corpus_res2 <- tm::VCorpus(tm::VectorSource(res_fulltext_df$text))
# corpus_res2

# str(corpus_res, list.len = 2)

# <<SimpleCorpus>>
#   Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 3192


# inspect created corpus
writeLines(as.character(corpus_res[[1]]))
  

# ---------------------  
# (15) REMOVING NUMBERS ####
# ---------------------

corpus_res <- tm_map(corpus_res, removeNumbers)
corpus_res
writeLines(as.character(corpus_res[[50]]))

  
# -------------------------  
# (16) REMOVING PUNCTUATION ####
# -------------------------

corpus_res <- tm_map(corpus_res, removePunctuation, preserve_intra_word_dashes = TRUE, ucp = TRUE)
corpus_res
writeLines(as.character(corpus_res[[50]]))


# -----------------------------  
# (17) TRANSFORMING  LOWER CASE ####
# -----------------------------

corpus_res <- tm_map(corpus_res, content_transformer(tolower))
writeLines(as.character(corpus_res[[50]]))



# -----------------------  
# (18) REMOVING STOPWORDS ####
# -----------------------
myStopWords <- c("session", "agenda", "item", "resolution", "january",
                 "february", "march", "april", "may", "june", "july", 
                 "august", "september", "october", "november", "december")

corpus_res <- tm_map(corpus_res, removeWords, c(stopwords("english"), myStopWords))
corpus_res
writeLines(as.character(corpus_res[[1]]))
writeLines(as.character(corpus_res[[50]]))


# ------------------------------  
# (19) REMOVING EXTRA WHITESPACE ####
# ------------------------------

corpus_res <- tm_map(corpus_res, stripWhitespace)
corpus_res
writeLines(as.character(corpus_res[[1]]))



# --------------------------------------------  
# (20) LEMMATIZING USING HASH_LEMMA DICTIONARY ####
# --------------------------------------------

#convert corpus back to a data frame
res_fulltext_df_clean <- data_frame(object = corpus_res %>% 
 `class<-`("list") %>% 
   use_series(content) ) %>%
    rowwise %>%
     mutate(content = object %>%
       names %>%
         extract(1) )

# adding doc_id columns to the created data frame
res_fulltext_df_clean$doc_id <- res_fulltext_df$doc_id

res_fulltext_df_clean <- res_fulltext_df_clean %>%
  rename(text=object)

# applying lemmatization-algorithm (hash_lemmas dictionary)
tic()
temp <- textstem::lemmatize_strings(res_fulltext_df_clean$text, dictionary = lexicon::hash_lemmas)
toc()
# 7.87 sec elapsed

# replacing the dirty text by the lemmatized one
res_fulltext_df_clean$text <-  NULL
res_fulltext_df_clean$text <-  temp

# temp1 <- res_fulltext_df_clean$text[2]
# temp2 <- gsub("[[:punct:]]", "", temp1)
# removing strings having lr



# --------------------------------------------------------  
# (21) SAVING A PERMANENT VERSION OF THE CLEANED DATA FRAME ###
# ---------------------------------------------------------

# saving a permanent version of the dataset
saveRDS(res_fulltext_df_clean, file = "./data/res_fulltext_df_clean.rds")
temp <- readRDS(file='./data/res_fulltext_df_clean.rds')
