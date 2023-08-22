#==============================
# (6) DATA PREPARATION FOR LDA ####
#=============================



# 1- each single resolution has a full_text and a title --> document
#
# 2- each document will be represented as mixture of topics --> to be determined
#
# 3- each 

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
#  1- load data 
#
#  2- order resolutions by date   
#
#  3- get number of resolutions per date(/month/year)
#
#  4- select a single resolution and find all related resolutions
#
#  5- draw the network
# -----------------------------------------------------------------------------


# ------------------------------  
# (71) LOADING REQUIRED PACKAGES ####
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


# --------------------------  
# (72) LOADING REQUIRED DATA ####
# --------------------------

# getting the required data (resolution data) from a local db (connection issue with )
message("--- loading all available resolution full text data : ---")
resolution_data_fulltext <- readRDS(file='./data/resolution_data_fulltext.rds')

# converting VoteDate to date format
resolution_data_fulltext$VoteDate <- lubridate::as_date(resolution_data_fulltext$VoteDate)



# --------------------------------------------  
# (73) EXTRACTING REQUIRED TEXT TO BE ANALYZED ####
# --------------------------------------------


### --- we will work with 80% of the data as training data set --- ###

# random selection of 2/3resolution texts from the basis-data frame
set.seed(05042014)
temp <- resolution_data_fulltext  %>%
  slice_sample(n=3200)  # 80% of the data for the training --> the  rest will be used as test data set


# getting total number of documents
nbr_docs <- nrow(temp)

# extracting all required full texts
tic()
for(i in 1:nbr_docs){
  form <-  parse(text = paste("document_",i, " <- resolution_data_fulltext$ResolutionFullText[",i, "]", sep=""))
  eval(form)
}
toc()



# ---------------------------------------------------------------------  
# (74) CREATING A DATA FRAME CONTAINING REQUIRED FULL TEXTS AND DOC_IDS ####
# ---------------------------------------------------------------------

#combining all loaded full_texts
df_list <- mget(ls(pattern = "^document_.*"))
res_text <- as.vector(Reduce(cbind, df_list))

# creating document id vector
doc_id <- paste0(rep("d_", nbr_docs), 1:nbr_docs)

# creating a data frame containing the both character vectors
res_fulltext_df <- data.frame(doc_id=character(nbr_docs), text=character(nbr_docs))

res_fulltext_df$doc_id <- doc_id
res_fulltext_df$text <- res_text


# removing unnecessary documents
tic()
for(i in 2:nbr_docs){
  form <-  parse(text = paste("rm('document_",i, "')", sep=""))
  eval(form)
}
toc()

# Identifying missing texts and removing them
ind <- which(is.na(res_fulltext_df$text))
# > ind
# [1]  299  344  353  355  379  557 1748 2846

res_fulltext_df <- res_fulltext_df[-ind, ]


# res_dictionary <- as.data.frame(c("resolution", "res", "united", "nations", "distr", "assembly",
#     "general", "agenda", "january", "february", "march", "april", "may", "june", "july", "august",
#     "september", "december", "session"))
# names(res_dictionary) <- "word"



# ----------------------------------------------  
# (75) CREATING A CORPUS BASED ON THE FULL TEXTS ####
# ----------------------------------------------

corpus_res <- tm::Corpus(tm::VectorSource(res_fulltext_df$text ))
corpus_res
str(corpus_res, list.len = 2)

# <<SimpleCorpus>>
#   Metadata:  corpus specific: 1, document level (indexed): 0
# Content:  documents: 3192


# inspect created corpus
writeLines(as.character(corpus_res[[1]]))
  

# ---------------------  
# (76) REMOVING NUMBERS ####
# ---------------------

corpus_res <- tm_map(corpus_res, removeNumbers)
corpus_res
writeLines(as.character(corpus_res[[1]]))

  
# -------------------------  
# (77) REMOVING PUNCTUATION ####
# -------------------------

corpus_res <- tm_map(corpus_res, removePunctuation)
corpus_res
writeLines(as.character(corpus_res[[1]]))


# ------------------------------  
# (77) REMOVING EXTRA WHITESPACE ####
# ------------------------------

corpus_res <- tm_map(corpus_res, stripWhitespace)
corpus_res
writeLines(as.character(corpus_res[[1]]))


# -----------------------------  
# (78) TRANSFORMING  LOWER CASE ####
# -----------------------------

corpus_res <- tm_map(corpus_res, content_transformer(tolower))
writeLines(as.character(corpus_res[[1]]))


# -----------------------  
# (79) REMOVING STOPWORDS ####
# -----------------------

corpus_res <- tm_map(corpus_res, removeWords, stopwords("english"))
corpus_res
writeLines(as.character(corpus_res[[1]]))



# --------------------------------------------  
# (80) LEMMATIZING USING HASH_LEMMA DICTIONARY ####
# --------------------------------------------

#convert corpus back to a data frame
res_fulltext_df_clean <- data_frame(object = corpus_res %>% 
   `class<-`("list") %>% 
    use_series(content) ) %>%
  rowwise %>%
  mutate(content = 
           object %>%
           names %>%
           extract(1) )

# adding doc_id columns to the created data frame
res_fulltext_df_clean$doc_id <- res_fulltext_df$doc_id

res_fulltext_df_clean <- res_fulltext_df_clean %>%
  rename(text=object)

# applying lemmatization-algorithm (hash_lemmas dictionary)
temp <- textstem::lemmatize_strings(res_fulltext_df_clean$text, dictionary = lexicon::hash_lemmas)



# ----------------------------------------------------------------------  
# (81) ESTIMATING TERM FREQUENCY AND INVERTED DOCUMENT FREQUENCY (TF/IDF) ####
# -----------------------------------------------------------------------

# tokenizing the cleaned full_texts
res_words <- res_fulltext_df_clean %>%
  tidytext::unnest_tokens(output = word, input=text) %>%
   count(doc_id, word, sort= TRUE)

# verify the result produced



















# ------------------------------------------------------------------------  
# (75) GENERATING APPROPRIATED TOKENS AND THE RELATED DOCUMENT TERM MATRIX ####
# ------------------------------------------------------------------------

dtm <- corpus_res %>%
  tidytext::unnest_tokens(output = word, input=text, drop=TRUE, to_lower=TRUE) %>%
   anti_join(stop_words, by="word") %>%
    anti_join(res_dictionary, by="word") %>%
     filter(str_detect(word, "_", negate=TRUE))  %>%
       filter(str_detect(word, "[\\d]", negate=TRUE))  %>%
        count(id, word)  %>%
         cast_dtm(document=id, term=word, value=n)

# display the matrix
dtm_df <- as.data.frame(as.matrix(dtm))
dim(dtm_df)
#[1]   5 639
  
# -------------------------------  
# (76) BUILDING A SIMPLE LDA MODEL ####
# -------------------------------

# set a seed so that the output of the model is predictable
res_lda <- topicmodels::LDA(dtm, k =3, control = list(seed = 05042014))
res_lda
# A LDA_VEM topic model with 2 topics. --> with the seededlda we will automatically get the optimal number of topics



# -----------------------------  
# (76) WORD-TOPIC PROBABILITIES ####
# -----------------------------

res_topics <- tidytext::tidy(res_lda, matrix="beta")
res_topics


res_top_terms <- res_topics %>%
 group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
   ungroup() %>%
   arrange(topic, -beta)

res_top_terms %>%
 mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
   geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()



# ---------------------------------  
# (77) DOCUMENT-TOPIC PROBABILITIES ####
# ---------------------------------

# having a look at the document-topics probabilities
document_topics_probs <- tidy(res_lda, matrix="gamma")
View(document_topics_probs)
document_topics_probs

# visualizin the probs distribution
tidy(res_lda, matrix="gamma") %>%
  ggplot(aes(x=document, y=gamma)) + 
   geom_col(aes(fill=as.factor(topic)))



