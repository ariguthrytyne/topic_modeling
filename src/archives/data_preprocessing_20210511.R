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
library(doParallel)
library(foreach)
library(future)
library(future.apply)
library(tm)
library(stringr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(dplyr)


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

# random selection of 5 resolution texts from the basis-data frame
set.seed(100082008)

temp <- resolution_data_fulltext  %>%
  slice_sample(n=5) 

# selecting 5 random resolution full text (documents)
document_1 <- temp$ResolutionFullText[1]
document_2 <- temp$ResolutionFullText[2]
document_3 <- temp$ResolutionFullText[3]
document_4 <- temp$ResolutionFullText[4]
document_5 <- temp$ResolutionFullText[5]


# ---------------------------------------------  
# (74) BUILDING A CORPUS FROM THE RES_FULL_TEXT ####
# ---------------------------------------------

# res_full_text_word <- res_full_text %>%
#   tidytext::unnest_tokens(word, ResolutionFullText)
# 
# # find document-word counts
# word_counts <- res_full_text_word %>%
#   anti_join(stop_words) %>%
#    count(word, sort = TRUE) %>%
#     ungroup()

# ---------------------------------------------  
# (74) BUILDING A CORPUS FROM THE RES_FULL_TEXT ####
# ---------------------------------------------

# corpus_res <- quanteda::corpus(c(document_1, document_2, document_3, document_4, document_5))   # type of dataset
corpus_res <- as.data.frame(c(document_1, document_2, document_3, document_4, document_5))
id <- c("d_1","d_2", "d_3", "d_4","d_5")
names(corpus_res) <- c("text")
corpus_res$id <- id


res_dictionary <- as.data.frame(c("resolution", "res", "united", "nations", "distr", "assembly",
    "general", "agenda", "january", "february", "march", "april", "may", "june", "july", "august",
    "september", "december", "session"))
names(res_dictionary) <- "word"
  
  
# ------------------------------------------------------------------------  
# (75) GENERATING APPROPRIATED TOKENS AND THE RELATED DOCUMENT TERM MATRIX ####
# ------------------------------------------------------------------------


# dfm <- corpus_res %>% 
#   tokens(remove_punct=TRUE, remove_numbers=TRUE) %>%
#    tokens_wordstem(language="en") %>%
#     tokens_remove(stopwords("en")) %>%
#      anti_join(specific_dictionary)
#      tokens_tolower()  %>%
#       dfm()
     

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
View(dtm_df)
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

# visualizin the probs distribution
tidy(res_lda, matrix="gamma") %>%
  ggplot(aes(x=document, y=gamma)) + 
   geom_col(aes(fill=as.factor(topic)))



