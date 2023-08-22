#=======================
# (2) BUILDING LDA-MODEL ####
#=======================


# ------------------------------  
# (20) LOADING REQUIRED PACKAGES ####
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
library(textmineR)
library(tictoc)
library(textmineR)
library(ldatuning)


# ------------------------  
# (21) LOADING CLEANED DATA ####
# -------------------------

# getting the required data (resolution data) from a local db (connection issue with )
message("--- loading cleaned resolution data : ---")
res_fulltext_df_clean <- readRDS(file='./data/res_fulltext_df_clean.rds')


# ---------------------------------------------  
# (22) CREATING APPROPRIATE DOCUMENT TERM MATRIX ####
# ---------------------------------------------

# creating tokens and counting word frequency by document
tic()
res_words <- res_fulltext_df_clean %>%
 tidytext::unnest_tokens(output = word, input=text) %>%
   count(doc_id, word, sort= TRUE)
toc()
# 22.2 sec elapsed


# removing words having only 3 characters (93% remaining !!)
res_words_nchar <- res_words %>%
  filter(nchar(word) > 3)

# removing remaining digits and punctuation (they should be actually done !!!) 
res_words_nchar$word <- gsub('[[:digit:]]+', '', res_words_nchar$word)
res_words_nchar$word <- gsub('[[:punct:]]+', '', res_words_nchar$word)  # all fine , nothing removed

# creating the required document-term-matrix
tic()
dfm <- res_words_nchar %>%
  group_by(word) %>% 
   filter(sum(n) >= 10) %>% 
    ungroup() %>% 
     cast_dfm(document=doc_id, term=word, value=n) # cast_dtm -> cast tidy-text to dtm-object

toc()

# filtering words that appear less than 7.58% and more than 90%
dfm.trim <-
  dfm_trim(
    dfm,
    min_docfreq = 0.075,     # min 7.5%
    max_docfreq = 0.90,     # max 90%
    docfreq_type = "prop"
  ) 

# converting the dfm.trim-object to a dtm-object
dtm <-  quanteda::convert(x=dfm.trim, to="topicmodels")


# -------------------------------------  
# (22) FINDING OPTIMAL NUMBER OF TOPICS ####
# -------------------------------------

# find optimal number of topics
result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)


# --------------------------------------  
# (22) VISUALIZING THE CALCULATED METRICS  ####
# --------------------------------------

load(file="./models/results_ldatuning_50_last.rda")

FindTopicsNumber_plot(result)


# -------------------------------  
# (22) INITIAL RUN OF AN LDA-MODEL ####
# -------------------------------

# assigning an arbitrary topic model
topic.count <- 50 # we have around initial 50 expert-based topics in the original data frame 
                  # 194 in total but most of them are country-related topics

# convert the trimmed dfm to a topicmodels object
dfm2topicmodels <- quanteda::convert(dfm.trim, to = "topicmodels")

# calculating LDA-model with quanteda's LLDA
tic()
lda.model <- LDA(dfm2topicmodels, k=topic.count)
toc()

lda.model


# -------------------------------  
# (23) CHECKING TOPICS SIMILARITY ####
# -------------------------------

# checking 10 created terms and the belonging words
terms_lda_model <- as.data.frame(terms(lda.model, 10))


# checking lda topics similarity
lda.similarity <- as.data.frame(lda.model@beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

# checking the documents where the topics are well represented
topics_doc  <- data.frame(Thema = topics(lda.model))



# ---------------------------------------
# (24) SELECTING OPTIMAL NUMBER OF TOPICS  ####
# ---------------------------------------

###### --- Analyzing Topic coherence

# creating a document term matrix
dtm <- CreateDtm(res_fulltext_df_clean$text, 
                 doc_names = res_fulltext_df_clean$doc_id,
                 ngram_window = c(1, 1))

# exploring basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

# Eliminate words appearing less than 2 times or in more than half of the documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm

# evaluating the topics coherence
k_list <- seq(1, 50, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))

if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- textmineR::TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- textmineR::FitLdaModel(dtm = dtm, k = k, iterations = 500)
    
    m$k <- k
    m$coherence <- textmineR::CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines


# ----------------------------------------  
# (22) SELECTING OPTIMAL NUMBER OF TOPICS ####
# ----------------------------------------

# creating a list to com
models <- vector(mode="list", length = 50)


## calculating Log-Likelihood and perplexity
mod_log_lik <- numeric(50)
mod_perplexity <-  numeric(50)

for(i in 2:50){
  
  mod <- LDA(x=dtm, k=i, method="Gibbs", 
                         control=list(alpha=0.5, iter=50, seed=10082008, thin=1))
  mod_log_lik <- LogLik(mod)
  mod_perplexity <- perplexity(mod, dtm)
  
}
