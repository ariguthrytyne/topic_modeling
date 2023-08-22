#==========================
# (1) PREPARING ENVIRONMENT ####
#==========================


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
library(textmineR)
library(tictoc)
library(textmineR)


#=================================
# (2) VISUALIZING MODELING RESULTS ####
#=================================


# --------------------------  
# (21) LOADING MODEL RESULTS ####
# --------------------------

# getting the required data (resolution data) from a local db (connection issue with )
message("--- loading cleaned resolution data : ---")
load(file="./models/full_model_22.rda")


# displaying top 10 words for each created topic
terms_lda_model <- as.data.frame(terms(lda.model, 10))

# exporting results as excel file
write.csv(terms_lda_model, file="./results/topics_words_k_22.csv")


# --------------------------------------------  
# (22) DISPLAYING DOCUMENT TOPICS DISTRIBUTION ####
# --------------------------------------------

# inspecting structure of the lda.model
str(lda.model)

# extracting gamma-matrix
m <- tidy(lda.model, matrix="gamma")

# selecting aleatory five (5) documents
all_docs <- unique(m$document)
sample_doc <- sample(all_docs, 5)

# visualizing the topic distribution
m <- tidy(lda.model, matrix="gamma") %>%
  filter(document %in% sample_doc)


m  %>% ggplot(aes(x=document, y=gamma)) +
   geom_col(aes(fill=as.factor(topic))) + 
    ggtitle("Topics document distribution")


# ---------------------------------  
# (22) DISPLAYING TOPICS SIMILARITY ####
# ---------------------------------


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


# ------------------------------  
# (22) DISPLAYING TF/IDF RESULTS (WORDCLOUD) ####
# ------------------------------

# loading selected terms using tf/idf
res_words <- readRDS(file='./data/res_words_tf_idf_select.rds')

# visualizing selected words using a wordcloud
count_res_words <- res_words %>%
  count(word) %>%
   arrange(desc(n)) %>%
    rename(freq=n)

# library(wordcloud)
# set.seed(1234)
# wordcloud::wordcloud(words = count_res_words$word, freq = count_res_words$n, min.freq = 1,
#                      max.words=1000, random.order=FALSE, rot.per=0.35,
#                      colors=brewer.pal(8, "Dark2"))

library(wordcloud2)
set.seed(1234)

wordcloud2(data=count_res_words, size = 0.6, minSize = 0, gridSize =  0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

# exporting created wordcloud
install.packages("webshot")
webshot::install_phantomjs()
library(wordcloud2)
library(htmlwidgets)


hw <- wordcloud2(data=count_res_words, size = 0.6, minSize = 0, gridSize =  0,
                 fontFamily = 'Segoe UI', fontWeight = 'bold',
                 color = 'random-dark', backgroundColor = "white",
                 minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
                 rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
                 widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

saveWidget(hw,"tf_idf_wordcloud.html",selfcontained = F)
webshot::webshot("tf_idf_wordcloud.html","tf_idf_wordcloud.png",vwidth = 1992, vheight = 1744, delay =10)


