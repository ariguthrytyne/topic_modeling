'TOPIC MODELING'
# The following file contains the script used for the development of a Topic Model 
# for Text Classification of Resolutions

# Resolution: formal decision or statement of the opinion (will) by 
# United Nation General Assembly 
# preamble + operative part (mostly one sentence)

# Change History
# Date			  Developer				      Action
# 14.08.2023  Ryan Aaron Tchouake   Initial Creation

#' Main function and activities that will be performed:
#'
#'  1- Preparing Environment
#'  
#'  2- Data Loading
#'  
#'  3- Data Exploration
#' 
#'  4- Text Pre-Processing: 
#'        a- Lemmatization
#'        b- Stemming
#'        c- Stop-Words removal
#'        d- Words Appearance
#'        e- Document Term Matrix
#'        f- Sentiment Analysis
#'        g- Corpus
#'        h- Normalization
#'        i- feature hashing
#'       
#'  5- Modeling 
#'        a- Latent Dirichlet Allocation (Unsupervised Learning)
#'        b- n-grams
#'        c- Regression (Supervised Learning)
#'        
#'  
#'  6- Visualization
#'      a- LDAVis
#'      b- Word Cloud
#'      c- Tag Cloud
#'      d- Slope Chart
#'      e- Sankey Chart
#'      
#'  
#'  7- deployment in production environment 
#'      a- Tests
#'      b- Dockerization
#'      c- Model Maintenance
#'      e- AWS-container & Instance



# 1. PREPARING ENVIRONMENT ----

# 1.1 CLEARING R-ENVIRONMENT
rm(list = ls(all.names = TRUE))

# 1.2 PATH CONSTRUCTION 
# constructing useful paths (data, codes, ..)
root <- getwd()
path.data <- file.path(root, "data", "raw")

# 1.3 LOADING PACKEGES
library(tidyverse)
library(tidytext)
library(magrittr)
library(dplyr)
library(SnowballC)
library(tm)
library(quanteda)
library(textstem)
library(wordcloud)
library(sos) # findFN
library(stringi)


# 2. DATA LOADING  ----

# 2.1 PATHS TO DATA 
# creating path to the required data
resolution_data_text <- readRDS(file.path(path.data, "resolution_data_fulltext.rds"))
message("--- loading all available resolution full text data : ---")
# OR
resolution_data_text <- readRDS("/Users/ryan/Library/CloudStorage/OneDrive-Personal/Ryan Aaron Tchouaké/Business/Kaeyros Analytics/Topic Modeling Project/resolution_data_fulltext.rds")
resolution_data_text <- readRDS("C:/Users/Ryan Tchouake/OneDrive/Ryan Aaron Tchouak?/Business/Kaeyros Analytics/Topic Modeling Project/resolution_data_fulltext.rds")
resolution_data_text <- readRDS("C:/Users/Ryan/OneDrive/Ryan Aaron Tchouaké/Business/Kaeyros Analytics/Topic Modeling Project/resolution_data_fulltext.rds")

# 2.2 NEW DATA FRAMES
# creating new data frame to focus on the resolution texts & titles
ResolutionText <- data.frame(resolution_data_text$YEAR, resolution_data_text$ResolutionFullText)
colnames(ResolutionText) <- c("YEAR", "ResolutionFullTEXT")

ResolutionTitle <- data.frame(resolution_data_text$YEAR, resolution_data_text$TitleofResolution)
colnames(ResolutionTitle) <- c("YEAR", "ResolutionFullTITEL")

# 2.3 MISSING VALUES ANALYSIS
# (column: full_text)
missing_full_text <- which(is.na(ResolutionText$ResolutionFullTEXT))
missing_full_text
# [1]   15   68 1267 1307 1315 1317 1339 2571 3572 3606 # 10 resolutions with no full text removed

# removing observation with missing text
ResolutionText <- ResolutionText[-missing_full_text, ]
ResolutionTitle <- ResolutionTitle[-missing_full_text, ]



# 3. DATA ELABORATION ----

# 3.1 OVERVIEW
# to check general info about the data set
dim(ResolutionText)
colnames(ResolutionText)
summary(ResolutionText)
str(ResolutionText)

# 3.2 N° OF WORDS IN RESOLUTIONS
# number of words in every of the 4007 resolution
ResolutionText$WordsNumber <- nchar(ResolutionText$ResolutionFullTEXT)

# 3.3 RESOLUTIONS PER YEAR
# shows how many resolution exist per year

# divide by years because it could be interesting to check, 
# which terms were the focus in which year
table(ResolutionText$YEAR)
# OR
ResolutionText %>%
  group_by(YEAR) %>%
  summarize(Number_Rows = n()) %>%
  print(n = 23)
# there are much more resolutions starting from the year 2000

prop.table(table(ResolutionText$YEAR))
# what percentage resolutions from a given year make up of the total resolutions

# 3.4 CHECKING VARIABLES
strsplit(resolution_data_text$AuthoringCountries[2], ",")[[1]]
# to check which countries were involved in der 2nd resolutions

# number of country participation in every resolution
CountryLength <- c()
for (i in 1:10){
  CountryLength <- c(CountryLength, length(strsplit(resolution_data_text$AuthoringCountries[i], ",")[[1]]))
}
CountryLength
# took first 10 as example

# 3.5 TOKENIZATION
# focus on the words that are used
TidyResolutionText <- ResolutionText %>%
  unnest_tokens(word, ResolutionFullTEXT)
# also checking sentences
TidyResolutionText_II <- ResolutionText %>%
  unnest_tokens(output = "sentece", token = "sentences", input = ResolutionFullTEXT)

TidyResolutionTitle <- ResolutionTitle %>%
  unnest_tokens(word, ResolutionFullTITEL)
# getting a data frame tokenized by total words used in the resolutions



# 4. TEXT PRE-PROCESSING ----

# 4.1 LEMMATIZING
# e.g.
lemmatize_words(c("run", "ran", "running"))
lemmatize_words(c("african", "africa", "afric"))
lemmatize_strings(c("african", "africa", "afric"))

tokens_replace(TidyResolutionText, pattern = c("african", "africa", "afric"), replacement = "africa")

# 4.2 STEMMING
# e.g.
wordStem(c("african", "africa", "afric"))
stem_words(c("african", "africa", "afric"))

# 4.3 STOP-WORDS
add_words1 <- as.character(grep("\\d+", TidyResolutionText$word, value = T))
custom_stopwords1 <- add_row(stop_words, word = add_words1 , lexicon = "custom")
# because it is full of digits without any meaning

TidyResolutionText$word[TidyResolutionText$word == "africa"] <- "african"
TidyResolutionText$word[TidyResolutionText$word == "afric"] <- "african"
# because it didn't worked out with lemmatizing or stemming

# Saving Changes
CleanResoulution_I <- TidyResolutionText %>%
  anti_join(custom_stopwords1) %>%
  mutate(word = lemmatize_words(word))
  

# Still too many words with little meaning, so so revision of the stop words
add_words2 <- c("unite", "union", "global", "country", "support", "include", "resolution", 
                "right", "organ", "international", "assembly", "note", "zone", "nation", 
                "government", "governmental", "referece", "main", "october", "session",
                "continue", "report", "implement", "confer", "programm", "general", 
                "secretary", "?z?mc?", "yuzhmorgeologiya", "conference", "recall", 
                "programme", "relevant", "call", "res", "conf", "corr","procedure", "iv",
                "general's", "preference", "convention", "organization", "e's", "e.gv", 
                "eel", "implementation", "committee", "declaration", "fifty","session", 
                "agenda", "item", "resolution", "january","february", "march", "april", 
                "may", "june", "july", "august", "september", "october", "november", 
                "december", "ii", "iii", "vi", "vii", "viii", "ix", "xi", "xii", "xiii", "xiv",
                "xv", "xvi", "xvii", "measure", "importance")
custom_stopwords2 <- add_row(custom_stopwords1, word = add_words2, lexicon = "custom")

# Saving Changes
CleanResoulution_II <- CleanResoulution_I %>%
  anti_join(custom_stopwords2)

# how often a term appears in total
WordAppearance <- CleanResoulution_II %>%
  dplyr::count(word, sort = T)

add_words3 <- grep("www\\.", WordAppearance$word, value = T)
custom_stopwords3 <- add_row(custom_stopwords2, word = add_words2 , lexicon = "custom")

# Saving Changes
CleanResoulution_III <- CleanResoulution_II %>%
  filter(grepl("[A-Za-z??????o?s']", word)) %>%
  anti_join(custom_stopwords3)
# filter words that are not part of our alphabet and links

# 4.4 WORD APPEARANCE 
# recheck the word appearance
WordAppearance <- CleanResoulution_III %>%
  dplyr::count(word, sort = T)

summary(WordAppearance)
var(WordAppearance$n)
# words that appears only one time (spelling mistakes)
WordAppearance$word[WordAppearance$n == 1]

# 4.5 DOCUMENT TERM MATRIX
Resolution_DTM <- CleanResoulution_III %>%
  count(word, YEAR, sort = TRUE) %>%
  cast_dtm(YEAR, word, n) %>%
  # overview of number of documents and terms and how many entries non-zero
  as.matrix()
# rows: years from 1995 to 2020
# columns: appearing words in the resolutions over these 23 years
# entries: how often a certain word occurs in a certain year

# ordered by years from 1995 - 2020
Resolution_DTM <- as.matrix(Resolution_DTM[order(rownames(Resolution_DTM)), ])

# 4.6 SENTIMENT ANALYSIS
# check which words can get a label of sentiment
SentimentResolutionWord <- WordAppearance %>%
  inner_join(get_sentiments("nrc"))

SentimentResolutionWord <- WordAppearance %>%
  inner_join(get_sentiments("loughran"))

SentimentResolutionWord <- WordAppearance %>%
  inner_join(get_sentiments("afinn"))

SentimentResolutionWord <- WordAppearance %>%
  inner_join(get_sentiments("bing"))

# ratio of words about which one can make a statement regarding sentiment
length(SentimentResolutionWord$word) / length(WordAppearance$word)
# 38,82%
# 8,17%
# 4,44%
# 10,12$

# the words that are not classify
setdiff(WordAppearance$word, SentimentResolutionWord$word)

# 4.7 CORPUS
# contains text and metadata
# Corpora: collections of documents containing natural language text
CorpusResolution <- tm::Corpus(tm::VectorSource(CleanResoulution_III))
CorpusResolution

### ADDITIONAL INFOS
# to check if there are word that appear every year
Word_Year <- CleanResoulution_III %>%
  group_by(word) %>%
  summarize(YEAR_Count = n_distinct(YEAR))
SelectedWords <- Word_Year %>%
  filter(YEAR_Count >= 15)
# there are lots of words that appears in more than 15 Years

###



# 6- VISUALIZATION
# 6.1 FREQUENCY PLOTS / BAR PLOTS

WordAppearancePlot <- CleanResoulution_III %>%
  count(word, sort = T) %>%
  slice_max(n, n = 100) %>%
  mutate(word = fct_reorder(word,n))
# to get as plot the frequency sorted by size

ggplot(
  WordAppearancePlot, aes(x = word, y = n)
) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total Resolution Words Frequency") +
  labs(x = "Words in the Resolutions", y = "Frequency")


# plotting the top 5 words of every Year
TopWordsYear <- CleanResoulution_III %>%
  count(YEAR, word) %>%
  arrange(YEAR, desc(n)) %>%
  group_by(YEAR) %>%
  top_n(5)
  
ggplot(
  TopWordsYear, aes(x = word, y = n, fill = YEAR)) + 
  geom_col(show.legend = F) +
  facet_wrap(~ YEAR, scales = "free_y") +
  coord_flip() +
  ggtitle("Top Words in Resolution from 1995 to 2022")


# plotting the top 15 words of every year separately 
for (i in unique(CleanResoulution_III$YEAR)[order(unique(CleanResoulution_III$YEAR))]){
  TopWordsYears <- CleanResoulution_III %>%
    count(YEAR, word) %>%
    filter(YEAR == i) %>%
    slice_max(n, n = 15)
  
  GraphTitle <- paste0("Top Words in Resolution of ", i)
  PL <- ggplot(
    TopWordsYears, aes(x = word, y = n, fill = YEAR))+ 
    geom_col(show.legend = F) +
    coord_flip() +
    labs(title = GraphTitle, x = "Top 10 Words", y = "Frequency")
  
  print(PL)
}

# 6.2 WORDCLOUD
# Total words
wordcloud(
  words = WordAppearancePlot$word,
  freq = WordAppearance$n,
  max.words = 50,
  ordered.colors = T,
  colors = rep(c("red", "blue", "orange", "purple", "green"), 20),
  title = "Top Words in the Resolutions")

# 2020
# repeated check of the most actual terms
WordsCounts2020 <- CleanResoulution_III %>%
  count(YEAR, word, sort = T) %>%
  filter(YEAR == "2020")
  
wordcloud(
  words = WordsCounts2020$word[1:50],
  freq = WordsCounts2020$n,
  max.words = 50,
  ordered.colors = T,
  colors = rep(c("red", "blue", "orange", "black", "green"), 10),
  title = "Top Words in the Resolution of 2020")

# 6.3 SENTIMENTS PLOT
SentimentWordCounts <- SentimentResolutionWord %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) 

ggplot(
  SentimentWordCounts, aes(x = word, y = n, fill = sentiment)) + 
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  labs(title = "Sentiment Word Count", x = "Words")
# bar chart for each mood (from nrc) with the top 10, most frequently occurring words,
# which can be assigned to this mood. the absolute frequency is shown on the x axis.


# 6.4 LINE CHART
matplot(Resolution_DTM[ ,1:7], type = "l", xlab = "Year", ylab = "Frequency", 
        col = c("blue", "red", "green", "black", "purple", "yellow", "orange"),
        lty = 1, lwd = 2, main = "Top 7 Word Frequency Over Years", xaxt = "n") +
axis(1, at = 1:length(rownames(Resolution_DTM)), labels = rownames(Resolution_DTM))
# shows the trend of usage of the 7 most common words over the 23 years

# 6.5 PIE CHART
# pie chart for the words of year 2020
pie(Resolution_DTM["2020", 1:30], labels = colnames(Resolution_DTM)[1:30], 
    main = "Word Distribution in Year 2020 ", col = rainbow(30))

# pie chart for every Year separately
for (i in rownames(Resolution_DTM)){
  PIE_YEAR <- Resolution_DTM[i, 1:30]
  PIE_TITLE <- paste0("Word Distribution in Year ", i)
  
  PIE <- pie(PIE_YEAR, labels = colnames(Resolution_DTM)[1:30], 
             main = PIE_TITLE, col = rainbow(30))
  print(PIE)
}
# shows what proportion of the 30 most common words (for each year) each word has
# compared to the other words in the resolutions of the year.
