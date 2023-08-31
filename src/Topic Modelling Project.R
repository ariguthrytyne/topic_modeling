'TOPIC MODELING'
# The following file contains the script used for the development of a Topic Model 
# for Text Classification of Resolutions

# Resolution: formal decision or statement of the opinion (will) by 
# United Nation General Assembly containing preamble + operative part (mostly one sentence)

# History
# Creation Date			  Developer				      Action
# 14.08.2023          Ryan Aaron Tchouake   Initial Creation

# Last Update
# 31.08.2023



#' Table of Contents
#'
#'  1- Preparing Environment
#'      1.1- Clearing R Environment 
#'      1.2- Loading Packages PATH CONSTRUCTION 
#'      1.3- Path Construction
#'      
#'  2- Data Loading
#'      2.1- Paths to Data & Loading
#'      2.2- Missing Values Analysis
#'      2.3- New Data Frame
#'      2.4- Corpus
#'      
#'  3- Data Exploration
#'      3.1- Overview
#'      3.2- N°of Letters in Resolutions
#'      3.3- Resolutions per Year
#'      3.4- Countries Variable Analysis
#' 
#'  4- Text Pre-Processing & Cleaning 
#'      4.1- Lower Case
#'      4.2- Punctuation
#'      4.3- Numbers
#'      4.4- Lemmatizing
#'      4.5- Stop Words
#'      4.6- Regular Expressions
#'      4.7- Back to Clean Data Frame
#'      
#'  5- Tokenization 
#'      
#'  6- Modelbuilding Analysis 
#'      6.1- DTM
#'      6.2- DFM
#'      6.3-
#'      6.4- Sentiment Analysis
#'      
#'  7- Modeling 
#'        7.1- Latent Dirichlet Allocation (Unsupervised Learning)
#'        7.2- n-grams
#'        7.3- Regression (Supervised Learning)
#'        
#'  8- Visualization
#'      8.1- LDA Visualization
#'      8.2- Wordcloud
#'      8.3- Tagcloud
#'      8.4- Slope Chart
#'      8.5- Sankey Chart
#'      
#'  9- Deployment in Production Environment 
#'      9.1- Tests
#'      9.2- Dockerization
#'      9.3- Model Maintenance
#'      9.4- AWS-container & Instance



# 1. PREPARING ENVIRONMENT ----

# 1.1 CLEARING R-ENVIRONMENT
rm(list = ls(all.names = TRUE))

# 1.2 LOADING PACKAGES
library(tidyverse)
library(tidytext)
library(textdata)
library(magrittr)
library(dplyr)
library(SnowballC)
library(tm)
library(quanteda)
library(textstem)
library(wordcloud)
library(sos) # findFN
library(stringi)
library(topicmodels)

# 1.3 PATH CONSTRUCTION 
# constructing useful paths (data, codes, ..)
root <- getwd()
path.data <- file.path(root, "data", "raw")

# 2. DATA LOADING  ----

# 2.1 PATHS TO DATA & LOADING
# creating path to the required data
resolution_data_text <- readRDS(file.path(path.data, "resolution_data_fulltext.rds"))
message("--- loading all available resolution full text data : ---")

# 2.2 MISSING VALUES ANALYSIS
# (column: full_text)
missing_full_text <- which(is.na(resolution_data_text$ResolutionFullText))
missing_full_text
# [1]   15   68 1267 1307 1315 1317 1339 2571 3572 3606 # 10 resolutions with no full text removed

# removing observation with missing text
resolution_data_text <- resolution_data_text[-missing_full_text, ]

# 2.3 NEW DATA FRAME
# adding an id-variable to the data frame
Vitual_ID <- paste0(rep("Document_", nrow(resolution_data_text)), 1:nrow(resolution_data_text))
resolution_data_text$ID <- Vitual_ID

# creating new data frame to focus on the resolution texts & titles
ResolutionText <- data.frame(resolution_data_text$ID, resolution_data_text$YEAR, 
                             resolution_data_text$AuthoringCountries,
                             resolution_data_text$TitleofResolution, 
                             resolution_data_text$ResolutionFullText)
colnames(ResolutionText) <- c("Document_ID", "YEAR", "Countries", "ResolutionTITLE",
                              "ResolutionFullTEXT")

# 2.4 CORPUS
# contains text and metadata
# Corpora: collections of documents containing natural language text
CorpusResolution <- tm::Corpus(tm::VectorSource(ResolutionText$ResolutionFullTEXT))
# Inspection of one 



# 3. DATA ELABORATION ----

# 3.1 OVERVIEW
# to check general info about the data set
dim(ResolutionText)
colnames(ResolutionText)
summary(ResolutionText)
str(ResolutionText)
# to check general info about the corpus
inspect(CorpusResolution[[1]])
CorpusResolution[[1]]$content
CorpusResolution[[1]]$meta
CorpusResolution[[1]]$meta$id
CorpusResolution[[1]]$meta$language

# 3.2 N? OF LETTERS IN RESOLUTIONS
# number of letters in every of the 3997 resolution left
ResolutionText$WordsNumber <- nchar(ResolutionText$ResolutionFullTEXT)

# 3.3 RESOLUTIONS PER YEAR
# shows how many resolution exist per year

# divide by years because it could be interesting to check, 
# how many resolutions exist in every year
table(ResolutionText$YEAR)
# OR
ResolutionText %>%
  group_by(YEAR) %>%
  summarize(Number_Rows = n()) %>%
  print(n = 23)
# there are much more resolutions starting from the year 2000

prop.table(table(ResolutionText$YEAR))
# what percentage resolutions from a given year make up of the total resolutions

# 3.4 COUNTRIES VARIABLE ANALYSIS
strsplit(ResolutionText$Countries[2], ",")[[1]]
# to check which countries were involved in the 2nd resolutions

# number of country participation in every resolution
CountryLength <- c()
for (i in 1:10){
  CountryLength <- c(CountryLength, length(strsplit(resolution_data_text$AuthoringCountries[i], ",")[[1]]))
}
CountryLength
# took first 10 as example



# 4. TEXT PRE-PROCESSING ----

# 4.1 LOWER CASE
# transforming lower case
CorpusResolution <- tm_map(CorpusResolution, content_transformer(tolower))

# 4.2 REMOVING PUNCTUATION
CorpusResolution <- tm_map(CorpusResolution, removePunctuation, preserve_intra_word_dashes = FALSE, ucp = TRUE)

# 4.3 REMOVING NUMBERS
CorpusResolution <- tm_map(CorpusResolution, removeNumbers)

# 4.4 LEMMATIZING
# e.g.
lemmatize_words(c("run", "ran", "running"))
lemmatize_words(c("african", "africa", "afric"))
lemmatize_strings(c("african", "africa", "afric"))

# focus on lemmatize words
CorpusResolution <- tm_map(CorpusResolution, content_transformer(lemmatize_strings))

# 4.5 STOP-WORDS
# creating custom stop words
CustomStopwords <- c("unite", "union","distr","global","country","support","include",
                     "resolution", "right","organ","international","assembly","note","zone",
                     "nation","ares", "government","governmental","reference","refer","main",
                     "october","session","agendum","alrev","adopt","aadd","ee","aa","add",
                     "continue","report","implement","conference","programm","general","isl",
                     "oo","nn","ff","gg","aladd","aad",
                     "secretary","yuzhmorgeologiya","conference","recall","programme","al",
                     "relevant","call","res","conf","corr","procedure","measure","importance",
                     "item","general's","preference","convention","organization","e's","e.gv",
                     "eel","implementation","committee","declaration","twenty","thirty",
                     "forty","fifty","sixty","seventy","eighty","ninety","hundred",
                     "thirteen","fourteen","fifteen","eighteen","sixteen","nineteen",
                     "seventeen",
                     "first","second","third","fourth","fifth","sixth","seventh","eighth",
                     "ninth",
                     "fiftythird","fiftyfifth","fiftysixth","fiftyseventh",
                     "fiftyeighth","fiftyninth",
                     "sixtyfirst","sixtysecond","sixtythird","sixtyfourth","sixtyfifth",
                     "sixtysixth","sixtyseventh","sixtyeighth","sixtyninth",
                     "seventyfirst","seventysecond","seventythird","seventyfourth",
                     "seventysixth","seventyeight","seventyninth",
                     "session","agenda","item","resolution",
                     "january","february","march","april", "may","june","july","august",
                     "september","october","november","december",
                     "ii","iii","iiia","iv","vi","vii","viii","ix","xi","xii","xiii","xiv",
                     "xv","xvi","xvii","st","nd","rd")
# removing stop words
CorpusResolution <- tm_map(CorpusResolution, removeWords, c(stop_words$word, CustomStopwords))

# 4.6 REGULAR EXPRESSIONS
# create custom function to remove other misc characters
TextPreprocessing <- function(x){
  gsub("[^a-z ]","",x) # remove non alphabetic
  gsub("\\<african\\>|\\<afric\\>", "africa",x)
  gsub("www\\S+\\s*","",x) # remove URLs
  gsub("[[:cntrl:]]","",x) # remove controls and special characters
  gsub("\\_|-","",x) # remove special characters
  gsub("^.$","",x) # remove words with only one letter
  
  gsub("^[[:space:]]*","",x) # remove leading white spaces
  gsub("[[:space:]]*$","",x) # remove trailing white spaces
  gsub(" +"," ",x) # remove extra white spaces
}
# removing by using regular expressions
CorpusResolution <- tm_map(CorpusResolution, TextPreprocessing)
inspect(CorpusResolution[[50]])

# 4.7 BACK TO CLEAN DATA FRAME
TextDataFrame <- tibble(ResolutionFullTEXT = sapply(CorpusResolution, as.character))

ResolutionText$ResolutionFullTEXT <- NULL
ResolutionText$ResolutionFullTEXT <- TextDataFrame$ResolutionFullTEXT



# 5. TOKENIZATION ----
# focus on every words that are used
TidyResolutionTextI <- ResolutionText %>%
  unnest_tokens(word, ResolutionFullTEXT) %>%
  filter(grepl("^\\w+$", ignore.case = T, word))
# focus on three words combination
TidyResolutionTextIII <- ResolutionText %>%
unnest_tokens(output = "word",input = ResolutionFullTEXT, token = "words", to = 3)
# getting a data frame tokenized by total words used in the resolutions



# 6. MODELBUILDING ANALYSIS ----

# 6.1 DTM
Resolution_DTM <- TidyResolutionTextI  %>%
  count(word, Document_ID, sort = T) %>%
  cast_dtm(Document_ID, word, n) %>%
  # overview of number of documents and terms that appears i every document
  as.matrix()
# rows: Document
# columns: appearing words in the resolutions over these document
# entries: how often a certain word occurs in a certain document
Resolution_DTM <- as.matrix(Resolution_DTM[order(rownames(Resolution_DTM)), ])
# ordered by years from 1 - 3997

ResolutionYEAR_DTM <- TidyResolutionTextI %>%
  count(word, YEAR, sort = TRUE) %>%
  cast_dtm(YEAR, word, n) %>%
  # overview of number of documents and terms and how many entries non-zero
  as.matrix()
# rows: years from 1995 to 2020
# columns: appearing words in the resolutions over these 23 years
# entries: how often a certain word occurs in a certain year
ResolutionYEAR_DTM <- as.matrix(ResolutionYEAR_DTM[order(rownames(ResolutionYEAR_DTM)), ])
# ordered by years from 1995 - 2020

# 6.2 DFM


# 6.3 WORD APPEARANCE 
# recheck the word appearance
WordAppearance <- TidyResolutionTextI %>%
  dplyr::count(word, sort = T)

summary(WordAppearance)
var(WordAppearance$n)
# words that appears only one time (spelling mistakes)
WordAppearance$word[WordAppearance$n == 1]


# 6.4 SENTIMENT ANALYSIS
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

# the words that are not classify
setdiff(WordAppearance$word, SentimentResolutionWord$word)


### 
# ADDITIONAL INFOS
# to check if there are word that appear every year
Word_Year <- TidyResolutionTextI %>%
  group_by(word) %>%
  summarize(YEAR_Count = n_distinct(YEAR))
SelectedWords <- Word_Year %>%
  filter(YEAR_Count >= 15)
# there are lots of words that appears in more than 15 Years
###



# 7. MODELING ----

# 7.1- Latent Dirichlet Allocation (Unsupervised Learning)
dim(Resolution_DTM)
SampleSize <- floor(0.8 * ncol(Resolution_DTM))
Train_IND <- sample(ncol(Resolution_DTM), size = SampleSize)
# getting randomly 80% of the words
TrainDATA <- Resolution_DTM[ ,Train_IND]
TestDATA <- Resolution_DTM[ ,-Train_IND]

LDA_Model <- LDA(TrainDATA, k = 5, method = "Gibbs", control = list(seed = 42))
  
LDA_Model_BETA <- tidy(LDA_Model, matrix = "beta") # how related is the term to the topic
LDA_Model_GAMMA <- tidy(LDA_Model, matrix = "gamma") # how much every topic make of document...

# checking top words appearances in topic 3
LDA_Model_BETA %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta) %>%
  filter(topic == 3)

LDA_Model_GAMMA %>%
  filter(topic == 3) %>%
  arrange(desc(gamma)) %>%
  select(term)

# 8. VISUALIZATION ----

# 8.1 LDA VISUALISATIONS
WordsBETA <- LDA_Model_BETA %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  mutate(term = fct_reorder(term, beta))
ggplot(
  WordsBETA,
  aes(x = term, y = beta, fill = as.factor(topic))
)+
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") +
  coord_flip()

WordsGAMMA <- LDA_Model_GAMMA %>%
  group_by(topic) %>%
  slice_max(gamma, n = 15) %>%
  ungroup() %>%
  mutate(term = fct_reorder(term, gamma))
ggplot(
  WordsGAMMA,
  aes(x = term, y = gamma, fill = as.factor(topic))
)+
  geom_col(show.legend = F) + 
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# 8.2 FREQUENCY PLOTS / BAR PLOTS
WordAppearancePlot <- TidyResolutionTextI %>%
  count(word, sort = T) %>%
  slice_max(n, n = 100) %>%
  mutate(word = fct_reorder(word,n))
# to get as plot the frequency sorted by size

ggplot(
  WordAppearancePlot, aes(x = word, y = n)
) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_gradientn(colors = terrain.colors(10))+
  ggtitle("Total Resolution Words Frequency") +
  labs(x = "Words in the Resolutions", y = "Frequency")


# plotting the top 5 words of every Year
TopWordsYear <- TidyResolutionTextI %>%
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
for (i in unique(TidyResolutionTextI$YEAR)[order(unique(TidyResolutionTextI$YEAR))]){
  TopWordsYears <- TidyResolutionTextI %>%
    count(YEAR, word) %>%
    filter(YEAR == i) %>%
    slice_max(n, n = 15)
  
  GraphTitle <- paste0("Top Words in Resolution of ", i)
  PL <- ggplot(
    TopWordsYears, aes(x = word, y = n, fill = YEAR))+ 
    scale_colour_gradientn(colors = terrain.colors(10))+
    geom_col(show.legend = F) +
    coord_flip() +
    labs(title = GraphTitle, x = "Top 10 Words", y = "Frequency")
  
  print(PL)
}

# 8.3 WORDCLOUD
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
WordsCounts2020 <- TidyResolutionTextI %>%
  count(YEAR, word, sort = T) %>%
  filter(YEAR == "2020")
  
wordcloud(
  words = WordsCounts2020$word[1:50],
  freq = WordsCounts2020$n,
  max.words = 50,
  ordered.colors = T,
  colors = rep(c("red", "blue", "orange", "black", "green"), 10),
  title = "Top Words in the Resolution of 2020")

# 8.4 LINE CHART
matplot(Resolution_DTM[ ,1:7], type = "l", xlab = "Year", ylab = "Frequency", 
        col = c("blue", "red", "green", "black", "purple", "yellow", "orange"),
        lty = 1, lwd = 2, main = "Top 7 Word Frequency Over Years", xaxt = "n") +
axis(1, at = 1:length(rownames(Resolution_DTM)), labels = rownames(Resolution_DTM))
# shows the trend of usage of the 7 most common words over the 23 years

# 8.5 PIE CHART
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
