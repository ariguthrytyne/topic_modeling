'TOPIC MODELING'

# Resolution: formal decision or statement of the opinion (will) by 
# United Nation General Assembly 
# preamble + operative part (mostly one sentence)

library(tidyverse)
library(tidytext)
library(magrittr)
library(dplyr)
library(SnowballC)
library(tm)

resolution_data_text <- readRDS("/Users/ryan/Library/CloudStorage/OneDrive-Personal/Ryan Aaron Tchouaké/Business/Kaeyros Analytics/Topic Modeling Project/resolution_data_fulltext.rds")
resolution_data_text <- readRDS("C:/Users/Ryan Tchouake/OneDrive/Ryan Aaron Tchouak�/Business/Kaeyros Analytics/Topic Modeling Project/resolution_data_fulltext.rds")
resolution_data_text <- readRDS("C:/Users/Ryan/OneDrive/Ryan Aaron Tchouaké/Business/Kaeyros Analytics/Topic Modeling Project/resolution_data_fulltext.rds")


### OVERVIEW

# to check general info about the data set
dim(resolution_data_text)
colnames(resolution_data_text)
summary(resolution_data_text)

# check if there is any missing Data
length(which(!complete.cases(resolution_data_text)))
# which?
which(is.na(resolution_data_text), arr.ind = T)

# create a new data frame to focus on the resolution texts and titles
ResolutionText <- data.frame(resolution_data_text$YEAR, resolution_data_text$ResolutionFullText)
colnames(ResolutionText) <- c("YEAR", "ResolutionFullTEXT")

ResolutionTitle <- data.frame(resolution_data_text$YEAR, resolution_data_text$TitleofResolution)
colnames(ResolutionTitle) <- c("YEAR", "ResolutionFullTITEL")

# number of words in every of the 4007 resolution
nchar(ResolutionText$ResolutionFullTEXT)
  
# divide by years because it could be interesting to check, 
# which terms were the focus in which year
range(ResolutionText$YEAR)

# shows how many resolution exist per year
ResolutionText %>%
  group_by(YEAR) %>%
  summarize(Number_Rows = n())
# there are much more starting from 2000



### TOKENIZING

# focus on the words that are used
TidyResolutionText <- ResolutionText %>%
  unnest_tokens(word, ResolutionFullTEXT)
# also checking sentences
TidyResolutionText_II <- ResolutionText %>%
  unnest_tokens(output = "sentece", token = "sentences", input = ResolutionFullTEXT)

TidyResolutionTitle <- ResolutionTitle %>%
  unnest_tokens(word, ResolutionFullTITEL)
# getting a data frame tokenized by total words used in the resolutions



### CLEANING

install.packages("quanteda")
library(quanteda)

# to do Lemmatization
install.packages("textstem")
library(textstem)
# e.g.
lemmatize_words(c("run", "ran", "running"))

lemmatize_words(c("african", "africa", "afric"))
lemmatize_strings(c("african", "africa", "afric"))

wordStem(c("african", "africa", "afric"))
stem_words(c("african", "africa", "afric"))

tokens_replace(TidyResolutionText, pattern = c("african", "africa", "afric"), replacement = "africa")


custom_stopwords1 <- add_row(stop_words, word = as.character(grep("\\d+", TidyResolutionText$word, value = T)), lexicon = "custom")
# because it is full of digits without any meaning
TidyResolutionText$word[TidyResolutionText$word == "africa"] <- "african"
TidyResolutionText$word[TidyResolutionText$word == "afric"] <- "african"

CleanResoulution_I <- TidyResolutionText %>%
  anti_join(custom_stopwords1) %>%
  mutate(word = lemmatize_words(word))
  

# Still too many words with little meaning, so so revision of the stop words
add_words1 <- c("unite", "union", "global", "country", "support", "include", "resolution", "december","september", "right", 
                "organ", "international", "assembly", "note", "zone", "nation", "government", "governmental", "referece", "main", "october",
                "november", "session", "continue", "report", "implement", "confer", "programm", "gener", "secretary",
                "?z?mc?", "yuzhmorgeologiya", "conference", "recall", "programme", "relevant", "call", "res", "conf", "corr",
                "procedure", "iv", "general's", "preference", "convention", "organization", "e's", "e.gv", "eel",
                "implementation", "committee", "declaration", "fifty")

c("unit", "union", "global", "countri", "support", "includ", "resolut", "decemb","septemb", "right", 
  "organ", "intern", "assembli", "note", "zone", "nation", "govern", "refer", "main", "octob",
  "novemb", "session","continu", "report", "implement", "confer", "programm", "gener", "secretari",
  "?z?mc?", "yuzhmorgeologiya")
custom_stopwords2 <- add_row(custom_stopwords1, word = add_words1, lexicon = "custom")

CleanResoulution_II <- CleanResoulution_I %>%
  #mutate(word = wordStem(word)) %>%
  anti_join(custom_stopwords2)


# how often a term appears in total
WordAppearance <- CleanResoulution_II %>%
  dplyr::count(word, sort = T)

summary(WordAppearance$n)

# check words that are not part of our alphabet 
grep("[^A-Za-z??????o?s'._]", WordAppearance$word, value = T)
# check for links with www.
add_words2 <- grep("www\\.", WordAppearance$word, value = T)

custom_stopwords3 <- add_row(custom_stopwords2, word = add_words2 , lexicon = "custom")

# filter these
CleanResoulution_III <- CleanResoulution_II %>%
  filter(grepl("[A-Za-z??????o?s']", word)) %>%
  anti_join(custom_stopwords3)



# ADDITIONAL INFOS

prop.table(table(CleanResoulution_III$YEAR))
# what percentage resolutions from a given year make up of the total resolutions

# to check if there are word that appear every year
Word_Year <- CleanResoulution_III %>%
  group_by(word) %>%
  summarize(YEAR_Count = n_distinct(YEAR))
SelectedWords <- Word_Year %>%
  filter(YEAR_Count >= 15)
# there are lots of words that appears in more than 15 Years

# recheck the word appearance
WordAppearance <- CleanResoulution_III %>%
  dplyr::count(word, sort = T) %>%
  mutate(word = fct_reorder(word,n))
# to get as plot the frequency sorted by size

summary(WordAppearance$n)
var(WordAppearance$n)
# words that appears only one time (spelling mistakes)
WordAppearance$word[WordAppearance$n == 1]



### FREQUENCY PLOTS / BAR PLOTS

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



### WORDCLOUD

library(wordcloud)

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



### SENTIMENTS PLOT

# check which words can get a label of sentiment
SentimentResolutionWord <- WordAppearance %>%
  inner_join(get_sentiments("nrc"))

SentimentResolutionWord <- WordAppearance %>%
  inner_join(get_sentiments("loughran")) # or afinn and bing

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

# ratio of words about which one can make a statement regarding sentiment
length(SentimentResolutionWord$word) / length(WordAppearance$word)
# 19,15% (very low)
# 38,82% (still low)

# the words that are not classify
levels(WordAppearance$word[!(SentimentResolutionWord$word %in% WordAppearance$word)])



### DOCUMENT TERM MATRIX

Resolution_DTM <- CleanResoulution_III %>%
  count(word, YEAR, sort = TRUE) %>%
  cast_dtm(YEAR, word, n) %>%
  # overview of number of documents and terms and how many entries non-zero
  as.matrix()
# rows: years from 1995 to 2020
# columns: appearing words in the resolutions over these 23 years
# entries: how often a certain word occurs in a certain year

# dimension
dim(Resolution_DTM)

# ordered by years from 1995 - 2020
Resolution_DTM <- as.matrix(Resolution_DTM[order(rownames(Resolution_DTM)), ])



### LINE CHART
matplot(Resolution_DTM[ ,1:7], type = "l", xlab = "Year", ylab = "Frequency", 
        col = c("blue", "red", "green", "black", "purple", "yellow", "orange"),
        lty = 1, lwd = 2, main = "Top 7 Word Frequency Over Years", xaxt = "n") +
axis(1, at = 1:length(rownames(Resolution_DTM)), labels = rownames(Resolution_DTM))
# shows the trend of usage of the 7 most common words over the 23 years



### PIE CHART

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
