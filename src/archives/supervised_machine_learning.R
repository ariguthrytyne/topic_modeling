# --------------------------------------------------
# SUPERVISED MACHINE LEARNING FOR TEXT ANALYSIS IN R
# --------------------------------------------------


# CHAPTER 1: Language and Modeling
# ---------
#
# (1) Machine Learning and Deep Learning  Models --> designed and created by humans
#
# (2) NLP practitioners: assumptions about what language is and how language works 
#     will be brought into the task of creating modeling features from natural and
#     using those features as inputs into statistical models              
#
# (3) Linguistics: study of how language works
#
#      * phonetics   (sounds that people use in language)
#      * phonology   (systems of sounds in particular languages)
#      * morphology  (how words are formed)
#      * syntax      (how sentences are formed from words)
#      * semantics   (what sentences mean)
#      * pragmatics  (how language is used in context)
#
#  
# CHAPTER 2: Tokenization
# ---------
#
# To build features for supervised machine learning from natural language, we need some
# way of representing raw text as numbers 
#
# (4) tokenization: split text into tokens ()
#
# (5) types of tokens: characters, words, sentences, lines, paragraphs, n-grams
#

# loading requires packages
library(tokenizers)
library(tidyverse)
library(tidytext)
require(hcandersenr)

# loading test data (from the book the fir tree)
the_fir_tree <- hcandersen_en %>%
  filter(book == "The fir tree") %>%
  pull(text)

head(the_fir_tree, 9)


# splitting the first two lines by any character that are not numeric
strsplit(the_fir_tree[1:2], "[^a-zA-Z0-9]+")

# results are not always optimal since words-compound like "fire-tree" for example can be 
#  lost on the way

# fast and consistent tokenizers were implemented by tokenizers and spacy
library(tokenizers)

strsplit(the_fir_tree[1:2], "[^a-zA-Z0-9]+")
the_fir_tree[1:2]
tokenize_words(the_fir_tree[1:2])

# exploring tokenizers package
sample_vector <- c("Far down in the forest",
                   "grew a pretty little fir-tree")
sample_tibble <- tibble(text = sample_vector)
tokenize_words(sample_vector)

sample_tibble %>%
  unnest_tokens(word, text, token = "words", strip_punct = FALSE)  # same result but in vertical-format


# creating character token (split into character)
tft_token_characters <- tokenize_characters(x = the_fir_tree,
    lowercase = TRUE,strip_non_alphanum = TRUE, simplify = FALSE)

head(tft_token_characters) %>%
  glimpse()

tokenize_characters("ﬂowers")

# tokenization of emojis
flags <- "🇨🇦🇦🇶🇪🇺🇯🇵"
tokenize_characters(flags)

# tokenization at word level
tft_token_words <- tokenize_words(x = the_fir_tree,lowercase = TRUE,
     stopwords = NULL,strip_punct = TRUE, strip_numeric = FALSE)

# create a tibble 
hcandersen_en %>%
  filter(book %in% c("The fir tree", "The little mermaid")) %>%
   unnest_tokens(word, text) %>%
    count(book, word) %>%
    group_by(book) %>%
     arrange(desc(n)) %>%
     slice(1:5)


# tokenization by n-grams (words-compound og length n)

# unigram: “Hello,” “day,” “my,” “little”
# bigram: “fir tree,” “fresh air,” “to be,” “Robin Hood”
# trigram: “You and I,” “please let go,” “no time like,” “the little mermaid”


tft_token_ngram <- tokenize_ngrams(x = the_fir_tree,lowercase = TRUE,n = 3L,
    n_min = 3L, stopwords = character(), ngram_delim = " ", simplify = FALSE)

tft_token_ngram[[1]]

# Lines, sentence, and paragraph tokens

add_paragraphs <- function(data) {
  pull(data, text) %>%
    paste(collapse = "\n") %>%
    tokenize_paragraphs() %>%
    unlist() %>%
    tibble(text = .) %>%
    mutate(paragraph = row_number())
}


library(janeaustenr)

northangerabbey_paragraphed <- tibble(text = northangerabbey) %>%
  mutate(chapter = cumsum(str_detect(text, "^CHAPTER "))) %>%
    filter(chapter > 0, !str_detect(text, "^CHAPTER ")) %>%
     nest(data = text) %>%
      mutate(data = map(data, add_paragraphs)) %>%
       unnest(cols = c(data))

glimpse(northangerabbey_paragraphed)


the_fir_tree_sentences <- the_fir_tree %>%
 paste(collapse = " ") %>%
  tokenize_sentences()


head(the_fir_tree_sentences[[1]])



# CHAPTER 3: Stop words
# ---------


# once we have split text into tokens, it often becomes clear that not all word scarry
# the same amount of information 



# loading require libraries
library(stopwords)
length(stopwords(source = "smart"))
length(stopwords(source = "snowball"))
length(stopwords(source = "stopwords-iso"))


# removing stop words
library(hcandersenr)
library(tidyverse)
library(tidytext)

fir_tree <- hca_fairytales() %>%
  filter(book == "The fir tree",
         language == "English")

tidy_fir_tree <- fir_tree %>%
  unnest_tokens(word, text)

tidy_fir_tree_light <- tidy_fir_tree %>%
  anti_join(get_stopwords(source = "snowball"))


# creating custom stop words list
# this will be done by estimating the idf: inverted document frequency

library(rlang)

calc_idf <- function(df, word, document) {
  words <- df %>% pull({{word}}) %>% unique()
  n_docs <- length(unique(pull(df, {{document}})))
  n_words <- df %>%
    nest(data = c({{word}})) %>%
    pull(data) %>%
    map_dfc(~ words %in% unique(pull(.x, {{word}}))) %>%
    rowSums()
  
  tibble(word = words,
         idf = log(n_docs / n_words))
}


# getting from 3288 words to 1547 words after removing stop words


# CHAPTER 4: Stemming and Lemmatization
# ---------

# When we deal with text, often documents contain different versions of one base word, 
# often called a stem. “The Fir-Tree,” for example, contains more than one version 
# (i.e., inflected form) of the word "tree"


# loading required packages
library(hcandersenr)
library(tidyverse)
library(tidytext)

# loading test data
fir_tree <- hca_fairytales() %>%
 filter(book == "The fir tree",language == "English")

# tokenization of the text contained in the test_data and removing sop words 
tidy_fir_tree <- fir_tree %>%
 tidytext::unnest_tokens(word, text) %>%
  anti_join(get_stopwords(language="en", source="snowball"), by="word")

# estimating word frequency and detecting all words containing the string "tree"
tidy_fir_tree_count <- tidy_fir_tree %>%
  count(word, sort=TRUE) %>%
  filter(str_detect(word, "^tree"))

# # A tibble: 3 x 2
# word       n
# <chr>  <int>
#   1 tree      76
# 2 trees     12
# 3 tree's     1

tidy_fir_tree_count_wo_tree <- tidy_fir_tree %>%
 count(word, sort=TRUE) %>%
  filter(str_detect(word, "^tree", negate=TRUE))

# lemmatizing using another package

# sometimes we are not interested in the differences between tree, tress and tree's 
# this is the heart of the "stemming" activity --> identifying the base word (stem)
# for a data set of words. Stemming -_> concerned with the linguistic subfield of morphology


# How to stem ?

# loading required library
library(SnowballC)

tidy_fir_tree_stem <- tidy_fir_tree %>%
  mutate(stem = wordStem(word, language=)) %>%
  count(stem, sort = TRUE)

# tidy_fir_tree_stem_2 <- tidy_fir_tree %>%
#   tokenize_word_stems(word, language="english")


# stemming works also for other languages
stopword_df <- tribble(~language, ~two_letter,
                       "danish",  "da",
                       "english", "en",
                       "french",  "fr",
                       "german",  "de",
                       "spanish", "es")


# tokenization and stemming
tidy_by_lang <- hca_fairytales() %>%
 filter(book == "The fir tree") %>%
  select(text, language) %>%
   mutate(language = str_to_lower(language)) %>%
    unnest_tokens(word, text) %>%
     nest(data = word)

# stemming by various languages and displaying the top 20 stems

tidy_by_lang %>%
  inner_join(stopword_df) %>%
  mutate(data = map2(
    data, two_letter, ~ anti_join(.x, get_stopwords(language = .y)))
  ) %>%
  unnest(data) %>%
   mutate(stem = wordStem(word, language = language)) %>%
    group_by(language) %>%
    count(stem) %>%
  top_n(20, n) %>%
  ungroup %>%
  ggplot(aes(n, fct_reorder(stem, n), fill = language)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~language, scales = "free_y", ncol = 2) +
  labs(x = "Frequency", y = NULL)

# stemming using the hunspell algorithm
library(hunspell)

tidy_fir_tree_hun <- tidy_fir_tree %>%
 mutate(stem = hunspell_stem(word)) %>%
  unnest(stem) %>%
   count(stem, sort = TRUE)


# should you use stemming at all?
# stemming is a pre-processing step that can be used for reduction of the feature
# for text data  -_> example using data from United D´States Supreme Court 


# install.packages("remotes")
remotes::install_github("EmilHvitfeldt/scotus")
library(scotus)

tidy_scotus <- scotus_filtered %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())

tidy_scotus_count <- tidy_scotus %>%
  count(word, sort = TRUE)

# constructing the related document term matrix
tidy_scotus_count_dtm <- tidy_scotus %>%
  count(case_name, word) %>%
   cast_dfm(case_name, word, n)


# --------------------------------------------------------------------------------------------
# (a) Lemmatization: doesn't use rules to cut words down to their stems, but uses knowledge
#     about language structure to reduce words down to their lemmas ---> Linguistic based
#   
#
# (b) Lemmas: a heading indicating the subject or argument of a literary composition, an 
#             annotation , or a dictionary entry.
#
# (c) Lemmatization require more information than the rule-based stemmers- context and 
#     word mean in their context is also required 
#
# (d) wordnet library: https://wordnet.princeton.edu/
#



# using spacyr-library for lemmatization

library(spacyr)
library(quanteda)
spacy_initialize(entity = FALSE)

fir_tree_lemma <- fir_tree %>%
 mutate(doc_id = paste0("doc", row_number())) %>%
  select(doc_id, everything()) %>%
  spacy_parse() %>%
  anti_join(get_stopwords(), by = c("lemma" = "word")) %>%
  count(lemma, sort = TRUE) %>%
  top_n(20, n) %>%
  ggplot(aes(n, fct_reorder(lemma, n))) +
  geom_col() +
  labs(x = "Frequency", y = NULL)


fir_tree_lemma <- fir_tree %>%
  mutate(doc_id = paste0("doc", row_number())) %>%
  select(doc_id, everything()) %>%
  spacy_parse() %>%
  anti_join(get_stopwords(), by = c("lemma" = "word")) %>%
  count(lemma, sort = TRUE) %>%
  top_n(20, n) %>%
  ggplot(aes(n, fct_reorder(lemma, n))) +
  geom_col() +
  labs(x = "Frequency", y = NULL)


# using textstem-library for lemmatization
library(textstem)
library(koRpus)


