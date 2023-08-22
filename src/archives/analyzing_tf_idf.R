# ----------------------------------------------
# ANALYZING WORD AND DOCUMENT FREQUENCY / TF-IDF (Tutorial from https://www.tidytextmining.com/tfidf.html)
# ----------------------------------------------


# Central question (text mining + NLP):
# -------------------------------------
#
#  1- how to quantify what a document is about
#
#  2- tf: a measure of how important a word may be is term frequency (tf)
#
#  3- inverse document frequency (idf): decreases the weight for commonly used words 
#
#  4- tf-idf: measures how important a word is to a document in a 
#             collection (corpus) of documents !!! --> so words that are not important can be removed from the document
#
# ------------------------------------------------------------------------------------


# loading required library
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)

# loading required data
book_words <- austen_books() %>%
 unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
 group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words
#> # A tibble: 40,379 x 4
#>    book              word      n  total
#>    <fct>             <chr> <int>  <int>
#>  1 Mansfield Park    the    6206 160460
#>  2 Mansfield Park    to     5475 160460
#>  3 Mansfield Park    and    5438 160460
#>  4 Emma              to     5239 160996
#>  5 Emma              the    5201 160996
#>  6 Emma              and    4896 160996
#>  7 Mansfield Park    of     4778 160460
#>  8 Pride & Prejudice the    4331 122204
#>  9 Emma              of     4291 160996
#> 10 Pride & Prejudice to     4162 122204
#> # … with 40,369 more rows


# Visualizing words distribution among the books
ggplot(book_words, aes(n/total, fill = book)) +
 geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
   facet_wrap(~book, ncol = 2, scales = "free_y")


# Zipf's law --> the law states that a word appears is inversely proportional to its rank
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()


# examining the Zipf's law
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

# visualizing application of the Zipf's law
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() 

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,rank > 10)

# --------------------------------------------------------------------------------------------------------
#
# Summary: 
#  
#   1- The idea of tf-idf is to find the important words for the content of each document by decreasing
#      the weight for commonly used words and increasing the weight for words that are not used very much
#
#   2- calculating tf-idf attempts to find the words that are important but not too common !!
# --------------------------------------------------------------------------------------------------------


# calculating  the tf_idf for the book_words-data frame
book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)
