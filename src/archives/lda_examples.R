# ============================================================================================================
#
# Every document is a mixture of topics: 
# -------------------------------------
# We imagine that each document may contain words from several topics
# in particular proportions. For example, in a two-topic model we could say 
# “Document 1 is 90% topic A and 10% topic B, while Document 2 is 30% topic A and 70% topic B.”
#
#
# Every topic is a mixture of words. 
# ----------------------------------
# For example, we could imagine a two-topic model of American news, 
# with one topic for “politics” and one for “entertainment.” The most common words in the politics topic 
# might be “President”, “Congress”, and “government”, while the entertainment topic may be made up of words 
# such as “movies”, “television”, and “actor”. Importantly, words can be shared between topics; a word like 
# “budget” might appear in both equally.
# 
# ============================================================================================================


# loading required library
library(topicmodels)
library(tictoc)


### --------------------- EXAMPLE 1: Associated Press Data  -------------------------- ###


# loading required document term matrix
data("AssociatedPress")
AssociatedPress  # dtm: document-term-matrix
#> <<DocumentTermMatrix (documents: 2246, terms: 10473)>>
#> Non-/sparse entries: 302031/23220327
#> Sparsity           : 99%
#> Maximal term length: 18
#> Weighting          : term frequency (tf)

# set a seed so that the output of the model is predictable
tic()
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
toc()
# 23.111 sec elapsed


ap_lda
#> A LDA_VEM topic model with 2 topics.
# str(ap_lda)
#
# The model contains two important matrices:
#
#   1- beta (matrix of probability that a given term was generated by a specific topic
# 
#   2- gamma (matrix of probability that a term belong to a specific topic)

# Fitting the model was the “easy part”: the rest of the analysis will involve exploring and 
# interpreting the model using tidying functions from the tidytext package.


# word-topics probabilities
library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# finding terms that are most commons within each topic
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
 group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
   ungroup() %>%
    arrange(topic, -beta)

ap_top_terms %>%
 mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
   geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
     scale_y_reordered()

# Terms having the biggest difference between topics
library(tidyr)

beta_wide <- ap_topics %>%
 mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
   filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))

beta_wide
#> # A tibble: 198 x 4
#>    term              topic1      topic2 log_ratio
#>    <chr>              <dbl>       <dbl>     <dbl>
#>  1 administration 0.000431  0.00138         1.68 
#>  2 ago            0.00107   0.000842       -0.339
#>  3 agreement      0.000671  0.00104         0.630
#>  4 aid            0.0000476 0.00105         4.46 
#>  5 air            0.00214   0.000297       -2.85 
#>  6 american       0.00203   0.00168        -0.270
#>  7 analysts       0.00109   0.000000578   -10.9  
#>  8 area           0.00137   0.000231       -2.57 
#>  9 army           0.000262  0.00105         2.00 
#> 10 asked          0.000189  0.00156         3.05 
#> # … with 188 more rows



# document-topics probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents
#> # A tibble: 4,492 x 3
#>    document topic    gamma
#>       <int> <int>    <dbl>
#>  1        1     1 0.248   
#>  2        2     1 0.362   
#>  3        3     1 0.527   
#>  4        4     1 0.357   
#>  5        5     1 0.181   
#>  6        6     1 0.000588
#>  7        7     1 0.773   
#>  8        8     1 0.00445 
#>  9        9     1 0.967   
#> 10       10     1 0.147   
#> # … with 4,482 more rows

# tidying the document-term-matrix
dtm_values <- tidy(AssociatedPress) %>%
 filter(document == 6) %>%
  arrange(desc(count))


#> # A tibble: 287 x 3
#>    document term           count
#>       <int> <chr>          <dbl>
#>  1        6 noriega           16
#>  2        6 panama            12
#>  3        6 jackson            6
#>  4        6 powell             6
#>  5        6 administration     5
#>  6        6 economic           5
#>  7        6 general            5
#>  8        6 i                  5
#>  9        6 panamanian         5
#> 10        6 american           4
#> # … with 277 more rows




### --------------------- EXAMPLE 2: The great library heist -------------------------- ###

# Four books were mixed  ---> Question: identify 
# Suppose a vandal has broken into your study and torn apart four of your books:
#   
#   1- Great Expectations by Charles Dickens
# 
#   2- The War of the Worlds by H.G. Wells
# 
#   3- Twenty Thousand Leagues Under the Sea by Jules Verne
#   
#   4- Pride and Prejudice by Jane Austen

# loading book titles
titles <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds",
            "Pride and Prejudice", 
            "Great Expectations")

# loading texts of the four books
library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")
