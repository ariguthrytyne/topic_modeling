# example coming from:
#
#   (1) https://datamathstat.wordpress.com/2019/10/25/text-preprocessing-for-nlp-and-machine-learning-using-r/
#
#   (2) https://www.thelearningpoint.net/computer-science/r-for-data-text-preprocessing-in-r
#


#  loading required package
library(tm)
library(NLP)

# loading test data
text_corpus <- (VectorSource(data))
text_corpus <- Corpus(text_corpus)


docs <- c("This is a text.", "This another one.")
vs <- VectorSource(docs)
inspect(VCorpus(vs))


spacy_initialize()
# See Chap 5.1 of the NLTK book, http://www.nltk.org/book/ch05.html
txt <- "And now for something completely different."
spacy_parse(txt)
spacy_parse(txt, pos = TRUE, tag = TRUE)
spacy_parse(txt, dependency = TRUE)
