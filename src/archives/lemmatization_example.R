x <- c(
  'the dirtier dog has eaten the pies',
  'that shameful pooch is tricky and sneaky',
  "He opened and then reopened the food bag",
  'There are skies of blue and red roses too!',
  NA,
  "The doggies, well they aren't joyfully running.",
  "The daddies are coming over...",
  "This is 34.546 above")

y <- textstem::lemmatize_strings(x)


# using HUnspell dictionnary
lemma_dictionary <- make_lemma_dictionary(x, engine = 'hunspell')
z <- lemmatize_strings(x, dictionary = lemma_dictionary)


library(dplyr)
library(dplyr)
data(presidential_debates_2012)
View(presidential_debates_2012)


presidential_debates_2012$dialogue %>%
  lemmatize_strings() %>%
  head()
