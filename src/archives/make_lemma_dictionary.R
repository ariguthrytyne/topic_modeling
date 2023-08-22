x <- c('the dirtier dog has eaten the pies',
       'that shameful pooch is tricky and sneaky',
       "He opened and then reopened the food bag",
       'There are skies of blue and red roses too!'
)
make_lemma_dictionary(x)
## Not run: 
make_lemma_dictionary(x, engine = 'treetagger')

## End(Not run)