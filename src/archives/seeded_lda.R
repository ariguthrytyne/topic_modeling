library("topicmodels")
data("AssociatedPress", package = "topicmodels")

## We fit 6 topics.
## We specify five seed words for five topics, the sixth topic has no
## seed words.
library("slam")
set.seed(123)
i <- rep(1:5, each = 5)
j <- sample(1:ncol(AssociatedPress), 25)
SeedWeight <- 500 - 0.1
deltaS <- simple_triplet_matrix(i, j, v = rep(SeedWeight, 25),
                                nrow = 6, ncol = ncol(AssociatedPress))
set.seed(1000)
ldaS <- LDA(AssociatedPress, k = 6, method = "Gibbs", seedwords = deltaS, 
            control = list(alpha = 0.1, best = TRUE,
                           verbose = 500, burnin = 500, iter = 100, thin = 100, prefix = character()))

apply(deltaS, 1, function(x) which(x == SeedWeight))
apply(posterior(ldaS)$terms, 1, function(x) order(x, decreasing = TRUE)[1:5])