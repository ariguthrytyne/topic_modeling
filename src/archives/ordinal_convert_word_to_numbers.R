
temp <- english::ordinal(c(1, 9, 10, 11, 12, 19, 20, 21, 99, 100, 101, 109, 111,
          119, 1000, 1100, 1199, 9999, 10000, 10001), UK = TRUE)

temp <- english::ordinal(1:1000, UK=FALSE)

devtools::install_github("fsingletonthorn/words_to_numbers")

library(wordstonumbers)

example_input <- "I have ten apple and one orange"

words_to_numbers(example)

[1] "I have 10 apple and 1 orange"

example <- c("one", "barbara", "sixty")
example
example_cleaned <- words_to_numbers(example[1])
example_cleaned
