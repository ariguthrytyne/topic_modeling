install.packages("sos")
library(sos)
findFn("str_replace")

d<-"30,3"
class(d)

length(strsplit(d, ",")[[1]])
