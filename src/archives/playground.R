# # comparing the xml structures of the documents
# a <- xml_structure(read_html(url_global1))
# a <- fulltext::pdfx(file = html_r1, what = "parsed")
a = xmlParse(read_html(url_global1), asText = TRUE)
b = xmlParse(url_global2, asText = TRUE)
# temp <- compareXMLDocs(url_global1, url_global2)
# a <- convert_pdf(input_file, output_file = NULL, format = "xml",message = TRUE, api_key = Sys.getenv("pdftable_api"))



# # # get extractr packages
# library(remotes)
# remotes::install_github("sckott/extractr", force=TRUE)
# library(extractr)
# 
# #parse
# temp <- extractr::pdfx(file="./data/A_RES_55_6-EN.pdf", what="parsed")
# 
# path <- system.file("examples", "example1.pdf", package = "extractr")

tt = 
  '<x>
     <a>text</a>
     <b foo="1"/>
     <c bar="me">
        <d>a phrase</d>
     </c>
  </x>'

a = xmlParse(tt, asText = TRUE)
b = xmlParse(tt, asText = TRUE)

d = getNodeSet(b, "//d")[[1]]
xmlName(d) = "bob"
addSibling(xmlParent(d), newXMLNode("c"))

# compare a and b
compareXMLDocs(a, b)


# text analysis
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text
#> [1] "Because I could not stop for Death -"  
#> [2] "He kindly stopped for me -"            
#> [3] "The Carriage held but just Ourselves -"
#> [4] "and Immortality"

library(dplyr)
text_df <- tibble(line = 1:4, text = text)

text_df
#> # A tibble: 4 x 2
#>    line text                                  
#>   <int> <chr>                                 
#> 1     1 Because I could not stop for Death -  
#> 2     2 He kindly stopped for me -            
#> 3     3 The Carriage held but just Ourselves -
#> 4     4 and Immortality


library(tidytext)

text_df %>%
  unnest_tokens(sentence, text)
#> # A tibble: 20 x 2
#>     line word   
#>    <int> <chr>  
#>  1     1 because
#>  2     1 i      
#>  3     1 could  
#>  4     1 not    
#>  5     1 stop   
#>  6     1 for    
#>  7     1 death  
#>  8     2 he     
#>  9     2 kindly 
#> 10     2 stopped
#> # … with 10 more rows


# distance between strings
library(RecordLinkage)

levenshteinSim("apple", "apple")
jarowinkler("apple", "apple")

levenshteinSim("apple", "aaple")
jarowinkler("apple", "aaple")

levenshteinSim("apple", "appled")
jarowinkler("apple", "appled")

levenshteinSim("appl", "apple")
jarowinkler("appl", "apple")


test <- RecordLinkage::levenshteinSim("the apple is blue and will save hour", "the apple is red")

