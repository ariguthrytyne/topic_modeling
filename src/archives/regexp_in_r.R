# Intermediate regular expression using R (datacamp)
movie_titles <- c("Karate Kid",                          "The Twilight Saga: Eclispe"  ,       
"Knight & Day",                        "Shrek Forever After 3D"   ,          
"Marmaduke.",                          "Street Dance"    ,                   
"Predators",                           "StreetDance 3D"     ,                
"Robin Hood" ,                         "Micmacs A Tire-Larigot" ,            
"50 Shades of Grey",                   "Sex And the City 2"  ,               
 "Inception",                           "The Dark Knight"  ,                  
"300" ,                                "Toy Story 3 In Disney Digital 3D" ,  
"50 Shades of Gray",                   "Italien, Le" ,                       
 "Tournee"  ,                           "The A-Team" ,                        
"El Secreto De Sus Ojos",              "Kiss & Kill"   ,                     
"The Road"  ,                          "Cosa Voglio Di Piu"     ,            
"Nur für dich"  ,               "Prince Of Persia: The Sands Of Time",
 "Saw 4"   ,                            "Saw 5"  ,                            
"Saw 6",                               "21 Grams") 



# ---------
# AUFGABE 1
# ---------

# Familiarize yourself with the vector by printing it
movie_titles

# List all movies that start with "The"
movie_titles[str_detect(
  movie_titles,
  pattern = "^The"
)]

# List all movies that end with "3D"
movie_titles[str_detect(
  movie_titles,
  pattern = "3D$"
)]



# ---------
# AUFGABE 2
# ---------

# Here's an example pattern that will find the movie Saw 4
str_match(movie_titles, pattern = "Saw 4")

# Match all sequels of the movie "Saw"
str_match(movie_titles, pattern = "Saw .")

# Match the letter K and three arbitrary characters
str_match(movie_titles, pattern = "^K...")

# Detect whether the movie titles end with a full stop
str_detect(movie_titles, pattern = "\\.$")



# ---------
# AUFGABE 3
# ---------

# List all movies that end with a space and a digit
movie_titles[str_detect(movie_titles,
                        pattern = "\\s\\d$"
)]

# List all movies that contain "Grey" or "Gray"
movie_titles[str_detect(movie_titles,
                        pattern = "Gr[ae]y"
)]

# List all movies with strange characters (no word or space)
movie_titles[str_detect(movie_titles,
                        pattern = "[^\\w\\s]"
)]


# ---------
# AUFGABE 4
# ---------

# This lists all movies with two or more digits in a row
movie_titles[str_detect(
  movie_titles,
  pattern = "\\d{2,}"
)]

# List just the first words of every movie title
str_match(movie_titles, pattern = "\\w+")

# Match everything that comes before "Knight"
str_match(movie_titles, pattern = ".*Knight")


# ---------
# AUFGABE 5
# ---------

# Match both Screen and Screens by making the last "s" optional
str_match(lines, pattern = "Screens?")

# Match a random amount of arbitrary characters, followed by a comma
str_match(lines, pattern = ".*,")

# Match the same pattern followed by a comma, but the "lazy" way
str_match(lines, pattern = ".*?,")