## This query is for review rating greater than or equal to 8.

# Subsetting Data 

keeps <- c("Title","Genres","Review Rating")
IMDB_Horror_movies2 <- IMDB_Horror_movies[keeps]

#Dropping NA values

moviegenres <- na.omit(IMDB_Horror_movies2)
View(moviegenres)
moviegenres1 <- as.data.frame(moviegenres)

genreorder <- moviegenres[order(moviegenres1$Genres),]

#movies with rating greater than 8

Movie_Genre_Rating <- moviegenres1%>%filter(`Review Rating`>=8)

Movie_Genre_Rating2 <- moviegenres1%>%filter(`Review Rating`>=8.5)

Movie_Genre_Rating3 <- moviegenres1%>%filter(`Review Rating`>=9)

Movie_Genre_Rating4 <- moviegenres1%>%filter(`Review Rating`>=9.5)

library(xlsx)
write.csv(genreorder,"c:/genreorder.csv") 
write.csv(Movie_Genre_Rating, "c:/Movie_Genre_Rating.csv")
write.csv(Movie_Genre_Rating2, "c:/Movie_Genre_Rating2.csv")
write.csv(Movie_Genre_Rating3, "c:/Movie_Genre_Rating3.csv")
write.csv(Movie_Genre_Rating4, "c:/Movie_Genre_Rating4.csv")


