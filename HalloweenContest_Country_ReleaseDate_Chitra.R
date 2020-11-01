install.packages("dplyr")
library("dplyr")
library("ggplot2")

# Narrowing down the columns that will be used
Movies1 <- IMDB.Horror.movies %>% select(Title, Genres, Release.Date, Release.Country, Review.Rating)

# Arranging Review.Rating in descending order
Movies2 <- Movies1 %>% arrange(desc(Review.Rating))

# Filter top 4 countries 
MoviesByCountries <- Movies1 %>% filter(Release.Country %in% c("USA", "UK", "India", "Japan"))

Movies_Country_Rating <- MoviesByCountries %>% filter(Review.Rating >= 8)

Movies_Country_Rating$date <- format(as.Date(Movies_Country_Rating$Release.Date, format="%j-%b-%y"),"%y-%b-%j")
  #Not working -> need help!

USA <- Movies_Country_Rating %>% filter(Release.Country == "USA")

UK <- Movies_Country_Rating %>% filter(Release.Country == "UK")

India <- Movies_Country_Rating %>% filter(Release.Country == "India")

Japan <- Movies_Country_Rating %>% filter(Release.Country == "Japan")

## Export in excel 
install.packages("writexl")
library(writexl)

write_xlsx(x = Movies_Country_Rating, path = "Movies_Country_Rating.csv", col_names = TRUE)
write_xlsx(x = Movies_Country_Rating, path = "USA.csv", col_names = TRUE)
write_xlsx(x = Movies_Country_Rating, path = "UK.csv", col_names = TRUE)
write_xlsx(x = Movies_Country_Rating, path = "India.csv", col_names = TRUE)
write_xlsx(x = Movies_Country_Rating, path = "Japan.csv", col_names = TRUE)


