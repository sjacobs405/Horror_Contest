#import data after some basic wrangling in excel
#     including breaking up language into separate columns
#     separate and then drop year from title - redundant
#     separate filming locations into individual columns

library(dplyr)

horror = subset(movies, select = -c(Plot, Cast))
#create new df dropping plot and cast columns
#plot is too subjective
#cast would be interesting to examine if it was already broken out into M/F or 
#     if it included ages, ethnicity, etc. but there is too much data in that
#     column to individually verify for each listed cast member

# Now for factorization and renaming for efficiency
df <- horror %>%
  mutate(title = Title,
         genre = Genres,
         genre2 = X,
         genre3 = X.1,
         genre4 = X.2,
         genre5 = X.3,
         genre6 = X.4,
         genre7 = X.5,
         genre8 = X.6,
         genre9 = X.7,
         release_date = Release.Date,
         release_country = Release.Country,
         movie_rating = Movie.Rating,
         review_rating = Review.Rating,
         runtime = Movie.Run.Time,
         language = Language,
         language2 = X.8,
         language3 = X.9,
         language4 = X.10,
         language5 = X.11,
         language6 = X.12,
         language7 = X.13,
         language8 = X.14,
         location = as.factor(`X.15`),
         location2 = as.factor(`X.16`),
         location3 = as.factor(`X.17`),
         location4 = as.factor(`X.18`),
         location5 = as.factor(`X.19`),
         location6 = as.factor(`X.20`),
         budget = Budget) %>%
  select(title, genre, genre2, genre3, genre4, genre5, genre6,genre7, genre8,
         genre9, release_date, release_country, movie_rating, 
         review_rating, runtime, language, language2, language3,
         language4, language5, language6, language7, language8, location, location2,
         location3, location4, location5, location6, budget)

str(df)

#class(df$movie_rating)
#levels(df$movie_rating)
unique(df$movie_rating)
#find levels and then recode to combine "not rated" & "unrated"


df_clean <- df %>%
  mutate(movie_rating = fct_recode(movie_rating, "NOT RATED" = "UNRATED"))
levels(df_clean$movie_rating)
count(df_clean$movie_rating)
#movie_rating is missing a significant number of entries - approx half are blank

unique(df$release_country)
factor(df_clean$release_country)
count(df_clean$release_country)
#we see here that there are 72 levels in release_country with USA being the 
#the largest group by far. 

count(df_clean$genre)
#this shows that the primary genre - first listed - for the data
#'horror' is the top listing for more than half with 'comedy', 'drama' and 
#'action' coming in next

count(df_clean$genre2)
#Here we can see all but 1060 entries have at least 2 genre classifications
#I think that we should look at primary genre classification and subsequent
#genre classification as it relates to both rating and review

count(df_clean$genre3)
count(df_clean$genre4)
count(df_clean$genre5)
count(df_clean$genre6)
count(df_clean$genre7)
count(df_clean$genre8)
count(df_clean$genre9)
#just verifying that we have this fully broken out

count(df_clean$review_rating)
#it appears that this variable is on a 1-10 rating system 
#252 out of 3,328 are missing this data
####unique(horror$Review.Rating)
####sum(is.na(horror$Review.Rating))

count(df_clean$language)
#this shows that the primary language - first listed - for the data 
#'english' is most common by far

count(df_clean$language2) 
count(df_clean$language3)
count(df_clean$language4)
count(df_clean$language5)
count(df_clean$language6)
count(df_clean$language7)
count(df_clean$language8)
#verifying the levels of subsequent languages
#I think that we can use the first 3 columns and merge the rest 

count(df_clean$location)
#from the looks of this list, I think we should disregard this variable for now
#It is wildly inconsistent with the type of location identified and will require
#a lot of cleaning to be useful

#did some research on currency conversion and found fixerapi to load in
#I want to see if we can convert all budget entries to USD
      #install.packages("fixerapi")
      #library(fixerapi)
      # fixer_symbols()

      #x <- fixer_convert(from = "", to = "USD", amount = 1)

#stopped here to reassess conversion process. 
#maybe skip and move on to cleaning "run time" for analysis

######Code citation from https://www.kaggle.com/bvarkoly/horror-movies-01-data-cleaning

convertMinsToNumber <- Vectorize(function(mins_str) {
  as.numeric(unlist(str_split(mins_str, "\\s"))[1])
})

df_clean <- df_clean %>%
  mutate(runtime_in_min = convertMinsToNumber(runtime))

#####Code citation ends

str(df_clean)
#verify structure

#examine the data now with some exploration
#take a look at distributions
d <- ggplot(df_clean, aes(x = runtime_in_min))
d+geom_histogram(binwidth = 10, fill = "salmon", color = "orangered4")

horror_hist <- ggplot(df_clean, aes(x= runtime_in_min))
horror_hist + geom_histogram(binwidth = 10, fill = "salmon", color = "orangered4")
 
d2 <- ggplot(df_clean, aes(x = "", y = runtime_in_min))
d2 + geom_boxplot() + xlab("")
summary(df_clean$runtime_in_min)
ggplot(df_clean, aes(sample = runtime_in_min)) + geom_qq()
#####runtimes seem to have roughly normal distribution


####
#Trying to run analysis on runtime as it pertains to everything else
#

df_clean$runtime_in_min <- as.integer(df_clean$runtime_in_min)
df_clean$review_rating <- as.integer(df_clean$runtime_in_min)
#convert to integer

mean(df_clean$runtime_in_min)
summarize(df_clean, ave.runtime = mean(runtime_in_min))
df_clean %>% group_by(genre) %>% summarize(ave.runtime = mean(ave.runtime))

df_clean %>%
  group_by(review_rating) %>%
  summarise(mean_run = mean(runtime_in_min))







d2 <- ggplot(df_clean, aes(x = "", y = review_rating))
d2 + geom_boxplot() + xlab("")
summary(df_clean$review_rating)
ggplot(df_clean, aes(sample = review_rating)) + geom_qq()


ggplot(df_clean, aes(movie_rating)) + geom_bar()
barchart(df_clean$movie_rating, fill = "salmon", color ="orangered4")




as.Date(df_clean$release_date, "%Y-%m-%j")
#convert Release.Date to a number











sapply(df, function(x) length(unique(x)))
#looking at the number of unique variables in the data set 







#horror$Movie.RatingR <- NA

#horror$Movie.RatingR[horror$Movie.Rating=='""'] <- 0
#horror$Movie.RatingR[horror$Movie.Rating=='NOT RATED'] <- 1
#horror$Movie.RatingR[horror$Movie.Rating=='PG-13'] <- 2
#horror$Movie.RatingR[horror$Movie.Rating=='UNRATED'] <- 3
#horror$Movie.RatingR[horror$Movie.Rating=='R'] <- 4
#horror$Movie.RatingR[horror$Movie.Rating=='X'] <- 5
#horror$Movie.RatingR[horror$Movie.Rating=='TV-MA'] <- 6
#horror$Movie.RatingR[horror$Movie.Rating=='TV-14'] <- 7
#horror$Movie.RatingR[horror$Movie.Rating=='PG'] <- 8
#horror$Movie.RatingR[horror$Movie.Rating=='E'] <- 9
#horror$Movie.RatingR[horror$Movie.Rating=='TV-PG'] <- 10
#horror$Movie.RatingR[horror$Movie.Rating=='NC-17'] <- 11





as.Date(horror$Release.Date, "%Y-%m-%j")
#convert Release.Date to a number



