# Of the top 4 countries, where is it best to release a horror movie?

# Top 4 countries based on volume:
# USA: 2092, UK: 218, India: 107, Japan 98

CountryCounts <-(`IMDB.Horror.movies.(version.1)` %>% group_by(Release.Country) %>% summarise(count = n()));

TotalCounts <- filter(CountryCounts, Release.Country %in% c("USA", "UK", "India", "Japan"));

rename(TotalCounts, "Total Movie Count" ='count');


library(dplyr)


#what is the mean review rating for the entire population?
summary(`IMDB.Horror.movies.(version.1)`$Review.Rating)
#> summary(`IMDB.Horror.movies.(version.1)`$Review.Rating)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   4.000   5.000   5.077   6.100   9.800     252

#what is the mean review rating by Top4 country?

Top4CountriesRR <- filter(`IMDB.Horror.movies.(version.1)`, Release.Country %in% c("USA", "UK", "India", "Japan"));

summary(Top4CountriesRR$Review.Rating);

ggplot(Top4CountriesRR, aes(x=Release.Country, y=Review.Rating)) + geom_boxplot(aes(group=Release.Country));

#Separate by Country:
USARR <- filter(Top4CountriesRR, Release.Country == "USA")
UKRR <- filter(Top4CountriesRR, Release.Country == "UK")
IndiaRR <- filter(Top4CountriesRR, Release.Country == "India")
JapanRR <- filter(Top4CountriesRR, Release.Country =="Japan");


#Determine if the mean of 5.08 (mean of the entire population) is consistent across with Top4 countries using t Test:

tTestUSARR <- t.test(USARR$Review.Rating, mu = 5.08)
print(tTestUSARR)

tTestUKARR <- t.test(UKRR$Review.Rating, mu = 5.08)
print(tTestUKARR)

tTestIndiaRR <- t.test(IndiaRR$Review.Rating, mu = 5.08)
print(tTestIndiaRR)

tTestJapanRR <- t.test(JapanRR$Review.Rating, mu = 5.08)
print(tTestJapanRR);

summary(USARR)
summary(UKRR)
summary(IndiaRR)
summary(JapanRR)

#Good to Note:
#USA: 
#-Any Rating above 9 or below 1 is an outlier for the USA because IQR= 2*1.5 =3
#-The value below which 3 quarters of the values lie: 6.00
#-Mean 5.02


#UK: Any rating above 8.9 or below .9 is an outlier for UK because IQR =2*1.5 =3
#-The value below which 3 quarters of the values lie: 5.88
#-Mean 4.97

#India: Any Rating above 11 or below -.2 No outliers exist because 2.8*1.5 = 4.2
#-The value below which 3 quarters of the values lie: 6.80
#-Mean 5.48

#Japan: Any rating above 8.4 or below  2.8 is an outlier because IQR of 1.4*1.5=2.1 
#-The value below which 3 quarters of the values lie: 6.3
#-Mean 5.51

#Comparison of percentage in low, med, high in the Top4Countries

#Low Review Ratings:
Top4CountriesLow <- filter(`IMDB.Horror.movies.(version.1)`, Release.Country %in% c("USA", "UK", "India", "Japan"), Review.Rating <=4);

select(Top4CountriesLow, Release.Country, Review.Rating);

lowRatingCounts <- Top4CountriesLow %>% group_by(Release.Country) %>% summarise(count = n());

rename(lowRatingCounts, "Count of <4" ='count');

JoinCountsLow <- lowRatingCounts %>% full_join(TotalCounts, by="Release.Country");

PercentageLow <-mutate(JoinCountsLow, "% of total" = (count.x/count.y)*100);



#Mid Review Ratings: 

Top4CountriesMid <- filter(`IMDB.Horror.movies.(version.1)`, Release.Country %in% c("USA", "UK", "India", "Japan"), Review.Rating >4, Review.Rating <8)


select(Top4CountriesMid, Release.Country, Review.Rating)

MidRatingCounts <- Top4CountriesMid %>% group_by(Release.Country) %>% summarise(count = n());

rename(MidRatingCounts, "Count of Mid" ='count');

JoinCountsMid <- MidRatingCounts %>% full_join(TotalCounts, by="Release.Country");

PercentageMid <-mutate(JoinCountsMid, "% of total" = (count.x/count.y)*100)


#High Review Ratings: 
Top4Countries <- filter(`IMDB.Horror.movies.(version.1)`, Release.Country %in% c("USA", "UK", "India", "Japan"), Review.Rating >=8);

select(Top4Countries, Release.Country, Review.Rating);

RatingCounts <- Top4Countries %>% group_by(Release.Country) %>% summarise(count = n());

rename(RatingCounts, "Count of 8+" ='count');

df1 = TotalCounts(Release.Country=c("USA", "UK", "India", "Japan"))

CompareCounts <- merge(RatingCounts, TotalCounts, by="Release.Country");

JoinCounts <- RatingCounts %>% full_join(TotalCounts, by="Release.Country");

PercentageTop <-mutate(JoinCounts, "% of total" = (count.x/count.y)*100)
