# Of the top 4 countries, where is it best to release a horror movie?

# Top 4 countries based on volume:
# USA: 2092, UK: 218, India: 107, Japan 98

CountryCounts <-(`IMDB.Horror.movies.(version.1)` %>% group_by(Release.Country) %>% summarise(count = n()));

TotalCounts <- filter(CountryCounts, Release.Country %in% c("USA", "UK", "India", "Japan"));

rename(TotalCounts, "Total Movie Count" ='count');


library(dplyr)

Top4Countries <- filter(`IMDB.Horror.movies.(version.1)`, Release.Country %in% c("USA", "UK", "India", "Japan"), Review.Rating >=8);

select(Top4Countries, Release.Country, Review.Rating);

RatingCounts <- Top4Countries %>% group_by(Release.Country) %>% summarise(count = n());

rename(RatingCounts, "Count of 8+" ='count');

df1 = TotalCounts(Release.Country=c("USA", "UK", "India", "Japan"))

CompareCounts <- merge(RatingCounts, TotalCounts, by="Release.Country");
                       
JoinCounts <- RatingCounts %>% full_join(TotalCounts, by="Release.Country");

PercentageTop <-mutate(JoinCounts, "% of total" = (count.x/count.y)*100)





