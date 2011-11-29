# EDA of Scores
require(ggplot2)
require(lubridate)

# load data
allGames <- read.table("C:/users/Jared/FootballScores/csv/AllGames.csv", sep=",", header=TRUE)

# extract the year
allGames$Year <- year(allGames$Date)
# include game in January and February as part of the previous year's season
allGames$Season <- ifelse(month(allGames$Date) <= 2, allGames$Year - 1, allGames$Year)
head(allGames)

# only take 1991 and beyond to make it an even 20 years
allGames <- allGames[allGames$Season >= 1991, ]

# histograms
ggplot(data=allGames, aes(x=Final)) + geom_histogram() + facet_wrap(~Season) + opts(title="Distribution of Scores by Season")
ggplot(data=allGames, aes(x=Final%%10)) + geom_histogram() + facet_wrap(~Season) + opts(title="Distribution of Last Digit of Score by Season")