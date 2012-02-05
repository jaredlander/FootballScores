# EDA of Scores
require(ggplot2)
require(plyr)
require(lubridate)
require(reshape2)

# load data
allGames <- read.table("C:/users/Jared/FootballScores/csv/AllGames.csv", sep=",", header=TRUE)

# extract the year
allGames$Year <- year(allGames$Date)
# include game in January and February as part of the previous year's season
allGames$Season <- ifelse(month(allGames$Date) <= 2, allGames$Year - 1, allGames$Year)
allGames$Home <- ifelse(rowMeans(row(allGames)) %% 2 == 0, "Home", "Away")
head(allGames)

# only take 1991 and beyond to make it an even 20 years
allGames <- allGames[allGames$Season >= 1991, ]

# histograms
ggplot(data=allGames, aes(x=Final, fill=Home)) + geom_histogram() + facet_wrap(~Season) + opts(title="Distribution of Scores by Season", axis.text.x=theme_text(angle=90))
ggplot(data=allGames, aes(x=Final%%10, fill=Home)) + geom_histogram() + facet_wrap(~Season) + opts(title="Distribution of Scores by Season", axis.text.x=theme_text(angle=90))
ggplot(data=allGames, aes(x=Final%%10)) + geom_histogram() + facet_wrap(~Season) + opts(title="Distribution of Last Digit of Score by Season")
ggplot(data=allGames, aes(x=Final%%10, fill=Home)) + geom_histogram(position=position_dodge(width=10)) + facet_wrap(~Season) + opts(title="Distribution of Last Digit of Score by Season")


### check just the giants
giants <- allGames[allGames$X1 %in% allGames$X1[grep("Giants", allGames$Team)], ]
giants$Team <- ifelse(giants$Team == "New York Giants", "Giants", "Opponent")
ggplot(data=giants, aes(x=Final, fill=Team)) + geom_histogram() + facet_wrap(~Season) + opts(title="Distribution of Scores by Season", axis.text.x=theme_text(angle=90)) + scale_fill_discrete()

# get last digit
allGames$Last <- allGames$Final %% 10
#gamesWide <- dcast(data=allGames, formula=X1 + Team + Date + Record + Season ~ Home, value_var="Last")
BuildWide <- function(x)
{
    # get the row number for the home and away team
    away <- which(x$Home == "Away")
    home <- which(x$Home == "Home")
    
    # get the away team
    awayTeam <- x$Team[away]
    homeTeam <- x$Team[home]
    
    # get the date
    theDate <- x$Date[1]
    theSeason <- x$Season[1]
    
    # get the records
    awayRecord <- x$Record[away]
    homeRecord <- x$Record[home]
    
    # get scores
    awayFinal <- x$Final[away]
    homeFinal <- x$Final[home]
    awayLast <- x$Last[away]
    homeLast <- x$Last[home]
    
    # build the data.frame
    theData <- data.frame(HomeTeam=homeTeam, AwayTeam=awayTeam, Date=theDate, Season=theSeason, HomeRecord=homeRecord, 
                          AwayRecord=awayRecord, HomeFinal=homeFinal, AwayFinal=awayFinal, HomeLast=homeLast, AwayLast=awayLast)
    
    return(theData)
}
gamesWide <- ddply(allGames, .variables="X1", .fun=BuildWide)
gamesWide$Winner <- ifelse(gamesWide$HomeFinal > gamesWide$AwayFinal, "Home", ifelse(gamesWide$HomeFinal < gamesWide$AwayFinal, "Away", "Tie"))
head(gamesWide)

# This fluctuation plot is by far the most useful
ggfluctuation(table(gamesWide$HomeLast, gamesWide$AwayLast), type="size") + labs(x="Away", y="Home") + opts(title="Distribution of Last Digit of Score")


scoreDist <- ddply(gamesWide, .variables=c("HomeLast", "AwayLast", "Season"), length)
scoreDist <- scoreDist[, c("HomeLast", "AwayLast", "V1", "Season")]
head(scoreDist)
ggfluctuation(scoreDist) + facet_wrap(~Season)
ggstructure(gamesWide[, c("HomeLast", "AwayLast")])


ggplot(data=gamesWide, aes(x=AwayLast, y=HomeLast, colour=Winner)) + geom_point(position="jitter", alpha=1/3) + facet_wrap(~Season) + opts(title="Last Digit by Season")
ggplot(data=gamesWide[gamesWide$Season==2010, ], aes(x=AwayLast, y=HomeLast, colour=Winner)) + geom_point(position="jitter", alpha=1/2) + opts(title="Last Digit (2010)")