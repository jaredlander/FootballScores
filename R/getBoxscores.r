### Get boxscores
require(RCurl)
# read the prebuilt csv
allYears <- read.table("C:/users/Jared/FootballScores/csv/YearlyBoxscores.csv", sep=",", header=TRUE)
# we only want years since 1960
allYears <- allYears[allYears$Year >= 1990, c("Year", "URL")]
# shorten the URL b/c we don't need "boxscores/" repeatedly
allYears$URL <- sub("boxscores/", "", allYears$URL)


system.time({for(a in allYears$URL)
{
    # build URL
    theURL <- paste("http://www.pro-football-reference.com/boxscores", a, sep="/")
    # get boxscore page
    thePage <- RCurl::getURL(url=theURL)
    # save to an RData file
    save(thePage, file=sprintf("C:/users/Jared/FootballScores/objects/%s.RData", a))
}
})