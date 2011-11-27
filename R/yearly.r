## This code figures out how many games were in each season and then the links to those weeks
require(RCurl)
require(XML)
require(plyr)

years <- 1940:2010

getBoxURL <- function(theYear)
{
    # build the URL
    theURL <- sprintf("http://www.pro-football-reference.com/years/%s/games.htm", theYear)
    # get the page
    thePage <- RCurl::getURL(url=theURL)
    
    # Find all links to boxscores
    boxes <- str_extract_all(string=thePage, pattern="boxscores/[0-9]{9}[a-z]{3}\\.htm")[[1]]
    
    return(data.frame(Year=theYear, URL=boxes))
}

system.time(allYears <- adply(years, 1, getBoxURL))

write.table(allYears, "C:/users/Jared/FootballScores/csv/YearlyBoxscores.csv", sep=",", row.names=FALSE)