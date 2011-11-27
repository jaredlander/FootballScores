## This code figures out how many games were in each season and then the links to those weeks
require(RCurl)
require(XML)
require(plyr)
tester <- RCurl::getURL(url="http://www.pro-football-reference.com/years/2010/games.htm")

years <- 1940:2010

getBoxURL <- function(theYear)
{
    theURL <- sprintf("http://www.pro-football-reference.com/years/%s/games.htm", theYear)
    thePage <- RCurl::getURL(url=theURL)
    
    boxes <- str_extract_all(string=thePage, pattern="boxscores/[0-9]{9}[a-z]{3}\\.htm")[[1]]
    
    return(data.frame(URL=boxes))
}

system.time(allYears <- adply(years, 1, getBoxURL))
allYears