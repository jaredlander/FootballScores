## This code figures out how many games were in each season and then the links to those weeks
require(RCurl)
require(XML)
tester <- RCurl::getURL(url="http://www.pro-football-reference.com/years/2002/games.htm")

years <- 
boxes <- str_extract_all(string=tester, pattern="boxscores/[0-9]{9}[a-z]{3}\\.htm")[[1]]
length(boxes)