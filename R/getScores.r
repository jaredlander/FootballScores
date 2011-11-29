## Process box scores
require(stringr)
require(plyr)

# read the prebuilt csv
allYears <- read.table("C:/users/Jared/FootballScores/csv/YearlyBoxscores.csv", sep=",", header=TRUE)
# we only want years since 1960
allYears <- allYears[allYears$Year >= 1990, c("Year", "URL")]

# shorten the URL b/c we don't need "boxscores/" repeatedly
gameFiles <- sub("boxscores/", "", allYears$URL)
gameFiles <- sprintf("C:/users/Jared/FootballScores/objects/%s.RData", gameFiles)

# put all of the boxscores into data.frames in a list
allGameInfo <- alply(gameFiles, 1, function(x) { load(x); print(x);BuildGameInfo(thePage) })
# save as an RData file
save(allGameInfo, file="C:/users/Jared/FootballScores/objects/AllGames.RData")

# convert to a data.frame, probably could do this with the step above
allGames <- ldply(allGameInfo)

# write to a csv
write.table(allGames, "C:/users/Jared/FootballScores/csv/AllGames.csv", sep=",", row.names=FALSE)

BuildGameInfo <- function(gamePage)
{
    # extract boxscore info
    theInfo <- str_extract_all(string=gamePage, pattern="\">[A-Za-z0-9. ]*</a> \\([0-9]+-[0-9]+-[0-9]+\\)</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td>(<td align=\"right\">[0-9]{1,3}</td>)*")[[1]]
    
    # get the teams
    theTeams <- unlist(str_extract_all(string=theInfo, pattern=">[A-Za-z0-9. ]*</a>"))
    # clean it up
    theTeams <- str_replace_all(string=theTeams, pattern=">|(</a>)", replacement="")
    
    # get the teams' records
    theRecords <- unlist(str_extract_all(string=theInfo, pattern="\\([0-9]+-[0-9]+-[0-9]+\\)"))    
    
    # get the scores by quarter and convert to numeric
    theScores <- laply(str_extract_all(string=theInfo, pattern=">[0-9]{1,3}<"), rbind)
    theScores <- matrix(as.numeric(str_replace_all(string=theScores, pattern=">|<", replacement="")), nrow=2, byrow=FALSE)

    # get the date
    theDate <- as.Date(str_extract(string=gamePage, pattern="[A-Za-z]+ [0-9]{1,2}, [0-9]{4}"), format="%B %d, %Y")
    
    # build a data.frame to hold all of the information
    empties <- rep(NA, 2)
    
    gameInfo <- data.frame(Team=theTeams, Date=theDate, Record=theRecords, First=empties, Second=empties, Third=empties, Fourth=empties, OT=empties, Final=empties)
    
    ## if there were 6 score columns, that means there was overtim
    if(NCOL(theScores) == 6)
    {
        gameInfo[, c("First", "Second", "Third", "Fourth", "OT", "Final")] <- theScores
    }else
    {
        gameInfo[, c("First", "Second", "Third", "Fourth", "Final")] <- theScores
    }
    
    rm(gamePage, empties, theTeams, theScores, theRecords); gc()         # housekeeping
    
    return(gameInfo)
}

