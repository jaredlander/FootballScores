## Process box scores
require(stringr)
require(plyr)

theInfo <- str_extract_all(string=thePage, pattern="\">[A-Za-z ]*</a> \\([0-9]+-[0-9]-[0-9]\\)</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td>")[[1]]
#theTeams <- str_replace_all(string=theInfo, pattern="\">([A-Za-z ]*)</a> \\([0-9]+-[0-9]-[0-9]\\)</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td><td align=\"right\">[0-9]{1,3}</td>", replace="\\1")
theTeams <- ldply(str_extract_all(string=theInfo, pattern=">[A-Za-z ]*</a>"))
theTeams <- str_replace_all(string=theTeams$V1, pattern=">|(</a>)", replacement="")
theScores <- str_extract_all(string=theInfo, pattern=">[0-9]{1,3}<")
theScores <- str_replace_all(string=theScores, pattern=">|<", replacement="")