## fluctuation
scores <- ddply(.data=gamesWide, .variables="Season", function(x) { as.data.frame(table(x$HomeLast, x$AwayLast)) } )
names(scores)[c(2, 3)] <- c("Away", "Home")
scores <- transform(scores, Away = as.factor(Away), Home = as.factor(Home), Freq = Freq)
ceiling <- max(scores$Freq, na.rm = TRUE)
floor <- 0
scores <- transform(scores, Freq = sqrt(pmin(Freq, ceiling)/ceiling), border = ifelse(is.na(Freq), "grey90", ifelse(Freq > ceiling, "grey30", "grey50")))
scores[is.na(scores$Freq), "Freq"] <- 1
scores <- subset(scores, Freq * ceiling >= floor)
nx <- length(levels(scores$Away))
ny <- length(levels(scores$Home))
p <- ggplot(scores, aes_string(x = "Away", y = "Home", height = "Freq", width = "Freq", fill = "border")) + geom_tile(colour = "white") + scale_fill_identity() + opts(aspect.ratio = ny/nx)
p + facet_wrap(~Season) + opts(title="Distribution of Last Digit of Score by Year")