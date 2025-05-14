# Elo ratings

source('code/setupElo.R')


getTeamElo <- function(abbr){
    games <- allgames[which(allgames$away_team_abbr == abbr | allgames$home_team_abbr == abbr), ]
    home <- games$home_team_abbr == abbr
    eloPre <- ifelse(home, games$home_elo_pre, games$away_elo_pre)
    eloPost <- ifelse(home, games$home_elo_post, games$away_elo_post)
    # check
    eloPreTrim <- eloPre[!is.na(eloPre)]
    eloPostTrim <- eloPost[!is.na(eloPost)]
    stopifnot(all(eloPreTrim[-1] == eloPostTrim[-length(eloPostTrim)] |
                      eloPreTrim[-1] == (eloPostTrim[-length(eloPostTrim)] + 1500) / 2))
    # add initial rating
    elo <- c(eloPre[1], eloPost)
    date <- c(games$Date[1]-1, games$Date)
    season <- c(games$season[1], games$season)
    return(data.frame(Date = date, elo = elo, season = season))
}


abbrs <- unique(c(allgames$home_team_abbr, allgames$away_team_abbr))
elos <- lapply(abbrs, function(x){
    elo <- getTeamElo(x)
    return(elo[!is.na(elo$elo),])
})
names(elos) <- abbrs

tmp <- do.call(rbind, elos)
maxElo <- max(tmp$elo)
minElo <- min(tmp$elo)
plot(range(tmp$Date), c(minElo,maxElo), col='white')
for(elo in elos){
    lines(elo, col='grey80')
}
lines(elos[["LAS"]], lwd=2, col = 2)


png(filename = '~/Desktop/nyl.png', width=1750, height = 1000)
layout(matrix(1:28,nrow=4,byrow = TRUE))
abbr <- "NYL"
for(year in 1997:2024){
    plot(range(tmp$Date[which(tmp$season == year)]), c(minElo,maxElo), col='white', main=year, xlab='',ylab='')
    abline(h=1500)
    for(elo in elos){
        lines(elo[which(elo$season == year),], col='grey60', lwd=3)
    }
    elo <- elos[[abbr]]
    elo <- elo[which(elo$season == year),]
    lines(elo, col=2, lwd=5)
    # check if team won championship
    lastgame <- allgames[which(allgames$season == year), ]
    lastgame <- lastgame[which.max(lastgame$Date), ]
    champ <- ifelse(lastgame$PTShome > lastgame$PTSvis, lastgame$home_team_abbr, lastgame$away_team_abbr)
    if(abbr == champ){
        text(elo$Date[nrow(elo)], elo$elo[nrow(elo)], label = '*', col='gold2', font=2, cex=2)
    }
}
layout(1)
dev.off()


# check win prob accuracy
homewin <- allgames$PTShome > allgames$PTSvis
cuts <- quantile(allgames$homeWinProb, probs = seq(0,1, .05))
cuts[length(cuts)] <- 1
predProb <- sapply(2:length(cuts), function(ii){
    mean(allgames$homeWinProb[allgames$homeWinProb >= cuts[ii-1] & allgames$homeWinProb < cuts[ii]])
})
actualProb <- sapply(2:length(cuts), function(ii){
    mean(homewin[allgames$homeWinProb >= cuts[ii-1] & allgames$homeWinProb < cuts[ii]])
})
plot(predProb, actualProb, asp=1, xlim=0:1,ylim=0:1)
abline(0,1)

# with model
mod <- glm(homewin ~ allgames$homeWinProb, family='binomial')
mod$aic
# current: 7880.546 (smaller = better)


# how much is home court advantage worth?
# exclude playoffs because "home" is not independent of team quality
ptsdiff <- (allgames$PTShome - allgames$PTSvis)[which(!allgames$playoffs)]
hist(ptsdiff, breaks=50)
abline(v=mean(ptsdiff))

# home court advantage fell off ~2020 (2020 was bubble season)
pdbs <- sapply(1997:2024, function(year){
    ptsdiff <- (allgames$PTShome - allgames$PTSvis)[which(!allgames$playoffs & allgames$season == year)]
    return(mean(ptsdiff))
})
plot(1997:2024, pdbs, type='b')
abline(h=0)



# league average Elo
activeTeams <- sapply(1997:2024, function(szn){
    curryear <- allgames[which(allgames$season == szn), ]
    return(unique(c(curryear$away_team_abbr, curryear$home_team_abbr)))
})
names(activeTeams) <- 1997:2024
AvgElo <- sapply(1:nrow(allgames), function(ii){
    game <- allgames[ii,]
    elos <- sapply(activeTeams[[as.character(game$season)]], prevElo, date = game$Date, season = game$season)
    return(mean(elos))
})
plot(allgames$Date, AvgElo,type='b')



