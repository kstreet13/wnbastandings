# Elo ratings

# combine all games data
allgames <- NULL
for(year in 1997:2024){
    sched <- readRDS(paste0('data/sched',year,'.rds'))
    # remove unnamed column
    sched <- sched[,names(sched) != ""]
    # add Notes column, if missing
    if(!"Notes" %in% names(sched)){
        sched$Notes <- ""
    }
    # annotate playoffs
    sched$playoffs <- FALSE
    if(any(sched$`Visitor/Neutral` == 'Playoffs')){
        cut <- which.max(sched$`Visitor/Neutral` == 'Playoffs')
        sched$playoffs[cut:nrow(sched)] <- TRUE
        sched <- sched[-cut, ]
    }
    sched$season <- year
    allgames <- rbind(allgames, sched)
}

# add Commissioner's Cup championship games


allgames$Date <- as.Date(allgames$Date, format = "%a, %b %d, %Y")
names(allgames)[3] <- 'PTSvis'
names(allgames)[5] <- 'PTShome'
allgames$PTSvis <- as.numeric(allgames$PTSvis)
allgames$PTShome <- as.numeric(allgames$PTShome)
teams <- unique(c(allgames$`Visitor/Neutral`, allgames$`Home/Neutral`))

# Abbreviations
allgames$home_team_abbr <- ""
allgames$away_team_abbr <- ""
# Teams and the league were collectively owned by the NBA until the end of 2002
# [...] This led to two teams moving: [...] Orlando moved to Connecticut
allgames$home_team_abbr[allgames$`Home/Neutral` %in% c("Orlando Miracle","Connecticut Sun")] <- "CON"
allgames$away_team_abbr[allgames$`Visitor/Neutral` %in% c("Orlando Miracle","Connecticut Sun")] <- "CON"

# Teams and the league were collectively owned by the NBA until the end of 2002
# [...] This led to two teams moving: Utah moved to San Antonio [...] in 2018
# the San Antonio Stars went to Nevada, becoming the Las Vegas Aces.
allgames$home_team_abbr[allgames$`Home/Neutral` %in% c("Utah Starzz","San Antonio Silver Stars","Las Vegas Aces")] <- "LVA"
allgames$away_team_abbr[allgames$`Visitor/Neutral` %in% c("Utah Starzz","San Antonio Silver Stars","Las Vegas Aces")] <- "LVA"

# The Detroit Shock was the sister team of the Pistons until the teams' owner
# sold the Shock to investors who moved the team to Tulsa, Oklahoma. The
# franchise relocated again in 2016, this time to the Dallas–Fort Worth metro
# area to become the Dallas Wings.
allgames$home_team_abbr[allgames$`Home/Neutral` %in% c("Detroit Shock","Tulsa Shock","Dallas Wings")] <- "DAL"
allgames$away_team_abbr[allgames$`Visitor/Neutral` %in% c("Detroit Shock","Tulsa Shock","Dallas Wings")] <- "DAL"

allgames$home_team_abbr[allgames$`Home/Neutral` == "Atlanta Dream"] <- "ATL"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Atlanta Dream"] <- "ATL"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Charlotte Sting"] <- "CHA"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Charlotte Sting"] <- "CHA"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Chicago Sky"] <- "CHI"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Chicago Sky"] <- "CHI"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Cleveland Rockers"] <- "CLE"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Cleveland Rockers"] <- "CLE"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Houston Comets"] <- "HOU"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Houston Comets"] <- "HOU"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Indiana Fever"] <- "IND"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Indiana Fever"] <- "IND"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Los Angeles Sparks"] <- "LAS"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Los Angeles Sparks"] <- "LAS"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Miami Sol"] <- "MIA"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Miami Sol"] <- "MIA"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Minnesota Lynx"] <- "MIN"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Minnesota Lynx"] <- "MIN"
allgames$home_team_abbr[allgames$`Home/Neutral` == "New York Liberty"] <- "NYL"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "New York Liberty"] <- "NYL"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Phoenix Mercury"] <- "PHX"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Phoenix Mercury"] <- "PHX"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Portland Fire"] <- "POR"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Portland Fire"] <- "POR"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Sacramento Monarchs"] <- "SAC"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Sacramento Monarchs"] <- "SAC"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Seattle Storm"] <- "SEA"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Seattle Storm"] <- "SEA"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Washington Mystics"] <- "WAS"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Washington Mystics"] <- "WAS"
allgames$home_team_abbr[allgames$`Home/Neutral` == "Golden State Valkyries"] <- "GSV"
allgames$away_team_abbr[allgames$`Visitor/Neutral` == "Golden State Valkyries"] <- "GSV"

stopifnot(!any(allgames$home_team_abbr == ""))
stopifnot(!any(allgames$away_team_abbr == ""))

# add columns for pre- and post-game Elo rating, as well as home team win prob.
allgames$away_elo_pre <- allgames$away_elo_post <- allgames$home_elo_pre <- allgames$home_elo_post <- allgames$homeWinProb <- NA

# add columns for home/away game number (for the season)
allgames$home_gameNo <- sapply(1:nrow(allgames), function(ii){
    ytd <- allgames[which(allgames$season == allgames$season[ii] & allgames$Date < allgames$Date[ii] &
                              (allgames$away_team_abbr == allgames$home_team_abbr[ii] | allgames$home_team_abbr == allgames$home_team_abbr[ii])), ]
    return(nrow(ytd)+1)
})
allgames$away_gameNo <- sapply(1:nrow(allgames), function(ii){
    ytd <- allgames[which(allgames$season == allgames$season[ii] & allgames$Date < allgames$Date[ii] &
                              (allgames$away_team_abbr == allgames$away_team_abbr[ii] | allgames$home_team_abbr == allgames$away_team_abbr[ii])), ]
    return(nrow(ytd)+1)
})

# abbr <- allgames$home_team_abbr[idx]
# date <- allgames$Date[idx]
# season <- allgames$season[idx]

adjEloDiff <- function(awayElo, homeElo, playoff){
    # tried 75, 80 was better
    (homeElo + 80 - awayElo) * 1.25^playoff
}
prevElo <- function(abbr, date, season){
    ytd <- allgames[which(allgames$season == season & allgames$Date < date &
                              (allgames$away_team_abbr == abbr | allgames$home_team_abbr == abbr)), ]
    if(nrow(ytd) == 0){
        # first game of new season
        if(season == 1997){
            # all teams start at 1500
            prevElo <- 1500
        }else{
            prevYear <- allgames[which(allgames$season == season-1 & allgames$Date < date &
                                           (allgames$away_team_abbr == abbr | allgames$home_team_abbr == abbr)), ]
            if(nrow(prevYear) == 0){
                # expansion franchise
                prevElo <- 1300
            }else{
                # update rating from previous year
                last <- prevYear[nrow(prevYear), ]
                if(last$home_team_abbr == abbr){
                    prevElo <- last$home_elo_post
                }else if(last$away_team_abbr == abbr){
                    prevElo <- last$away_elo_post
                }
                prevElo <- (prevElo + 1500) / 2
            }
        }
    }else{
        # take post-game Elo from last game played
        last <- ytd[nrow(ytd), ]
        if(last$home_team_abbr == abbr){
            prevElo <- last$home_elo_post
        }else if(last$away_team_abbr == abbr){
            prevElo <- last$away_elo_post
        }
    }
    return(prevElo)
}
homeWinProb <- function(game){
    # game should have pre-game Elo ratings
    if(is.na(game$home_elo_pre) | is.na(game$away_elo_pre)){
        stop('need pre-game Elo for both teams before calculating win probs')
    }
    elodiff <- adjEloDiff(game$away_elo_pre, game$home_elo_pre, game$playoffs)
    return(1/(10^(-elodiff/400)+1))
}
homeEloShift <- function(game){
    # should have everything except post-game elo
    # 20 gives better AIC, but looks worse
    # K <- 28
    # adaptive K (higher early on in the season, to account for roster moves)
    K <- ifelse(game$home_gameNo <= 10, 28, 20)
    # changed home court advantage from 80 to 75. Historically, it's about 2.83 points
    elodiff <- adjEloDiff(game$away_elo_pre, game$home_elo_pre, game$playoffs)
    if(game$PTShome > game$PTSvis){
        if(elodiff > 0){ # home team was expected to win
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*elodiff)
        }else{
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*(-elodiff))
        }
        pregameFavMult <- 1 - game$homeWinProb # actual - expected
    }else if(game$PTSvis > game$PTShome){
        if(elodiff < 0){ # away team was expected to win
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*(-elodiff))
        }else{
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*elodiff)
        }
        pregameFavMult <- 0 - game$homeWinProb # actual - expected
    }
    return(K * MoVmult * pregameFavMult)
}
awayEloShift <- function(game){
    # should have everything except post-game elo
    # 20 gives better AIC, but looks worse
    # K <- 28
    # adaptive K (higher early on in the season, to account for roster moves)
    K <- ifelse(game$away_gameNo <= 10, 28, 20)
    # changed home court advantage from 80 to 75. Historically, it's about 2.83 points
    elodiff <- adjEloDiff(game$away_elo_pre, game$home_elo_pre, game$playoffs)
    if(game$PTShome > game$PTSvis){
        if(elodiff > 0){ # home team was expected to win
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*elodiff)
        }else{
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*(-elodiff))
        }
        pregameFavMult <- 0 - (1-game$homeWinProb) # actual - expected
    }else if(game$PTSvis > game$PTShome){
        if(elodiff < 0){ # away team was expected to win
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*(-elodiff))
        }else{
            MoVmult <- (abs(game$PTShome - game$PTSvis)+3)^.8 / (7.5 + .006*elodiff)
        }
        pregameFavMult <- 1 - (1-game$homeWinProb) # actual - expected
    }
    return(K * MoVmult * pregameFavMult)
}

for(season in 1997:2024){
    for(gi in 1:sum(allgames$season == season)){
        idx <- which(allgames$season == season)[gi]
        game <- allgames[idx,]
        
        # search for previous Elo ratings
        game$away_elo_pre <- prevElo(game$away_team_abbr, game$Date, game$season)
        game$home_elo_pre <- prevElo(game$home_team_abbr, game$Date, game$season)
        
        # calculate win prob
        game$homeWinProb <- homeWinProb(game)
        
        # calculate update
        hshift <- homeEloShift(game)
        if(game$PTShome > game$PTSvis) stopifnot(hshift > 0)
        ashift <- awayEloShift(game)
        if(game$PTShome < game$PTSvis) stopifnot(ashift > 0)
        if(game$home_gameNo==game$away_gameNo){
            stopifnot(round(hshift, digits = 4) == round(-ashift, digits = 4))
        }
        # update
        game$away_elo_post <- game$away_elo_pre + ashift
        game$home_elo_post <- game$home_elo_pre + hshift
        allgames[idx,] <- game
    }
}


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

# home court advantage fell off ~2020
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

