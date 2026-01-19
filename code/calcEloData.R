# combine all games data
allgames <- NULL
for(year in 1997:2025){
    sched <- readRDS(paste0('data/sched',year,'.rds'))
    # remove unnamed column
    sched <- sched[,names(sched) != ""]
    # add Notes column, if missing
    if(!"Notes" %in% names(sched)){
        sched$Notes <- ""
    }
    sched$season <- year
    # annotate playoffs
    sched$playoffs <- FALSE
    if(any(sched$`Visitor/Neutral` == 'Playoffs')){
        cut <- which.max(sched$`Visitor/Neutral` == 'Playoffs')
        sched$playoffs[cut:nrow(sched)] <- TRUE
        sched <- sched[-cut, ]
    }
    allgames <- rbind(allgames, sched)
}

# add Commissioner's Cup championship games
CC <- as.data.frame(matrix(c(
    'Thu, Aug 12, 2021','Connecticut Sun','57','Seattle Storm','79','Neutral site','2021',
    'Tue, Jul 26, 2022','Las Vegas Aces','93','Chicago Sky','83','','2022',
    'Tue, Aug 15, 2023','New York Liberty','82','Las Vegas Aces','63','','2023',
    'Tue, Jun 25, 2024','Minnesota Lynx','94','New York Liberty','89','','2024',
    'Tue, Jul 1, 2025','Indiana Fever','74','Minnesota Lynx','59','','2025'
), ncol = 7, byrow = TRUE))
CC$playoffs <- TRUE
names(CC) <- names(allgames)
allgames <- rbind(allgames, CC)
rm(CC)

allgames$Date <- as.Date(allgames$Date, format = "%a, %b %d, %Y")
names(allgames)[3] <- 'PTSvis'
names(allgames)[5] <- 'PTShome'
allgames$PTSvis <- as.numeric(allgames$PTSvis)
allgames$PTShome <- as.numeric(allgames$PTShome)
allgames$season <- as.numeric(allgames$season)
teams <- unique(c(allgames$`Visitor/Neutral`, allgames$`Home/Neutral`))
allgames <- allgames[order(allgames$Date), ]
allgames$neutral <- FALSE
allgames$neutral[grep('neutral', tolower(allgames$Notes))] <- TRUE
allgames$neutral[which(allgames$season == 2020)] <- TRUE # 2020 bubble season

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

adjEloDiff <- function(awayElo, homeElo, neutral, playoff){
    # tried 75, 80 was better
    (homeElo + 80*(!neutral) - awayElo) * 1.25^playoff
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
    elodiff <- adjEloDiff(game$away_elo_pre, game$home_elo_pre, game$neutral, game$playoffs)
    return(1/(10^(-elodiff/400)+1))
}
homeEloShift <- function(game){
    # should have everything except post-game elo
    # 20 gives better AIC, but looks worse
    # K <- 28
    # adaptive K (higher early on in the season, to account for roster moves)
    K <- ifelse(game$home_gameNo <= 10, 28, 20)
    # changed home court advantage from 80 to 75. Historically, it's about 2.83 points
    elodiff <- adjEloDiff(game$away_elo_pre, game$home_elo_pre, game$neutral, game$playoffs)
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
    elodiff <- adjEloDiff(game$away_elo_pre, game$home_elo_pre, game$neutral, game$playoffs)
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

for(season in 1997:2025){
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

rm(game,sched)

saveRDS(allgames, file = 'data/allgames97_25.rds')
