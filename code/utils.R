
############
# GET DATA #
############
getSchedule <- function(year){
    require(rvest)
    sched <- html_table(read_html(paste0('https://www.basketball-reference.com/wnba/years/',year,'_games.html')))[[1]]
    return(sched)
}
getStandings <- function(year){
    require(rvest)
    standings <- html_table(read_html(paste0('https://www.basketball-reference.com/wnba/years/',year,'_standings.html')))[[1]]
    return(standings)
}

###############
# ELO RELATED #
###############
adjEloDiff <- function(awayElo, homeElo, neutral, playoff){
    # checks
    stopifnot(is.numeric(awayElo))
    stopifnot(is.numeric(homeElo))
    stopifnot(is.logical(neutral))
    stopifnot(is.logical(playoff))
    # tried 75, 80 was better
    (homeElo + 80*(!neutral) - awayElo) * 1.25^playoff
}
prevElo <- function(franchise, date, season, allgames){
    # checks
    stopifnot(is.numeric(franchise))
    stopifnot(is.numeric(season))
    ytd <- allgames[which(allgames$season == season & allgames$Date < date &
                              (allgames$away_franchise == franchise | allgames$home_franchise == franchise)), ]
    if(nrow(ytd) == 0){
        # first game of new season
        if(season == 1997){
            # all teams start at 1500
            prevElo <- 1500
        }else{
            prevYear <- allgames[which(allgames$season == season-1 & allgames$Date < date &
                                           (allgames$away_franchise == franchise | allgames$home_franchise == franchise)), ]
            if(nrow(prevYear) == 0){
                # expansion franchise
                prevElo <- 1300
            }else{
                # update rating from previous year
                last <- prevYear[nrow(prevYear), ]
                if(last$home_franchise == franchise){
                    prevElo <- last$home_elo_post
                }else if(last$away_franchise == franchise){
                    prevElo <- last$away_elo_post
                }
                prevElo <- (prevElo + 1500) / 2
            }
        }
    }else{
        # take post-game Elo from last game played
        last <- ytd[nrow(ytd), ]
        if(last$home_franchise == franchise){
            prevElo <- last$home_elo_post
        }else if(last$away_franchise == franchise){
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

# when in season: update `allgames` to include partial season
# sched <- getSchedul(year)
addSched2Allgames <- function(year, sched, allgames){
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
    sched$Date <- as.Date(sched$Date, format = "%a, %b %d, %Y")
    names(sched)[3] <- 'PTSvis'
    names(sched)[5] <- 'PTShome'
    sched$PTSvis <- as.numeric(sched$PTSvis)
    sched$PTShome <- as.numeric(sched$PTShome)
    sched$season <- as.numeric(sched$season)
    teams <- unique(c(sched$`Visitor/Neutral`, sched$`Home/Neutral`))
    sched <- sched[order(sched$Date), ]
    sched$neutral <- FALSE
    sched$neutral[grep('neutral', tolower(sched$Notes))] <- TRUE
    
    # Abbreviations and franchise numbers
    stopifnot(all(sched$`Home/Neutral` %in% teamdata$team))
    stopifnot(all(sched$`Visitor/Neutral` %in% teamdata$team))
    sched$home_abbr <- teamdata$abbr[match(sched$`Home/Neutral`, teamdata$team)]
    sched$away_abbr <- teamdata$abbr[match(sched$`Visitor/Neutral`, teamdata$team)]
    sched$home_franchise <- teamdata$franchise[match(sched$`Home/Neutral`, teamdata$team)]
    sched$away_franchise <- teamdata$franchise[match(sched$`Visitor/Neutral`, teamdata$team)]
    stopifnot(!any(is.na(sched$home_abbr)))
    stopifnot(!any(is.na(sched$away_abbr)))
    stopifnot(!any(is.na(sched$home_franchise)))
    stopifnot(!any(is.na(sched$away_franchise)))
    
    # add columns for pre- and post-game Elo rating, as well as home team win prob.
    sched$away_elo_pre <- sched$away_elo_post <- sched$home_elo_pre <- sched$home_elo_post <- sched$homeWinProb <- NA
    
    # add columns for home/away game number (for the season)
    sched$home_gameNo <- sapply(1:nrow(sched), function(ii){
        ytd <- sched[which(sched$season == sched$season[ii] & sched$Date < sched$Date[ii] &
                               (sched$away_abbr == sched$home_abbr[ii] | sched$home_abbr == sched$home_abbr[ii])), ]
        return(nrow(ytd)+1)
    })
    sched$away_gameNo <- sapply(1:nrow(sched), function(ii){
        ytd <- sched[which(sched$season == sched$season[ii] & sched$Date < sched$Date[ii] &
                               (sched$away_abbr == sched$away_abbr[ii] | sched$home_abbr == sched$away_abbr[ii])), ]
        return(nrow(ytd)+1)
    })
    
    # add current season to all games
    allgames <- rbind(allgames, sched)
    allgames <- allgames[order(allgames$Date), ]
    
    # calculate Elo for current season
    season <- year
    for(gi in 1:sum(allgames$season == season)){
        idx <- which(allgames$season == season)[gi]
        game <- allgames[idx,]
        
        # search for previous Elo ratings
        game$away_elo_pre <- prevElo(game$away_franchise, game$Date, game$season, allgames)
        game$home_elo_pre <- prevElo(game$home_franchise, game$Date, game$season, allgames)
        
        # calculate win prob
        if(!is.na(game$home_elo_pre) & !is.na(game$away_elo_pre)){
            game$homeWinProb <- homeWinProb(game)
        }
        
        # calculate update
        if(!is.na(game$PTShome) & !is.na(game$PTSvis)){
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
        }
        allgames[idx,] <- game
    }
    return(allgames)
}


