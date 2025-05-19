
allgames <- readRDS('data/allgames97_24.rds')

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

# single-year plot, similar to standings
makeWNBAeloGraph <- function(year, allgames, mode = c('light','dark')){
    mode <- match.arg(mode)
    
    games <- allgames[which(allgames$season == year), ]
    
    eloA <- games[,c('Date','Visitor/Neutral','season','away_team_abbr','away_elo_pre','away_elo_post')]
    eloH <- games[,c('Date','Home/Neutral','season','home_team_abbr','home_elo_pre','home_elo_post')]
    names(eloA) <- c('Date','Team_Name','season','team_abbr','elo_pre','elo_post')
    names(eloH) <- names(eloA)
    elo <- rbind(eloA, eloH)
    elo <- elo[order(elo$Date), ]
    rm(eloA,eloH)
    
    teams <- unique(elo$Team_Name)
    
    # check
    stopifnot(all(teams %in% teamcolors$name))
    # stopifnot(all(teamcolors$name %in% teams)) # teams can be added/removed
    
    # QC stuff. missing values, etc.
    
    # add initial value
    opening <- min(elo$Date)
    for(team in teams){
        toAdd <- elo[which.max(elo$Team_Name == team), ]
        toAdd$Date <- opening - 1
        toAdd$elo_post <- toAdd$elo_pre
        elo <- rbind(toAdd, elo)
    }
    finale <- max(games$Date[which(!games$playoffs)])
    
    curves <- lapply(teams, function(team){
        idx <- which(elo$Team_Name == team)
        curve <- data.frame(Date = elo$Date[idx],
                            elo = elo$elo_post[idx])
        # already added starting value, but want intermediate values, so it's clear when games happen
        supp <- data.frame(Date = curve$Date-1,
                           elo = c(0,curve$elo[-nrow(curve)]))
        supp <- supp[-1, ]
        supp <- supp[!supp$Date %in% curve$Date, ]
        curve <- rbind(supp,curve)
        curve <- curve[order(curve$Date), ]
        # curve <- curve[!is.na(curve$margin), ] # only count played games
        # if(is.na(curve$WL[nrow(curve)])){
        #     curve <- curve[-nrow(curve), ]
        # }
        # 
        return(curve)
    })
    names(curves) <- teams
    

    # make sure curves start the day before opening day and don't end before the current date
    today <- Sys.Date() - 1 # it's actually yesterday, because this is set to run in the early morning
    # don't extend past the end of the season
    if(today > max(games$Date)){
        today <- max(games$Date)
    }
    # remove NAs (only relevant for current season)
    curves <- lapply(curves, function(x){ x[!is.na(x$elo), ]})
    for(i in seq_along(curves)){
        curve <- curves[[i]]
        if(! today %in% curve$Date){
            # make sure "today" is after opening day.
            # stop extending lines when regular season ends, only playoff teams should continue
            if(today > opening & today <= finale){
                toAdd <- data.frame(Date = today, elo = curve$elo[nrow(curve)])
                curve <- rbind(curve, toAdd)
            }
        }
        curve <- curve[order(curve$Date), ]
        curves[[i]] <- curve
    }
    # remove points after "today" (only relevant for current season)
    curves <- lapply(curves, function(x){ x[x$Date <= today, ]})
    
    # sort by final elo
    curves <- curves[order(sapply(curves, function(x){ x$elo[nrow(x)] }))]
    teamdata <- teamcolors[match(names(curves), teamcolors$name), ]
    teamdata$elo <- sapply(curves, function(x){ x$elo[nrow(x)] })
    # all(names(curves) == teamdata$name) # check
    
    # build legend df
    lgnd <- teamdata[nrow(teamdata):1, ]
    lgnd$nameElo <- paste0(lgnd$name,' (',round(lgnd$elo),')')
    
    
    layout(matrix(c(1,1,1,2), nrow=1))
    par(mar = c(5,5,4,0)+.1, cex.lab = 1.25, cex.main = 2, bg = '#fffaf6')
    if(mode == 'dark'){
        par(fg = 'white', bg = '#272935', col.axis = 'white', col.lab = 'white', col.main = 'white', col.sub = 'white')
    }
    
    require(scales)
    xl <- range(do.call(rbind, curves)$Date)
    yl <- range(sapply(curves,function(x){x$elo}))
    plot(xl, yl, col = 'transparent',
         xlab = 'Date', ylab='Elo Rating', main = paste(year, 'WNBA Elo Ratings'), las = 1)
    rect(finale, -9999, finale+10000, 9999, col = alpha(par()$fg, alpha = .15), border = NA)
    abline(h = 1500, lty = 2)
    
    #rect(min(sched$Date)-9999,-9999,max(sched$Date)+9999,9999, col='grey95')
    for(i in 1:length(curves)){
        cc <- as.character(teamdata[teamdata$name == names(curves)[i], ][,c('color1','color2')])
        #points(curves[[i]]$Date, curves[[i]]$elo, col=cc[2], pch=16, cex=.5)
        #lines(curves[[i]]$Date, curves[[i]]$elo, col=cc[2], lwd=4)
        lines(curves[[i]]$Date, curves[[i]]$elo, col=cc[1], lwd=4)
        lines(curves[[i]]$Date, curves[[i]]$elo, col=cc[2], lwd=1)
        points(curves[[i]]$Date[nrow(curves[[i]])], curves[[i]]$elo[nrow(curves[[i]])], col=cc[2], pch=16, cex=2)
        points(curves[[i]]$Date[nrow(curves[[i]])], curves[[i]]$elo[nrow(curves[[i]])], col=cc[1], pch=16, cex = 1.5)
    }
    if(today > finale){
        text(xl[1], yl[1], labels = "Regular Season", pos = 4, offset = 0, font = 2)
        text(xl[2], yl[1], labels = "Playoffs", pos = 2, offset = 0, font = 2)
    }
    
    par(mar = c(5,0,4,1)+.1)
    plot.new()
    legend('left', legend = rep('', nrow(lgnd)), bty='n', lwd=4.5, 
           col = lgnd$color1, cex = 1.3)
    legend('left', legend = lgnd$nameElo, bty='n', lwd=1.1, 
           col = lgnd$color2, cex = 1.3)
    
}

# single-year plot with one team highlighted, for team histories
makeWNBAeloHiliteGraph <- function(year, team, teamnames, allgames, mode = c('light','dark')){
    mode <- match.arg(mode)
    
    games <- allgames[which(allgames$season == year), ]
    
    eloA <- games[,c('Date','Visitor/Neutral','season','away_team_abbr','away_elo_pre','away_elo_post')]
    eloH <- games[,c('Date','Home/Neutral','season','home_team_abbr','home_elo_pre','home_elo_post')]
    names(eloA) <- c('Date','Team_Name','season','team_abbr','elo_pre','elo_post')
    names(eloH) <- names(eloA)
    elo <- rbind(eloA, eloH)
    elo <- elo[order(elo$Date), ]
    rm(eloA,eloH)
    
    teams <- unique(elo$Team_Name)
    
    # check
    stopifnot(all(teams %in% teamcolors$name))
    # stopifnot(all(teamcolors$name %in% teams)) # teams can be added/removed
    
    # QC stuff. missing values, etc.
    
    # add initial value
    opening <- min(elo$Date)
    for(team.i in teams){
        toAdd <- elo[which.max(elo$Team_Name == team.i), ]
        toAdd$Date <- opening - 1
        toAdd$elo_post <- toAdd$elo_pre
        elo <- rbind(toAdd, elo)
    }
    finale <- max(games$Date[which(!games$playoffs)])
    
    curves <- lapply(teams, function(team){
        idx <- which(elo$Team_Name == team)
        curve <- data.frame(Date = elo$Date[idx],
                            elo = elo$elo_post[idx])
        # already added starting value, but want intermediate values, so it's clear when games happen
        supp <- data.frame(Date = curve$Date-1,
                           elo = c(0,curve$elo[-nrow(curve)]))
        supp <- supp[-1, ]
        supp <- supp[!supp$Date %in% curve$Date, ]
        curve <- rbind(supp,curve)
        curve <- curve[order(curve$Date), ]
        # curve <- curve[!is.na(curve$margin), ] # only count played games
        # if(is.na(curve$WL[nrow(curve)])){
        #     curve <- curve[-nrow(curve), ]
        # }
        # 
        return(curve)
    })
    names(curves) <- teams
    
    # only for past seasons
    today <- max(games$Date)
    
    for(i in seq_along(curves)){
        curve <- curves[[i]]
        if(! today %in% curve$Date){
            # make sure "today" is after opening day.
            # stop extending lines when regular season ends, only playoff teams should continue
            if(today > opening & today <= finale){
                toAdd <- data.frame(Date = today, elo = curve$elo[nrow(curve)])
                curve <- rbind(curve, toAdd)
            }
        }
        curve <- curve[order(curve$Date), ]
        curves[[i]] <- curve
    }
    teamdata <- teamcolors[match(names(curves), teamcolors$name), ]
    teamdata$elo <- sapply(curves, function(x){ x$elo[nrow(x)] })
    # all(names(curves) == teamdata$name) # check
    
    par(cex.lab = 1.25, cex.main = 1.5, bg = '#fffaf6')
    if(mode == 'dark'){
        par(fg = 'white', bg = '#272935', col.axis = 'white', col.lab = 'white', col.main = 'white', col.sub = 'white')
    }
    
    require(scales)
    xl <- range(do.call(rbind, curves)$Date)
    yl <- range(c(allgames$home_elo_post, allgames$away_elo_post))
    if(team %in% teamdata$name){ # active
        active <- TRUE
        yearcol <- par('fg')
    }else{
        active <- FALSE
        yearcol <- alpha(par('fg'), alpha=.4)
    }
    plot(xl, yl, col = 'transparent', axes = FALSE,
         xlab = '', ylab='', main = year, las = 1, col.main = yearcol)
    abline(h = 1500, lty = 2, col = yearcol)
    
    for(i in 1:length(curves)){
        #points(curves[[i]]$Date, curves[[i]]$elo, col=cc[2], pch=16, cex=.5)
        #lines(curves[[i]]$Date, curves[[i]]$elo, col=cc[2], lwd=4)
        lines(curves[[i]]$Date, curves[[i]]$elo, col=alpha(par("fg"), alpha = .3), lwd=2)
    }
    for(team in teamnames){
        if(team %in% teamdata$name){
            cc <- as.character(teamdata[teamdata$name == team, ][,c('color1','color2')])
            curve <- curves[[team]]
            if(active){
                lines(curve$Date, curve$elo, col=cc[1], lwd=4)
                lines(curve$Date, curve$elo, col=cc[2], lwd=1)
            }else{
                lines(curve$Date, curve$elo, col=alpha(cc[1], alpha=.6), lwd=3)
            }
            # check if team won championship
            lastgame <- allgames[which(allgames$season == year), ]
            lastgame <- lastgame[which.max(lastgame$Date), ]
            champ <- ifelse(lastgame$PTShome > lastgame$PTSvis, lastgame$`Home/Neutral`, lastgame$`Visitor/Neutral`)
            if(team == champ){
                if(active){
                    points(curve$Date[nrow(curve)], curve$elo[nrow(curve)], pch = 21, bg='gold2', cex=1.75)
                }else{
                    points(curve$Date[nrow(curve)], curve$elo[nrow(curve)], pch = 21, bg=alpha('gold2', alpha = .6), cex=1.75)
                }
            }
        }
    }
}
