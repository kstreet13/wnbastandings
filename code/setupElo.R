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
    'Tue, Jun 25, 2024','Minnesota Lynx','94','New York Liberty','89','','2024'
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

rm(game,sched)



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
    # remove NAs (only relevant for current season)
    curves <- lapply(curves, function(x){ x[!is.na(x$elo), ]})
    
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
makeWNBAeloHiliteGraph <- function(year, allgames, mode = c('light','dark')){
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
    # sort by final elo
    curves <- curves[order(sapply(curves, function(x){ x$elo[nrow(x)] }))]
    teamdata <- teamcolors[match(names(curves), teamcolors$name), ]
    teamdata$elo <- sapply(curves, function(x){ x$elo[nrow(x)] })
    # all(names(curves) == teamdata$name) # check
    
    par(cex.lab = 1.25, cex.main = 2, bg = '#fffaf6')
    if(mode == 'dark'){
        par(fg = 'white', bg = '#272935', col.axis = 'white', col.lab = 'white', col.main = 'white', col.sub = 'white')
    }
    
    require(scales)
    xl <- range(do.call(rbind, curves)$Date)
    yl <- range(c(allgames$home_elo_post, allgames$away_elo_post))
    plot(xl, yl, col = 'transparent',
         xlab = '', ylab='', main = year, las = 1)
    rect(finale, -9999, finale+10000, 9999, col = alpha(par()$fg, alpha = .15), border = NA)
    abline(h = 1500, lty = 2)
    
    #rect(min(sched$Date)-9999,-9999,max(sched$Date)+9999,9999, col='grey95')
    for(i in 1:length(curves)){
        #points(curves[[i]]$Date, curves[[i]]$elo, col=cc[2], pch=16, cex=.5)
        #lines(curves[[i]]$Date, curves[[i]]$elo, col=cc[2], lwd=4)
        lines(curves[[i]]$Date, curves[[i]]$elo, col='grey50', lwd=2)
    }
    cc <- as.character(teamdata[teamdata$name == team, ][,c('color1','color2')])
    curve <- curves[[team]]
    lines(curve$Date, curve$elo, col=cc[1], lwd=4)
    lines(curve$Date, curve$elo, col=cc[2], lwd=1)
}
