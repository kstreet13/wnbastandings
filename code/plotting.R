#############
# STANDINGS #
#############
makeWNBAstandingsGraph <- function(year, sched, mode = c('light','dark')){
    mode <- match.arg(mode)
    
    sched <- sched[,1:5]
    # date could be in two formats, with or without commas
    if(length(grep(',', sched$Date)) > 0){
        sched$Date <- as.Date(sched$Date, format = "%a, %b %d, %Y")
    }else{
        sched$Date <- as.Date(sched$Date, format = "%a %b %d %Y")
    }
    names(sched) <- c('Date','Visitor','PTSvis','Home','PTShome')
    # remove playoffs
    if(any(sched$Visitor == 'Playoffs')){
        sched <- sched[1:(which.max(sched$Visitor == 'Playoffs')-1), ]
    }
    sched$PTSvis <- as.numeric(sched$PTSvis)
    sched$PTShome <- as.numeric(sched$PTShome)
    teams <- unique(c(sched$Visitor, sched$Home))
    # check
    stopifnot(all(teams %in% teamdata$team))
    
    # QC stuff. missing values, ties
    if(any(!is.na(sched$PTShome) & (sched$PTShome==sched$PTSvis))){
        stop('ties detected')
    }
    
    curves <- lapply(teams, function(team){
        away <- sched[which(sched$Visitor == team), ]
        away$WL <- away$PTSvis > away$PTShome
        home <- sched[which(sched$Home == team), ]
        home$WL <- home$PTShome > home$PTSvis
        curve <- rbind(away, home)
        curve <- curve[order(curve$Date), ]
        curve$margin <- cumsum(c(-1,1)[factor(curve$WL, levels = c('FALSE','TRUE'))])
        
        curve <- curve[, c('Date','WL','margin')]
        supp <- data.frame(Date = curve$Date-1,
                           WL = NA,
                           margin = c(0,curve$margin[-nrow(curve)]))
        supp <- supp[!supp$Date %in% curve$Date, ]
        curve <- rbind(supp,curve)
        curve <- curve[order(curve$Date), ]
        curve <- curve[!is.na(curve$margin), ] # only count played games
        if(is.na(curve$WL[nrow(curve)])){
            curve <- curve[-nrow(curve), ]
        }
        return(curve)
    })
    names(curves) <- teams
    # make sure curves start the day before opening day and don't end before the current date
    opening <- min(sched$Date)
    today <- Sys.Date() - 1 # it's actually yesterday, because this is set to run in the early morning
    # don't extend past the end of the season
    if(today > max(sched$Date)){
        today <- max(sched$Date)
    }
    for(i in seq_along(curves)){
        curve <- curves[[i]]
        if(! opening %in% curve$Date){
            toAdd <- data.frame(Date = opening, WL = NA, margin = 0)
            curve <- rbind(toAdd, curve)
        }
        if(! today %in% curve$Date){
            # make sure "today" is after opening day
            if(today > opening){
                toAdd <- data.frame(Date = today, WL = NA, margin = curve$margin[nrow(curve)])
                curve <- rbind(curve, toAdd)
            }
        }
        curves[[i]] <- curve
    }
    curves <- curves[order(sapply(curves, function(x){ x$margin[nrow(x)] }))]
    teamdata <- teamdata[match(names(curves), teamdata$team), ]
    teamdata$margin <- sapply(curves, function(x){ x$margin[nrow(x)] })
    teamdata$W <- sapply(curves, function(x){ sum(x$WL, na.rm=TRUE) })
    teamdata$L <- sapply(curves, function(x){ sum(!x$WL, na.rm=TRUE) })
    # all(names(curves) == teamdata$team) # check
    
    # adjust final point locations when there are ties (for visibility)
    for(mar in unique(teamdata$margin)){
        # maximum tie size to break is 10 teams (relevant at start of season)
        if(sum(teamdata$margin == mar) > 1 & sum(teamdata$margin == mar) < 10){
            ind <- rev(which(teamdata$margin == mar))
            offset <- 4*(1:length(ind))-4 - 4*(length(ind)-1)/2
            for(i in 1:length(ind)){
                curves[[ind[i]]]$Date[nrow(curves[[ind[i]]])] <- 
                    curves[[ind[i]]]$Date[nrow(curves[[ind[i]]])] + offset[i]/24
            }
        }
    }
    
    # check against actual standings
    # standings$Team <- gsub('[*]','', standings$Team)
    # for(team in teams){
    #     s.i <- which(standings$Team == team)
    #     c.i <- which(names(curves) == team)
    #     stopifnot(curves[[c.i]]$margin[nrow(curves[[c.i]])] == standings$W[s.i] - standings$L[s.i])
    # }
    
    # build legend df with blank row to separate non/playoff teams
    lgnd <- teamdata[nrow(teamdata):1, ]
    lgnd <- lgnd[match(names(curves), lgnd$team), ]
    lgnd <- lgnd[nrow(lgnd):1, ]
    lgnd$nameRecord <- paste0(lgnd$team,' (',lgnd$W,'-',lgnd$L,')')
    # lgnd <- rbind(lgnd[1:8,], NA, lgnd[9:nrow(lgnd), ]) # add gap
    
    layout(matrix(c(1,1,1,2), nrow=1))
    par(mar = c(5,5,4,0)+.1, cex.lab = 1.25, cex.main = 2, bg = '#fffaf6')
    if(mode == 'dark'){
        par(fg = 'white', bg = '#272935', col.axis = 'white', col.lab = 'white', col.main = 'white', col.sub = 'white')
    }
    
    require(scales)
    plot(range(do.call(rbind, curves)$Date), range(sapply(curves,function(x){x$margin})), col = 'transparent',
         xlab = 'Date', ylab='Games Over/Under .500', main = paste(year, 'WNBA Standings'), las = 1)
    #rect(min(sched$Date)-9999,-9999,max(sched$Date)+9999,9999, col='grey95')
    abline(h = 0)
    abline(h = seq(-100,-5, by=5), lty=2, col = alpha(par()$fg,.3))
    abline(h = seq(5,100, by=5), lty=2, col = alpha(par()$fg,.3))
    for(i in 1:length(curves)){
        cc <- as.character(teamdata[teamdata$team == names(curves)[i], ][,c('color1','color2')])
        #points(curves[[i]]$Date, curves[[i]]$margin, col=cc[2], pch=16, cex=.5)
        #lines(curves[[i]]$Date, curves[[i]]$margin, col=cc[2], lwd=4)
        lines(curves[[i]]$Date, curves[[i]]$margin, col=cc[1], lwd=4)
        lines(curves[[i]]$Date, curves[[i]]$margin, col=cc[2], lwd=1)
        points(curves[[i]]$Date[nrow(curves[[i]])], curves[[i]]$margin[nrow(curves[[i]])], col=cc[2], pch=16, cex=2)
        points(curves[[i]]$Date[nrow(curves[[i]])], curves[[i]]$margin[nrow(curves[[i]])], col=cc[1], pch=16, cex = 1.5)
    }
    
    par(mar = c(5,0,4,1)+.1)
    plot.new()
    legend('left', legend = rep('', nrow(lgnd)), bty='n', lwd=4.5, col = lgnd$color1, cex = 1.3)
    legend('left', legend = lgnd$nameRecord, 
           bty='n', lwd=1.1, col = lgnd$color2, cex = 1.3)
    
}

require(plotly)
interactiveWNBAstandingsGraph <- function(year, sched, mode = c('light','dark')){
    mode <- match.arg(mode)
    
    sched <- sched[,1:5]
    # date could be in two formats, with or without commas
    if(length(grep(',', sched$Date)) > 0){
        sched$Date <- as.Date(sched$Date, format = "%a, %b %d, %Y")
    }else{
        sched$Date <- as.Date(sched$Date, format = "%a %b %d %Y")
    }
    names(sched) <- c('Date','Visitor','PTSvis','Home','PTShome')
    # remove playoffs
    if(any(sched$Visitor == 'Playoffs')){
        sched <- sched[1:(which.max(sched$Visitor == 'Playoffs')-1), ]
    }
    sched$PTSvis <- as.numeric(sched$PTSvis)
    sched$PTShome <- as.numeric(sched$PTShome)
    teams <- unique(c(sched$Visitor, sched$Home))
    # check
    stopifnot(all(teams %in% teamdata$team))
    # stopifnot(all(teamdata$team %in% teams)) # teams can be added/removed
    
    
    # QC stuff. missing values, ties
    if(any(!is.na(sched$PTShome) & (sched$PTShome==sched$PTSvis))){
        stop('ties detected')
    }
    
    curves <- lapply(teams, function(team){
        away <- sched[which(sched$Visitor == team), ]
        away$WL <- away$PTSvis > away$PTShome
        home <- sched[which(sched$Home == team), ]
        home$WL <- home$PTShome > home$PTSvis
        curve <- rbind(away, home)
        curve <- curve[order(curve$Date), ]
        curve$margin <- cumsum(c(-1,1)[factor(curve$WL, levels = c('FALSE','TRUE'))])
        
        curve <- curve[, c('Date','WL','margin')]
        supp <- data.frame(Date = curve$Date-1,
                           WL = NA,
                           margin = c(0,curve$margin[-nrow(curve)]))
        supp <- supp[!supp$Date %in% curve$Date, ]
        curve <- rbind(supp,curve)
        curve <- curve[order(curve$Date), ]
        curve <- curve[!is.na(curve$margin), ] # only count played games
        if(is.na(curve$WL[nrow(curve)])){
            curve <- curve[-nrow(curve), ]
        }
        return(curve)
    })
    names(curves) <- teams
    # make sure curves start the day before opening day and don't end before the current date
    opening <- min(sched$Date)
    today <- Sys.Date() - 1 # it's actually yesterday, because this is set to run in the early morning
    # don't extend past the end of the season
    if(today > max(sched$Date)){
        today <- max(sched$Date)
    }
    for(i in seq_along(curves)){
        curve <- curves[[i]]
        if(! opening %in% curve$Date){
            toAdd <- data.frame(Date = opening, WL = NA, margin = 0)
            curve <- rbind(toAdd, curve)
        }
        if(! today %in% curve$Date){
            # make sure "today" is after opening day
            if(today > opening){
                toAdd <- data.frame(Date = today, WL = NA, margin = curve$margin[nrow(curve)])
                curve <- rbind(curve, toAdd)
            }
        }
        # add running total of wins and losses
        win <- curve$WL
        win[is.na(win)] <- FALSE
        curve$wins <- cumsum(win)
        loss <- !curve$WL
        loss[is.na(loss)] <- FALSE
        curve$losses <- cumsum(loss)
        
        curves[[i]] <- curve
    }
    curves <- curves[order(sapply(curves, function(x){ x$margin[nrow(x)] }))]
    teamdata <- teamdata[match(names(curves), teamdata$team), ]
    teamdata$margin <- sapply(curves, function(x){ x$margin[nrow(x)] })
    teamdata$W <- sapply(curves, function(x){ sum(x$WL, na.rm=TRUE) })
    teamdata$L <- sapply(curves, function(x){ sum(!x$WL, na.rm=TRUE) })
    # all(names(curves) == teamdata$team) # check
    
    for(i in 1:length(curves)){
        curves[[i]]$team <- names(curves)[i]
    }
    df <- do.call(rbind, curves)
    
    # teams need to be a factor sorted by final standings
    df$team <- factor(df$team, levels = rev(names(curves)))
    df$abbr <- teamdata$team[match(df$team, teamdata$team)]
    
    cv <- teamdata[teamdata$team %in% df$team, ]
    cv <- cv$distinct[match(rev(names(curves)), cv$team)]
    
    fig <- plot_ly(df, x=~Date, y=~margin, color = ~team, type = 'scatter', mode='lines', colors = cv, 
                   line = list(width = 3), hoverinfo = 'text',
                   text = ~paste0(abbr,' ',Date,' ',wins,'-',losses)) |> 
        layout(legend = list(x = 100, y = 0.5), hovermode = 'x',
               yaxis = list(title = 'Games Over/Under .500'))
    
    if(mode == 'light'){
        fig <- fig |> layout(plot_bgcolor = '#fffaf6', paper_bgcolor = '#fffaf6')
    }else{
        fig <- fig |> layout(plot_bgcolor = '#272935', paper_bgcolor = '#272935',
                             font = list(color = '#FFFFFF'))
    }
    fig
}


###############
# ELO RELATED #
###############
# single-year plot, similar to standings
makeWNBAeloGraph <- function(year, allgames, mode = c('light','dark')){
    mode <- match.arg(mode)
    
    games <- allgames[which(allgames$season == year), ]
    
    eloA <- games[,c('Date','Visitor/Neutral','season','away_abbr','away_elo_pre','away_elo_post')]
    eloH <- games[,c('Date','Home/Neutral','season','home_abbr','home_elo_pre','home_elo_post')]
    names(eloA) <- c('Date','Team_Name','season','team_abbr','elo_pre','elo_post')
    names(eloH) <- names(eloA)
    elo <- rbind(eloA, eloH)
    elo <- elo[order(elo$Date), ]
    rm(eloA,eloH)
    
    teams <- unique(elo$Team_Name)
    
    # check
    stopifnot(all(teams %in% teamdata$team))
    # stopifnot(all(teamdata$team %in% teams)) # teams can be added/removed
    
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
    teamdata <- teamdata[match(names(curves), teamdata$team), ]
    teamdata$elo <- sapply(curves, function(x){ x$elo[nrow(x)] })
    # all(names(curves) == teamdata$team) # check
    
    # build legend df
    lgnd <- teamdata[nrow(teamdata):1, ]
    lgnd$nameElo <- paste0(lgnd$team,' (',round(lgnd$elo),')')
    
    
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
        cc <- as.character(teamdata[teamdata$team == names(curves)[i], ][,c('color1','color2')])
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
    
    eloA <- games[,c('Date','Visitor/Neutral','season','away_abbr','away_elo_pre','away_elo_post')]
    eloH <- games[,c('Date','Home/Neutral','season','home_abbr','home_elo_pre','home_elo_post')]
    names(eloA) <- c('Date','Team_Name','season','team_abbr','elo_pre','elo_post')
    names(eloH) <- names(eloA)
    elo <- rbind(eloA, eloH)
    elo <- elo[order(elo$Date), ]
    rm(eloA,eloH)
    
    teams <- unique(elo$Team_Name)
    
    # check
    stopifnot(all(teams %in% teamdata$team))
    # stopifnot(all(teamdata$team %in% teams)) # teams can be added/removed
    
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
    teamdata <- teamdata[match(names(curves), teamdata$team), ]
    teamdata$elo <- sapply(curves, function(x){ x$elo[nrow(x)] })
    # all(names(curves) == teamdata$team) # check
    
    par(cex.lab = 1.25, cex.main = 1.5, bg = '#fffaf6')
    if(mode == 'dark'){
        par(fg = 'white', bg = '#272935', col.axis = 'white', col.lab = 'white', col.main = 'white', col.sub = 'white')
    }
    
    require(scales)
    xl <- range(do.call(rbind, curves)$Date)
    yl <- range(c(allgames$home_elo_post, allgames$away_elo_post), na.rm=TRUE)
    if(team %in% teamdata$team){ # active
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
        if(team %in% teamdata$team){
            cc <- as.character(teamdata[teamdata$team == team, ][,c('color1','color2')])
            curve <- curves[[team]]
            if(active){
                lines(curve$Date, curve$elo, col=cc[1], lwd=5)
                lines(curve$Date, curve$elo, col=cc[2], lwd=2)
            }else{
                lines(curve$Date, curve$elo, col=alpha(cc[1], alpha=.6), lwd=5)
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

# interactive version of the single-year plot, similar to standings
require(plotly)
interactiveWNBAeloGraph <- function(year, allgames, mode = c('light','dark')){
    mode <- match.arg(mode)
    
    games <- allgames[which(allgames$season == year), ]
    
    eloA <- games[,c('Date','Visitor/Neutral','season','away_abbr','away_elo_pre','away_elo_post')]
    eloH <- games[,c('Date','Home/Neutral','season','home_abbr','home_elo_pre','home_elo_post')]
    names(eloA) <- c('Date','Team_Name','season','team_abbr','elo_pre','elo_post')
    names(eloH) <- names(eloA)
    elo <- rbind(eloA, eloH)
    elo <- elo[order(elo$Date), ]
    rm(eloA,eloH)
    
    teams <- unique(elo$Team_Name)
    
    # check
    stopifnot(all(teams %in% teamdata$team))
    # stopifnot(all(teamdata$team %in% teams)) # teams can be added/removed
    
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
    teamdata <- teamdata[match(names(curves), teamdata$team), ]
    teamdata$elo <- sapply(curves, function(x){ x$elo[nrow(x)] })
    # all(names(curves) == teamdata$team) # check
    
    for(i in 1:length(curves)){
        curves[[i]]$team <- names(curves)[i]
    }
    df <- do.call(rbind, curves)
    
    # teams need to be a factor sorted by final Elo
    df$team <- factor(df$team, levels = rev(names(curves)))
    df$abbr <- teamdata$team[match(df$team, teamdata$team)]
    
    cv <- teamdata[teamdata$team %in% df$team, ]
    cv <- cv$distinct[match(rev(names(curves)), cv$team)]
    
    # helper function for drawing vertical line
    vline <- function(x = 0, color = 1, width = 1) {
        list(
            type = "line", 
            y0 = 0, 
            y1 = 1, 
            yref = "paper",
            x0 = x, 
            x1 = x, 
            line = list(color = color, width = width)
        )
    }
    
    fig <- plot_ly(df, x=~Date, y=~elo, color = ~team, type = 'scatter', mode='lines', colors = cv, 
                   line = list(width = 3), hoverinfo = 'text',
                   text = ~paste(abbr, Date, round(elo))) |> 
        layout(legend = list(x = 100, y = 0.5), hovermode = 'x',
               yaxis = list(title = 'Elo Rating'))
    
    if(mode == 'light'){
        fig <- fig |> layout(plot_bgcolor = '#fffaf6', paper_bgcolor = '#fffaf6')
    }else{
        fig <- fig |> layout(plot_bgcolor = '#272935', paper_bgcolor = '#272935',
                             font = list(color = '#FFFFFF'))
    }
    if(today >= finale){
        fig <- fig |> layout(shapes = list(vline(x = finale, width = .5)))
    }
    fig
}


#############
# UTILITIES #
#############
spaceVertically <- function(y, height = .07, maxh = NULL, minh = NULL, maxiters = 100){
    laby <- y # adjust laby until labels look nice
    text(rep(1.1,10),laby, labels = 1:10)
    
    height <- .075 # fraction of vertical space reserved for each label
    maxiters <- 100
    h <- height * (par()$usr[4] - par()$usr[3]) # actual height
    if(is.null(maxh)){
        maxh <- par()$usr[4] - h/3
    }
    if(is.null(minh)){
        minh <- par()$usr[3] + h/3
    }
    
    converged <- FALSE
    iters <- 0
    while(!converged){
        iters <- iters+1
        print(iters)
        # calculate the "force" on each label
        f <- sapply(seq_along(laby), function(i){
            yi <- laby[i]
            fi <- 0
            if(any(abs(laby[-i] - yi) <= h)){
                ds <- abs(laby - yi)
                names(ds) <- seq_along(laby)
                ds <- sort(ds, decreasing = FALSE)[-1] # point is distance 0 from itself
                toconsider <- as.numeric(names(ds)[ds < h])
                # closest one exerts normal force
                # after that, force is successively halved
                for(j in seq_along(toconsider)){
                    d <- yi - laby[toconsider[j]]
                    fi <- fi + (1/2^(j-1)) * sign(d)*(h-abs(d))/2
                }
            }
            if(abs(yi - y[i]) > h*.01){
                fi <- fi + h*.01*sign(y[i]-yi)
            }
            return(fi)
        })
        
        # move the labels
        laby <- laby + f
        laby[laby < minh] <- minh
        laby[laby > maxh] <- maxh
        
        # check convergence
        converged <- sum(abs(f)) < h/100 | iters >= maxiters
    }
    return(laby)
}
