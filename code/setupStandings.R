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

teamcolors <- read.csv(text = 'team,name,color1,color2
ATL,Atlanta Dream,#C8102E,#418FDE
CHI,Chicago Sky,#418FDE,#FFCD00
CON,Connecticut Sun,#FC4C02,#0C2340
DAL,Dallas Wings,#0C2340,#C4D600
IND,Indiana Fever,#041E42,#FFCD00
LVA,Las Vegas Aces,#000000,#8A8D8F
LAS,Los Angeles Sparks,#702F8A,#FFC72C
MIN,Minnesota Lynx,#0C2340,#236192
NYL,New York Liberty,#6ECEB2,#000000
PHX,Phoenix Mercury,#201747,#CB6015
SEA,Seattle Storm,#2C5234,#FBE122
WSH,Washington Mystics,#C8102E,#0C2340
GSV,Golden State Valkyries,#AD96DC,#010101
CHA,Charlotte Sting,#00778B,#280071
CLE,Cleveland Rockers,#280071,#009FDF
DET,Detroit Shock,#003DA5,#041E42
HOU,Houston Comets,#BA0C2F,#041E42
MIA,Miami Sol,#A6192E,#010101
ORL,Orlando Miracle,#0057B7,#010101
POR,Portland Fire,#C8102E,#010101
SAC,Sacramento Monarchs,#753BBD,#010101
SAS,San Antonio Silver Stars,#010101,#8D9093
TUL,Tulsa Shock,#FFB81C,#010101
UTA,Utah Starzz,#006271,#753BBD')

teamcolors$distinct <- teamcolors$color1
teamcolors$distinct[teamcolors$name=='Seattle Storm'] <- '#78BE21'
teamcolors$distinct[teamcolors$name=='Minnesota Lynx'] <- '#236192'
teamcolors$distinct[teamcolors$name=='Dallas Wings'] <- '#C4D600'
teamcolors$distinct[teamcolors$name=='Indiana Fever'] <- '#FFCD00'
teamcolors$distinct[teamcolors$name=='Phoenix Mercury'] <- '#CB6015'
teamcolors$distinct[teamcolors$name=='Washington Mystics'] <- '#8A8D8F'

# grid <- cbind(rep(1:6,each=4), rep(1:4, times=6))
# ord <- sample(nrow(teamcolors))
# plot(grid, pch=16, col = teamcolors$distinct[ord])
# text(grid, labels = teamcolors$team[ord], col = teamcolors$distinct[ord])

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
    stopifnot(all(teams %in% teamcolors$name))
    # stopifnot(all(teamcolors$name %in% teams)) # teams can be added/removed
    
    
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
    teamdata <- teamcolors[match(names(curves), teamcolors$name), ]
    teamdata$margin <- sapply(curves, function(x){ x$margin[nrow(x)] })
    teamdata$W <- sapply(curves, function(x){ sum(x$WL, na.rm=TRUE) })
    teamdata$L <- sapply(curves, function(x){ sum(!x$WL, na.rm=TRUE) })
    # all(names(curves) == teamdata$name) # check
    
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
    # lgnd <- merge(lgnd, standings, by.x = 'name', by.y = 'Team')
    lgnd <- lgnd[match(names(curves), lgnd$name), ]
    lgnd <- lgnd[nrow(lgnd):1, ]
    lgnd$nameRecord <- paste0(lgnd$name,' (',lgnd$W,'-',lgnd$L,')')
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
        cc <- as.character(teamdata[teamdata$name == names(curves)[i], ][,c('color1','color2')])
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
