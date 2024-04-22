year <- 2023

# team colors from: https://teamcolorcodes.com/wnba-color-codes/
# can go back as far as 2018 with these teams
teamdata <- read.csv(text = 'team,name,color1,color2
ATL,Atlanta Dream,#C8102E,#418FDE
CHI,Chicago Sky,#418FDE,#FFCD00
CON,Connecticut Sun,#A6192E,#041E42
DAL,Dallas Wings,#0C2340,#C4D600
IND,Indiana Fever,#041E42,#C8102E
LV,Las Vegas Aces,#000000,#BA0C2F
LA,Los Angeles Sparks,#702F8A,#FFC72C
MIN,Minnesota Lynx,#0C2340,#236192
NY,New York Liberty,#6ECEB2,#000000
PHX,Phoenix Mercury,#201747,#CB6015
SEA,Seattle Storm,#2C5234,#FBE122
WSH,Washington Mystics,#0C2340,#C8102E')

require(rvest)
sched <- html_table(read_html(paste0('https://www.basketball-reference.com/wnba/years/',year,'_games.html')))[[1]]
sched$Date <- as.Date(sched$Date, format = "%a, %b %d, %Y")
names(sched)[3] <- 'PTSvis'
names(sched)[5] <- 'PTShome'
# remove playoffs
if(any(sched$`Visitor/Neutral` == 'Playoffs')){
    sched <- sched[1:(which.max(sched$`Visitor/Neutral` == 'Playoffs')-1), ]
}
sched$PTSvis <- as.numeric(sched$PTSvis)
sched$PTShome <- as.numeric(sched$PTShome)
teams <- unique(c(sched$`Visitor/Neutral`, sched$`Home/Neutral`))
# check
stopifnot(all(teams %in% teamdata$name))
stopifnot(all(teamdata$name %in% teams))


# QC stuff. missing values, ties
if(any(!is.na(sched$PTShome) & (sched$PTShome==sched$PTSvis))){
    stop('ties detected')
}

curves <- lapply(teams, function(team){
    away <- sched[which(sched$`Visitor/Neutral` == team), ]
    away$WL <- away$PTSvis > away$PTShome
    home <- sched[which(sched$`Home/Neutral` == team), ]
    home$WL <- home$PTShome > home$PTSvis
    curve <- rbind(away, home)
    curve <- curve[order(curve$Date), ]
    curve$margin <- cumsum(c(-1,1)[factor(curve$WL)])
    
    curve <- curve[, c('Date','margin')]
    supp <- data.frame(Date = curve$Date-1,
                       margin = c(0,curve$margin[-nrow(curve)]))
    supp <- supp[!supp$Date %in% curve$Date, ]
    curve <- rbind(supp,curve)
    curve <- curve[order(curve$Date), ]
    
    return(curve[!is.na(curve$margin), ]) # only count played games
})
names(curves) <- teams
# make sure curves start the day before opening day and don't end before the current date
opening <- min(do.call(rbind, curves)$Date)
today <- max(do.call(rbind, curves)$Date)
for(i in seq_along(curves)){
    curve <- curves[[i]]
    if(! opening %in% curve$Date){
        curve <- rbind(c(opening, 0), curve)
    }
    if(! today %in% curve$Date){
        curve <- rbind(curve, c(today, curve$margin[nrow(curve)]))
    }
    curves[[i]] <- curve
}
curves <- curves[order(sapply(curves, function(x){ x$margin[nrow(x)] }))]
teamdata <- teamdata[match(names(curves), teamdata$name), ]
teamdata$margin <- sapply(curves, function(x){ x$margin[nrow(x)] })
# all(names(curves) == teamdata$name) # check


# check against actual standings
standings <- html_table(read_html(paste0('https://www.basketball-reference.com/wnba/years/',year,'_standings.html')))[[1]]
standings$Team <- gsub('[*]','', standings$Team)
for(team in teams){
    s.i <- which(standings$Team == team)
    c.i <- which(names(curves) == team)
    stopifnot(curves[[c.i]]$margin[nrow(curves[[c.i]])] == standings$W[s.i] - standings$L[s.i])
}

# build legend df with blank row to separate non/playoff teams
lgnd <- teamdata[nrow(teamdata):1, ]
lgnd <- merge(lgnd, standings, by.x = 'name', by.y = 'Team')
lgnd <- lgnd[match(names(curves), lgnd$name), ]
lgnd <- lgnd[nrow(lgnd):1, ]
lgnd$nameRecord <- paste0(lgnd$name,' (',lgnd$W,'-',lgnd$L,')')
lgnd <- rbind(lgnd[1:8,], NA, lgnd[9:nrow(lgnd), ])


layout(matrix(c(1,1,1,2), nrow=1))
par(mar = c(5,4,4,0)+.1)

plot(range(do.call(rbind, curves)$Date), range(sapply(curves,function(x){x$margin})), col = 'transparent',
     xlab = 'Date', ylab='', main = paste(year, 'WNBA Standings'), las = 1)
rect(min(sched$Date)-9999,-9999,max(sched$Date)+9999,9999, col='grey90')
abline(h = 0)
abline(h = seq(-100,-5, by=5), lty=2, col = rgb(0,0,0,.3))
abline(h = seq(5,100, by=5), lty=2, col = rgb(0,0,0,.3))
for(i in 1:length(curves)){
    cc <- as.character(teamdata[teamdata$name == names(curves)[i], ][,c('color1','color2')])
    #points(curves[[i]]$Date, curves[[i]]$margin, col=cc[2], pch=16, cex=.5)
    lines(curves[[i]]$Date, curves[[i]]$margin, col=cc[2], lwd=4)
    lines(curves[[i]]$Date, curves[[i]]$margin, col=cc[1], lwd=3)
    points(curves[[i]]$Date[nrow(curves[[i]])], curves[[i]]$margin[nrow(curves[[i]])], col=cc[2], pch=16, cex=2)
    points(curves[[i]]$Date[nrow(curves[[i]])], curves[[i]]$margin[nrow(curves[[i]])], col=cc[1], pch=16, cex = 1.5)
}

par(mar = c(5,0,4,2)+.1)
plot.new()
legend('left', legend = rep('', nrow(lgnd)), bty='n', lwd=3, col = lgnd$color2, cex = 1)
legend('left', legend = lgnd$nameRecord, 
       bty='n', lwd=2, col = lgnd$color1, cex = 1)

# layout(1)
# par(mar = c(5,4,4,2)+.1)



