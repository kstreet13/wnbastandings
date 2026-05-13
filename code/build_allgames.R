# This code calculates the elo data for all previous years
# during season: all years except the current season
# during offseason: all years
# This produces "allgames.rds", which is used as an input for setup.R

source('code/setup.R')
rm(allgames) # setup.R includes loading allgames.rds, which we are about to build

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

# Abbreviations and franchise numbers
stopifnot(all(allgames$`Home/Neutral` %in% teamdata$team))
stopifnot(all(allgames$`Visitor/Neutral` %in% teamdata$team))
allgames$home_abbr <- teamdata$abbr[match(allgames$`Home/Neutral`, teamdata$team)]
allgames$away_abbr <- teamdata$abbr[match(allgames$`Visitor/Neutral`, teamdata$team)]
allgames$home_franchise <- teamdata$franchise[match(allgames$`Home/Neutral`, teamdata$team)]
allgames$away_franchise <- teamdata$franchise[match(allgames$`Visitor/Neutral`, teamdata$team)]
stopifnot(!any(is.na(allgames$home_abbr)))
stopifnot(!any(is.na(allgames$away_abbr)))
stopifnot(!any(is.na(allgames$home_franchise)))
stopifnot(!any(is.na(allgames$away_franchise)))

# add columns for pre- and post-game Elo rating, as well as home team win prob.
allgames$away_elo_pre <- allgames$away_elo_post <- allgames$home_elo_pre <- allgames$home_elo_post <- allgames$homeWinProb <- NA

# add columns for home/away game number (for the season)
allgames$home_gameNo <- sapply(1:nrow(allgames), function(ii){
    ytd <- allgames[which(allgames$season == allgames$season[ii] & allgames$Date < allgames$Date[ii] &
                              (allgames$away_abbr == allgames$home_abbr[ii] | allgames$home_abbr == allgames$home_abbr[ii])), ]
    return(nrow(ytd)+1)
})
allgames$away_gameNo <- sapply(1:nrow(allgames), function(ii){
    ytd <- allgames[which(allgames$season == allgames$season[ii] & allgames$Date < allgames$Date[ii] &
                              (allgames$away_abbr == allgames$away_abbr[ii] | allgames$home_abbr == allgames$away_abbr[ii])), ]
    return(nrow(ytd)+1)
})

################
# CALCULATIONS #
################
source('code/utils.R')

for(season in 1997:2025){
    for(gi in 1:sum(allgames$season == season)){
        idx <- which(allgames$season == season)[gi]
        game <- allgames[idx,]
        
        # search for previous Elo ratings
        game$away_elo_pre <- prevElo(game$away_franchise, game$Date, game$season)
        game$home_elo_pre <- prevElo(game$home_franchise, game$Date, game$season)
        
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
