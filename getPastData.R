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

# can't actually run all at once, basketball-reference gets mad
for(year in 1997:2024){
    sched <- getSchedule(year)
    saveRDS(sched, file = paste0('data/sched',year,'.rds'))
    
    standings <- getStandings(year)
    saveRDS(standings, file = paste0('data/standings',year,'.rds'))
}

