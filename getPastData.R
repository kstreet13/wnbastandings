
for(year in 2018:2023){
    sched <- getSchedule(year)
    saveRDS(sched, file = paste0('data/sched',year,'.rds'))
    
    standings <- getStandings(year)
    saveRDS(standings, file = paste0('data/standings',year,'.rds'))
}

