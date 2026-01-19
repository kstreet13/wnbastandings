source('code/setupStandings.R')

# Dallas Wings are the template (multiple moves)
code <- readLines('pages/Dallas Wings.qmd')

teamnames <- teamcolors$name
teamnames <- teamnames[teamnames != 'Toronto Tempo'] # no history yet
# figure out how to handle Portland Fire

for(team in teamnames){
    # replace team
    new <- gsub('Dallas Wings',team, code)
    
    # write new qmd
    writeLines(new, paste0('pages/',team,'.qmd'))
}

