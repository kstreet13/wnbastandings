source('code/setupStandings.R')

# Dallas Wings are the template (multiple moves)
code <- readLines('pages/Dallas Wings.qmd')

teamnames <- teamcolors$name
teamnames <- teamnames[teamnames != 'Golden State Valkyries'] # no history yet

for(team in teamnames){
    # replace team
    new <- gsub('Dallas Wings',team, code)
    
    # write new qmd
    writeLines(new, paste0('pages/',team,'.qmd'))
}

