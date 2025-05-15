
# 2024 is the template
code <- readLines('pages/2024.qmd')

for(year in 1997:2023){
    # replace year
    new <- gsub('2024',year, code)
    
    # write new qmd
    writeLines(new, paste0('pages/',year,'.qmd'))
}
