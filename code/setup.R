
# LOAD GAME DATA
allgames <- readRDS('data/allgames97_25.rds')

#############
# TEAM DATA #
#############
teamdata <- read.csv(text = 'abbr,team,color1,color2,franchise
ATL,Atlanta Dream,#C8102E,#418FDE,18
CHI,Chicago Sky,#418FDE,#FFCD00,17
CON,Connecticut Sun,#FC4C02,#0C2340,12
DAL,Dallas Wings,#0C2340,#C4D600,9
IND,Indiana Fever,#041E42,#FFCD00,13
LVA,Las Vegas Aces,#000000,#8A8D8F,8
LAS,Los Angeles Sparks,#702F8A,#FFC72C,4
MIN,Minnesota Lynx,#0C2340,#236192,11
NYL,New York Liberty,#6ECEB2,#000000,5
PHX,Phoenix Mercury,#201747,#CB6015,6
SEA,Seattle Storm,#2C5234,#FBE122,16
WSH,Washington Mystics,#C8102E,#0C2340,10
GSV,Golden State Valkyries,#AD96DC,#010101,19
CHA,Charlotte Sting,#00778B,#280071,1
CLE,Cleveland Rockers,#280071,#009FDF,2
DET,Detroit Shock,#003DA5,#041E42,9
HOU,Houston Comets,#BA0C2F,#041E42,3
MIA,Miami Sol,#A6192E,#010101,14
ORL,Orlando Miracle,#0057B7,#010101,12
POR,Portland Fire,#C8102E,#010101,15
SAC,Sacramento Monarchs,#753BBD,#010101,7
SAS,San Antonio Silver Stars,#010101,#8D9093,8
TUL,Tulsa Shock,#FFB81C,#010101,9
UTA,Utah Starzz,#006271,#753BBD,8
TOR,Toronto Tempo,#612C51,#B8CCEA,20')

teamdata$distinct <- teamdata$color1
teamdata$distinct[teamdata$name=='Seattle Storm'] <- '#78BE21'
teamdata$distinct[teamdata$name=='Minnesota Lynx'] <- '#236192'
teamdata$distinct[teamdata$name=='Dallas Wings'] <- '#C4D600'
teamdata$distinct[teamdata$name=='Indiana Fever'] <- '#FFCD00'
teamdata$distinct[teamdata$name=='Phoenix Mercury'] <- '#CB6015'
teamdata$distinct[teamdata$name=='Washington Mystics'] <- '#8A8D8F'

# Franchise info
# Teams and the league were collectively owned by the NBA until the end of 2002
# [...] This led to two teams moving: [...] Orlando moved to Connecticut
# allgames$home_team_abbr[allgames$`Home/Neutral` %in% c("Orlando Miracle","Connecticut Sun")] <- "CON"
# allgames$away_team_abbr[allgames$`Visitor/Neutral` %in% c("Orlando Miracle","Connecticut Sun")] <- "CON"
# Teams and the league were collectively owned by the NBA until the end of 2002
# [...] This led to two teams moving: Utah moved to San Antonio [...] in 2018
# the San Antonio Stars went to Nevada, becoming the Las Vegas Aces.
# allgames$home_team_abbr[allgames$`Home/Neutral` %in% c("Utah Starzz","San Antonio Silver Stars","Las Vegas Aces")] <- "LVA"
# allgames$away_team_abbr[allgames$`Visitor/Neutral` %in% c("Utah Starzz","San Antonio Silver Stars","Las Vegas Aces")] <- "LVA"
# The Detroit Shock was the sister team of the Pistons until the teams' owner
# sold the Shock to investors who moved the team to Tulsa, Oklahoma. The
# franchise relocated again in 2016, this time to the Dallas–Fort Worth metro
# area to become the Dallas Wings.
# allgames$home_team_abbr[allgames$`Home/Neutral` %in% c("Detroit Shock","Tulsa Shock","Dallas Wings")] <- "DAL"
# allgames$away_team_abbr[allgames$`Visitor/Neutral` %in% c("Detroit Shock","Tulsa Shock","Dallas Wings")] <- "DAL"


# grid <- cbind(rep(1:6,each=4), rep(1:4, times=6))
# ord <- sample(nrow(teamdata))
# plot(grid, pch=16, col = teamdata$distinct[ord])
# text(grid, labels = teamdata$team[ord], col = teamdata$distinct[ord])






