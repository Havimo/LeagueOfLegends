#loading packages and sourcing external files

#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("leaps")
#install.packages("rjson")

#setwd('~/SLProject')
setwd('C:/Users/Ausra/Documents/SLProject')

library(data.table)
library(ggplot2)
library(scales)
library(leaps)
library(rjson)
library(glmnet)
library(MASS)
source('plot.functions.R')
source('models.R')
source('relativeDataset.R')
source('Normalize.R')

#<<- assign to global environnement
ReadData <- function(){
  match.dt <<- fread('matches.csv')
  #merging the two datasets removes 3 players from the 'participants' that do not appear in 'stats'. 
  #My guess is they never connected to the game.
  players.dt <<- merge(fread('participants.csv'),rbind(fread('stats1.csv'),fread('stats2.csv')),by='id')
  teambans.dt <<- fread('teambans.csv')
  teamstats.dt <<- fread('teamstats.csv')
}

GetJsonFiles <- function(){
  #this function parses the Json files directly from the data dragon LoL API website and formats them into a R list.
  #it allows us to map the itemid, summoner spell id and champion id to their corresponding name.
  
  summoner.json <- fromJSON(file='http://ddragon.leagueoflegends.com/cdn/6.24.1/data/en_US/summoner.json')
  summoner.dt <- c()
  for(obj in summoner.json$data){
    summoner.dt <-rbind(summoner.dt,c(obj$name,obj$key))
  }
  summoner.dt <- as.data.table(summoner.dt)
  setnames(summoner.dt,c('Name','id'))
  
  champion.json <- fromJSON(file='http://ddragon.leagueoflegends.com/cdn/6.24.1/data/en_US/champion.json')
  champion.dt <- c()
  for(obj in champion.json$data){
    champion.dt <- rbind(champion.dt,c(obj$name,obj$key))
  }
  champion.dt <- as.data.table(champion.dt)
  setnames(champion.dt,c('Name','id'))
  
  item.json <- fromJSON(file='http://ddragon.leagueoflegends.com/cdn/6.24.1/data/en_US/item.json')
  item.dt <- c()
  for(obj in names(item.json$data)){
    item.dt <- rbind(item.dt,c(item.json$data[[obj]]$name,as.numeric(obj)))
  }
  item.dt <- as.data.table(item.dt)
  setnames(item.dt,c('Name','id'))
  
  return(list('item' = item.dt,'summoner' = summoner.dt, 'champions' = champion.dt))
}

FormatPlayerData <- function(players.dt){
  #removing buggy columns
  #players.dt[,wardsbought := NULL]#some errors in its fields
  #players.dt[,timecc := NULL]#always0
  #players.dt[,neutralminionskilled:=NULL] #this is ownjungle + enemyjungle, so having all 3 results in a non full rank matrix.
  del <- c("wardsbought", "timecc", "neutralminionskilled")
  players.dt[, !(names(players.dt) %in% del)]
  
  #fixing one row with missing data:
  id.bugged <- na.omit(players.dt,invert=T)$id
  players.dt[id==id.bugged,totdmgdealt := magicdmgdealt+physicaldmgdealt]
  players.dt[id==id.bugged,truedmgdealt := 0]
  
  #adding duration for ease of computation later on
  players.dt <- merge(players.dt,match.dt[,.(id,duration,queueid)],by.x='matchid',by.y='id')
  
  #adding the teamid to merge easily with the other datasets
  players.dt[player <=5,teamid := 100]
  players.dt[player >=6,teamid := 200]
  
  #removing the 3v3 games and the weird 3 games with only 8 and 9players(i.e. the 3 participants missing above)
  players.dt <- players.dt[queueid %in% c(420,440)]
  players.dt <- players.dt[!(matchid %in% players.dt[,.N,.(matchid)][N<10]$matchid)]
  
  #transforming caracter-variables in numerical
  # put the roles as numerical values 
  players.dt[, roleNONE := as.numeric(players.dt$role=="NONE")]
  players.dt[, roleSOLO := as.numeric(players.dt$role=="SOLO")]
  players.dt[, roleDUO_CARRY := as.numeric(players.dt$role=="DUO_CARRY")]
  players.dt[, roleDUO_SUPPORT := as.numeric(players.dt$role=="DUO_SUPPORT")]
  players.dt[, roleDUO := as.numeric((players.dt$role=="DUO"))]
  players.dt[, role:= NULL]
  
  # put the positions as numerical values 
  players.dt[, positionBOT := as.numeric(players.dt$position=="BOT")]
  players.dt[, positionMID := as.numeric(players.dt$position=="MID")]
  players.dt[, positionTOP := as.numeric(players.dt$position=="TOP")]
  players.dt[, positionJUNGLE := as.numeric(players.dt$position=="JUNGLE")]
  players.dt[, position:= NULL]
  
  #diagnostic to check I did it right :) 
  # testing <- merge(teamstats.dt[,.(matchid,teamid,firstblood)],players.dt[,sum(firstblood),.(matchid,teamid)],by=c("matchid","teamid"))
  # players.dt[matchid %in% testing[firstblood!=V1]$matchid, .N,.(matchid)]
  # players.dt[, .N,.(matchid)][,.N,N]
  
  return(players.dt)
}

CreateTeamData <- function(players.dt){
  #create the "team" dataset, i.e. we aggregate the players metric to a team level, by summing, averaging or taking the min max.
  team.dt <- players.dt[,list(
      kills = sum(kills,na.rm=T),
      deaths = sum(deaths,na.rm=T),
      assists = sum(assists,na.rm=T),
      largestkillingspree = sum(largestkillingspree,na.rm=T),
      largestmultikill = sum(largestmultikill,na.rm=T),
      killingsprees = sum(killingsprees,na.rm=T),
      longesttimespentliving = sum(longesttimespentliving,na.rm=T),
      doublekills = sum(doublekills,na.rm=T),
      triplekills = sum(triplekills,na.rm=T),
      quadrakills = sum(quadrakills,na.rm=T),
      pentakills = sum(pentakills,na.rm=T),
      legendarykills = sum(legendarykills,na.rm=T),
      totdmgdealt = sum(totdmgdealt,na.rm=T),
      magicdmgdealt = sum(magicdmgdealt,na.rm=T),
      physicaldmgdealt = sum(physicaldmgdealt,na.rm=T),
      truedmgdealt = sum(truedmgdealt,na.rm=T),
      largestcrit = max(largestcrit,na.rm=T),
      totdmgtochamp = sum(totdmgtochamp,na.rm=T),
      magicdmgtochamp = sum(magicdmgtochamp,na.rm=T),
      physdmgtochamp = sum(physdmgtochamp,na.rm=T),
      truedmgtochamp = sum(truedmgtochamp,na.rm=T),
      totheal = sum(totheal,na.rm=T),
      totunitshealed = sum(totunitshealed,na.rm=T),
      dmgselfmit = sum(dmgselfmit,na.rm=T),
      dmgtoobj = sum(dmgtoobj,na.rm=T),
      dmgtoturrets = sum(dmgtoturrets,na.rm=T),
      visionscore = sum(visionscore,na.rm=T),
      #timecc = sum(timecc,na.rm=T),
      totdmgtaken = sum(totdmgtaken,na.rm=T),
      magicdmgtaken = sum(magicdmgtaken,na.rm=T),
      physdmgtaken = sum(physdmgtaken,na.rm=T),
      truedmgtaken = sum(truedmgtaken,na.rm=T),
      goldearned = sum(goldearned,na.rm=T),
      goldspent = sum(goldspent,na.rm=T),
      totminionskilled = sum(totminionskilled,na.rm=T),
      #neutralminionskilled = sum(neutralminionskilled,na.rm=T),
      ownjunglekills = sum(ownjunglekills,na.rm=T),
      enemyjunglekills = sum(enemyjunglekills,na.rm=T),
      totcctimedealt = sum(totcctimedealt,na.rm=T),
      maxchamplvl = max(champlvl,na.rm=T),
      avgchamplvl = sum(champlvl,na.rm=T)/5,
      minchamplvl = min(champlvl,na.rm=T),
      pinksbought = sum(pinksbought,na.rm=T),
      wardsplaced = sum(wardsplaced,na.rm=T),
      wardskilled = sum(wardskilled,na.rm=T)
  ),.(matchid,teamid,duration,win)]
  
  #get team specific metric from teamstats
  team.dt <- merge(teamstats.dt,team.dt,by=c('matchid','teamid'))
  
  return(team.dt)
}


#### Execution ####
# Data
ReadData()
players.dt <- FormatPlayerData(players.dt)
teams.dt <- CreateTeamData(players.dt)
id.mapping.list <- GetJsonFiles()

# Check for missing values
sum(is.na(players.dt))
sum(is.na(teams.dt))


################### Create train/test set for team.dt ###########
set.seed(0)
# Which ratio of the data used for training, testing, validating
train.ratio <- 0.5
test.ratio <- 0.4
validate.ratio <- 1 - train.ratio - test.ratio

# Deduce the number of samples needed in our training set
nb.train <- floor(train.ratio*dim(teams.dt)[1])
# Define which observations will be used for our training set
selection <- sample(c(rep(TRUE, times=nb.train), rep(FALSE, times=dim(teams.dt)[1]-nb.train)), dim(teams.dt)[1])
train.teams.dt <- teams.dt[selection]
rest <- teams.dt[!selection]

# Deduce the number of samples needed in our testing set
nb.test <- floor(test.ratio*dim(teams.dt)[1])
# Define which observations will be used for our training set
selection <- sample(c(rep(TRUE, times=nb.test), rep(FALSE, times=dim(rest)[1]-nb.test)), dim(rest)[1])
test.teams.dt <- rest[selection]

# Define which observations will be used for our validation set 
validate.teams.dt <- rest[!selection]

################### Create train/test set for rel.team.dt ###########

# Construct rel.team.dt
rel.teams.dt <- relativeDataset(teams.dt)

# Deduce the number of samples needed in our training set
nb.train.rel <- floor(train.ratio*dim(rel.teams.dt)[1])
# Define which observations will be used for our training set
selection <- sample(c(rep(TRUE, times=nb.train.rel), rep(FALSE, times=dim(rel.teams.dt)[1]-nb.train.rel)), dim(rel.teams.dt)[1])
train.rel.teams.dt <- rel.teams.dt[selection]
rest <- rel.teams.dt[!selection]

# Deduce the number of samples needed in our testing set
nb.test.rel <- floor(test.ratio*dim(rel.teams.dt)[1])
# Define which observations will be used for our training set
selection <- sample(c(rep(TRUE, times=nb.test.rel), rep(FALSE, times=dim(rest)[1]-nb.test.rel)), dim(rest)[1])
test.rel.teams.dt <- rest[selection]

# Define which observations will be used for our validation set 
validate.rel.teams.dt <- rest[!selection]


####################### OTHER COMPUTINGS ########################
model.team <- CompleteTeamModel(teams.dt)
model.player <- CompletePlayerModel(players.dt,T)
k.fold.results <- CompleteTeamModel_kCV(teams.dt)
