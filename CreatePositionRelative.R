#### Load libraries and functions ####
#DataSet:
#players.dt

CreatePositionRel <- function(players.dt, NormalizeDuration = T){
 ####Check which games to delete####
  playerscount <- players.dt[,list(
    count = sum(duration/duration)
  ),.(matchid, position,teamid,win)]
  #56584 of them
  RemoveUnusualteams = subset(playerscount, (position =='BOT'& count!=2)|((position =='JUNGLE'& count!=1))|(position =='TOP'& count!=1)|(position =='MID'& count!=1))$matchid
  players = subset(players.dt, !(matchid %in% RemoveUnusualteams))  
  
  #Remove useless variables
  players =players[,!c('ss1','ss2','item1','item2','item3','item4','item5','item6','championid','trinket','largestkillingspree','largestmultikill','killingsprees','doublekills',
                       'triplekills','quadrakills','pentakills','legendarykills','largestcrit','totheal','dmgtoobj','totcctimedealt',
                       'magicdmgdealt','physicaldmgdealt','truedmgdealt','magicdmgtochamp', 
                       'physdmgtochamp','truedmgtochamp','magicdmgtaken','physdmgtaken','truedmgtaken','timecc',
                       'wardsbought','queueid','pinksbought')]
  
  #Check which games to delete
  playerscount <- players[,list(
    count = sum(duration/duration)
  ),.(matchid,teamid,win)]
  RemoveLess5 = subset(playerscount, count<5)$matchid
  players = subset(players, !(matchid %in% RemoveLess5))
  
  #Normalize duration?
  if(NormalizeDuration){
    #compute normalization (could use apply here for speed but w/e)
    for(sd in setdiff(colnames(players),c("matchid","win","turretkills","inhibkills","champlvl",
                                          "firstblood","duration",'role','position','id','player','Name','teamid'))){
      players <- players[,as.character(sd):=get(sd)/duration]
    }
    players <- players[,!"duration"]
  }
  
  #Players for each positon
  players.Jungle = subset(players, position == 'JUNGLE') #234774
  players.Bot = subset(players, position == 'BOT')      #469548
  players.Top = subset(players, position == 'TOP')     #234774
  players.Mid = subset(players, position == 'MID')     #234774
  #Only 117387 Games left
  players.Jungle = players.Jungle[order(players.Jungle[,'id']),]
  players.Bot = players.Bot[order(players.Bot[,'id']),] 
  players.Top = players.Top[order(players.Top[,'id']),]
  players.Mid = players.Mid[order(players.Mid[,'id']),]  
  
  #####

 #Separate along different teams
  #And remove non numeric column
  Jungle100 <- players.Jungle[teamid==100][ ,!c('role','position','Name','championid','player')]
  Jungle200 <- players.Jungle[teamid==200][ ,!c('role','position','Name','championid','player')]
  BOT100 <- players.Bot[teamid==100][ ,!c('role','position','Name','championid','player')] #234774
  BOT200 <- players.Bot[teamid==200][ ,!c('role','position','Name','championid','player')] #..
  MID100 <- players.Mid[teamid==100][ ,!c('role','position','Name','teamid','championid','player')] #117387
  MID200 <- players.Mid[teamid==200][ ,!c('role','position','Name','teamid','championid','player')] #117387
  TOP100 <- players.Top[teamid==100][ ,!c('role','position','Name','teamid','championid','player')] #..
  TOP200 <- players.Top[teamid==200][ ,!c('role','position','Name','teamid','championid','player')] #..
  
  
  
  Rel.Jungle = Jungle100[order(Jungle100[,'id']),] - Jungle200[order(Jungle200[,'id']),]
  Rel.BOT = BOT100[order(BOT100[,'id']),]-BOT200[order(BOT200[,'id']),]
  Rel.TOP = TOP100[order(TOP100[,'id']),]-TOP200[order(TOP200[,'id']),]
  Rel.MID = MID100[order(MID100[,'id']),]-MID200[order(MID200[,'id']),]
  
  # restore some parameters 
  #matchid
  Rel.BOT[, matchid:= BOT100[order(BOT100[,'id']),]$matchid]
  Rel.Jungle[, matchid:= Jungle100[order(Jungle100[,'id']),]$matchid]
  Rel.MID[, matchid:= MID100[order(MID100[,'id']),]$matchid]
  Rel.TOP[, matchid:= TOP100[order(TOP100[,'id']),]$matchid]
  #win
  Rel.BOT[, win:= BOT100[order(BOT100[,'id']),]$win]
  Rel.Jungle[, win:= Jungle100[order(Jungle100[,'id']),]$win]
  Rel.MID[, win:= MID100[order(MID100[,'id']),]$win]
  Rel.TOP[, win:= TOP100[order(TOP100[,'id']),]$win]
  
  #remove 'id', 'teamid'
  Rel.BOT = Rel.BOT[,!c('teamid','id','duration','firstblood')]
  Rel.Jungle = Rel.Jungle[,!c('teamid','id','duration','firstblood')]
  Rel.MID = Rel.MID[,!c('teamid','id','duration','firstblood')]
  Rel.TOP = Rel.TOP[,!c('teamid','id','duration','firstblood')]
  
  #For the BOT, take the mean of the 2 players
  Rel.BOT = aggregate(Rel.BOT, list(Rel.BOT$matchid,Rel.BOT$win), FUN=mean)

  
  #Need to re-name the column before merging
  colnames(Rel.BOT) <- c("matchid","win",paste0("BOT.",colnames(Rel.BOT[,-c(1,2)]), sep = "" ))
  colnames(Rel.MID) <- paste0("MID.",colnames(Rel.MID), sep = "")
  colnames(Rel.TOP) <- paste0("TOP.",colnames(Rel.TOP), sep = "")
  colnames(Rel.Jungle) <- paste0("Jungle.",colnames(Rel.Jungle), sep = "")
  
  
  #Merge them by matchid&win:
  Position.Rel = cbind(Rel.BOT,Rel.TOP,Rel.MID,Rel.Jungle)
  #Set win as numeric
  Position.Rel = transform(Position.Rel, win = as.numeric(win))
  #Remove Bot.matchid, Bot.win, top.matchid, top.win... [,!c('BOT.matchid',..)] ne marche pas?!
  Position.Rel$BOT.matchid <- NULL
  Position.Rel$BOT.win <- NULL
  Position.Rel$TOP.matchid <- NULL
  Position.Rel$TOP.win <- NULL
  Position.Rel$MID.matchid <- NULL
  Position.Rel$MID.win <- NULL
  Position.Rel$Jungle.matchid <- NULL
  Position.Rel$Jungle.win <- NULL
    #delete some other column (Linear dependencies)
  Position.Rel$BOT.ownjunglekills <- NULL
  Position.Rel$Bot.neutralminionskilled <- NULL
  Position.Rel$TOP.neutralminionskilled <- NULL
  Position.Rel$MID.neutralminionskilled <- NULL
  Position.Rel$Jungle.neutralminionskilled <- NULL
  
  #Delete Gold & inhibkills
  Position.Rel$BOT.goldearned <- NULL
  Position.Rel$BOT.goldspent <- NULL
  Position.Rel$TOP.goldearned <- NULL
  Position.Rel$TOP.goldspent <- NULL
  Position.Rel$MID.goldearned <- NULL
  Position.Rel$MID.goldspent <- NULL
  Position.Rel$Jungle.goldearned <- NULL
  Position.Rel$Jungle.goldspent <- NULL
  Position.Rel$BOT.inhibkills <- NULL
  Position.Rel$TOP.inhibkills <- NULL
  Position.Rel$MID.inhibkills <- NULL
  Position.Rel$Jungle.inhibkills <- NULL
  Position.Rel$BOT.champlvl <- NULL
  
  
  return(Position.Rel)
}

Position.Rel = CreatePositionRel(players.dt,T)

