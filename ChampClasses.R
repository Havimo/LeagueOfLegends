#Quelles sont les classes de champions(heal, tank..)?


CreateChampStats <- function(players.dt){
  #create the "Champion" dataset, for only some of the variables (taking the mean)
  ChampStat.dt <- players.dt[,list(
    kills = mean(kills/duration,na.rm=T),
    deaths = mean(deaths/duration,na.rm=T),
    assists = mean(assists/duration,na.rm=T),
#    largestkillingspree = mean(largestkillingspree,na.rm=T),
#    largestmultikill = mean(largestmultikill,na.rm=T),
#    killingsprees = mean(killingsprees,na.rm=T),
    longesttimespentliving = mean(longesttimespentliving/duration,na.rm=T),
#    doublekills = mean(doublekills,na.rm=T),
#    triplekills = mean(triplekills,na.rm=T),
#    quadrakills = mean(quadrakills,na.rm=T),
#    pentakills = mean(pentakills,na.rm=T),
#    legendarykills = mean(legendarykills,na.rm=T),
    totdmgdealt = mean(totdmgdealt/duration,na.rm=T),
#    magicdmgdealt = mean(magicdmgdealt,na.rm=T),
#    physicaldmgdealt = mean(physicaldmgdealt,na.rm=T),
#    truedmgdealt = mean(truedmgdealt,na.rm=T),
#    largestcrit = max(largestcrit,na.rm=T),
    totdmgtochamp = mean(totdmgtochamp/duration,na.rm=T),
#    magicdmgtochamp = mean(magicdmgtochamp,na.rm=T),
#    physdmgtochamp = mean(physdmgtochamp,na.rm=T),
#    truedmgtochamp = mean(truedmgtochamp,na.rm=T),
    totheal = mean(totheal/duration,na.rm=T),
    totunitshealed = mean(totunitshealed/duration,na.rm=T),
    dmgselfmit = mean(dmgselfmit/duration,na.rm=T),
    dmgtoobj = mean(dmgtoobj/duration,na.rm=T),
    dmgtoturrets = mean(dmgtoturrets/duration,na.rm=T),
    visionscore = mean(visionscore/duration,na.rm=T),
    #timecc = sum(timecc,na.rm=T),
    totdmgtaken = mean(totdmgtaken/duration,na.rm=T),
#    magicdmgtaken = mean(magicdmgtaken,na.rm=T),
#    physdmgtaken = mean(physdmgtaken,na.rm=T),
#    truedmgtaken = mean(truedmgtaken,na.rm=T),
    goldearned = mean(goldearned/duration,na.rm=T),
    goldspent = mean(goldspent/duration,na.rm=T),
    totminionskilled = mean(totminionskilled/duration,na.rm=T),
#    neutralminionskilled = mean(neutralminionskilled,na.rm=T),
    ownjunglekills = mean(ownjunglekills/duration,na.rm=T),
    enemyjunglekills = mean(enemyjunglekills/duration,na.rm=T),
#    totcctimedealt = mean(totcctimedealt,na.rm=T),
    # maxchamplvl = mean(champlvl,na.rm=T),
    #  avgchamplvl = mean(champlvl,na.rm=T)/5,
    #  minchamplvl = mean(champlvl,na.rm=T),
#    pinksbought = mean(pinksbought,na.rm=T),
    wardsplaced = mean(wardsplaced/duration,na.rm=T),
    wardskilled = mean(wardskilled/duration,na.rm=T),
    NbrGames = sum(duration/duration, na.rm=T)
    
  ),.(Name)]
  
  return(ChampStat.dt)
}

#To re-attribute the name to each champion
#ChampStat.dt <- CreateChampStats(players.dt)
#View(ChampStat.dt)
#Keep only datas for (Champion,role) played more than 100 times
#ChampStat.dt = subset(ChampStat.dt, NbrGames > 100)


#PCA:
#PCA:
#X = as.matrix(ChampStat.dt[,!c("Name","win","NbrGames")])
#pca <- prcomp(X, scale = T)
#PlotUsing ggbiplot 
#Nice plot
#g <- ggbiplot(pca, obs.scale = 1, var.scale =1,
#              labels = ChampStat.dt$Name)
#g <- g + scale_color_discrete(name = 'Position')
#print(g)
#Try to do the same for all lanes (mid,top,bot..)


#biplot(pca, cex=c(0.5, 0.8),xlabs=ChampStat.dt$Name)
#Semle indiquer au moins 3 types/roles de champions 
#En haut: Jungle + Tanks
#Bas a droite : Vision
#Bas gauche (autres)


