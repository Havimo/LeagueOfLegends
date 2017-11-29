library(stringr)
library(devtools)
install_github("ggbiplot","vqv")
library(ggbiplot)

#Create a dataset contening the pairs (Champions/Position) and the mean of theirs stats

CreateChampJungle <- function(players.dt, Position = "Other"){

  if(Position == "JUNGLE"){Jungle = as.character(c("JUNGLE"))}
  else {Jungle = as.character(c("LANE"))}
  
  #Take the mean of the Normalized stats for each couples (Champ, Position)
  Champ.Position.dt <- players.dt[,list(
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
    win = mean(win,na.rm=T),
    NamePOS = str_c(Name,Jungle),
    NbrGames = sum(duration/duration,na.rm = T)  #Number of games played with a champion
  ),.(Name)]

  return(Champ.Position.dt)
}




#Champ.LANE.dt = CreateChampJungle(subset(players.dt,position != "JUNGLE"))
#Champ.JUNGLE.dt = CreateChampJungle(subset(players.dt, position == "JUNGLE"),Position = "JUNGLE")
#View(Champ.JUNGLE.dt)

#Keep only datas for (Champion,role) played more than 100 times
#Champ.LANE.dt = subset(Champ.LANE.dt, NbrGames > 100)
#Champ.JUNGLE.dt = subset(Champ.JUNGLE.dt, NbrGames > 100)

#Champ.Pos.dt = rbind(Champ.JUNGLE.dt,Champ.LANE.dt)


#PCA:
#X = as.matrix(Champ.Pos.dt[,!c("Name","NamePOS","win","NbrGames")])
#pca <- prcomp(X, scale = T)
#View(pca$rotation)

# Plot pour PC1 vs PC2: Pas tres lisibles
#x12 <- summary(pca)$x[,1:2]
#plot(x12, pch =16, cex = 0.8)
#text(x12, labels=Champ.Position.dt$NamePOS, cex= 0.5, pos=1)
#biplot(pca, cex=c(0.5, 0.8),xlabs = Champ.Position.dt$NamePOS)

#PlotUsing ggbiplot 
#Nice plot
#Pos = c(rep("Jungle",nrow(Champ.JUNGLE.dt)),rep("Lane",nrow(Champ.LANE.dt)))
#g <- ggbiplot(pca, obs.scale = 1, var.scale =1,
#              groups = Pos, ellipse = TRUE)
              #labels = Champ.Pos.dt$NamePOS)
             # circle = TRUE)
#g <- g + scale_color_discrete(name = 'Position')
#print(g)
#Try to do the same for all lanes (mid,top,bot..)


