#### Load libraries and functions ####
library(data.table)
library(ggplot2)
library(scales)
library(leaps)
library(rjson)
library(MASS)
library(glmnet)
library(ggbiplot)
source('read.data.R')
source('plot.functions.R')
source('models.R')
source('relativeDataset.R')
source('teamsbyduration.R')
source('ChampPosition.R')
source('CreateChampPosition2.R')
source('ChampClasses.R')

#### Read data ####
ReadData()
players.dt <- FormatPlayerData(players.dt)
teams.dt <- CreateTeamData(players.dt) 
teams.normalized.dt <- CreateTeamData(players.dt,T)
teams.new.dt <- CreateTeamData(players.dt,T,T,T)
id.mapping.list <- GetJsonFiles()

TeamRel.dt <- relativeDataset(teams.new.dt)
#View(TeamRel.dt)

#Problem: 54k observations have value towerkills !=0, dmgtoturrets==0... Remove them?
#teams.dt = subset(teams.dt, teams.dt$dmgtoturrets != 0 )

#####Correllations####
library(corrplot)
#Toutes les variables
M = cor(teams.normalized.dt)
corrplot(M, method = "square", type ='lower')
#Qqunes seulement
M = cor(teams.normalized.dt[,c('firstinhib','towerkills','kills','deaths','goldearned')])
corrplot.mixed(M, upper='square')

#### Make a training set ####
#train = 75% for train
#test = 25% 
#set.seed(0)
#train = sample(1:nrow(teams.new.dt), floor(0.75*nrow(teams.new.dt)))
#teams.train = teams.new.dt[train, !c("matchid","teamid")]
#teams.test = teams.new.dt[-train, !c("matchid","teamid")]
DivideTeamsData(teams.new.dt)


#### First subsets selection ####
####Exhaustive ####
BestFW = regsubsets(win ~., teams.train, nvmax = 6, nbest =1, really.big = T)
BestFW.summ = summary(BestFW)
plot(BestFW, scale ='r2')

# 1 : towerkills
# 2 : totdmgtaken
# 3 : goldearned
# 4 : goldspent
# 5 : firstinhib


####Forward ####
BestFW = regsubsets(win ~., teams.train, nvmax = 6, nbest =1,  method = "forward")
BestFW.summ = summary(BestFW)
plot(BestFW, scale ='r2')

# 1 : towerkills
# 2 : totdmgtaken
# 3 : goldearned
# 4 : goldspent
# 5 : firstinhib

#BestFW.by.cp <- which.min(BestFW.summ$cp)       # k=47
#BestFW.by.adjr2 <- which.max(BestFW.summ$adjr2) # k=49
#BestFW.by.bic <- which.min(BestFW.summ$bic)     # k=43

#plots for r2,cp,adjr2, bic
plot(BestFW$rss, xlab="Number of Variables", ylab="RSS")
plot(BestFW.summ$adjr2, xlab="Number of Variables", ylab="Adjusted RSq")
points(BestFW.by.adjr2, BestFW.summ$adjr2[BestFW.by.adjr2], col="red", cex =2, pch =20)
plot(BestFW.summ$cp, xlab="Number of Variables", ylab="CP")
points(BestFW.by.cp, BestFW.summ$cp[BestFW.by.cp], col="red", cex =2, pch =20)
plot(BestFW.summ$bic, xlab="Number of Variables", ylab="BIC")
points(BestFW.by.bic, BestFW.summ$bic[BestFW.by.bic], col="red", cex =2, pch =20)

####Backward ####
BestBW = regsubsets(win ~., teams.train, nvmax = 6, nbest = 1,method = 'backward')
BestBW.summ = summary(BestBW)
plot(BestBW, scale ='r2')

# 1 : goldearned
# 2 : goldspent
# 3 : towerkills
# 4 : deaths
# 5 : firstinhib

#BestBW.by.cp <- which.min(BestBW.summ$cp)       # 48
#BestBW.by.adjr2 <- which.max(BestBW.summ$adjr2) # 48
#BestBW.by.bic <- which.min(BestBW.summ$bic)     #37

#plots for r2,cp,adjr2, bic
plot(BestBW$rss, xlab="Number of Variables", ylab="RSS")
plot(BestBW.summ$adjr2, xlab="Number of Variables", ylab="Adjusted RSq")
points(BestBW.by.adjr2, BestBW.summ$adjr2[BestBW.by.adjr2], col="red", cex =2, pch =20)
plot(BestBW.summ$cp, xlab="Number of Variables", ylab="CP")
points(BestBW.by.cp, BestBW.summ$cp[BestBW.by.cp], col="red", cex =2, pch =20)
plot(BestBW.summ$bic, xlab="Number of Variables", ylab="BIC")
points(BestBW.by.bic, BestBW.summ$bic[BestBW.by.bic], col="red", cex =2, pch =20)



#### Lasso by Cross-Validation ####
X = model.matrix(win~., teams.train)[,-1]
cv.out = cv.glmnet(x = X, y=teams.train[,win],family ='binomial', nfolds =5, alpha =1)
plot(cv.out$lambda,cv.out$cvm)
plot(cv.out)
lambdamin = cv.out$lambda.1se
FitLassoLambda <- glmnet(y = teams.train[,win], x =as.matrix(teams.train[,!c("win")]), family = 'binomial', alpha = 1, lambda = lambdamin)
coef(FitLassoLambda, s = lambdamin)
Lasso.pred = predict(FitLassoLambda, newx = model.matrix(win~.,teams.test)[,-1], type ="response")
Result.test = table(Lasso.pred, teams.test$win)
class.test.error = (Result.test[1,2]+Result.test[2,1])/(length(teams.test$win)) # +/- equal to 0 but way too big model
#Lambda1se verry small, still keeps a lot of variables (~ 30)

####KNN for Goldearned//Deaths####
library(class)
library(e1071)
DivideTeamsData(teams.new.dt[,c('goldearned', 'deaths','win')])
#k=1
knn.pred = knn(train.teams.dt[,!c('win')], test.teams.dt[,!c('win')], train.teams.dt$win, k=1)
table(knn.pred, test.teams.dt$win)                
(59246+59371)/(59246+59371+9768+9722)  #85% Accuracy for k=1
#k=2
knn.pred = knn(train.teams.dt[,!c('win')], test.teams.dt[,!c('win')], train.teams.dt$win, k=2)
table(knn.pred, test.teams.dt$win)                
(59299+59604)/(59246+59371+9768+9722)  #86% Accuracy for k=2

#Cross-Validation for KNN
Y=as.factor(teams.new.dt[1:100000,]$win)
cv.knn = tune.knn(x=teams.new.dt[1:100000,c('goldearned','deaths')],y=Y, k = 2:10,tunecontrol=tune.control(sampling="cross"), cross=10)
summary(cv.knn)
#- best parameters: k=7
# best performance: 0.07706
#k=7
knn.pred = knn(train.teams.dt[,!c('win')], test.teams.dt[,!c('win')], train.teams.dt$win, k=7)
table(knn.pred, test.teams.dt$win)                
(64432+63242)/(138107)  #92.44571% test-Accuracy for k=7

####PCA: ####

####For TeamRel.dt####
X = as.matrix(TeamRel.dt[,!c("win","matchid")])
pca <- prcomp(X, scale = T)
plot(pca)
#PC1 : Not so much variable outstanding (max: towerkills, inhibkills ~0.2), but mostly objectives
#PC2 : physical/magicdmgdealt, magicdmgtaken mostly (~0.45), and dmg in general

# Plot pour PC1 vs PC2:
g <- ggbiplot(pca, obs.scale = 1, var.scale =1,
              groups = as.factor(TeamRel.dt$win), ellipse = TRUE,
              circle = TRUE,
              varname.adjust = 1.3)
g <- g + scale_color_discrete(name = '')
print(g)

#Proportion of Variance explained
std_dev <- pca$sdev
#compute variance
pr_var <- std_dev^2

prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
abline(h=0.90, col ='red')
plot(cumsum(prop_varex))
#Around 50% variance explained by 2/3 Principal Components
#Around 90% variance explained by 15 Principal Components
plot(pca)

#PCA uniquement pour certaines variables choisies par regsubsets:
X = as.matrix(TeamRel.dt[,c("minchamplvl","goldearned","deaths","kills","towerkills","firstinhib","totaldmgtaken")])
pca <- prcomp(X, scale = T)
View(pca$rotation)

g <- ggbiplot(pca, obs.scale = 1, var.scale =1,
              groups = as.factor(TeamRel.dt$win), ellipse = TRUE,
              circle = TRUE,
              varname.adjust = 1.3)
g <- g + scale_color_discrete(name = '')
print(g)

#Proportion of Variance explained
std_dev <- pca$sdev
#compute variance
pr_var <- std_dev^2

prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
abline(h=0.90, col ='red')
#Around 90% variance explained by 3 Principal Components
plot(pca)












####For dataset contening players' position####
ChampStat.dt <- CreateChampStats(players.dt)
#PCA:
X = as.matrix(ChampStat.dt[,!c("Name","win","NbrGames")])
pca <- prcomp(X, scale = T)
#PlotUsing ggbiplot 
#Nice plot
g <- ggbiplot(pca, obs.scale = 1, var.scale =1,
              labels = ChampStat.dt$Name)
g <- g + scale_color_discrete(name = 'Position')
print(g)
#Try to do the same for all lanes (mid,top,bot..)


#####For dataset relative to each couple (champion, Jungle/Lane)####
Champ.LANE.dt = CreateChampJungle(subset(players.dt,position != "JUNGLE"))
Champ.JUNGLE.dt = CreateChampJungle(subset(players.dt, position == "JUNGLE"),Position = "JUNGLE")
View(Champ.JUNGLE.dt)
Champ.Pos.dt = rbind(Champ.JUNGLE.dt,Champ.LANE.dt)

#PCA:
X = as.matrix(Champ.Pos.dt[,!c("Name","NamePOS","win","NbrGames")])
pca <- prcomp(X, scale = T)
#PlotUsing ggbiplot 
#Nice plot
Pos = c(rep("Jungle",nrow(Champ.JUNGLE.dt)),rep("Lane",nrow(Champ.LANE.dt)))
g <- ggbiplot(pca, obs.scale = 1, var.scale =1,
              groups = Pos, ellipse = TRUE) 
             #, labels = Champ.Pos.dt$NamePOS)
# ,circle = TRUE)
g <- g + scale_color_discrete(name = 'Position')
print(g)
#Try to do the same for all lanes (mid,top,bot..)



####For dataset relative to each couple (champion, position)#####
Champ.Position.dt = CreateChampPosition(players.dt)

#PCA 
X = as.matrix(Champ.Position.dt[,!c("Name","NamePOS","win","NbrGames","position")])
pca <- prcomp(X, scale = T)
#PlotUsing ggbiplot 
#Nice plot
g <- ggbiplot(pca, obs.scale = 1, var.scale =1,
              groups = Champ.Position.dt$position, ellipse = TRUE)
#,circle = TRUE)
g <- g + scale_color_discrete(name = 'Position')
print(g)



#Question:how many teams have a different compositions than 2/1/1+1? #4332
SoloBOT.dt = subset(players.dt, role=='SOLO' & position=='BOT')
View(SoloBOT.dt)
#percentage of winning: 22.7%
nrow(subset(SoloBOT.dt, win==1))/4332

#Question: Did the other team adapt?
winning.SoloBot.players = subset(SoloBOT.dt, win==1)
nrow(subset(SoloBOT.dt,win==0 & matchid %in% winning.SoloBot.players$matchid))
#127 matchs on 4332-127=4205 have both teams solo bot
(nrow(subset(SoloBOT.dt, win==1))-127)/(4332-2*127)
#Playing SoloBot against normal has ~21% of win
#Ok, delete those match for our analysis...
players.bot.dt = subset(players.dt, position=="BOT")
nrow(subset(players.bot.dt, role=='SOLO')) #4332 SOLO BOT
players.bot.dt = subset(players.dt, position=="TOP")
nrow(subset(players.bot.dt, role!='SOLO')) #1927 DUO TOP
players.bot.dt = subset(players.dt, position=="MID" & role!='SOLO')
nrow(subset(players.bot.dt, role!='SOLO')) #16432 DUO TOP
nrow(subset(players.bot.dt, win==1))
View(subset(players.dt, matchid==40991))
#44154 teams with less than 5 players?
playerscount <- players.dt[,list(
  count = sum(duration/duration)
),.(matchid,teamid)]
nrow(subset(playerscount, count <5))