#
source('test_Adrien.R')
source('models_Adrien.R')

#PCA 
pca <- princomp(teams.dt[,!c("matchid","teamid","win")])
pca.norm <- princomp(teams.normalized.dt[,!c("matchid","teamid","win")])

#regsubsets for win
reg.exhaustive <- regsubsets(x=win~.,data=teams.new.dt[,!c('matchid','teamid')],really.big = T,method = 'exhaustive')
reg.backward <- regsubsets(x=win~.,data=teams.new.dt[,!c('matchid','teamid')],really.big = T,method = 'backward')
reg.forward <- regsubsets(x=win~.,data=teams.new.dt[,!c('matchid','teamid')],really.big = T,method = 'forward')

plot(reg.exhaustive)
plot(reg.backward)
plot(reg.forward)

#regsubsets for firstinhib
reg.exhaustive <- regsubsets(x=firstinhib~.,data=teams.new.dt[,!c('matchid','teamid','win','towerkills','inhibkills')],really.big = T,method = 'exhaustive')
reg.backward <- regsubsets(x=firstinhib~.,data=teams.new.dt[,!c('matchid','teamid','win')],really.big = T,method = 'backward')
reg.forward <- regsubsets(x=firstinhib~.,data=teams.new.dt[,!c('matchid','teamid','win')],really.big = T,method = 'forward')

plot(reg.exhaustive)
plot(reg.backward)
plot(reg.forward)


#regsubsets for deaths
reg.exhaustive <- regsubsets(x=deaths~.,data=teams.new.dt[,!c('win','firstinhib','inhibkills','towerkills','firstbaron','firstdragon','matchid','teamid',
                                                              'dragonkills','harrykills')],really.big = T,method = 'exhaustive')
reg.backward <- regsubsets(x=firstinhib~.,data=teams.new.dt[,!c('matchid','teamid','win')],really.big = T,method = 'backward')
reg.forward <- regsubsets(x=firstinhib~.,data=teams.new.dt[,!c('matchid','teamid','win')],really.big = T,method = 'forward')

plot(reg.exhaustive)
plot(reg.backward)
plot(reg.forward)


#Plot test
# error.dt <- as.data.table(cbind(1:nrow(reg.summary$which),reg.summary$rsq,reg.summary$rss,reg.summary$adjr2,reg.summary$cp,reg.summary$bic))
# setnames(error.dt,c('model','rsq','rss','adjr2','cp','bic'))
# error.dt <- melt(error.dt,id.vars='model',variable.name='ErrorType',value.name='ErrorValue')
# ggplot(error.dt) + geom_line(aes(x=model,y=ErrorValue,color=ErrorType))

#glmnet
glmnet.dt <- teams.normalized.dt[,!c('matchid','teamid')]
matrix.input = model.matrix(win~., teams.normalized.dt)
matrix.response = teams.normalized.dt$win
glmnet.model <- glmnet(x = matrix.input, y = matrix.response, family = 'binomial')


NormalizedGoldEarnedBoxPlot(teams.dt,teams.normalized.dt)


#base model to test variables interaction and behavior
test.model <- glm(teams.new.dt[,.(win,deaths,goldearned)],formula= win~deaths+goldearned+I(deaths^2)+I(goldearned^2),family='binomial')
test.model <- lm(teams.new.dt[,!c('win','firstinhib','inhibkills','towerkills','firstbaron','firstdragon','matchid','teamid')],formula= deaths~.)

#plotting
ClusteringPlot(teams.new.dt[sample(1:nrow(teams.new.dt),200000)],'deaths','goldearned')
CompleteTeamModel_kCV(teams.new.dt,k=10,formula=' win~deaths+goldearned+I(deaths^2)+I(goldearned^2)')
