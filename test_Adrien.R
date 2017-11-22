
# Check for missing values
sum(is.na(players.dt))
sum(is.na(teams.dt))

# NORMALIZE THE DATA 
#PCA 
pca <- princomp(teams.dt[,!c("matchid","teamid","win")])
pca.norm <- princomp(teams.normalized.dt[,!c("matchid","teamid","win")])

#regsubsets


reg.exhaustive <- regsubsets(x=win~.,data=teams.normalized.dt[,!c('matchid','teamid')],really.big = T,nvmax=15,method = 'exhaustive')
reg.backward <- regsubsets(x=win~.,data=teams.normalized.dt[,!c('matchid','teamid')],really.big = T,nvmax=15,method = 'backward')
reg.forward <- regsubsets(x=win~.,data=teams.normalized.dt[,!c('matchid','teamid')],really.big = T,nvmax=15,method = 'forward')

plot(reg.exhaustive)
plot(reg.backward)
plot(reg.forward)
reg.summary <- summary(regsubsets(x=win~.,data=teams.normalized.dt[,!c('matchid','teamid')],really.big = T))



error.dt <- as.data.table(cbind(1:nrow(reg.summary$which),reg.summary$rsq,reg.summary$rss,reg.summary$adjr2,reg.summary$cp,reg.summary$bic))
setnames(error.dt,c('model','rsq','rss','adjr2','cp','bic'))
error.dt <- melt(error.dt,id.vars='model',variable.name='ErrorType',value.name='ErrorValue')
ggplot(error.dt) + geom_line(aes(x=model,y=ErrorValue,color=ErrorType))

#glmnet
glmnet.dt <- teams.normalized.dt[,!c('matchid','teamid')]
matrix.input = model.matrix(win~., teams.normalized.dt)
matrix.response = teams.normalized.dt$win
glmnet.model <- glmnet(x = matrix.input, y = matrix.response, family = 'binomial')


NormalizedVariableBoxPlot(teams.dt,teams.normalized.dt)
