
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


################# Create train/test set for team.dt #############
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


######################### LASSO #################################
#formulation of the LASSO problem for train set
X = model.matrix(win~., train.teams.dt)[, -1]
y = train.teams.dt$win
#define the sequence of lambdas
lamb = 10^seq(-4, 20, length=50) 

#solving LASSO for train set
lassofit = glmnet(X, y, alpha = 1, lambda = lamb) 
#extract coefficients
coeflassofit = coef(lassofit) #our model
coef(lassofit, s = 0.01) #nah, fix ce truc
#display LASSO path
plot.glmnet(lassofit, label= TRUE)

#computing test error
Xhat = model.matrix(win~., test.teams.dt)
yhat = test.teams.dt$win
errlassotrain = 1/dim(Xhat)[1]*colSums((Xhat%*%coeflassotrain - yhat)^2);
indx = which.min(errlassotrain)
#extracts the lambda for which the test error is the smallest
lamb[indx] # bizarre ... genre vraiment


######################### BEST SUBSET ###########################
reg.teams <- regsubsets(win~. , train.teams.dt, nvmax = 58, method = "backward", really.big = TRUE)
plot(reg.teams)