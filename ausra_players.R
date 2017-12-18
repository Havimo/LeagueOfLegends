#fit lm in high dimension -> ridiculus 
# nothing to do w/ reg subset (here applies)
# --> LASSO
# elast net (lasso/ridge mixture)
# ridge shrinks 
# do cv 
# pca another way to do this 
# L2 boosting also another way -> glmboost : (componentwise L2 boosing --> google) ls + min rss : selection of error
# cv -> find the m from glmboost , names(coef()) to find the variables selected
# lasso and glmboost are similar -> coefficients similar, variables selected are similar
# l2boost()
# dev.off() - reset plot console
# boosting : can be used to approximate functions f.e. TS 

#### some functions ####
NormalizeDuration <- function(players.dt){
  #sapply(players.dt, class)
  players.dt$duration <- as.numeric(players.dt$duration)
  players.dt$firstblood <- as.numeric(players.dt$firstblood)
  
  #compute normalization
  for(sd in setdiff(colnames(players.dt),c("championid", "ss1", "ss2", "win","item1", "item2", "item3", "item4", "item5", "item6", "trinket", "firstblood", "duration", "role", "position"))){
    #"firsttower","firstinhib","firstbaron","firstdragon","firstharry","duration"
    players.dt <- players.dt[,as.character(sd):=get(sd)/duration]
  }
  players.dt <- players.dt[,!"duration"]
  return(players.dt)
}

kfoldlogLASSO <- function(players.dt, k){
  #kfoldLASSO computes k-fold LASSO model selection
  # IN : 
  # players.dt              NxM table     our data
  # k                       scalar        the number of folds
  
  #sample the data
  N <- dim(players.dt)[1]
  #M <- dim(players.dt)[2]
  samp <- sample(1:k, N, replace = TRUE)
  
  #define the sequence of lambdas
  lamb <- exp(seq(-1, -8, length=100))
  
  #define matrices for later
  err <- matrix(rep(0, length(lamb)*k), k, length(lamb))
  
  for(i in 1:k){
    print(i)
    
    #define training and testing sets for fold k
    dat.train <- players.dt[!(samp==i)]
    dat.test <- players.dt[(samp==i)]
    
    #prediction on the training set
    X <- model.matrix(win~., dat.train)[, -1]
    y <- dat.train$win
    
    #solving LASSO for train set
    lassofit <- glmnet(X, y, alpha = 1, lambda = lamb, family="binomial") 
    
    #display LASSO path
    #plot.glmnet(lassofit, label= TRUE, xvar="lambda")
    
    #best lambda 
    #lambi <- cv.glmnet(X, y)$lambda.1se
    #extract coefficients
    coefflassofit <- coef(lassofit)
    
    #computing testing error
    Xnew <- model.matrix(win~., dat.test)
    ynew <- dat.test$win
    errlassotest <- 1/dim(Xnew)[1]*colSums(abs((Xnew%*%coefflassofit)>=0.5 - ynew))
    print("errlassotest:")
    print(errlassotest)
    #plot(log(lamb), errlassotest, type="l")
    err[i,] <- errlassotest
    
  }
  
  # CV error 
  erre <- (1/k)*colSums(err)
  
  # compute sd 
  erre2 <- mean((sweep(err,2,erre))^2)
  sd <- 1/sqrt(k)*sqrt((erre2))
  
  print("sd:")
  print(sd)
  
  plot(log(lamb), erre, xlab="log lambda", ylab="CV", type="l")
  lines(log(lamb), sd*rep(1, length(lamb)))
  
  #extracts the lambda for which the test error is the smallest
  vect <- (1:length(lamb))
  indx <- which.max(vect[(erre)>=sd])
  lambi <- lamb[indx]
  
  #compute validation error 
  #Xval <- model.matrix(win~., validate.players.dt)
  #yval <- validate.players.dt$win
  #errlassoval <- 1/dim(Xval)[1]*sum((Xval%*%coefflassofit[,indx] - yval)^2)
  #errlassoval
  
  #COMMENT FAIRE POUR LES COEFS ? 
  
  return(lambi)
  
}

FormatPlayerData2 <- function(players.dt){
  
  # convert discrete values for regression
  uniq.item1 <- unique(players.dt$item1)
  for(i in 1:length(uniq.item1)){
    players.dt[, paste("item1", uniq.item1[i], sep="_")] <- as.numeric(players.dt$item1==uniq.item1[i])
  }
  players.dt[,"item1":=NULL]
  
  uniq.item2 <- unique(players.dt$item2)
  for(i in 1:length(uniq.item2)){
    players.dt[, paste("item2", uniq.item2[i], sep="_")] <- as.numeric(players.dt$item2==uniq.item2[i])
  }
  players.dt[, "item2" :=NULL]
  
  uniq.item3 <- unique(players.dt$item3)
  for(i in 1:length(uniq.item3)){
    players.dt[, paste("item3", uniq.item3[i], sep="_")] <- as.numeric(players.dt$item3==uniq.item3[i])
  }
  players.dt[, "item3" :=NULL]
  
  uniq.item4 <- unique(players.dt$item4)
  for(i in 1:length(uniq.item4)){
    players.dt[, paste("item4", uniq.item4[i], sep="_")] <- as.numeric(players.dt$item4==uniq.item4[i])
  }
  players.dt[, "item4" :=NULL]
  
  uniq.item5 <- unique(players.dt$item5)
  for(i in 1:length(uniq.item5)){
    players.dt[, paste("item5", uniq.item5[i], sep="_")] <- as.numeric(players.dt$item5==uniq.item5[i])
  }
  players.dt[, "item5" :=NULL]
  
  uniq.item6 <- unique(players.dt$item6)
  for(i in 1:length(uniq.item6)){
    players.dt[, paste("item6", uniq.item6[i], sep="_")] <- as.numeric(players.dt$item6==uniq.item6[i])
  }
  players.dt[, "item6" :=NULL]
  
  uniq.ss1 <- unique(players.dt$ss1)
  for(i in 1:length(uniq.ss1)){
    players.dt[, paste("ss1", uniq.ss1[i], sep="_")] <- as.numeric(players.dt$ss1==uniq.ss1[i])
  }
  players.dt[, "ss1" :=NULL]
  
  uniq.ss2 <- unique(players.dt$ss2)
  for(i in 1:length(uniq.ss2)){
    players.dt[, paste("ss2", uniq.ss2[i], sep="_")] <- as.numeric(players.dt$ss2==uniq.ss2[i])
  }
  players.dt[, "ss2" :=NULL]
  
  uniq.championid <- unique(players.dt$championid)
  for(i in 1:length(uniq.championid)){
    players.dt[, paste("championid", uniq.championid[i], sep="_")] <- as.numeric(players.dt$championid==uniq.championid[i])
  }
  players.dt[, "championid" :=NULL]
  
  uniq.trinket <- unique(players.dt$trinket)
  for(i in 1:length(uniq.trinket)){
    players.dt[, paste("trinket", uniq.trinket[i], sep="_")] <- as.numeric(players.dt$trinket==uniq.trinket[i])
  }
  players.dt[, "trinket" :=NULL]
  
  #put the roles as numerical values 
  players.dt[, roleNONE := as.numeric(players.dt$role=="NONE")]
  players.dt[, roleSOLO := as.numeric(players.dt$role=="SOLO")]
  players.dt[, roleDUO_CARRY := as.numeric(players.dt$role=="DUO_CARRY")]
  players.dt[, roleDUO_SUPPORT := as.numeric(players.dt$role=="DUO_SUPPORT")]
  #players.dt[, roleDUO := as.numeric((players.dt$role=="DUO"))] #avoid lin dep
  players.dt[, role:= NULL]
  
  #put the positions as numerical values 
  players.dt[, positionBOT := as.numeric(players.dt$position=="BOT")]
  players.dt[, positionMID := as.numeric(players.dt$position=="MID")]
  players.dt[, positionTOP := as.numeric(players.dt$position=="TOP")]
  #players.dt[, positionJUNGLE := as.numeric(players.dt$position=="JUNGLE")] #avoid lin dep
  players.dt[, position:= NULL]
}
#### preprocessing ####

#to get rid of linear dependencies
#players.dt[,positionJUNGLE:=NULL]
#players.dt[,roleDUO:=NULL]
players.dt[,teamid:=NULL]
players.dt[,matchid:=NULL]
players.dt[,queueid:=NULL]
players.dt[,id:=NULL]
players.dt[,player:=NULL]

#/!\ fix et faire gaffe de tourner sur de bons trucs /!\

players.dt <- NormalizeDuration(players.dt)

# reducing the dataset
players.dt <- players.dt[1:10000,]

# Transform discrete variables
players.dt <- FormatPlayerData2(players.dt)

# get rid of zero columns
players.dt <- players.dt[, colSums(players.dt!=0)>0]

#### Train/Test/Validate sets ####
set.seed(0)

# Which ratio of the data used for training, testing, validating
train.ratio <- 0.5
test.ratio <- 0.3
validate.ratio <- 1 - train.ratio - test.ratio

# Deduce the number of samples needed in our training set
nb.train.players <- floor(train.ratio*dim(players.dt)[1])
# Define which observations will be used for our training set
selection <- sample(c(rep(TRUE, times=nb.train.players), rep(FALSE, times=dim(players.dt)[1]-nb.train.players)), dim(players.dt)[1])
train.players.dt <- players.dt[selection]
rest <- players.dt[!selection]

# Deduce the number of samples needed in our testing set
nb.test.players <- floor(test.ratio*dim(players.dt)[1])
# Define which observations will be used for our training set
selection <- sample(c(rep(TRUE, times=nb.test.players), rep(FALSE, times=dim(rest)[1]-nb.test.players)), dim(rest)[1])
test.players.dt <- rest[selection]

# Define which observations will be used for our validation set 
validate.players.dt <- rest[!selection]

#### Subset Selection ####
reg.players <- regsubsets(win~., train.players.dt, nvmax=100, method="backward", really.big = TRUE)
suma <- summary(reg.players)
plot(suma$bic, xlab = "level", ylab = "bic")
plot(suma$cp, xlab = "level", ylab = "Mallows Cp")
plot(suma$adjr2, xlab = "level", ylab = "adjusted r squared")

# reduce dataset wrt subset selection + bic
tab <- suma$which[which.min(suma$bic),]
tab.names <- names(tab[which(tab)])[-1]
matched.names <- match(tab.names, names(players.dt))

# update datasets
train.win <- train.players.dt$win
train.subs.players.dt <- train.players.dt[,..matched.names]
train.subs.players.dt[, win:=train.win]

test.win <- test.players.dt$win
test.subs.players.dt <- test.players.dt[,..matched.names]
test.subs.players.dt[, win:=test.win]

validate.win <- validate.players.dt$win
validate.subs.players.dt <- validate.players.dt[,..matched.names]
validate.subs.players.dt[, win:=validate.win]

#### LASSO - logit ####

# run LASSO-logit on train set
X <- model.matrix(win~., train.players.dt)
y <- train.players.dt$win

lamb <- exp(seq(2, -5, length=100))

logLASSO <- glmnet(X, y, alpha = 1, lambda = lamb, family = "binomial") 
plot(logLASSO, xvar = "lambda")
plot(logLASSO, xvar = "norm")
#lines(rep(log(lambda.min), 6), -3:2, type="l", col="grey")

# compute error on test set and sleect lambda
Xhat <- model.matrix(win~., test.players.dt)
yhat <- predict(logLASSO, Xhat, type = "response", s = lamb)>=0.5
missclass <- colSums(abs(sweep(yhat, 1, test.players.dt$win)))
err <- missclass/dim(yhat)[1]
lambda.min <- err[which.min(err)]
lambda.min

# compute validation error
Xval <- model.matrix(win~., validate.players.dt)
yval <- predict(logLASSO, Xval, type="response", s = lambda.min)>=0.5
missclass <- colSums(abs(sweep(yval, 1, validate.players.dt$win)))
err <- missclass/dim(yval)[1]
err #0.2365 

coefLM <- coef(logLASSO, s=lambda.min)

#### TODO : ONE STD RULE ####
#compute validation error 
Xval <- model.matrix(win~., validate.players.dt)
yval.1se <- Xval%*%coef.1se>=0.5
err.1se <- mean(abs(yval.lse - validate.players.dt$win))

yval.min <- Xval%*%coef.min>=0.5
err.min <- mean(abs(yval.min - validate.players.dt$win))

#### ELASTIC-NET - logit ####
# run LASSO-logit on train set
X <- model.matrix(win~., train.players.dt)
y <- train.players.dt$win

lamb <- exp(seq(2, -5, length=100))

logELASNET <- glmnet(X, y, alpha = 0.5, lambda = lamb, family = "binomial") 
plot(logELASNET, xvar = "lambda")
#lines(rep(log(lambda.min), 6), -3:2, type="l", col="grey")

# compute error on test set and sleect lambda
Xhat <- model.matrix(win~., test.players.dt)
yhat <- predict(logELASNET, Xhat, type = "response", s = lamb)>=0.5
missclass <- colSums(abs(sweep(yhat, 1, test.players.dt$win)))
err <- missclass/dim(yhat)[1]
lambda.min <- err[which.min(err)]
lambda.min

# compute validation error
Xval <- model.matrix(win~., validate.players.dt)
yval <- predict(logLASSO, Xval, type="response", s = lambda.min)>=0.5
missclass <- colSums(abs(sweep(yval, 1, validate.players.dt$win)))
err <- missclass/dim(yval)[1]
err #0.272 

#### TODO : ONE STD RULE ####

#### PCA ####

# get the wins out of training set
train.win <- train.players.dt$win
train.pca.players.dt <- train.players.dt[, -1]

#compute PCA
pca.train <- prcomp(train.pca.players.dt, scale =TRUE, center=TRUE)

#plot singular values and built threshold for data
eig <- pca.train$sdev
plot(eig, type="l", ylab = "singular values")
gamma <- 1.2 #threshold
lines(0:length(eig), gamma*rep(1, length(eig)+1), col="red", type="l")
cut <- which.min(eig>gamma)
cut

#amount of data kept
cut/length(eig) #20%

#extract PCAed data
train.pca.players.dt <- as.data.frame(pca.train$x[,1:cut]) #extract scores
train.pca.players.dt$win =train.win

test.pca.players.dt <- as.data.frame(predict(pca.train, test.players.dt[, -1])[, 1:cut])
test.pca.players.dt$win <- test.players.dt$win

validate.pca.players.dt <- as.data.frame(predict(pca.train, validate.players.dt[, -1])[, 1:cut])
validate.pca.players.dt$win <- validate.players.dt$win 

#apply it on logistic to see results 
logPCA <- glm(win~., train.pca.players.dt, family = "binomial")
yhat <- predict(logPCA, test.pca.players.dt)>=0.5
err <- mean(abs(yhat-test.players.dt$win))
err #0.2803333


#### LDA on Subset Selection ####
lda.fit <- lda(win~., data = train.subs.players.dt)

#train error
lda.pred.train <- predict(lda.fit, train.subs.players.dt)
prediction <- lda.pred.train$posterior[, 1]<0.5
classif.lda <- abs(prediction - train.subs.players.dt$win)
err.lda.train <- sum(classif.lda)/length(classif.lda)
err.lda.train #0.0888

#test error
lda.pred <- predict(lda.fit, test.subs.players.dt)
prediction <- lda.pred$posterior[, 1]<0.5
classif.lda <- abs(prediction - test.subs.players.dt$win)
err.lda <- 1/length(classif.lda)*sum(classif.lda)
err.lda #0.108

#### QDA on Subset Selection ####
train.subs.players.dt <- train.subs.players.dt[, colSums(train.subs.players.dt!=0)>0]
qda.fit <- qda(win~., train.subs.players.dt)

#training error
qda.pred.train <- predict(qda.fit, train.players.dt)
prediction <- qda.pred.train$posterior[, 1]<0.5
classif.qda <- abs(prediction - train.players.dt$win)
err.qda.train <- sum(classif.qda)/length(classif.qda)
err.qda.train #0.322926

#testing error
qda.pred <- predict(qda.fit, test.subs.players.dt)
prediction <- qda.pred$posterior[, 1]<0.5
classif.qda <- abs(prediction - test.subs.players.dt$win)
err.qda <- 1/length(classif.qda)*sum(classif.qda)
err.qda #0.3243263

#### LDA on PCA ####
lda.fit <- lda(win~., data = train.pca.players.dt)

#test error
lda.pred <- predict(lda.fit, test.pca.players.dt)
prediction <- lda.pred$posterior[, 1]<0.5
classif.lda <- abs(prediction - test.subs.players.dt$win)
err.lda <- 1/length(classif.lda)*sum(classif.lda)
err.lda #0.266

#train error
lda.pred.train <- predict(lda.fit, train.pca.players.dt)
prediction <- lda.pred.train$posterior[, 1]<0.5
classif.lda <- abs(prediction - train.pca.players.dt$win)
err.lda.train <- sum(classif.lda)/length(classif.lda)
err.lda.train #0.2362

#### QDA on PCA ####
qda.fit <- qda(win~., train.pca.players.dt)

#training error
qda.pred.train <- predict(qda.fit, train.pca.players.dt)
prediction <- qda.pred.train$posterior[, 1]<0.5
classif.qda <- abs(prediction - train.pca.players.dt$win)
err.qda.train <- mean(classif.qda)
err.qda.train #0.282

#testing error
qda.pred <- predict(qda.fit, test.pca.players.dt)
prediction <- qda.pred$posterior[, 1]<0.5
classif.qda <- abs(prediction - test.pca.players.dt$win)
err.qda <- mean(classif.qda)
err.qda #0.424

#### OTHERS ####
lm.players <- lm(win~., train.players.dt)
summary(lm.players)
#looks to work badly