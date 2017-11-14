#################### EXPLORE THE DATA ###########################

############### General Remarks
#mean(train.rel.teams.dt$win) # team 100 wins a little bit more often in train set 
mean(train.teams.dt$win) # win rate seems quite equal between teams

#################################################################
# blue = win 
# red = loose

############### TEAMS.DT

plot(train.teams.dt$kills, train.teams.dt$goldearned, xlab = "kills", ylab="gold earned", col=c("red", "blue")[train.teams.dt$win +1])
# they look correlated
# no advances for 

plot(train.teams.dt$maxchamplvl, train.teams.dt$kills, xlab="maxchamplvl", ylab="kills", col=c("red", "blue")[train.teams.dt$win +1])
# champion lvl influences the number of kills

plot(train.teams.dt$maxchamplvl, train.teams.dt$goldearned, xlab="maxchamplvl", ylab="goldearned", col=c("red", "blue")[train.teams.dt$win +1])
# really high lvl champions earn a lot of gold

plot(train.teams.dt$minchamplvl, train.teams.dt$totdmgdealt, xlab="avgchamplvl", ylab="tot domage dealt", col=c("red", "blue")[train.teams.dt$win +1])

# bestsubset (nvmax = 4) towerkills/assists/kills/goldspent
plot(train.teams.dt$towerkills, train.teams.dt$assists, xlab="towerkills", ylab="assists", col=c("red", "blue")[train.teams.dt$win +1])
plot(train.teams.dt$towerkills, train.teams.dt$kills, xlab="towerkills", ylab="kills", col=c("red", "blue")[train.teams.dt$win +1])
plot(train.teams.dt$towerkills, train.teams.dt$goldspent, xlab="towerkills", ylab="gold spent", col=c("red", "blue")[train.teams.dt$win +1])
plot(train.teams.dt$kills, train.teams.dt$assists, xlab="kills", ylab="assists", col=c("red", "blue")[train.teams.dt$win +1]) # all mixed.
plot(train.teams.dt$kills, train.teams.dt$goldspent, xlab="kills", ylab="gold spent", col=c("red", "blue")[train.teams.dt$win +1])
plot(train.teams.dt$goldspent, train.teams.dt$assists, xlab="gold spent", ylab="assists", col=c("red", "blue")[train.teams.dt$win +1])

############### REL.TEAMS.DT

plot(train.rel.teams.dt$kills, train.rel.teams.dt$goldearned, xlab = "kills", ylab="gold earned", col=c("red", "blue")[train.rel.teams.dt$win +1])

plot(train.rel.teams.dt$maxchamplvl, train.rel.teams.dt$kills, xlab="maxchamplvl", ylab="kills", col=c("red", "blue")[train.rel.teams.dt$win +1])

plot(train.rel.teams.dt$maxchamplvl, train.rel.teams.dt$goldearned, xlab="maxchamplvl", ylab="goldearned", col=c("red", "blue")[train.rel.teams.dt$win +1])

plot(train.rel.teams.dt$minchamplvl, train.rel.teams.dt$totdmgdealt, xlab="avgchamplvl", ylab="tot domage dealt", col=c("red", "blue")[train.rel.teams.dt$win +1])

#extrait de best-subset
plot(train.rel.teams.dt$inhibkills, train.rel.teams.dt$avgchamplvl, xlab="inhibkills", ylab="avgchamplvl", col=c("red", "blue")[train.rel.teams.dt$win +1])

######################### LASSO #################################
#formulation of the LASSO problem for train set
X = model.matrix(win~., train.rel.teams.dt)[, -1]
y = train.rel.teams.dt$win
#define the sequence of lambdas
lamb = 10^seq(-4, 20, length=50) 

#solving LASSO for train set
lassofit <- glmnet(X, y, alpha = 1, lambda = lamb) 
#extract coefficients
coeflassofit <- coef(lassofit) #our model
coef(lassofit, s = 10) #nah, fix ce truc
#display LASSO path
plot.glmnet(lassofit, label= TRUE)

#computing test error
Xhat = model.matrix(win~., test.teams.dt)[,-1]
yhat = test.teams.dt$win
errlassotrain = 1/dim(Xhat)[1]*colSums((Xhat%*%coeflassofit - yhat)^2);
indx = which.min(errlassotrain)
#extracts the lambda for which the test error is the smallest
lamb[indx] # bizarre ... genre vraiment


######################### BEST SUBSET ###########################
reg.rel.teams <- regsubsets(win~. , train.rel.teams.dt, nvmax = 4, method = "backward", really.big = TRUE)
plot(reg.rel.teams) #bizarre : on ne devrait pas avoir de trous, right ?

reg.teams <- regsubsets(win~. , train.teams.dt, nvmax = 4, method = "backward", really.big = TRUE)
plot(reg.teams)

########################## Logit ################################
logit.fit <- glm(win~inhibkills + avgchamplvl, data  = train.rel.teams.dt, family = binomial)
summary.glm(logit.fit)
coef.rel.logit.fit <- coef(logit.fit)
logit.prob <- predict(logit.fit, test.rel.teams.dt, type="response")
logit.pref <- logit.prob>=0.5

err.rel.logit <- 1/length(logit.pref)*sum(abs(logit.pref-test.rel.teams.dt$win))
err.rel.logit 

plot(logitfit, label=TRUE)


########################## LDA ##################################
lda.fit <- lda(win~inhibkills+avgchamplvl, data = train.rel.teams.dt)
lda.fit

lda.pred <- predict(lda.fit, test.rel.teams.dt)
names(lda.pred)

mean(lda.pred$class==1)

table(lda.pred$class, test.rel.teams.dt$win)

sum(lda.pred$posterior[, 1]>=0.5)
sum(lda.pred$posterior[, 1]<0.5)

prediction <- lda.pred$posterior[, 1]<0.5
classif.lda <- abs(prediction - test.rel.teams.dt$win)
err.lda <- 1/length(classif.lda)*sum(classif.lda)
err.lda

######################### QDA ###################################
qda.fit <- qda(win~inhibkills + avgchamplvl, data = train.rel.teams.dt)
qda.fit

qda.pred <- predict(qda.fit, test.rel.teams.dt)
mean(qda.pred$class==1)

prediction <- qda.pred$posterior[, 1]<0.5
classif.qda <- abs(prediction - test.rel.teams.dt$win)
err.qda <- 1/length(classif.qda)*sum(classif.qda)
err.qda

#################### COMPUTINGS FOR teams.dt ####################
######### with param : towerkills/assists/kills/goldspent #######

############## logit
logit.fit <- glm(win~towerkills + assists + kills + goldspent, data  = train.teams.dt, family = binomial)
summary.glm(logit.fit)
coef.logit.fit <- coef(logit.fit)
logit.prob <- predict(logit.fit, test.teams.dt, type="response")
logit.pref <- logit.prob>=0.5

err.rel.logit <- 1/length(logit.pref)*sum(abs(logit.pref-test.teams.dt$win))
err.rel.logit 

plot(logitfit, label=TRUE)

############## LDA 
lda.fit <- lda(win~towerkills + assists + kills + goldspent, data = train.teams.dt)
lda.fit

lda.pred <- predict(lda.fit, test.teams.dt)
names(lda.pred)

mean(lda.pred$class==1)

table(lda.pred$class, test.teams.dt$win)

prediction <- lda.pred$posterior[, 1]<0.5
classif.lda <- abs(prediction - test.teams.dt$win)
err.lda <- 1/length(classif.lda)*sum(classif.lda)
err.lda

############## QDA 
qda.fit <- qda(win~towerkills + assists + kills + goldspent, data = train.teams.dt)
qda.fit

qda.pred <- predict(qda.fit, test.teams.dt)
mean(qda.pred$class==1)

prediction <- qda.pred$posterior[, 1]<0.5
classif.qda <- abs(prediction - test.teams.dt$win)
err.qda <- 1/length(classif.qda)*sum(classif.qda)
err.qda
# QDA fait moins bien .... 