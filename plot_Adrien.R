
WinningHistogramCategorical <- function(teams.dt,variable,filter=-1){
  #plot that gives the average winnig percentage per value of 'variable'
  dataplot <- teams.dt[,.(VarSum = sum(as.numeric(get(variable)))),.(matchid,win)]
  #dataplot[,VarSum:=cut(VarSum,breaks=100)]
  dataplot <- dataplot[,.N,.(VarSum,win)]
  dataplot[,total := sum(N),.(VarSum)]
  dataplot[,Perc := N/total,.(VarSum)]
  if(filter>0)dataplot<-dataplot[VarSum<=filter]
  
  plot1 <- ggplot(dataplot[win==1])+
    geom_bar(aes(x=VarSum,y=Perc),stat='identity',fill='salmon') +
    scale_y_continuous(label=percent,limits=c(0,1))+
    labs(title=paste('Winning percentage per',variable),x=variable,y='Winning Percentage')
  print(plot1)
}

WinningHistogramNumerical <- function(teams.dt,variable,filter=-1){
  #plot that gives the average winnig percentage per value of 'variable'
  dataplot <- teams.dt[,.(VarSum = sum(as.numeric(get(variable)))),.(matchid,win)]
  dataplot[,VarSum:=cut_number(VarSum,n=20)]
  dataplot <- dataplot[,.N,.(VarSum,win)]
  dataplot[,total := sum(N),.(VarSum)]
  dataplot[,Perc := N/total,.(VarSum)]
  if(filter>0)dataplot<-dataplot[VarSum<=filter]
  
  plot1 <- ggplot(dataplot[win==1])+
    geom_bar(aes(x=VarSum,y=Perc,fill=total),stat='identity') +
    scale_y_continuous(label=percent,limits=c(0,1))+
    labs(title=paste('Winning percentage per',variable),x=variable,y='Winning Percentage') + 
    theme(axis.text.x = element_text(angle = 90 ))
  print(plot1)
}


WinningHistogramNormalized <- function(players.dt,variable,filter=-1){
  
  #plot that gives the average winnig percentage per value of normalized 'variable' i.e. per 'variable' per second
  #also buckets the value, as divinding by duration may lead to only one observation otherwise per value.
  
  dataplot <- players.dt[,.(VarSum = sum(as.numeric(get(variable)))/duration),.(matchid,win)]
  dataplot[,VarSum:=cut(VarSum,breaks=100)]
  dataplot <- dataplot[,.N,.(VarSum,win)]
  dataplot[,total := sum(N),.(VarSum)]
  dataplot[,Perc := N/total,.(VarSum)]
  if(filter>0)dataplot<-dataplot[VarSum<=filter]
  
  plot1 <- ggplot(dataplot[win==1])+
    geom_bar(aes(x=VarSum,y=Perc,fill=total),stat='identity') +
    scale_y_continuous(label=percent,limits=c(0,1))+
    labs(title=paste('Winning percentage per',variable, 'per second'),x=paste(variable,'per second'),'Winning Percentage')+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90))
  print(plot1)
}

WinningProbBoxPlot <- function(validation){
  validation[Actual==1,ActualFactor:='Won']
  validation[Actual==0,ActualFactor:='Lost']
  plot1 <- ggplot(data = validation) + 
    geom_boxplot(aes(x = ActualFactor,y = WinningProbality,fill=ActualFactor)) +
    theme_bw() +
    guides(fill=F)+ 
    labs(title='Predicted Winning Probability vs Actual',x='Actual Game Outcome',y='Predicted Winning Probability')
  print(plot1)
}

NormalizedGoldEarnedBoxPlot <- function(teams.dt,teams.normalized.dt){
  n <- nrow(teams.dt)
  dataplot <- as.data.table(rbind(cbind(rep('Total',n),teams.dt$goldearned),cbind(rep('Per second',n),teams.normalized.dt$goldearned)))
  setnames(dataplot,c('GoldEarned','Value'))
  dataplot[,Value:=as.numeric(Value)]
  plot1 <- ggplot(dataplot) +
    geom_boxplot(aes(x=GoldEarned,y=Value)) + 
    facet_wrap( ~GoldEarned, scales = 'free') + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y='GoldEarned')
  print(plot1)
  
}

ClusteringPlot <- function(teams.dt,variable.x,variable.y){
  dataplot <- teams.dt[,.(get(variable.x),get(variable.y),win)]
  dataplot[win==1,GameOutcome := 'Won']
  dataplot[win==0,GameOutcome := 'Lost']

  plot1 <- ggplot(dataplot) +
    geom_point(aes(x=V1,y=V2,color=GameOutcome,shape=GameOutcome))+
    theme_bw()+
    labs(title = paste('Game Outcome by',variable.x,'and',variable.y), x = variable.x, y = variable.y)
  print(plot1)
}

ClusteringPlotAreas <- function(teams.dt){
  
  test.model <- glm(teams.dt[,.(win,deaths,goldearned)],formula= win~deaths+goldearned+I(deaths^2)+I(goldearned^2),family='binomial')
  fake.data <- as.data.table(expand.grid(seq(min(teams.dt$deaths),max(teams.dt$deaths),(min(teams.dt$deaths)+max(teams.dt$deaths))/100),
                                   seq(min(teams.dt$goldearned),max(teams.dt$goldearned),(min(teams.dt$goldearned)+max(teams.dt$goldearned))/100)))
  setnames(fake.data,c('deaths','goldearned'))
  fake.data <- cbind(fake.data,predict(test.model,fake.data,type='response'))
  
  
  dataplot <- teams.dt[,.(goldearned,deaths,win)]
  dataplot[win==1,GameOutcome := 'Won']
  dataplot[win==0,GameOutcome := 'Lost']
  
  plot1 <- ggplot(dataplot) +
    geom_point(aes(x=deaths,y=goldearned,color=GameOutcome,shape=GameOutcome))+
    geom_tile(data=fake.data,aes(x=deaths,y=goldearned,fill=V2),alpha=0.5)+
    theme_bw()+
    labs(title = paste('Game Outcome by goldearned and deaths'))
  print(plot1)
}


ClusteringPlotLines <- function(teams.dt){
  
  test.model <- glm(teams.dt[,.(win,deaths,goldearned)],formula= win~deaths+goldearned+I(deaths^2)+I(goldearned^2),family='binomial')
  fake.data <- as.data.table(expand.grid(seq(min(teams.dt$deaths),max(teams.dt$deaths),(min(teams.dt$deaths)+max(teams.dt$deaths))/100),
                                         seq(min(teams.dt$goldearned),max(teams.dt$goldearned),(min(teams.dt$goldearned)+max(teams.dt$goldearned))/100)))
  setnames(fake.data,c('deaths','goldearned'))
  fake.data <- cbind(fake.data,predict(test.model,fake.data,type='response'))
  fake.data[,win := 0+1*(V2>0.5)]
  fake.data[win==1,GameOutcome := 'Won']
  fake.data[win==0,GameOutcome := 'Lost']
  
  dataplot <- teams.dt[,.(goldearned,deaths,win)]
  dataplot[win==1,GameOutcome := 'Won']
  dataplot[win==0,GameOutcome := 'Lost']
  
  plot1 <- ggplot(dataplot) +
    geom_point(aes(x=deaths,y=goldearned,color=GameOutcome,shape=GameOutcome))+
    geom_tile(data=fake.data,aes(x=deaths,y=goldearned,fill=GameOutcome),alpha=0.3)+
    theme_bw()+
    labs(title = paste('Game Outcome by goldearned and deaths'))
  print(plot1)
}



ggregsubsets <- function(x){
  require(dplyr); require(ggplot2); require(tidyr)
  if(inherits(x, "regsubsets")) x <- summary(x)
  if(!inherits(x, "summary.regsubsets"))
    stop("The input to ggregsubsets() should be the result of regsubsets().")
  df <- bind_cols(
    as.data.frame(x$which), 
    as.data.frame(x[c("rsq","rss","adjr2","cp","bic")]),
    data.frame(nvars = 1:nrow(x$which))
  )
  names(df)[1] <- "Int"
  df %>% 
    mutate(rsq = 100*rsq, adjr2 = 100*adjr2) %>% 
    gather(variable, is_in, -rsq, -rss, -adjr2, -cp, -bic, -nvars) %>% 
    gather(measure, value, -nvars, -variable, -is_in) %>% 
    ggplot(aes(variable, factor(round(value)))) +
    geom_tile(aes(fill = is_in)) +
    facet_wrap(~ measure, scales = "free") +
    scale_fill_manual("", values = c("TRUE" = "black", "FALSE" = "white"), guide = FALSE) +
    labs(x = "", y = "")
}
