#library(parallel)
library(splines)
library(reshape2)
library(lmtest)
#cl <- makeCluster(3)

source('functions.r')
source('../generalCode/estimationFunctions.r')

saveMod <- function(mod,file,...){
  mod$data <- NULL
  mod$qr <- NULL
  nm <- toString(match.call()[[2]])
  assign(nm,mod)
  save(list=nm,file=file,...)
}

load('vetSurvData.RData')
dat <- filter(dat,agep>=25)

rates <- dat%>%group_by(deaf)%>%
  group_map(~data.frame(level=factor(levels(dat$postSec),levels=levels(dat$postSec)),
    percent=sapply(levels(dat$postSec),function(x) svmean(.x$postSec>=x,.x$pwgtp))))%>%
  spread(deaf,percent)


## enrollment proportions
enrModD <- glm(enrolled~ns(agep,5),family=binomial,data=filter(dat,deaf=='deaf'),weights=pwgtp)
enrModH <- glm(enrolled~ns(agep,5),family=binomial,data=filter(dat,deaf=='hearing'),weights=pwgtp)
pd <- data.frame(agep=seq(25,55))
pdD <- pd%>%mutate(pred=predict(enrModD,pd,type='response'),deaf='deaf')
pdH <- pd%>%mutate(pred=predict(enrModH,pd,type='response'),deaf='hearing')
pd <- rbind(pdD,pdH)
ggplot(pd,aes(agep,pred,group=deaf,color=deaf))+geom_line()+xlab('Proportion Enrolled')+ylab('Age')

enrProp <- dat%>%group_by(deaf,agep)%>%summarize(propEnr=svmean(enrolled,pwgtp))

ggplot(full_join(enrProp,pd),aes(agep,propEnr,color=deaf,group=deaf))+geom_point()+geom_line(aes(y=pred))+ylab('Proportion Enrolled')+xlab('Age')
ggsave('enrolledProportionByAge.jpg')

## transform data
ldat <- transformData(dat)


## now just deaf
modDeaf <- model(data=filter(ldat,deaf=='deaf'),surveySEs=FALSE)

## now both, just with interactions
modInt <- model(event~level*deaf,data=ldat,surveySEs=FALSE)
plotInt(modInt,ldat)
ggsave('noCovariates.pdf',width=10,height=5)
plotDiff(modInt,ldat)
ggsave('noCoveraitesDiff.pdf')


### now control for stuff
ldat$female <- ldat$sex=='Female'
ldat$raceEth <- relevel(factor(ldat$raceEth),ref='White')
ldat$ageCentered <- ldat$agep-35
modFull <- model(event~level*deaf+female+ns(ageCentered,df=5)*deaf+raceEth+nativity,data=ldat,surveySEs=FALSE)
save(modFull,file='fullMod.RData')
plotInt(modFull,ldat)
ggsave('full.pdf',width=10,height=6)
plotInt(modFull,ldat,reverse=TRUE)
ggsave('fullReverse.pdf',width=10,height=6)
survFull <- survivalHazard(modFull,ldat)%>%select(level,deaf,hazard,overallAttainment=attainment)%>%
  mutate(advanceProb=1-hazard)%>%select(hazard,advanceProb,overallAttainment,everything())%>%
  melt()%>%dcast(level~variable+deaf)
fullCoef <- coeftest(modFull,vcov.=modFull$vcov)
hazardOddsRatios(modFull,ldat,age=35)
ggsave('oddRatiosFull35.pdf')

modFullRecent <-  model(event~level*deaf+female+ns(ageCentered,df=5)*deaf+raceEth+nativity,data=filter(ldat,recentVet),surveySEs=TRUE)
save(modFullRecent,file='fullModRecent.RData')
plotInt(modFullRecent,ldat)+ggtitle('Post-9/11 Vets')
ggsave('recent.pdf',width=10,height=6)

######### what about drat==5? (i.e. disability rating .70, 80, 90, or 100 percent)

dat2 <- filter(dat,agep>=25,(deaf=='hearing')|(drat==5))

xtabs(~drat+deaf,dat2,addNA=TRUE)

rates2 <- dat2%>%group_by(deaf)%>%
  group_map(~data.frame(level=factor(levels(dat$postSec),levels=levels(dat$postSec)),
    percent=sapply(levels(dat$postSec),function(x) svmean(.x$postSec>=x,.x$pwgtp))))%>%
  spread(deaf,percent)

## transform data
ldat2 <- transformData(dat2)


## now just deaf
modDeaf2 <- model(data=filter(ldat2,deaf=='deaf'),surveySEs=FALSE)

## now both, just with interactions
modInt2 <- model(event~level*deaf,data=ldat2,surveySEs=FALSE)
plotInt(modInt2,ldat2)
ggsave('noCovariates2.pdf',width=10,height=5)
plotDiff(modInt2,ldat2)
ggsave('noCoveraitesDiff2.pdf')


### now control for stuff
ldat2$female <- ldat2$sex=='Female'
ldat2$raceEth <- relevel(factor(ldat2$raceEth),ref='White')
ldat2$ageCentered <- ldat2$agep-35
modFull2 <- model(event~level*deaf+female+ns(ageCentered,df=5)*deaf+raceEth+nativity,data=ldat2,surveySEs=FALSE)
save(modFull2,file='fullMod2.RData')
plotInt(modFull2,ldat=ldat2)+ggtitle('70%+ Disability Rating')
ggsave('full2.pdf',width=10,height=6)
plotInt(modFull2,ldat=ldat2,reverse=TRUE)+ggtitle('70%+ Disability Rating')
ggsave('fullReverse2.pdf',width=10,height=6)
survFull2 <- survivalHazard(modFull2,ldat=ldat2)%>%select(level,deaf,hazard,overallAttainment=attainment)%>%
  mutate(advanceProb=1-hazard)%>%select(hazard,advanceProb,overallAttainment,everything())%>%
  melt()%>%dcast(level~variable+deaf)
fullCoef2 <- coeftest(modFull2,vcov.=modFull2$vcov)
hazardOddsRatios(modFull2,ldat2,age=35)+ggtitle('70%+ Disability Rating')
ggsave('oddRatiosFull352.pdf')

modFullRecent <-  model(event~level*deaf+female+ns(ageCentered,df=5)*deaf+raceEth+nativity,data=filter(ldat2,recentVet),surveySEs=TRUE)
save(modFullRecent,file='fullModRecent.RData')
plotInt(modFullRecent)+ggtitle('Post-9/11 Vets')
ggsave('recent.pdf',width=10,height=6)

### what about when drat=1 (ie 0%)
ldat0 <- transformData(filter(dat,(deaf=='hearing')|(drat==1)))
ldat0$female <- ldat0$sex=='Female'
ldat0$raceEth <- relevel(factor(ldat0$raceEth),ref='White')
ldat0$ageCentered <- ldat0$agep-35
modFull0 <- model(event~level*deaf+female+ns(ageCentered,df=5)*deaf+raceEth+nativity,data=ldat0,surveySEs=FALSE)
save(modFull0,file='fullMod0.RData')
plotInt(modFull0,ldat=ldat0)+ggtitle('0% Disability Rating')
ggsave('full0.pdf',width=10,height=6)
plotInt(modFull0,ldat=ldat0,reverse=TRUE)+ggtitle('0% Disability Rating')
ggsave('fullReverse0.pdf',width=10,height=6)

hazardOddsRatios(modFull0,ldat0,age=35)+ggtitle('0% Disability Rating')
ggsave('oddRatiosFull350.pdf')
