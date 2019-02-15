options(stringsAsFactors=FALSE)
library(dplyr)
library(broom)

## if(!exists("dat")){
##     if('vetData.RData'%in%list.files()){
##         load('vetData.RData')
##     } else source('makeData.r')
## }


source('../../generalCode/estimationFunctions.r')
source('../../generalCode/median.r')

agerange <- paste(min(dat$agep),'-',max(dat$agep))

inf <- function(enr,ages=agerange,years='2012-16')
  cbind(c(
    'Estimated from ACS',
    paste('Years:',years),
    paste('Ages: ',ages),
    paste(ifelse(enr,'INCLUDES','EXCLUDES'),'people currently enrolled in school'),
    'Vet: On active duty in the past, but not now',
    'Vet/Served Since 9/2001: Served September 2001 or later',
    'Excludes Institutionalized People',
    'Occupational Category for full-time employed people only',
    'Field of Degree for people with Bachelors degrees or higher',''))


reformatAll <- function(lll,enr,ages=agerange,years='2012-16'){
  if('byDiss'%in%names(lll)){
    discol <- which(sapply(lll$byDiss,function(x) any(x=='disabled')))
    lll$byDiss <- lll$byDiss[!lll$byDiss[,discol]%in% c(
       "No Service Connected Disability" ,  "Not Vet"    ,
       "Vet. Service Connected Disability"),]
  }

  info <- lll$info
  lll$info <- NULL

  res <- do.call('rbind',lapply(names(lll),function(nn)reformat(lll[[nn]],nn)))
  #if(!is.null(info)) res <- list(res,info)
  if(!missing(enr)){
    nnn <- names(res)
    names(res) <- NULL
    info <- inf(enr,ages,years)
    res <- rbind(cbind(info,matrix('',nrow(info),ncol(res)-1)),nnn,as.matrix(res))
  }
  res
}

reformat1 <- function(ddd){
  deafCol <- which(sapply(ddd,function(x) any(x=='deaf')))
  vetCol <-  which(sapply(ddd,function(x) any(x=='vet')))

  est <- ddd[,!grepl('SE',names(ddd))]
  se <- ddd[,grepl('SE',names(ddd))]

  res <- NULL
  for(vv in c('vet','not vet','Served since 2001')){
    newEst <- NULL
    newSE <- NULL
    for(dd in c('deaf','hearing')){
      newEst <-rbind(newEst,est[est[,deafCol]==dd & est[,vetCol]==vv,])
      newSE <- rbind(newSE,se[est[,deafCol]==dd & est[,vetCol]==vv,])
    }
    diff <- abs(newEst[1,-c(1:2,ncol(est))]-newEst[2,-c(1:2,ncol(est))])
    se.diff <- sqrt(se[1,]^2+se[2,]^2)
    sig <- which(diff>2*se.diff)

    for(i in 1:nrow(newSE)) newSE[i,] <- paste0('(',newSE[i,],')')
    newSE[1,sig] <- paste0(newSE[1,sig],'*')
    newSE <- cbind("","",newSE,"")
    names(newSE) <- names(newEst)

    res <- rbind(res,newEst[1,],newSE[1,],newEst[2,],newSE[2,])
  }

  res <- res[,c(vetCol,deafCol,seq(ncol(res))[-c(vetCol,deafCol)])]
  res[res[,2]=='hearing',1] <- ''
  res
}

reformat <- function(ddd,name=NULL){
  deafCol <- which(sapply(ddd,function(x) any(x=='deaf')))
  vetCol <-  which(sapply(ddd,function(x) any(x=='vet')))
  names(ddd)[deafCol] <- 'deaf'
  names(ddd)[vetCol] <- 'vet'

  nsub <- sum(names(ddd)=='')
  if(nsub==0){
    res <- reformat1(ddd)
   res <- cbind(c('Overall',rep('',nrow(res)-1)),res)
  } else{
    stopifnot(nsub==1)
    subCol <- which(names(ddd)=='')

    res <- NULL
    for(ss in unique(ddd[,subCol])){
      newRes <- reformat1(ddd[ddd[,subCol]==ss,-subCol])
      res <- rbind(res,cbind(c(ss,rep('',nrow(newRes)-1)),newRes))
    }
  }
  names(res)[1] <- ''

  if(!is.null(name)) res <- rbind(c(name,rep('',ncol(res)-1)),res)

  res
}


FIX <- function(tib){
    lst <- sapply(tib,is.list)

    out <- NULL
    for(nn in names(tib)[lst]){
        out <- round(cbind(out,
                     do.call('rbind',tib[[nn]])),1)
    }
    out <- cbind(tib[,!lst],out)
    out[sapply(out,is.factor)] <- sapply(out[sapply(out,is.factor)],as.character)
    names(out)[1:sum(!lst)] <- ''

    out
}

overall <- function(x,FUN,dat){
    aa <- FIX(dat%>%group_by(deaf,vet)%>%do(x=FUN(x,sdat=.)))
    bb <- FIX(dat%>%filter(recentVet)%>%group_by(deaf)%>%do(x=FUN(x,sdat=.)))
    bb <- cbind(bb[,1],'Served since 2001',bb[,2:ncol(bb)])
    names(bb) <- names(aa)
    out <- rbind(aa,bb)
    out[order(out[,1]),]
}

overall2 <- function(x,FUN,dat){
    tib <- overall(x,FUN,dat)
    out <- t(tib[,-ncol(tib)])
    colnames(out) <- NULL
    out
}

bysub <- function(x,FUN,dat,subs){
    subs <- enquo(subs)

    aa <- FIX(dat%>%group_by(deaf,vet,!!subs)%>%do(x=FUN(x,sdat=.)))
    bb <- FIX(dat%>%filter(recentVet)%>%group_by(deaf,!!subs)%>%do(x=FUN(x,sdat=.)))
    bb <- cbind(bb[,1],'Served since 2001',bb[,2:ncol(bb)])
    names(bb) <- names(aa)
    out <- rbind(aa,bb)
    out[order(out[,1]),]
}


standard <- function(x,FUN,dat,...)
    list(
        overall=overall(x,FUN,dat),
        byAge=bysub(x,FUN,dat,Age),
        bySex=bysub(x,FUN,dat,sex),
        byRace=bysub(x,FUN,dat,raceEth),
        byDiss=rbind(
            bysub(x,FUN,dat,diss),
            bysub(x,FUN,dat,blind),
            bysub(x,FUN,dat,selfCare),
            bysub(x,FUN,dat,indLiv),
            bysub(x,FUN,dat,amb),
           # bysub(x,FUN,dat,servDis),
            bysub(x,FUN,dat,cogDif)),
        ...
    )


### attainment
attainment <- standard('attainCum',factorProps,dat)

### attainment (not currently enrolled)
attainmentNotEnrolled <- standard('attainCum',factorProps,filter(dat,enrolled=='not enrolled'))

### post-secondary (not currently enrolled)
postSecNotEnrolled <- standard('postSec',factorProps,filter(dat,enrolled=='not enrolled'))

### post-secondary incl. enrollment
postSecEnr <- standard('postSecEnrolled',factorProps,dat)


### employment
empFun <- function(x,sdat){
    emp <- factorProps('employment',sdat)
    ft <- estExpr(fulltime,employment=='Employed',sdat=sdat)
    ft <- c(ft[1],100-ft[1],ft[2:3])
    names(ft) <- c('% Employed Full-Time','% Employed Part-Time','Full/Part-Time SE','Employed n')
    c(emp,ft)
}

employment <- standard('employment',empFun,dat,
                       byAttainment=bysub(1,empFun,dat,attainCum)
                       )

inLaborForce <- lapply(employment,
                       function(x) setNames(data.frame(x[,colnames(x)==''],
                                                       100-x[['% Not In Labor Force']],
                                                       x[['Not In Labor Force SE']],x$n),
                                            c(rep('',sum(colnames(x)=='')),'% In Labor Force','SE','n')))

### median earnings
medianEarnings <-
    standard(~pernp,med,filter(dat,fulltime),
             byAttainment=bysub(~pernp,med,filter(dat,fulltime),attainCum),
             overall=overall(~pernp,med,dat),
             employed=overall(~pernp,med,filter(dat,employment=='Employed')))



medianEarnings <- lapply(medianEarnings,
                         function(x) {
                             names(x)[names(x)=='1'] <- 'Med. Earnings'
                             names(x)[names(x)=='2'] <- 'SE'
                             names(x)[names(x)=='3'] <- 'n'
                             x
                         })


#### population breakdown
pDeaf <- factorProps('deaf',dat)
pVet <- factorProps('vet',dat)
pRecentVet <- estExpr(recentVet,sdat=dat)
pVetByDeaf <- FIX(dat%>%group_by(deaf)%>%do(x=factorProps('vet',sdat=.)))
pRecentVetByDeaf <- FIX(dat%>%group_by(deaf)%>%do(x=estExpr(recentVet,sdat=.)))
pDeafByVet <- FIX(dat%>%group_by(vet)%>%do(x=factorProps('deaf',sdat=.)))
pDeafByRecentVet <- factorProps('deaf',filter(dat,recentVet))

pv <- c(pVet[c('% vet','vet SE')],pRecentVet)
pvbd <- cbind(pVetByDeaf[,1],pVetByDeaf[,c('% vet','vet SE')],pRecentVetByDeaf[,-1])
pv <- rbind(c('Overall Population',round(pv,1)),pvbd)

pd <- rbind(c('Overall Population',round(pDeaf[c('% deaf','deaf SE','n')],1)),
            cbind(pDeafByVet[,1],pDeafByVet[,c('% deaf','deaf SE','n')]),
            c('Post 9/2001 Vet',round(pDeafByRecentVet[c('% deaf','deaf SE','n')],1)))

op <- rbind(c('Proportion Deaf',rep('',ncol(pv)-1)),
            c('','% Deaf','SE','n','',''),
            cbind(as.matrix(pd,dimnames=NULL),matrix(nrow=nrow(pd),ncol=ncol(pv)-ncol(pd))),
            rep('',ncol(pv)),
            c('Proportion Veteran',rep('',ncol(pv)-1)),
            c('','% Veteran','SE','% Post 9/2001 Vet','SE','n'),
            as.matrix(pv,dimnames=NULL))

popBreakdown <- list(
    overallPercentages=op
)

for(subst in c('Age','sex','raceEth'))
    popBreakdown[[subst]] <- overall(subst,factorProps,dat)

popBreakdown$byDisability <-
    rbind(c('Deaf',rep('',2),'Hearing',rep('',2)),
          rep(c('not vet','vet','Served since 2001'),2),
    do.call('rbind',lapply(
                        c('diss','blind','selfCare','indLiv','amb','servDis','cogDif'),
                        function(x) overall2(x,factorProps,dat)[3:4,])))


info <- data.frame(c('Dataset: ACS',
                     'Years: 2012-2016',
                     'Ages: 25-64',
                     'Vet: "On active duty in the past, but not now"',
                     'Vet/Served Since 9/2001: "Served September 2001 or later"',
                     'Excludes Institutionalized People',
                     'Occupational Category for full-time employed people only',
                     'Field of Degree for people with Bachelors degrees or higher'),
                   stringsAsFactors=FALSE)

names(info) <- c('')

attainment$info <- info
employment$info <- rbind(info,'Full/Part-time expressed as percentage of employed people')
medianEarnings$info <- rbind(info,c('Earnings are for full-time employed people, except in "overall" and "employed" tabs'))
popBreakdown$info <- info

popBreakdown[1:4] <- lapply(popBreakdown[1:4],t)
inLaborForce$info <- rbind(info,'Calculated from employment numbers')




openxlsx::write.xlsx(inLaborForce,'LaborForceParticipation2012-16VETS.xlsx',colWidths='auto')
openxlsx::write.xlsx(reformatAll(attainment,enr=TRUE),'EducatonalAttainment2012-16VETS.xlsx',colWidths='auto')
openxlsx::write.xlsx(reformatAll(attainmentNotEnrolled,enr=FALSE),'EducatonalAttainment2012-16VETSnotEnrolled.xlsx',colWidths='auto')
openxlsx::write.xlsx(reformatAll(postSecNotEnrolled,enr=FALSE),'postSecAttainment2012-16VETSnotEnrolled.xlsx',colWidths='auto')
openxlsx::write.xlsx(reformatAll(postSecEnr,enr=TRUE),'EducatonalAttainment2012-16VETSwEnrollment.xlsx',colWidths='auto')
openxlsx::write.xlsx(employment,'employment2012-16VETS.xlsx',colWidths='auto')
openxlsx::write.xlsx(medianEarnings,'medianEarnings2012-16VETS.xlsx',colWidths='auto')
openxlsx::write.xlsx(popBreakdown,'populationBreakdown2012-16VETS.xlsx',rowNames=TRUE,colWidths='auto')

library(ggplot2)

## employment by age
empByAge <- FIX(dat%>%group_by(deaf,vet,agep)%>%do(x=estExpr(employment=="Employed",sdat=.)))
names(empByAge)[1:3] <- c('deaf','vet','Age')

ggplot(empByAge,aes(Age,est,color=deaf,linetype=vet))+geom_smooth()+labs(color=NULL,y='% Employed')
ggsave('employmentByAge.jpg')

## earnings by age
ernByAge <- FIX(dat%>%filter(fulltime)%>%group_by(deaf,vet,agep)%>%do(x=med(~pernp,sdat=.)))
names(ernByAge) <- c('deaf','vet','Age','ern','se','n')

ggplot(ernByAge,aes(Age,ern,color=deaf,linetype=vet))+geom_smooth()+labs(color=NULL,y='Median Earnings (Full-Time Employed)')
ggsave('earningsByAge.jpg')

baByAge <- FIX(dat%>%group_by(deaf,vet,agep)%>%do(x=estExpr(attainCum>='Bachelors',sdat=.)))
names(baByAge)[1:3] <- c('deaf','vet','Age')
ggplot(baByAge,aes(Age,est,color=deaf,linetype=vet))+geom_smooth()+ylab('% At Least BA')
ggsave('bachelorsByAge.jpg')

### age distribution of deaf & hearing vets
ageDistDeafVet <- factorProps('agep',filter(dat,deaf=='deaf',vet=='vet'),cum=FALSE)
ageDistHearVet <- factorProps('agep',filter(dat,deaf=='hearing',vet=='vet'),cum=FALSE)
ageDistDeafRecent <- factorProps('agep',filter(dat,deaf=='deaf',recentVet),cum=FALSE)
ageDistHearRecent <- factorProps('agep',filter(dat,deaf=='hearing',recentVet),cum=FALSE)

vetAge <- data.frame(age=rep(25:64,2),deaf=rep(c('deaf','hearing'),each=40),
                     prob=c(ageDistDeafVet[paste('%', 25:64)], ageDistHearVet[paste('%',25:64)]))

ggplot(vetAge,aes(age,prob))+geom_col()+facet_grid(deaf~.)+ggtitle('Distribution of Ages for Hearing & Deaf Veterans')
ggsave('vetAgeDist.jpg')

recentAge <- data.frame(age=rep(25:64,2),deaf=rep(c('deaf','hearing'),each=40),
                     prob=c(ageDistDeafRecent[paste('%', 25:64)], ageDistHearRecent[paste('%',25:64)]))

ggplot(recentAge,aes(age,prob))+geom_col()+facet_grid(deaf~.)+ggtitle('Distribution of Ages for Hearing & Deaf Veterans who Served 9/2001 or Later')
ggsave('recentVetAgeDist.jpg')





save(list=ls()[sapply(ls(),function(x) object.size(get(x)))<1e7],file='results.RData')

attainmentplot <- function(ddd){

  deafCol <- which(sapply(ddd,function(x) any(x=='deaf')))
  vetCol <-  which(sapply(ddd,function(x) any(x=='vet')))

  names(ddd)[deafCol] <- 'deaf'
  names(ddd)[vetCol] <- 'vet'


  ddd <- ddd[ddd$vet=='vet',-c(grep('No HS',names(ddd)),vetCol)]

  nsub <- sum(names(ddd)=='')
  if(nsub==0){
    noSub <- TRUE
    ddd <- cbind(deaf=c(ddd$deaf,'gap'),
      rbind(ddd[,-1],unlist( ddd[ddd$deaf=='deaf',-1]-ddd[ddd$deaf=='hearing',-1])))

  } else{
    stopifnot(nsub==1)
    noSub <- FALSE
    names(ddd)[names(ddd)==''] <- 'sub'
    for(ss in unique(ddd$sub))
      ddd <- cbind(
        deaf=c(ddd$deaf,'gap'),
        sub=c(ddd$sub,ss),
        rbind(ddd[,-c(1,2)],
          unlist(ddd[ddd$deaf=='deaf'&ddd$sub==ss,-c(1:2)]-ddd[ddd$deaf=='hearing'&ddd$sub==ss,-c(1:2)])))
  }

  est <- ddd[,!grepl('SE',names(ddd))]

  e2 <- tidyr::gather(est,level,percent,starts_with('%'))
  e2$level <- factor(e2$level,levels=unique(e2$level))

  e2$gap <- factor(ifelse(e2$deaf=='gap','Gap','Percent'),levels=c('Percent','Gap'))

  map <- if(noSub) aes(level,percent,group=deaf,color=deaf) else aes(level,percent,group=interaction(deaf,sub),linetype=sub,color=deaf)

  p <- ggplot(e2,aes(level,percent,group=deaf,color=deaf))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))
  p <- if(noSub) p+facet_wrap(~gap,scales="free_y") else p+facet_grid(gap~sub,scales="free_y")
  p
}


