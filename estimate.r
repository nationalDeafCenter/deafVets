options(stringsAsFactors=FALSE)
library(dplyr)
library(broom)

## if(!exists("dat")){
##     if('vetData.RData'%in%list.files()){
##         load('vetData.RData')
##     } else source('makeData.r')
## }


source('../generalCode/estimationFunctions.r')
source('../generalCode/median.r')
source('functions.r')


agerange <- paste0(min(dat$agep),'-',max(dat$agep))
totaln <- nrow(dat)




### attainment
attainment <- standard('attainCum',factorProps,dat)

### attainment (not currently enrolled)
attainmentNotEnrolled <- standard('attainCum',factorProps,filter(dat,enrolled=='not enrolled'))

### post-secondary (not currently enrolled)
postSecNotEnrolled <- standard('postSec',factorProps,filter(dat,enrolled=='not enrolled'))

### post-secondary incl. enrollment
postSecEnr <- standard('postSecEnrolled',factorProps,dat)


### employment

employment <- standard('employment',empFun,dat,
                       byAttainment=bysub(1,empFun,dat,attainCum)
                       )

employment <- sapply( employment,
  function(emp){
  for(j in which(names(emp)=='')){
    if('deaf'%in%emp[,j]){
      names(emp)[j] <- 'deaf'
    } else if('vet'%in%emp[,j]){
      names(emp)[j] <- 'vet'
    } else names(emp)[j] <- 'subgroup'
  }
  emp
  },simplify=FALSE)

do.call('bind_rows',employment)%>%dplyr::select(deaf,vet,subgroup,everything())

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
                     paste('Ages:',agerange),
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


dis <- sum(dat$diss=='disabled')>0



openxlsx::write.xlsx(
  list(
    attainment=reformatAll(attainment,enr=TRUE,diss=dis,ages=agerange,n=totaln),
    employment=employment
  ),
  file=paste0(
    'ages',
    agerange,
    '/attainmentEmployment',
    ifelse(dis,'Total','Nondisabled'),
    '.xlsx'
  )
)

openxlsx::write.xlsx(
  popBreakdown,
  paste0(
    'ages',
    agerange,
    '/populationBreakdown',
    ifelse(dis,'Total','Nondisabled'),
    '.xlsx'),
  rowNames=TRUE,colWidths='auto')


 ##  #inLaborForce,'LaborForceParticipation2012-16VETS.xlsx',colWidths='auto')
## openxlsx::write.xlsx(reformatAll(attainment,enr=TRUE),'EducatonalAttainment2012-16VETS.xlsx',colWidths='auto')
## openxlsx::write.xlsx(reformatAll(attainmentNotEnrolled,enr=FALSE),'EducatonalAttainment2012-16VETSnotEnrolled.xlsx',colWidths='auto')
## openxlsx::write.xlsx(reformatAll(postSecNotEnrolled,enr=FALSE),'postSecAttainment2012-16VETSnotEnrolled.xlsx',colWidths='auto')
## openxlsx::write.xlsx(reformatAll(postSecEnr,enr=TRUE),'EducatonalAttainment2012-16VETSwEnrollment.xlsx',colWidths='auto')
## openxlsx::write.xlsx(employment,'employment2012-16VETS.xlsx',colWidths='auto')
## openxlsx::write.xlsx(medianEarnings,'medianEarnings2012-16VETS.xlsx',colWidths='auto')






## library(ggplot2)

## ## employment by age
## empByAge <- FIX(dat%>%group_by(deaf,vet,agep)%>%do(x=estExpr(employment=="Employed",sdat=.)))
## names(empByAge)[1:3] <- c('deaf','vet','Age')

## ggplot(empByAge,aes(Age,est,color=deaf,linetype=vet))+geom_smooth()+labs(color=NULL,y='% Employed')
## ggsave('employmentByAge.jpg')

## ## earnings by age
## ernByAge <- FIX(dat%>%filter(fulltime)%>%group_by(deaf,vet,agep)%>%do(x=med(~pernp,sdat=.)))
## names(ernByAge) <- c('deaf','vet','Age','ern','se','n')

## ggplot(ernByAge,aes(Age,ern,color=deaf,linetype=vet))+geom_smooth()+labs(color=NULL,y='Median Earnings (Full-Time Employed)')
## ggsave('earningsByAge.jpg')

## baByAge <- FIX(dat%>%group_by(deaf,vet,agep)%>%do(x=estExpr(attainCum>='Bachelors',sdat=.)))
## names(baByAge)[1:3] <- c('deaf','vet','Age')
## ggplot(baByAge,aes(Age,est,color=deaf,linetype=vet))+geom_smooth()+ylab('% At Least BA')
## ggsave('bachelorsByAge.jpg')

## ### age distribution of deaf & hearing vets
## ageDistDeafVet <- factorProps('agep',filter(dat,deaf=='deaf',vet=='vet'),cum=FALSE)
## ageDistHearVet <- factorProps('agep',filter(dat,deaf=='hearing',vet=='vet'),cum=FALSE)
## ageDistDeafRecent <- factorProps('agep',filter(dat,deaf=='deaf',recentVet),cum=FALSE)
## ageDistHearRecent <- factorProps('agep',filter(dat,deaf=='hearing',recentVet),cum=FALSE)

## vetAge <- data.frame(age=rep(25:64,2),deaf=rep(c('deaf','hearing'),each=40),
##                      prob=c(ageDistDeafVet[paste('%', 25:64)], ageDistHearVet[paste('%',25:64)]))

## ggplot(vetAge,aes(age,prob))+geom_col()+facet_grid(deaf~.)+ggtitle('Distribution of Ages for Hearing & Deaf Veterans')
## ggsave('vetAgeDist.jpg')

## recentAge <- data.frame(age=rep(25:64,2),deaf=rep(c('deaf','hearing'),each=40),
##                      prob=c(ageDistDeafRecent[paste('%', 25:64)], ageDistHearRecent[paste('%',25:64)]))

## ggplot(recentAge,aes(age,prob))+geom_col()+facet_grid(deaf~.)+ggtitle('Distribution of Ages for Hearing & Deaf Veterans who Served 9/2001 or Later')
## ggsave('recentVetAgeDist.jpg')





save(list=ls()[sapply(ls(),function(x) object.size(get(x)))<1e7],file= paste0(
    'ages',
    agerange,
    '/results',
    ifelse(dis,'Total','Nondisabled'),
    '.RData'))


