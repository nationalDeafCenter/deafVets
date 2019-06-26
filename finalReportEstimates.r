options(stringsAsFactors=FALSE)
library(tidyverse)
library(magrittr)


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

dis <- sum(dat$diss=='disabled')>0
enr <- sum(dat$enrolled=='enrolled')>0

### attainment
attainment <- standard('attainCum',factorProps,dat,diss=dis)

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


#### population breakdown
percentDeaf <- tibble(desc='PERCENT DEAF',percent='percent',n='sample size')%>%
  add_row(
    desc='Percent of overall population who are deaf',
    percent=round(svmean(dat$deaf=='deaf',dat$pwgtp)*100,1),
    n=format(nrow(dat),big.mark=',')
  )%>%
  add_row(
    desc='Percent of all veterans who are deaf',
    percent=filter(dat,vet=='vet')%$% round(svmean(deaf=='deaf',pwgtp)*100,1),
    n=format(sum(dat$vet=='vet'),big.mark=',')
  )%>%
  add_row(
    desc='Percent of post-9/11 veterans who are deaf',
    percent=filter(dat,recentVet)%$%round(svmean(deaf=='deaf',pwgtp)*100,1),
    n=format(sum(dat$recentVet),big.mark=',')
  )%>%
  add_row(
    desc='Percent of non-veterans who are deaf',
    percent=filter(dat,vet=='not vet')%$% round(svmean(deaf=='deaf',pwgtp)*100,1),
    n=format(sum(dat$vet=='not vet'),big.mark=',')
  )%>%
  add_row(desc='',percent='',n='')%>%
  rbind(.,inf(enr=enr,diss=dis,ages=agerange,n=totaln,excludeRows=c('occ','fod'),nms=names(.)))

percentVet <- tibble(desc='PERCENT VETERAN',percent='percent',n='sample size')%>%
  add_row(
    desc='Percent of overall population who are veterans',
    percent=round(svmean(dat$vet=='vet',dat$pwgtp)*100,1),
    n=format(nrow(dat),big.mark=',')
  )%>%
  add_row(
    desc='Percent of overall population who are post-9/11 veterans',
    percent=round(svmean(dat$recentVet,dat$pwgtp)*100,1),
    n=format(nrow(dat),big.mark=',')
  )%>%
  add_row(
    desc='Percent of all deaf people who are vetrans',
    percent=filter(dat,deaf=='deaf')%$% round(svmean(vet=='vet',pwgtp)*100,1),
    n=format(sum(dat$deaf=='deaf'),big.mark=',')
  )%>%
  add_row(
    desc='Percent of all deaf people who are post-9/11 vetrans',
    percent=filter(dat,deaf=='deaf')%$% round(svmean(recentVet,pwgtp)*100,1),
    n=format(sum(dat$deaf=='deaf'),big.mark=',')
  )%>%
  add_row(
    desc='Percent of all hearing people who are vetrans',
    percent=filter(dat,deaf=='hearing')%$% round(svmean(vet=='vet',pwgtp)*100,1),
    n=format(sum(dat$deaf=='hearing'),big.mark=',')
  )%>%
  add_row(
    desc='Percent of all hearing people who are post-9/11 vetrans',
    percent=filter(dat,deaf=='hearing')%$% round(svmean(recentVet,pwgtp)*100,1),
    n=format(sum(dat$deaf=='hearing'),big.mark=',')
  )%>%
  add_row(desc='',percent='',n='')%>%
  rbind(.,inf(enr=enr,diss=dis,ages=agerange,n=totaln,excludeRows=c('occ','fod'),nms=names(.)))

popn <- function(expr) sum(dat$pwgtp[eval(substitute(expr),dat)])
populationSizes <-
  tibble(desc='POPULATION SIZES (# of people)',number=NA)%>%
  add_row(
    desc='# of people in total population',
    number=sum(dat$pwgtp)
  )%>%
  add_row(
    desc='# of people who are deaf',
    number=popn(deaf=='deaf')
  )%>%
  add_row(
    desc='# of veterans',
    number=popn(vet=='vet')
  )%>%
  add_row(
    desc='# deaf veterans',
    number=popn(deaf=='deaf'&vet=='vet')
  )%>%
  add_row(
    desc='# of post-9/11 veterans',
    number=popn(recentVet)
  )%>%
  add_row(
    desc='# of deaf post-9/11 veterans',
    number=popn(deaf=='deaf'&recentVet)
  )%>%
  add_row(desc=paste0('College (Undergrad) Enrollment (ages ',agerange,')'))%>%
  add_row(
    desc='# people enrolled in college (total)',
    number=popn(enrollment=='undergrad')
  )%>%
  add_row(
    desc='# veterans enrolled in college',
    number=popn(vet=='vet'&enrollment=='undergrad')
  )%>%
  add_row(
    desc='# deaf veterans enrolled in college',
    number=popn(enrollment=='undergrad'&vet=='vet'&deaf=='deaf')
  )%>%
  add_row(
    desc='# post-9/11 veterans enrolled in college',
    number=popn(enrollment=='undergrad'&recentVet)
  )%>%
  add_row(
    desc='# deaf post-9/11 veterans enrolled in college',
    number=popn(enrollment=='undergrad'&recentVet&deaf=='deaf')
  )%>%
  mutate(number=ifelse(is.na(number),'',format(number,big.mark=',')))%>%
  add_row(desc='')%>%
    rbind(.,inf(enr=enr,diss=dis,ages=agerange,n=totaln,excludeRows=c('occ','fod'),nms=names(.)))


popBreakdown <- list(
  `% deaf`=percentDeaf,
  `% vets`=percentVet,
  `pop sizes`=populationSizes
)


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
  rowNames=FALSE,colNames=FALSE,colWidths='auto')


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


