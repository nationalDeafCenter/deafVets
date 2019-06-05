###################################################################
###### now do everything for non-disabled only
###################################################################
options(stringsAsFactors=FALSE)
library(dplyr)

if(!exists("dat")){
    if('vetData.RData'%in%list.files()){
        load('vetData.RData')
    } else source('makeData.r')
}

if(!exists("ages")){
  ages <- c(18,54)
  warning('Setting age range to 18-54')
}

dat <- filter(dat,agep>=ages[1],agep<=ages[2])

source('generalCode/estimationFunctions.r')
source('generalCode/median.r')
source('functions.r')

nonDiss <- filter(dat,diss=='nondisabled')
rm(dat);gc()

agerange <- paste(min(nonDiss$agep),'-',max(nonDiss$agep))
totaln <- nrow(nonDiss)

### attainment
attainmentND <- standard('postSec',factorProps,nonDiss,diss=FALSE)

employmentND <- standard('employment',empFun,nonDiss,diss=FALSE,
                       byAttainment=bysub(1,empFun,nonDiss,attainCum)
                       )

inLaborForceND <- lapply(employmentND,
                       function(x) setNames(data.frame(x[,colnames(x)==''],
                                                       100-x[['% Not In Labor Force']],
                                                       x[['Not In Labor Force SE']],x$n),
                                            c(rep('',sum(colnames(x)=='')),'% In Labor Force','SE','n')))

### median earnings
medianEarningsND <-
    standard(~pernp,med,filter(nonDiss,fulltime),diss=FALSE,
             byAttainment=bysub(~pernp,med,filter(nonDiss,fulltime),attainCum),
             wholePopulation=overall(~pernp,med,nonDiss),
             employed=overall(~pernp,med,filter(nonDiss,employment=='Employed')))



medianEarningsND <- lapply(medianEarningsND,
                         function(x) {
                             names(x)[names(x)=='1'] <- 'Med. Earnings'
                             names(x)[names(x)=='2'] <- 'SE'
                             names(x)[names(x)=='3'] <- 'n'
                             x
                         })


#### population breakdown
pDeafND <- factorProps('deaf',nonDiss)
pVetND <- factorProps('vet',nonDiss)
pRecentVetND <- estExpr(recentVet,sdat=nonDiss)
pVetByDeafND <- FIX(nonDiss%>%group_by(deaf)%>%do(x=factorProps('vet',sdat=.)))
pRecentVetByDeafND <- FIX(nonDiss%>%group_by(deaf)%>%do(x=estExpr(recentVet,sdat=.)))
pDeafByVetND <- FIX(nonDiss%>%group_by(vet)%>%do(x=factorProps('deaf',sdat=.)))
pDeafByRecentVetND <- factorProps('deaf',filter(nonDiss,recentVet))

pvND <- c(pVetND[c('% vet','vet SE')],pRecentVetND)
pvbdND <- cbind(pVetByDeafND[,1],pVetByDeafND[,c('% vet','vet SE')],pRecentVetByDeafND[,-1])
pvND <- rbind(c('Overall Population',round(pvND,1)),pvbdND)

pdND <- rbind(c('Overall Population',round(pDeafND[c('% deaf','deaf SE','n')],1)),
            cbind(pDeafByVetND[,1],pDeafByVetND[,c('% deaf','deaf SE','n')]),
            c('Post 9/2001 Vet',round(pDeafByRecentVetND[c('% deaf','deaf SE','n')],1)))

opND <- rbind(c('Proportion Deaf',rep('',ncol(pvND)-1)),
            c('','% Deaf','SE','n','',''),
            cbind(as.matrix(pdND,dimnames=NULL),matrix(nrow=nrow(pdND),ncol=ncol(pvND)-ncol(pdND))),
            rep('',ncol(pvND)),
            c('Proportion Veteran',rep('',ncol(pvND)-1)),
            c('','% Veteran','SE','% Post 9/2001 Vet','SE','n'),
            as.matrix(pvND,dimnames=NULL))

popBreakdownND <- list(
    overallPercentages=opND
)

for(subst in c('Age','sex','raceEth'))
    popBreakdownND[[subst]] <- overall(subst,factorProps,nonDiss)


folder <- paste0('ages',gsub(' ','',agerange),'/nonDiss/')

openxlsx::write.xlsx(reformatAll(inLaborForceND,enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'LaborForceParticipation2012-16VETS.xlsx'),colWidths='auto')
openxlsx::write.xlsx(reformatAll(attainmentND,enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'EducatonalAttainment2012-16VETS.xlsx'),colWidths='auto')
openxlsx::write.xlsx(reformatAllEmp(employmentND,enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'employment2012-16VETS.xlsx'),colWidths='auto')
openxlsx::write.xlsx(reformatAll(medianEarningsND,enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'medianEarnings2012-16VETS.xlsx'),colWidths='auto')
openxlsx::write.xlsx(popBreakdownND,paste0(folder,'populationBreakdown2012-16VETS.xlsx'),rowNames=TRUE,colWidths='auto')


