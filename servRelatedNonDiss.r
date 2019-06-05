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

servRelated <- filter(dat,dratx==1)
rm(dat);gc()

agerange <- paste(min(servRelated$agep),'-',max(servRelated$agep))
totaln <- nrow(servRelated)

### attainment
attainmentND <- standard('postSec',factorProps,filter(servRelated,diss=='nondisabled'),diss=FALSE)

attainment <- standard('postSec',factorProps,servRelated,diss=FALSE)

#### population breakdown
pDeafND <- factorProps('deaf',servRelated)
pVetND <- factorProps('vet',servRelated)
pRecentVetND <- estExpr(recentVet,sdat=servRelated)
pVetByDeafND <- FIX(servRelated%>%group_by(deaf)%>%do(x=factorProps('vet',sdat=.)))
pRecentVetByDeafND <- FIX(servRelated%>%group_by(deaf)%>%do(x=estExpr(recentVet,sdat=.)))
pDeafByVetND <- FIX(servRelated%>%group_by(vet)%>%do(x=factorProps('deaf',sdat=.)))
pDeafByRecentVetND <- factorProps('deaf',filter(servRelated,recentVet))

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
    popBreakdownND[[subst]] <- overall(subst,factorProps,servRelated)


folder <- paste0('ages',gsub(' ','',agerange),'/servRelated/')


openxlsx::write.xlsx(reformatAll(attainmentND[1:2],enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'EducatonalAttainmentNotDisabled2012-16VETS.xlsx'),colWidths='auto')
openxlsx::write.xlsx(reformatAll(attainment[1:2],enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'EducatonalAttainment2012-16VETS.xlsx'),colWidths='auto')



openxlsx::write.xlsx(reformatAllEmp(employmentND,enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'employment2012-16VETS.xlsx'),colWidths='auto')
openxlsx::write.xlsx(reformatAll(medianEarningsND,enr=TRUE,diss=FALSE,n=totaln),paste0(folder,'medianEarnings2012-16VETS.xlsx'),colWidths='auto')
openxlsx::write.xlsx(popBreakdownND,paste0(folder,'populationBreakdown2012-16VETS.xlsx'),rowNames=TRUE,colWidths='auto')


