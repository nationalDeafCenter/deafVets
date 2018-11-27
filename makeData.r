library(readr) ## read in the csvs faster
library(dplyr)
library(openxlsx)
source('generalCode/estimationFunctions.r')
states <- read.csv('../../data/states.csv')

#1) a simple breakdown of current enrollment, and completion data, across type of institution (4 year colleges, community colleges, etc) using all the 'type of institution' data we have, so that would give us some nice descriptives and allow us to make a final decision on how we want to categorize 'community colleges and 2-year institutions'




varNames <- c('ST','AGEP','DDRS','DEAR','DEYE','DOUT','DPHY','DRATX','DREM','FDEARP','ESR','SCHL','RAC1P','HISP','SEX','PERNP','PINCP','SSIP','WKHP','WKW','ADJINC','PWGTP','RELP','DRAT','MIL',
              paste0('MLP',c('A','B','CD','E','FG',LETTERS[8:11])),
              paste0('pwgtp',1:80))

firstTry <- read_csv(paste0('../../data/acs5yr2016/ss16pusa.csv'), n_max=5)
colTypes <- ifelse(tolower(names(firstTry))%in%tolower(varNames),'i','-')
missingVars <- setdiff(tolower(varNames),tolower(names(firstTry)[colTypes=='i']))
if(length(missingVars)) cat('WARNING: Missing these variables:\n',missingVars,'\n')

colTypes <- paste(colTypes,collapse='')


datA <- read_csv('../../data/acs5yr2016/ss16pusa.csv',col_types=colTypes)
datB <- read_csv('../../data/acs5yr2016/ss16pusb.csv',col_types=colTypes)
datC <- read_csv('../../data/acs5yr2016/ss16pusc.csv',col_types=colTypes)
datD <- read_csv('../../data/acs5yr2016/ss16pusd.csv',col_types=colTypes)

stopifnot(all.equal(names(datA),names(datB)))
stopifnot(all.equal(names(datA),names(datC)))
stopifnot(all.equal(names(datA),names(datD)))


dat <- rbind(datA,datB,datC,datD)

rm(datA,datB,datC,datD); gc()


names(dat) <- tolower(names(dat))

dat$state <- states$abb[match(dat$st,states$x)]
gc()
### let's figure out what the definition of "veteran" is
### https://www.census.gov/acs/www/about/why-we-ask-each-question/veterans/ says 8%
### https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk "civilian veterans" 8.0%
## factorProps('mil',dat,cum=FALSE)
##     % 1            1 SE             % 2            2 SE             % 3
## 0.403647        0.002822        7.963292        0.009436        1.291155
##     3 SE             % 4            4 SE               n
## 0.004261       90.341905        0.011969 12364760.000000
## well, it looks like mil==2 does it.
##  from data dictionary (MIL):   2    .On active duty in the past, but not now


edlevs <- c(
    '<Grade 10',
    'Grade 10',
    'Grade 11',
    '12th grade - no diploma',
    'Regular high school diploma',
    'GED or alternative credential',
    'Some college, but less than 1 year',
    '1 or more years of college credit, no degree',
    'Associates degree',
    'Bachelors degree',
    'Masters degree',
    'Professional degree beyond a bachelors degree',
    'Doctorate degree')

dat$attain <- ifelse(dat$schl<13,1,dat$schl-11)
dat$attain <- factor(edlevs[dat$attain],levels=edlevs,ordered=TRUE)

gc()

dat <- dat%>%filter(agep>24,agep<65,relp!=16)%>% ## relp==16 for institutionalized
    mutate(
        selfCare=factor(ifelse(ddrs==1,'Self-Care Difficulty','No Self-Care Difficulty')),
        indLiv=factor(ifelse(dout==1,'Independent Living Difficulty','No Independent Living Difficulty')),
        amb=factor(ifelse(dphy==1,'Ambulatory Difficulty','No Ambulatory Difficulty')),
        servDis=factor(ifelse(is.na(dratx),'Not Vet',
                       ifelse(dratx==1,'Vet. Service Connected Disability','No Service Connected Disability'))),
        cogDif=factor(ifelse(drem==1,'Cognitive Difficulty','No Cognitive Difficulty')),
        deaf=factor(ifelse(dear==1,'deaf','hearing')),
        Age=ordered(ifelse(agep<35,'25-34',
            ifelse(agep<45,'35-44',
            ifelse(agep<55,'45-54','55-64')))),
        attainCum=ordered(
            ifelse(attain<'Regular high school diploma','No HS',
            ifelse(attain<'Some college, but less than 1 year','HS Diploma',
            ifelse(attain<'Associates degree','Some College',
            ifelse(attain<'Bachelors degree','Associates',
            ifelse(attain<'Masters degree','Bachelors','Post-Graduate'))))),
           levels=c('No HS','HS Diploma','Some College','Associates','Bachelors','Post-Graduate')),
        employment=factor(ifelse(esr%in%c(1,2,4,5),'Employed',
                   ifelse(esr==3,'Unemployed','Not In Labor Force'))),

        currentMil=factor(ifelse(esr%in%4:5,'Currently serving','Not currently serving')),
        vet=factor(ifelse(mil==2,'vet','not vet')), ## not counting reserves, nat.guard. see above
        recentVet=(vet=='vet')&!is.na(mlpa)&(mlpa==1), ## since 9/11 not currently active duty (except reservs,ng?)


        fulltime=(employment=='Employed')&(wkw==1 & wkhp>=35),

        raceEth=ifelse(hisp>1,"Hispanic",
                ifelse(rac1p==2,"African American",
                ifelse(rac1p==6| rac1p==7,"Asian/PacIsl",
                ifelse(rac1p%in%c(3,4,5),'American Indian',
                ifelse(rac1p==1,"White","Other"))))),

        diss=ifelse(ddrs==1|deye==1|dout==1|dphy==1|(!is.na(dratx)&dratx==1)|drem==1,'disabled','nondisabled'),
        blind=ifelse(deye==1,'blind','seeing'),

        sex=ifelse(sex==1,'Male','Female'))
gc()


save(dat,file='vetData.RData')
gc()
