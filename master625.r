source('makeData.r')

dat <- filter(dat,agep>24,agep<55)
gc()
source('estimate.r')

dat <- filter(dat,diss=='nondisabled')
source('estimate.r')
