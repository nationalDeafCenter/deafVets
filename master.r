source('makeData.r')

setwd('ages18-54')
source('../estimate.r')

setwd('../ages25-54')
dat <- filter(dat,agep>24)
source('../estimate.r')
