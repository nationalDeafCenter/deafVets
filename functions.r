inf <- function(enr,diss=TRUE,ages=agerange,years='2012-16',n=NULL){
  out <- cbind(c(
    'Estimated from ACS',
    paste('Years:',years),
    paste('Ages: ',ages),
    paste(ifelse(enr,'INCLUDES','EXCLUDES'),'people currently enrolled in school'),
    paste(ifelse(diss,'INCLUDES','EXCLUDES'),'people with disabilities'),
    'Vet: On active duty in the past, but not now',
    'Vet/Served Since 9/2001: Served September 2001 or later',
    'Excludes Institutionalized People',
    'Occupational Category for full-time employed people only',
    'Field of Degree for people with Bachelors degrees or higher'
  ))
  if(!is.null(n)) out <- rbind(out, paste('Total sample size:',format(n,big.mark=',')))
  #cbind(out,'')
  out
}


reformatAll <- function(lll,enr,diss=TRUE, ages=agerange,years='2012-16',n=NULL){

  lll <- sapply(lll, function(ll)
    as.data.frame(sapply(ll,function(l) if(is.factor(l)) as.character(l) else l,
      simplify=FALSE)))

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
    info <- inf(enr,diss,ages,years,n)
    res <- rbind(cbind(info,matrix('',nrow(info),ncol(res)-1)),nnn,as.matrix(res))
  }
  res
}

reformatAllEmp <- function(lll,enr,diss=TRUE,ages=agerange,years='2012-16',n=totaln){

  info <- lll$info
  lll$info <- NULL

  emp <- sapply(lll,function(x) cbind(x[,colnames(x)==''],x[,
    c("% Employed", "Employed SE", "% Not In Labor Force", "Not In Labor Force SE", "% Unemployed",
      "Unemployed SE" ,"n")]), simplify=FALSE)
  full <- sapply(lll,function(x) cbind(x[,colnames(x)==''],x[,
    c("% Employed Full-Time","Full/Part-Time SE", "Employed n" )]),simplify=FALSE)

  res <- cbind(reformatAll(emp),reformatAll(full)[,-c(1:3)])
  if(!missing(enr)){
    nnn <- names(res)
    names(res) <- NULL
    info <- inf(enr,diss,ages,years,n)
    res <- rbind(cbind(info,matrix('',nrow(info),ncol(res)-1)),nnn,as.matrix(res))
  }
  res
}

reformat1 <- function(ddd){
  deafCol <- which(sapply(ddd,function(x) any(x=='deaf')))
  vetCol <-  which(sapply(ddd,function(x) any(x=='vet')))

  est <- cbind(ddd[,!grepl('SE',names(ddd))])
  se <- cbind(ddd[,grepl('SE',names(ddd))])

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

  names(ddd)[grep('\\.[0-9]',names(ddd))] <- ''
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


standard <- function(x,FUN,dat,diss=TRUE,...){
  out <- list(
    overall=overall(x,FUN,dat),
    byAge=bysub(x,FUN,dat,Age),
    bySex=bysub(x,FUN,dat,sex),
    byRace=bysub(x,FUN,dat,raceEth),
    ...)
  if(diss)
    out$byDiss=rbind(
      bysub(x,FUN,dat,diss),
      bysub(x,FUN,dat,blind),
      bysub(x,FUN,dat,selfCare),
      bysub(x,FUN,dat,indLiv),
      bysub(x,FUN,dat,amb),
      # bysub(x,FUN,dat,servDis),
      bysub(x,FUN,dat,cogDif))
  out
}

empFun <- function(x,sdat){
    emp <- factorProps('employment',sdat)
    ft <- estExpr(fulltime,employment=='Employed',sdat=sdat)
    ft <- c(ft[1],100-ft[1],ft[2:3])
    names(ft) <- c('% Employed Full-Time','% Employed Part-Time','Full/Part-Time SE','Employed n')
    c(emp,ft)
}

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

