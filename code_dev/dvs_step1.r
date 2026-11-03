###################################################
## Calculate monitor/site level ozone design values
## Reference: Appendix H, I, P, U to 40 CFR Part 50
## Author: Ben Wells, Statistician, US EPA/OAQPS
## Updated: October 20, 2026
## years - vector of years (min=3) to calculate DVs
## method - which data handling appendix to use?
##   'H' = 1979 1-hour ozone NAAQS
##   'I' = 1997 8-hour ozone NAAQS
##   'P' = 2008 8-hour ozone NAAQS
##   'U' = 2015 8-hour ozone NAAQS
## level - NAAQS level in ppb
##   124 = 1979 1-hour ozone NAAQS
##    84 = 1997 8-hour ozone NAAQS
##    75 = 2008 8-hour ozone NAAQS
##    70 = 2015 8-hour ozone NAAQS
## nonreg - use non-regulatory data? (TRUE/FALSE)
## ee.type - data handling for exceptional events
##   'C' = remove concurred exceptional event data
##   'I' = include all exceptional event data
##   'R' = remove all exceptional event data
###################################################
design.values <- function(years,method,level,nonreg,ee.type) {
  
  ## Custom functions called within the main function
  count <- function(x) { return(sum(!is.na(x))) }
  max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
  mean.na <- function(x) { return(ifelse(all(is.na(x)),NA,mean(x,na.rm=TRUE))) }
  merge.all <- function(x,y) { merge(x,y,all=TRUE) }
  
  ## Function to calculate moving 8-hour averages
  avg8 <- function(x,sub) {
    n <- count(x)
    if (n == 0) { return(NA) }
    if (n >= 6) { return(mean(x,na.rm=TRUE)) }
    if (n > 0 & n < 6) {
      x.sub <- mean(replace(x,which(is.na(x)),sub))
      return(ifelse(x.sub > level,mean(x,na.rm=TRUE),NA))
    }
  }
  
  ## Set up working environment
  source("code_dev/get_monitors.r") }
  options(stringsAsFactors=FALSE); require(data.table);
  ny <- length(years); yr1 <- years[1]; dvyr <- years[ny];
  req.obs <- ifelse(method == "H",9,ifelse(method == "U",13,18))
  file.ext <- paste(method,ifelse(nonreg,"N","R"),ee.type,level,sep="")
  
  ## Retrieve monitor metadata from AQS, if needed
  seasons <- get.seasons(yr1=yr1,yr2=dvyr)
  methods <- get.methods(par=44201)
  monitor.file <- paste("data_dev/monitors_",yr1,"_",dvyr,".Rdata",sep="")
  if (file.exists(monitor.file)) { load(monitor.file) }
  if (!file.exists(monitor.file)) {
    monitors <- get.monitors(par=44201,yr1=yr1,yr2=dvyr,all=TRUE)
    save(monitors,file=monitor.file)
  }
  
  ## Calculate daily and annual statistics and design values for each year
  for (y in years) {
    
    ## Set up temporary files for current year
    daily.file <- paste("data_dev/daily",y,file.ext,".Rdata",sep="")
    annual.file <- paste("data_dev/annual",y,file.ext,".Rdata",sep="")
    dv.file <- paste("data_dev/dv",(y-2),y,file.ext,".Rdata",sep="")
    if (file.exists(daily.file)) { load(daily.file) }
    if (!file.exists(daily.file)) {
      
      ## Load hourly data for current year, format datetime values
      load(paste("data_dev/o3_",y,".Rdata",sep=""))
      o3$date <- as.Date(substr(o3$dt,1,10))
      o3$hour <- as.integer(substr(o3$dt,12,13))
      
      ## Remove missing days assumed less than the standard
      bg <- which(o3$flag == "BG" & o3$concur == "Y")
      if (length(bg) > 0) { o3$o3[bg] <- -99 }
      ## Remove concurred exceptional events (ee.type = "C")
      if (ee.type == "C") {
        ee <- which(substr(o3$flag,1,1) %in% c("E","R") & o3$concur == "Y")
        if (length(ee) > 0) { o3$o3[ee] <- -99 }
      }
      ## Remove all exceptional events (ee.type = "R")
      if (ee.type == "R") {
        ee <- which(substr(o3$flag,1,1) %in% c("E","R"))
        if (length(ee) > 0) { o3$o3[ee] <- -99 }
      }
      ## Remove non-regulatory data (if nonreg == FALSE)
      if (nonreg == FALSE) {
        ## Remove data with non-FEM/FRM method codes
        non.ref <- methods$method_code[which(methods$frm_fem_id == " ")]
        if (length(non.ref) > 0) {
          drop <- which(o3$method %in% non.ref)
          if (length(drop) > 0) { o3$o3[drop] <- NA }
        }
        ## Remove data with concurred NAAQS exclusions
        nr <- subset(monitors,nonreg_begin_date != " " & nonreg_concur == "Y" &
          as.numeric(substr(nonreg_begin_date,1,4)) <= y &
          (as.numeric(substr(nonreg_end_date,1,4)) >= y | nonreg_end_date == " "),
          select=c("id","nonreg_begin_date","nonreg_end_date"))
        nr <- nr[which(!duplicated(nr)),]
        for (i in 1:nrow(nr)) {
          bd <- as.Date(nr$nonreg_begin_date[i])
          ed <- ifelse(nr$nonreg_end_date[i] == " ",Sys.Date(),as.Date(nr$nonreg_end_date[i]))
          drop <- which(o3$id == nr$id[i] & o3$date >= bd & o3$date <= ed)
          if (length(drop) > 0) { o3$o3[drop] <- NA }
        }
      }
      ## Create temporary copies of original datasets
      td <- o3[,c("id","date","hour","o3")]
      tm <- monitors; rm(o3); gc();
      
      ## Fix issue with multiple site moves in Loudon County, TN
      if (any(td$id == "4710501081")) {
        td$id[which(td$id == "4710501081")] <- "4710501091"
        rows <- which(td$id == "4710501091")
        dups <- rows[which(duplicated(td[rows,c("id","date","hour")]))]
        if (length(dups) > 0) { td <- td[-c(dups),] }
      }
      
      ## Combine POCs at sites with multiple monitors
      sites <- substr(unique(td$id),1,9)
      combine.sites <- unique(sites[which(duplicated(sites))])
      td$site <- substr(td$id,1,9); tm$site <- substr(tm$id,1,9);
      if (length(combine.sites) > 0) {
        for (i in 1:length(combine.sites)) {
          if (!combine.sites[i] %in% tm$site) { next }
          rows <- which(td$site == combine.sites[i]); d <- td[rows,];
          m <- subset(tm,site == combine.sites[i] & primary_begin_date != " ",
            select=c("id","primary_begin_date","primary_end_date"))
          m <- m[which(!duplicated(m)),]; m <- m[order(m$primary_begin_date),];
          m$bd <- as.Date(m$primary_begin_date)
          m$ed <- sapply(m$primary_end_date,function(x) ifelse(x == " ",Sys.Date(),as.Date(x)))
          m <- subset(m,bd < as.Date(paste(y,"12-31",sep="-")) & ed >= as.Date(paste(y,"01-01",sep="-")))
          n.dts <- length(unique(paste(d$date,d$hour)))
          site.data <- data.frame(id="",date=d$date[1:n.dts],hour=d$hour[1:n.dts],o3=NA,site=combine.sites[i])
          for (j in 1:nrow(m)) {
            site.ind <- which(site.data$date >= m$bd[j] & site.data$date <= m$ed[j])
            pri.ind <- which(d$id == m$id[j] & d$date >= m$bd[j] & d$date <= m$ed[j])
            if (length(pri.ind) == 0) { next }
            site.data$o3[site.ind] <- d$o3[pri.ind]
            site.sub <- site.ind[which(is.na(site.data$o3[site.ind]))]
            if (length(site.sub) == 0) { next }
            t <- subset(d,id != m$id[j] & date >= m$bd[j] & date <= m$ed[j])
            rep.vals <- floor(apply(matrix(t$o3,ncol=length(unique(t$id))),1,mean.na))
            site.data$o3[site.sub] <- rep.vals[(site.sub-site.ind[1]+1)]
          }
          td[(nrow(td)+1):(nrow(td)+n.dts),] <- site.data
          td <- td[-c(rows),]
        }
      }
      
      ## Assign ozone seasons for each site
      ss <- subset(seasons,begin_year <= y & end_year >= y)
      ss$begin_date <- as.Date(paste(y,ss$begin_month,ss$begin_day,sep="-"))
      ss$end_date <- as.Date(paste(y,ss$end_month,ss$end_day,sep="-"))
      s1 <- subset(ss,county == " ") ## Case where only state is populated - use state default season
      tm$season_begin_date <- as.Date(sapply(tm$site,function(x) s1$begin_date[match(substr(x,1,2),s1$state)]))
      tm$season_end_date <- as.Date(sapply(tm$site,function(x) s1$end_date[match(substr(x,1,2),s1$state)]))
      s2 <- subset(ss,county != " " & site_id == " ") ## Case where state/county are populated but not site_id
      s2.ind <- which(substr(tm$site,1,5) %in% paste(s2$state,s2$county,sep=""))
      tm$season_begin_date[s2.ind] <- as.Date(sapply(tm$site[s2.ind],function(x) 
        s2$begin_date[match(substr(x,1,5),paste(s2$state,s2$county,sep=""))]))
      tm$season_end_date[s2.ind] <- as.Date(sapply(tm$site[s2.ind],function(x) 
        s2$end_date[match(substr(x,1,5),paste(s2$state,s2$county,sep=""))]))
      s3 <- subset(ss,site_id != " ") ## Case where site-specific ozone season waivers are applied
      s3.ind <- which(tm$site %in% paste(s3$state,s3$county,s3$site_id,sep=""))
      tm$season_begin_date[s3.ind] <- as.Date(sapply(tm$site[s3.ind],function(x)
        s3$begin_date[match(x,paste(s3$state,s3$county,s3$site_id,sep=""))]))
      tm$season_end_date[s3.ind] <- as.Date(sapply(tm$site[s3.ind],function(x)
        s3$end_date[match(x,paste(s3$state,s3$county,s3$site_id,sep=""))]))
      
      ## Perform site combinations - now with ozone season logic
      combos <- subset(tm,combo_site != " ",select=c("site","combo_site","combo_date"))
      combos <- combos[which(!duplicated(combos)),]
      if (nrow(combos) > 0) {
        combos <- combos[order(combos$combo_date),]
        for (i in 1:nrow(combos)) {
          cd <- as.Date(combos$combo_date[i])
          if (cd < as.Date(paste(y,"01-01",sep="-"))) { next }
          old.site <- combos$site[i]
          new.site <- tm$site[match(combos$combo_site[i],substr(tm$site,1,9))]
          old.ind <- which(tm$site == old.site)
          new.ind <- which(tm$site == new.site)
          old.rows <- which(td$site == old.site)
          new.rows <- which(td$site == new.site)
          ## Case where old site has no data - no action needed
          if (length(old.rows) == 0) { next }
          ## Case where new site has no data - change to new site ID, use ozone season from old site
          if (length(new.rows) == 0) { 
            td$site[old.rows] <- new.site
            tm$season_begin_date[new.ind] <- tm$season_begin_date[old.ind]
            tm$season_end_date[new.ind] <- tm$season_end_date[old.ind]
            next 
          }
          ## Case where both sites have data - copy data from old site over to new site
          row.copy <- old.rows[which(td$date[old.rows] < cd)]
          row.paste <- new.rows[which(td$date[new.rows] < cd)]
          if (length(row.copy) > 0) {
            td[row.paste,] <- td[row.copy,]
            td$site[row.paste] <- new.site
          }
          ## Delete data from old site
          td <- td[-c(old.rows),]
          ## Case where combo date is before the ozone season - no action needed
          if (cd < tm$season_begin_date[new.ind][1]) { next }
          ## Case where combo date is during the ozone season - copy old site begin_date
          if (cd >= tm$season_begin_date[new.ind][1]) { 
            tm$season_begin_date[new.ind] <- tm$season_begin_date[old.ind][1]
          }
          ## Case where combo date is after the ozone season - also copy old site end_date
          if (cd > tm$season_end_date[new.ind][1]) {
            tm$season_end_date[new.ind] <- tm$season_end_date[old.ind][1]
          }
        }
      }
      
      ## Subset, sort, and format hourly ozone data, monitor metadata
      obs <- tapply(td$o3,list(td$site),count)
      td <- subset(td,site %in% names(obs[obs > 0]) & site %in% unique(tm$site))
      td <- td[order(td$site,td$date,td$hour),c("site","date","hour","o3")]
      tm <- subset(tm,!duplicated(site) & site %in% unique(td$site))
      tm <- tm[order(tm$site),c(ncol(tm),2:(ncol(tm)-1))]
      
      ## Calculate daily summary statistics
      temp <- vector("list",nrow(tm))
      dates <- seq(from=as.Date(paste(y,"01-01",sep="-")),to=as.Date(paste(y,"12-31",sep="-")),by=1)
      n <- 24*length(dates)+7; s <- 1;      
      for (m in 1:nrow(tm)) {
        ## Set up matrix for calculating moving 8-hour averages
        t <- td[c(s:(s+n-1)),]
        if (method %in% c("I","P","U")) {
          t8 <- matrix(c(t$o3[1:(n-7)],t$o3[2:(n-6)],t$o3[3:(n-5)],t$o3[4:(n-4)],
            t$o3[5:(n-3)],t$o3[6:(n-2)],t$o3[7:(n-1)],t$o3[8:n]),nrow=n-7,ncol=8)
        }
        ## Calculate daily obs BEFORE removing exceptional events
        if (method == "H") {
          t <- t[c(1:(nrow(t)-7)),]
          obs <- apply(matrix(t$o3,nrow=24)[10:21,],2,count)
        }
        ## 1997 and 2008 NAAQS: use all 24 8-hour periods
        if (method %in% c("I","P")) {
          ma8 <- matrix(apply(t8,1,avg8,sub=2),nrow=24)
          obs <- apply(ma8,2,count)
        }
        ## 2015 NAAQS: use 8-hr periods starting 7AM - 11PM
        if (method == "U") {
          ma8 <- matrix(apply(t8,1,avg8,sub=0),nrow=24)
          obs <- apply(ma8[8:24,],2,count)
        }
        ## Set concurred EE and BG values to missing, re-calculate 8-hr averages
        if (any(t$o3 < 0,na.rm=TRUE)) {
          t$o3[which(t$o3 < 0)] <- NA
          if (method %in% c("I","P","U")) {
            t8 <- matrix(c(t$o3[1:(n-7)],t$o3[2:(n-6)],t$o3[3:(n-5)],t$o3[4:(n-4)],
              t$o3[5:(n-3)],t$o3[6:(n-2)],t$o3[7:(n-1)],t$o3[8:n]),nrow=n-7,ncol=8)
            ma8 <- matrix(apply(t8,1,avg8,sub=ifelse(method == "U",0,2)),nrow=24)
          }
        }
        ## Calculate daily max values
        if (method == "H") { dmax <- apply(matrix(t$o3,nrow=24),2,max.na) }
        if (method %in% c("I","P")) { dmax <- apply(matrix(ma8,nrow=24),2,max.na) }
        if (method == "U") { dmax <- apply(matrix(ma8,nrow=24)[8:24,],2,max.na) }
        ## Create list element for current monitor or site
        temp[[m]] <- data.frame(site=rep(tm$site[m],length(dates)),date=dates,obs,dmax)
        s <- s + n
      }
      ## Save temporary daily data file for the current year
      assign(paste("daily",y,sep="."),data.frame(rbindlist(temp)))
      save(list=paste("daily",y,sep="."),file=daily.file)
    }
    
    ## Calculate annual summary statistics
    if (file.exists(annual.file)) { load(annual.file) }
    if (!file.exists(annual.file)) {
      ## Create annual data.frame for the 1-hour ozone NAAQS
      if (method == "H") {
        t <- matrix(NA,nrow=nrow(tm),ncol=16,dimnames=list(tm$site,c("valid","amiss","req",
          "pct","exc","ee",paste(rep(c("max","date"),each=5),rep(c(1:5),times=2),sep=""))))
        ann <- data.frame(site=tm$site,t)
      }
      ## Create annual data.frame for the 8-hour ozone NAAQS
      if (method %in% c("I","P","U")) {
        t <- matrix(NA,nrow=nrow(tm),ncol=14,dimnames=list(tm$site,c("valid","req",
          "pct","exc",paste(rep(c("max","date"),each=5),rep(c(1:5),times=2),sep=""))))
        ann <- data.frame(site=tm$site,t)
      }
      for (m in 1:nrow(tm)) {
        ## Get daily data and ozone monitoring season for current site
        t <- temp[[m]]
        season <- which(t$date >= tm$season_begin_date[m] & t$date <= tm$season_end_date[m])
        ## Calculate required monitoring days and "valid days"
        valid <- t$obs >= req.obs | floor(t$dmax) > level
        t$dmax[!valid] <- NA
        max.ind <- order(t$dmax,decreasing=TRUE)
        ann$req[m] <- (tm$season_end_date[m] - tm$season_begin_date[m] + 1)
        ann$valid[m] <- length(intersect(season,which(valid)))
        ## Get 5 highest annual daily max values and their respective dates
        for (i in 1:5) {
          ann[m,paste("max",i,sep="")] <- floor(t$dmax[max.ind][i])
          ann[m,paste("date",i,sep="")] <- as.character(t$date[max.ind][i])
        }
        ## Annual statistics for the 1-hour ozone NAAQS
        if (method == "H") {
          day0 <- c(floor(t$dmax[2:nrow(t)]),NA)[season]
          day1 <- t$dmax[season]
          day2 <- c(NA,floor(t$dmax[1:(nrow(t)-1)]))[season]
          ann$amiss[m] <- sum(day0 <= 0.75*level & is.na(day1) &
            day2 <= 0.75*level,na.rm=TRUE)
          ann$pct[m] <- floor(100*(ann$valid[m]+ann$amiss[m])/ann$req[m]+0.5)
          ann$exc[m] <- sum(floor(t$dmax) > level,na.rm=TRUE)
          ann$ee[m] <- floor(10*ann$exc[m]/(ann$pct[m]/100)+0.5)/10
          if (ann$pct[m] == 0) { ann$ee[m] <- 0 }
        }
        ## Annual statistics for the 8-hour ozone NAAQS
        if (method %in% c("I","P","U")) {
          ann$pct[m] <- floor(100*ann$valid[m]/ann$req[m]+0.5)
          ann$exc[m] <- sum(floor(t$dmax) > level,na.rm=TRUE)
        }
      }
      ## Save temporary annual data file for the current year
      colnames(ann)[2:ncol(ann)] <- paste(colnames(ann)[2:ncol(ann)],y,sep=".")
      assign(paste("annual",y,sep="."),ann)
      save(list=paste("annual",y,sep="."),file=annual.file)
      if (y < years[3]) { cat(y,as.character(round(Sys.time())),"\n"); gc(); }
    }
    
    ## Calculate 3-year design values (skip this step for first 2 years)
    if (file.exists(dv.file)) { load(dv.file) }
    if (!file.exists(dv.file)) {
      if (y < years[3]) { next }
      ## Merge annual data sets
      temp <- merge.all(eval(parse(text=paste("annual",(y-2),sep="."))),
              merge.all(eval(parse(text=paste("annual",(y-1),sep="."))),
                        eval(parse(text=paste("annual",y,sep=".")))))
      ## Create design values data.frame for the 1-hour ozone NAAQS
      if (method == "H") {
        t <- matrix(NA,nrow=nrow(temp),ncol=4,
          dimnames=list(temp$site,c("dv","ee","code","pct")))
        dvs <- data.frame(site=temp$site,t)
      }
      ## Create design values data.frame for the 8-hour ozone NAAQS
      if (method %in% c("I","P","U")) {
        t <- matrix(NA,nrow=nrow(temp),ncol=3,
          dimnames=list(temp$site,c("dv","code","pct")))
        dvs <- data.frame(site=temp$site,t)
      }
      for (m in 1:nrow(temp)) {
        ## 3-year average percent completeness
        var.list <- paste("pct",(y-2):y,sep=".")
        pct <- as.numeric(temp[m,c(var.list)])
        pct <- replace(pct,which(is.na(pct)),0)
        dvs$pct[m] <- floor(mean(pct)+0.5)
        ## Design values for the 1-hour ozone NAAQS
        if (method == "H") {
          var.list <- paste("max",rep(1:4,times=3),".",rep((y-2):y,each=4),sep="")
          maxes <- as.numeric(temp[m,c(var.list)])
          dvs$dv[m] <- maxes[order(maxes,decreasing=TRUE)][4]
          if (sum(!is.na(maxes)) < 12) { dvs$dv[m] <- NA }
          var.list <- paste("ee",(y-2):y,sep=".")
          ee <- as.numeric(temp[m,c(var.list)])
          dvs$ee[m] <- floor(10*mean(ee)+0.5)/10
          dvs$code[m] <- ifelse(is.na(dvs$dv[m]),"I",ifelse(dvs$ee[m] > 1,"V",
            ifelse(min(pct) >= 75 & dvs$pct[m] >= 90,"A","I")))
        }
        ## Design values for the 8-hour ozone NAAQS
        if (method %in% c("I","P","U")) {
          var.list <- paste("max4",rep((y-2):y),sep=".")
          max4 <- as.numeric(temp[m,c(var.list)])
          dvs$dv[m] <- floor(mean(max4))
          dvs$code[m] <- ifelse(is.na(dvs$dv[m]),"I",ifelse(dvs$dv[m] > level,"V",
            ifelse(min(pct) >= 75 & dvs$pct[m] >= 90,"A","I")))
        }
      }
      ## Save temporary design value data file for the current year
      colnames(dvs)[2:ncol(dvs)] <- paste(colnames(dvs)[2:ncol(dvs)],(y-2),y,sep=".")
      assign(paste("dvs",(y-2),y,sep="."),dvs)
      save(list=paste("dvs",(y-2),y,sep="."),file=dv.file)
      cat(y,as.character(round(Sys.time())),"\n"); gc();
    }
  }
  
  ## Combine daily data objects, format monitor metadata for final output
  daily.list <- lapply(paste("daily",years,sep="."),get,envir=sys.frame(sys.nframe()))
  daily <- Reduce(rbind,daily.list)
  daily <- daily[order(daily$site,daily$date),]
  daily$date <- as.character(daily$date)
  save(daily,file=paste("data_dev/daily_",yr1,"_",dvyr,"_",file.ext,".Rdata",sep=""))
  monitors$site <- substr(monitors$id,1,9)
  tm <- subset(monitors,!duplicated(site) & site %in% unique(daily$site))
  naa.col <- paste("naa_name",switch(method,U=2015,P=2008,I=1997,H=1979),sep="_")
  m.out <- data.frame(tm[,c("site","site_name","address","latitude","longitude",
    "epa_region","state_name","county_name","cbsa_name","csa_name",naa.col)])
  
  ## Combine monitor metadata, DVs, annual summaries for final output file
  annual.list <- lapply(paste("annual",years,sep="."),
    get,envir=sys.frame(sys.nframe()))
  dv.list <- lapply(paste("dvs",years[1:(ny-2)],years[3:ny],sep="."),
    get,envir=sys.frame(sys.nframe()))
  t1 <- Reduce(merge.all,dv.list)
  t2 <- Reduce(merge.all,annual.list)
  out <- merge(m.out,merge(t1,t2,by="site",all=TRUE),by="site",all=FALSE)
  cc <- which(lapply(out,class) == "character")
  out[,cc] <- sapply(out[,cc],function(x) ifelse(is.na(x)," ",x))
  codes <- grep("code",colnames(out))
  out[,codes] <- sapply(out[,codes],function(x) ifelse(x == " ","I",x))
  save(out,file=paste("data_dev/dv_",yr1,"_",dvyr,"_",file.ext,".Rdata",sep=""))
  
  ## Delete temporary files
  for (y in years) {
    unlink(paste("data_dev/daily",y,file.ext,".Rdata",sep=""))
    unlink(paste("data_dev/annual",y,file.ext,".Rdata",sep=""))
    if (y >= years[3]) {
      unlink(paste("data_dev/dv",(y-2),y,file.ext,".Rdata",sep=""))
    }
  }
}
