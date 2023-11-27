####################################################################
## Ozone Watch: Preliminary ozone design values for the current year
## Author: Ben Wells, Statistician, US EPA OAQPS/AQAD/AQAG
## Last Updated: May 17, 2022
####################################################################

## Set up working environment
if (dir.exists("C:/")) { source("C:/Users/bwells01/Documents/R/get_monitors.r") }
if (dir.exists("/home/bwells01/")) { source("/home/bwells01/R/get_monitors.r") }
library(data.table); library(plyr); library(xlsx);
options(stringsAsFactors=FALSE)
max.na <- function(x) { return(ifelse(any(!is.na(x)),max(x,na.rm=TRUE),NA)) }
mean.na <- function(x) { return(ifelse(all(is.na(x)),NA,mean(x,na.rm=TRUE))) }
date.stamp <- format(Sys.Date(),"%Y%m%d")
year <- as.numeric(substr(date.stamp,1,4))-(as.numeric(substr(date.stamp,5,6)) < 4)
d1 <- paste(year,"-01-01 00:00:00",sep="")
d2 <- paste(year+1,"-01-01 06:00:00",sep="")
area.wb <- loadWorkbook("OzoneWatch/docs/Ozone Watch template.xlsx")
area.sheets <- getSheets(area.wb)
site.wb <- createWorkbook(type="xlsx")

## Function to calculate moving 8-hour averages
avg8 <- function(x,lvl) {
  n <- sum(!is.na(x))
  sub <- ifelse(lvl > 70,2,0)
  if (n == 0) { return(NA) }
  if (n >= 6) { return(mean(x,na.rm=TRUE)) }
  if (n > 0 & n < 6) {
    x.sub <- mean(replace(x,which(is.na(x)),sub))
    return(ifelse(floor(x.sub) > lvl,x.sub,NA))
  }
}

## Function to create a time-series of the cumulative 4th highest value
max4.cum <- function(x) {
  n <- length(x)
  out <- rep(NA,n)
  for (i in 4:n) {
    t <- x[1:i]
    out[i] <- t[order(t,decreasing=TRUE)][4]
  }
  return(floor(out))
}

## Get list of nonattainment areas to include for 2015, 2008, 1997, and 1979 standards
dv.dir <- paste("OzoneDV/DV",year-3,"_",year-1,sep="")
all.subdir <- list.files(dv.dir)[which(substr(list.files(dv.dir),1,1) %in% c(0:9))]
dir.num <- which.max(as.numeric(paste(substr(all.subdir,6,7),sprintf("%02d",
  match(tolower(substr(all.subdir,3,5)),tolower(month.abb))),substr(all.subdir,1,2),sep="")))
dv.subdir <- paste(dv.dir,all.subdir[dir.num],sep="/")
load(paste(dv.subdir,list.files(dv.subdir)[intersect(grep("dvtables",list.files(dv.subdir)),
  grep(".Rdata",list.files(dv.subdir)))],sep="/")[1])
naa.list.2015 <- table1a$naa_name
naa.list.2008 <- subset(table1b,(status == "Nonattainment" & classification != "Marginal") | dv > 0.07)$naa_name
naa.list.1997 <- subset(table1c,dv > 0.075)$naa_name
naa.list.1979 <- c("Baltimore Area, MD","Baton Rouge Area, LA",
  "Chicago-Gary-Lake County Area, IL-IN","Houston-Galveston-Brazoria Area, TX",
  "Los Angeles-South Coast Air Basin Area, CA","Milwaukee-Racine Area, WI",
  "Morongo Band of Mission Indians, CA","New York-N. New Jersey-Long Island Area, NY-NJ-CT",
  "Philadelphia-Wilmington-Trenton Area, PA-NJ-DE-MD","Sacramento Metro Area, CA",
  "San Joaquin Valley Area, CA","Southeast Desert Modified AQMA Area, CA",
  "Ventura County Area, CA","Washington Area, DC-MD-VA")

## Retrieve ozone monitor metadata and ozone monitoring seasons from AQS
monitors <- get.monitors(par=44201,yr1=year-2,yr2=year,all=FALSE)
seasons <- get.seasons(yr1=year,yr2=year)
methods <- get.methods(par=44201)

## Retrieve current year hourly ozone data from AQS
aqs <- subset(get.aqs.data(paste("SELECT DISTINCT
        rd.state_code || rd.county_code || rd.site_id || rd.poc AS id,
        TO_CHAR(rd.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
        FLOOR(GREATEST(rd.standard_sample_value*1000,0)) AS o3,
        rd.method_code AS method,
        COALESCE(rd.event_code || rd.null_data_code,' ') AS flag,
        COALESCE(rd.event_concurence_indicator || rd.null_code_concurrence,' ') AS concur
   FROM raw_data_concurrences rd
  WHERE rd.parameter_code = '44201'
    AND rd.sampling_begin_datetime >= TO_DATE('",d1,"','YYYY-MM-DD HH24:MI:SS')
    AND rd.sampling_begin_datetime <= TO_DATE('",d2,"','YYYY-MM-DD HH24:MI:SS')
    AND rd.state_code NOT IN ('80','CC')
  ORDER BY 1,2",sep="")),substr(dt,15,19) == "00:00")

## Retrieve current year hourly ozone data from AirNow
airnow <- subset(get.aqs.data(paste("SELECT DISTINCT
        sites.state_code || sites.county_code || sites.site_id || monitors.poc AS id,
        TO_CHAR(o3.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
        FLOOR(GREATEST(o3.std_sample_value*1000,0)) AS o3,
        '999' AS method,
        ' ' AS flag,
        ' ' AS concur
  FROM  airnow.raw_data o3,
        airnow.monitor_protocols mp,
        airsraqs.monitors monitors,
        airsraqs.site_basic sites
  WHERE o3.mp_mo_mo_id = mp.mo_mo_id
    AND mp.mo_mo_id = monitors.mo_id
    AND monitors.si_si_id = sites.si_id
    AND monitors.pa_parameter_code = '44201'
    AND monitors.status_ind = 'P'
    AND o3.status_ind = 'P'
    AND sites.status_ind = 'P'
    AND sites.state_code NOT IN ('80','CC')
    AND o3.sampling_begin_datetime >= TO_DATE(",d1,",'YYYY-MM-DD-HH24:MI:SS')
    AND o3.sampling_begin_datetime <= TO_DATE(",d2,",'YYYY-MM-DD-HH24:MI:SS')
  ORDER BY 1,2",sep="'")),substr(dt,15,19) == "00:00")

## Combine current year AQS/AirNow hourly concentration data
temp <- rbind(aqs,airnow)
temp <- temp[!duplicated(temp[,c("id","dt")]),]
ids <- as.character(unique(temp$id))
dts <- as.character(seq(as.POSIXct(d1,tz="UTC"),as.POSIXct(d2,tz="UTC"),3600))
all <- data.frame(id=rep(ids,each=length(dts)),dt=rep(dts,times=length(ids)))
aqs.airnow <- subset(merge(all,temp,by=c("id","dt"),all=TRUE),id %in% monitors$id)
aqs.airnow$date <- as.Date(substr(aqs.airnow$dt,1,10))
aqs.airnow$hour <- as.integer(substr(aqs.airnow$dt,12,13))

## Remove missing days assumed less than the standard, if any
bg <- which(aqs.airnow$flag == "BG" & aqs.airnow$concur == "Y")
if (length(bg) > 0) { aqs.airnow$o3[bg] <- -99 }

## Remove concurred exceptional events, if any
ee <- which(substr(aqs.airnow$flag,1,1) %in% c("E","R") & aqs.airnow$concur == "Y")
if (length(ee) > 0) { aqs.airnow$o3[ee] <- -99 }

## Remove data collected with non-FEM/FRM method codes, if any
non.ref <- methods$method_code[which(methods$frm_fem_id == " ")]
drop <- which(aqs.airnow$method %in% non.ref)
if (length(drop) > 0) { aqs.airnow$o3[drop] <- NA }

## Create temporary copies of original datasets
td <- aqs.airnow[,c("id","date","hour","o3")]
tm <- monitors

## Remove non-regulatory data
nr <- subset(tm,nonreg_begin_date != " " & nonreg_concur == "Y",
  select=c("id","nonreg_begin_date","nonreg_end_date"))
for (i in 1:nrow(nr)) {
  bd <- as.Date(nr$nonreg_begin_date[i])
  ed <- ifelse(nr$nonreg_end_date[i] == " ",Sys.Date(),as.Date(nr$nonreg_end_date[i]))
  if (bd > as.Date(paste(year+1,"01-01",sep="-"))) { next }
  if (ed < as.Date(paste(year,"01-01",sep="-"))) { next }
  drop <- which(td$id == nr$id[i] & td$date >= bd & td$date <= ed)
  if (length(drop) > 0) { td$o3[drop] <- NA }
}

## Combine POCs at sites with multiple monitors
sites <- substr(unique(td$id),1,9)
combine.sites <- unique(sites[which(duplicated(sites))])
td$site <- substr(td$id,1,9); tm$site <- substr(tm$id,1,9);
if (length(combine.sites) > 0) {
  sd <- vector("list",length(combine.sites))
  for (i in 1:length(combine.sites)) {
    if (!combine.sites[i] %in% tm$site) { next }
    rows <- which(td$site == combine.sites[i]); d <- td[rows,];
    m <- subset(tm,site == combine.sites[i] & primary_begin_date != " ",
      select=c("id","primary_begin_date","primary_end_date"))
    m <- m[which(!duplicated(m)),]; m <- m[order(m$primary_begin_date),];
    m$bd <- as.Date(m$primary_begin_date)
    m$ed <- sapply(m$primary_end_date,function(x) ifelse(x == " ",Sys.Date(),as.Date(x)))
    m <- subset(m,bd < as.Date(paste(year,"12-31",sep="-")) &
      ed >= as.Date(paste(year,"01-01",sep="-")))
    n.dts <- length(unique(paste(d$date,d$hour)))
    sd[[i]] <- data.frame(id="",date=d$date[1:n.dts],hour=d$hour[1:n.dts],o3=NA,
      site=combine.sites[i])
    for (j in 1:nrow(m)) {
      site.ind <- which(sd[[i]]$date >= m$bd[j] & sd[[i]]$date <= m$ed[j])
      pri.ind <- which(d$id == m$id[j] & d$date >= m$bd[j] & d$date <= m$ed[j])
      sd[[i]]$o3[site.ind] <- d$o3[pri.ind]
      site.sub <- site.ind[which(is.na(sd[[i]]$o3[site.ind]))]
      if (length(site.sub) == 0) { next }
      t <- subset(d,id != m$id[j] & date >= m$bd[j] & date <= m$ed[j])
      rep.vals <- floor(apply(matrix(t$o3,ncol=length(unique(t$id))),1,mean.na))
      sd[[i]]$o3[site.sub] <- rep.vals[(site.sub-site.ind[1]+1)]
    }
    td <- td[-c(rows),]
  }
  td <- rbind(td,as.data.frame(rbindlist(sd)))
}

## Perform site combinations
combos <- subset(tm,combo_site != " ",c("site","combo_site","combo_date"))
combos <- combos[which(!duplicated(combos)),]
if (nrow(combos) > 0) {
  combos <- combos[order(combos$combo_date),]
  for (i in 1:nrow(combos)) {
    cd <- as.Date(combos$combo_date[i])
    if (cd < as.Date(paste(year,"01-01",sep="-"))) { next }
    old.site <- combos$site[i]
    new.site <- tm$site[match(combos$combo_site[i],substr(tm$site,1,9))]
    old.rows <- which(td$site == old.site)
    new.rows <- which(td$site == new.site)
    if (length(old.rows) == 0) { next }
    if (length(new.rows) == 0) {
      td$site[old.rows] <- new.site
      next
    }
    row.copy <- old.rows[which(td$date[old.rows] < cd)]
    row.paste <- new.rows[which(td$date[new.rows] < cd)]
    if (length(row.copy) > 0) {
      td[row.paste,] <- td[row.copy,]
      td$site[row.paste] <- new.site
    }
    td <- td[-c(old.rows),]
  }
}

## Subset and sort hourly ozone data, monitor metadata
obs <- tapply(td$o3,list(td$site),function(x) sum(!is.na(x)))
td <- subset(td,site %in% names(obs[obs > 0]) & site %in% unique(tm$site))
td <- td[order(td$site,td$date,td$hour),c("site","date","hour","o3")]
tm <- subset(tm,!duplicated(site) & site %in% unique(td$site))
tm <- tm[order(tm$site),c(ncol(tm),2:(ncol(tm)-1))]

## Calculate site-level design values for each ozone standard
for (p in 12:9) {
  
  ## Set constants based on current standard id
  level <- ifelse(p == 9,124,ifelse(p == 10,84,ifelse(p == 11,75,70)))
  method <- ifelse(p == 9,"H",ifelse(p == 10,"I",ifelse(p == 11,"P","U")))
  naaqs <- ifelse(p == 9,"1979",ifelse(p == 10,"1997",ifelse(p == 11,"2008","2015")))
  req.obs <- ifelse(p == 9,9,ifelse(p == 12,13,18))
  
  ## Calculate daily summary statistics
  temp <- vector("list",nrow(tm))
  dates <- seq(as.Date(paste(year,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")),1)
  n <- 24*length(dates)+7; s <- 1;
  
  ## Loop over sites
  for (m in 1:nrow(tm)) {
    
    ## Set up matrix for calculating moving 8-hour averages
    t <- td[c(s:(s+n-1)),]
    if (p > 9) {
      t8 <- matrix(c(t$o3[1:(n-7)],t$o3[2:(n-6)],t$o3[3:(n-5)],t$o3[4:(n-4)],
        t$o3[5:(n-3)],t$o3[6:(n-2)],t$o3[7:(n-1)],t$o3[8:n]),nrow=n-7,ncol=8)
    }
    
    ## Calculate daily obs BEFORE removing exceptional events
    if (p == 9) {
      t <- t[c(1:(nrow(t)-7)),]
      obs <- apply(matrix(t$o3,nrow=24)[10:21,],2,function(x) sum(!is.na(x)))
    }
    
    ## 1997 and 2008 NAAQS: use all 24 8-hour periods
    if (p == 10 | p == 11) {
      ma8 <- matrix(apply(t8,1,avg8,lvl=level),nrow=24)
      obs <- apply(ma8,2,function(x) sum(!is.na(x)))
    }
    
    ## 2015 NAAQS: use 8-hr periods starting 7AM - 11PM
    if (p == 12) {
      ma8 <- matrix(apply(t8,1,avg8,lvl=level),nrow=24)
      obs <- apply(ma8[8:24,],2,function(x) sum(!is.na(x)))
    }
    
    ## Set concurred EE and BG values to missing, re-calculate 8-hr averages
    if (any(t$o3 < 0,na.rm=TRUE)) {
      t$o3[which(t$o3 < 0)] <- NA
      if (p > 9) {
        t8 <- matrix(c(t$o3[1:(n-7)],t$o3[2:(n-6)],t$o3[3:(n-5)],t$o3[4:(n-4)],
          t$o3[5:(n-3)],t$o3[6:(n-2)],t$o3[7:(n-1)],t$o3[8:n]),nrow=n-7,ncol=8)
        ma8 <- matrix(apply(t8,1,avg8,lvl=level),nrow=24)
      }
    }
    
    ## Calculate daily max values based on correct data handling
    if (p == 9) { dmax <- apply(matrix(t$o3,nrow=24),2,max.na) }
    if (p == 10 | p == 11) { dmax <- apply(matrix(ma8,nrow=24),2,max.na) }
    if (p == 12) { dmax <- apply(matrix(ma8,nrow=24)[8:24,],2,max.na) }
    
    ## Create list element for current site
    temp[[m]] <- data.frame(site=rep(tm$site[m],length(dates)),date=dates,obs,dmax)
    s <- s + n
  }
  temp <- as.data.frame(rbindlist(temp))
  daily <- data.frame(site=temp$site,date=temp$date,o3=mapply(function(obs,dmax)
    ifelse(obs >= req.obs | dmax >= (level + 1),floor(dmax),NA),temp$obs,temp$dmax))
  
  ## Bring in design value statistics for previous 2 years
  load(paste(dv.subdir,"/dv_",year-12,"_",year-1,"_",method,"RC",level,".Rdata",sep=""))
  vals <- subset(out,!is.na(eval(parse(text=paste("max1",(year-2),sep=".")))) |
    !is.na(eval(parse(text=paste("max1",(year-1),sep=".")))),c(1:11,
    grep((year-2),colnames(out)),grep((year-1),colnames(out))))
  colnames(vals) <- gsub(".","_",colnames(vals),fixed=TRUE)
  
  ## Create objects used within site-level DV calculation loop
  start <- 1; types <- unlist(lapply(vals,"class"));
  all.sites <- unique(c(tm$site,vals$site))
  n.days <- ifelse(year %% 4 == 0,366,365)
  dv.list <- vector("list",length(all.sites))
  
  ## Loop over sites
  for (m in 1:length(all.sites)) {
    
    ## Get annual summary statistics for previous 2 years
    dv <- subset(vals,site == all.sites[m])
    if (nrow(dv) == 0) {
      dv <- tm[m,c(colnames(vals)[1:10],paste("naa_name",naaqs,sep="_"))]
      colnames(dv)[11] <- "naa_name"
      for (i in 12:ncol(vals)) {
        dv[,colnames(vals)[i]] <- ifelse(types[i] == "character"," ",NA)
      }
    }
    
    ## Calculate current year summary statistics
    if (m <= nrow(tm)) {
      t <- daily[start:(start + n.days - 1),]
      max.ind <- order(t$o3,decreasing=TRUE)[1:5]
      sr <- tail(intersect(which(seasons$state == substr(all.sites[m],1,2)),
        intersect(which(seasons$county %in% c(" ",substr(all.sites[m],3,5))),
        which(seasons$site_id %in% c(" ",substr(all.sites[m],6,9))))),n=1)
      ts <- seasons[sr,]
      bd <- as.Date(paste(year,ts$begin_month,ts$begin_day,sep="-"))
      ed <- as.Date(paste(year,ts$end_month,ts$end_day,sep="-"))
      req.days <- as.numeric(ed - bd + 1)
      val.days <- (t$date >= bd) & (t$date <= ed) & (!is.na(t$o3))
      pct <- sum(val.days)/req.days
      exc <- sum(t$o3 > level,na.rm=TRUE)
      start <- start + n.days
    }
    
    ## Get 5 highest daily max 8-hour values and dates
    for (i in 1:5) {
      dv[,paste("max",i,"_",year,sep="")] <- ifelse(m <= nrow(tm),t$o3[max.ind][i],NA)
      dv[,paste("date",i,"_",year,sep="")] <- ifelse(m <= nrow(tm),
        as.character(t$date[max.ind][i])," ")
    }
    
    ## Get annual percent data completeness and exceedance counts
    dv[,paste("pct",year,sep="_")] <- ifelse(m <= nrow(tm),round(100*pct),NA)
    dv[,paste("exc",year,sep="_")] <- ifelse(m <= nrow(tm),exc,NA)
    
    ## Design values for the 1979 1-hour NAAQS
    if (p == 9) {
      maxes <- dv[1,paste("max",rep(c(1:4),3),"_",rep(c((year-2):year),each=4),sep="")]
      dv[,paste("dv",(year-2),year,sep="_")] <- maxes[order(maxes,decreasing=TRUE)][4]
      dv[,paste("ee",(year-2),year,sep="_")] <- round((
        round(dv[,paste("ee",(year-2),sep="_")],1) +
        round(dv[,paste("ee",(year-1),sep="_")],1) +
        round(dv[,paste("exc",year,sep="_")],1))/3,1)
      dv[,paste("pct",(year-2),year,sep="_")] <- round((
        pmax(dv[,paste("pct",(year-2),sep="_")],0,na.rm=TRUE) +
        pmax(dv[,paste("pct",(year-1),sep="_")],0,na.rm=TRUE) +
        pmax(dv[,paste("pct",year,sep="_")],0,na.rm=TRUE))/3)
      dv$status <- ifelse(dv[,paste("ee",(year-2),year,sep="_")] > 1,"V",
                   ifelse(dv[,paste("pct",(year-2),sep="_")] >= 75 &
                          dv[,paste("pct",(year-1),sep="_")] >= 75,"A","I"))
      if (is.na(dv$status)) { dv$status <- "I" }
      var.list <- c("site","site_name","address","latitude","longitude","epa_region",
        "state_name","county_name","cbsa_name","csa_name","naa_name",
        paste(c("dv","ee"),(year-2),year,sep="_"),"status",
        paste("pct",(year-2),year,sep="_"),paste("pct",c((year-2):year),sep="_"),
        paste("ee",c((year-2):(year-1)),sep="_"),paste("exc",year,sep="_"),
        paste(paste(rep(c("max","date"),each=15),rep(1:5,times=6),sep=""),
          rep(c((year-2):year),each=5,times=2),sep="_"))
    }
    
    ## Design values for the 8-hour NAAQS (1997, 2008, and 2015)
    if (p > 9) {
      dv[,paste("dv",(year-2),year,sep="_")] <- floor((
        floor(dv[,paste("max4",(year-2),sep="_")]) +
        floor(dv[,paste("max4",(year-1),sep="_")]) +
        floor(dv[,paste("max4",year,sep="_")]))/3)
      dv[,paste("pct",(year-2),year,sep="_")] <- round((
        pmax(dv[,paste("pct",(year-2),sep="_")],0,na.rm=TRUE) +
        pmax(dv[,paste("pct",(year-1),sep="_")],0,na.rm=TRUE) +
        pmax(dv[,paste("pct",year,sep="_")],0,na.rm=TRUE))/3)
      dv$status <- ifelse(dv[,paste("dv",(year-2),year,sep="_")] > level,"V",
                   ifelse(dv[,paste("pct",(year-2),sep="_")] >= 75 &
                          dv[,paste("pct",(year-1),sep="_")] >= 75,"A","I"))
      if (is.na(dv$status)) { dv$status <- "I" }
      dv[,paste("critical",year,sep="_")] <- pmax(3*(level+1) - 
        floor(dv[,paste("max4",(year-2),sep="_")]) -
        floor(dv[,paste("max4",(year-1),sep="_")]),0)
      dv[,paste("critical",(year+1),sep="_")] <- pmax(3*(level+1) - 
        floor(dv[,paste("max4",(year-1),sep="_")]) -
        floor(dv[,paste("max4",year,sep="_")]),0)     
      var.list <- c("site","site_name","address","latitude","longitude","epa_region",
        "state_name","county_name","cbsa_name","csa_name","naa_name",
        paste("dv",(year-2),year,sep="_"),"status",
        paste("pct",(year-2),year,sep="_"),
        paste("pct",c((year-2):year),sep="_"),
        paste("exc",c((year-2):year),sep="_"),
        paste("critical",c(year,(year+1)),sep="_"),
        paste(paste(rep(c("max","date"),each=15),rep(1:5,times=6),sep=""),
          rep(c((year-2):year),each=5,times=2),sep="_"))
    }
    dv.list[[m]] <- dv[,var.list]
  }
  
  ## Combine site-level DVs for current standard into a data.frame
  sitedv <- data.frame(rbindlist(dv.list))
  new.combos <- subset(combos,as.numeric(substr(combo_date,1,4)) == year)
  if (nrow(new.combos) > 0) { ## Add current year site combinations, if any
    for (i in 1:nrow(new.combos)) {
      old.site <- which(sitedv$site == new.combos$site[i])
      new.site <- which(sitedv$site == new.combos$combo_site[i])
      if (length(old.site) == 0 | length(new.site) == 0) { next }
      cols <- c(paste(rep(c("pct","exc"),each=2),rep(year-2:1,times=2),sep="_"),
        paste("critical",year,sep="_"),paste(rep(c("max","date"),each=10),
        rep(1:5,times=2),"_",rep(year-2:1,each=5),sep=""))
      if (p == 9) { cols <- gsub("exc","ee",cols)[-5] }
      sitedv[new.site,cols] <- sitedv[old.site,cols]
      sitedv[new.site,paste("pct",year-2,year,sep="_")] <- round((
        pmax(sitedv[new.site,paste("pct",year-2,sep="_")],0,na.rm=TRUE) +
        pmax(sitedv[new.site,paste("pct",year-1,sep="_")],0,na.rm=TRUE) +
        pmax(sitedv[new.site,paste("pct",year,sep="_")],0,na.rm=TRUE))/3)
      if (p > 9) {
        sitedv[new.site,paste("dv",year-2,year,sep="_")] <- floor((
          floor(sitedv[new.site,paste("max4",year-2,sep="_")]) +
          floor(sitedv[new.site,paste("max4",year-1,sep="_")]) +
          floor(sitedv[new.site,paste("max4",year,sep="_")]))/3)
        sitedv[new.site,"status"] <- 
          ifelse(sitedv[new.site,paste("dv",year-2,year,sep="_")] > level,"V",
          ifelse(sitedv[new.site,paste("pct",year-2,sep="_")] >= 75 &
                 sitedv[new.site,paste("pct",year-1,sep="_")] >= 75,"A","I"))
        sitedv[new.site,paste("critical",year+1,sep="_")] <- pmax(3*(level+1) -
          floor(sitedv[new.site,paste("max4",year-1,sep="_")]) -
          floor(sitedv[new.site,paste("max4",year,sep="_")]),0)
      }
      if (p == 9) {
        maxes <- sitedv[new.site,paste("max",rep(c(1:4),3),"_",rep(c((year-2):year),each=4),sep="")]
        sitedv[new.site,paste("dv",(year-2),year,sep="_")] <- maxes[order(maxes,decreasing=TRUE)][4]
        sitedv[new.site,paste("ee",(year-2),year,sep="_")] <- round((
          round(sitedv[new.site,paste("ee",(year-2),sep="_")],1) +
          round(sitedv[new.site,paste("ee",(year-1),sep="_")],1) +
          round(sitedv[new.site,paste("exc",year,sep="_")],1))/3,1)
        sitedv[new.site,"status"] <- 
          ifelse(sitedv[new.site,paste("ee",year-2,year,sep="_")] > 1,"V",
          ifelse(sitedv[new.site,paste("pct",(year-2),sep="_")] >= 75 &
                 sitedv[new.site,paste("pct",(year-1),sep="_")] >= 75,"A","I"))
      }
      sitedv <- sitedv[-c(old.site),]
    }
  }
  sitedv <- sitedv[order(sitedv$site),]
  if (p == 12) { sitedv$naa_name <- gsub(" - Revised","",sitedv$naa_name,fixed=TRUE) }
  rownames(sitedv) <- c(1:nrow(sitedv))
  assign(paste("sitedv",naaqs,sep="."),sitedv)
  
  ## Generate 8-hour NAAQS outputs
  if (p > 9) {
    ## Create current year site-level data objects for shiny application
    curr.vals <- ddply(daily,"site",summarize,year=substr(date,1,4),
      day=substr(date,6,10),dmax=o3,max4=max4.cum(o3))
    site.vals <- split(curr.vals,curr.vals$site)
    dvs <- subset(eval(parse(text=paste("sitedv",naaqs,sep="."))),
      site %in% unique(curr.vals$site) & !duplicated(site))
    dvdf <- data.frame(site=dvs$site,year=year,dv=dvs[,paste("dv",(year-2),year,sep="_")],
      status=dvs[,"status"],pct3yr=dvs[,paste("pct",(year-2),year,sep="_")],
      pct=dvs[,paste("pct",year,sep="_")],exc=dvs[,paste("exc",year,sep="_")],
      crit1=dvs[,paste("critical",year,sep="_")],crit2=dvs[,paste("critical",(year+1),sep="_")],
      max1=dvs[,paste("max1",year,sep="_")],max2=dvs[,paste("max2",year,sep="_")],
      max3=dvs[,paste("max3",year,sep="_")],max4=dvs[,paste("max4",year,sep="_")],      
      max5=dvs[,paste("max5",year,sep="_")],date1=dvs[,paste("date1",year,sep="_")],
      date2=dvs[,paste("date2",year,sep="_")],date3=dvs[,paste("date3",year,sep="_")],
      date4=dvs[,paste("date4",year,sep="_")],date5=dvs[,paste("date5",year,sep="_")])
    curr <- vector("list",length(site.vals))
    for (i in 1:length(curr)) {
      curr[[i]]$site <- dvs$site[i]
      curr[[i]]$site_name <- dvs$site_name[i]
      curr[[i]]$address <- dvs$address[i]
      curr[[i]]$latitude <- dvs$latitude[i]
      curr[[i]]$longitude <- dvs$longitude[i]
      curr[[i]]$epa_region <- dvs$epa_region[i]
      curr[[i]]$state_name <- dvs$state_name[i]
      curr[[i]]$county_name <- dvs$county_name[i]
      curr[[i]]$cbsa_name <- dvs$cbsa_name[i]
      curr[[i]]$csa_name <- dvs$csa_name[i]
      curr[[i]]$naa_name <- dvs$naa_name[i]
      curr[[i]]$dv_data <- dvdf[i,]
      curr[[i]]$daily_data <- site.vals[[i]]
    }
    assign(paste("curr",naaqs,sep="."),curr)
    
    ## Create Excel spreadsheet object for site-level DVs
    sheet <- createSheet(site.wb,sheetName=paste(naaqs,"NAAQS",sep="_"))
    sitedv <- eval(parse(text=paste("sitedv",naaqs,sep=".")))
    nr <- nrow(sitedv); nc <- ncol(sitedv);
    colnames(sitedv) <- c("AQS Site ID","Site Name","Address","Latitude","Longitude","EPA Region",
      "State Name","County Name","CBSA Name","CSA Name","Nonattainment Area Name",
      paste("Preliminary ",year-2,"-",year,"Design Value (ppb)"),
      "Status (A=Attaining, I=Incomplete, V=Violating)",
      paste(year-2,"-",year,"Average Percent Complete"),
      paste(c((year-2):year),"Percent Complete"),
      paste(c((year-2):year),"Exceedance Day Count"),
      paste(c(year:(year+1)),"Critical Value (ppb)"),
      paste(rep(c((year-2):year),each=5),rep(c("1st","2nd","3rd","4th","5th"),3),"Highest Daily Max (ppb)"),
      paste(rep(c((year-2):year),each=5),rep(c("1st","2nd","3rd","4th","5th"),3),"Highest Date"))
    title <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_CENTER",
      vertical="VERTICAL_BOTTOM",wrapText=TRUE),font=Font(site.wb,isBold=TRUE))
    left <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_LEFT"))
    center <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_CENTER"))
    right <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_RIGHT"),
      dataFormat=DataFormat("##00.000000"))
    cs.list <- vector("list",nc); names(cs.list) <- 1:nc;
    for (i in c(2,3,7:11)) { cs.list[[i]] <- left }
    for (i in c(1,6,12:nc)) { cs.list[[i]] <- center }
    for (i in c(4:5)) { cs.list[[i]] <- right }
    addDataFrame(sitedv,sheet,row.names=FALSE,colStyle=cs.list,colnamesStyle=title); gc();
    createFreezePane(sheet,rowSplit=2,colSplit=1,startRow=2,startCol=1)
    setColumnWidth(sheet,colIndex=c(2:3,7:13),colWidth=15)
    setColumnWidth(sheet,colIndex=c(1,4:6,14:nc),colWidth=12)
  }
  
  ## Table 1: Design values in 2015 8-hour NAAQS nonattainment areas
  if (p == 12) {
    ## Get highest design value for each nonattainment area
    sitedv <- subset(sitedv.2015,naa_name %in% naa.list.2015)
    dv.var <- paste("dv",(year-2),year,sep="_")
    sitedv[which(sitedv$status == "I"),dv.var] <- NA
    temp <- NULL
    for (i in 1:length(naa.list.2015)) {
      t <- subset(sitedv,naa_name == naa.list.2015[i])
      tmax <- t[order(t[,dv.var],t[,paste("max4",year,sep="_")],decreasing=TRUE)[1],]
      temp <- rbind(temp,tmax)
    }
    ## Get current year exceedance days for each area
    t1 <- subset(daily,site %in% sitedv$site)
    t2 <- merge(sitedv[,c("site","naa_name")],t1,by="site",all.x=FALSE,all.y=TRUE)
    t3 <- tapply(t2$o3,list(t2$naa_name,t2$date),max.na)
    t4 <- apply(t3,1,function(x) sum(x > level,na.rm=TRUE))
    temp[,paste("exc",year,sep="_")] <- sapply(temp$naa_name,function(x)
      ifelse(x %in% names(t4),t4[which(names(t4) == x)],0))
    ## Format Table 1 for export
    temp$meets_naaqs <- sapply(temp$status,function(x) switch(x,A="Yes",V="No","Inc"))
    temp$status <- subset(table1a,naa_name %in% naa.list.2015)$status
    temp$classification <- subset(table1a,naa_name %in% naa.list.2015)$classification
    table1 <- temp[,c("naa_name","status","classification",dv.var,"meets_naaqs",
      paste("exc",year,sep="_"),paste("max4",c((year-2):year),sep="_"),
      paste("critical",c(year,(year+1)),sep="_"))]
    colnames(table1) <- c("2015 NAAQS Nonattainment Area","Status","Classification",
      paste("Preliminary",year-2,"-",year,"Design Value"),"Meets 2015 NAAQS?",
      paste(year,"Exceedance Days*"),paste((year-2):year,"4th Highest Daily Maximum**"),
      paste(year:(year+1),"Critical Value***"))
    ## Create Excel spreadsheet object
    title.row <- CellBlock(area.sheets[[1]],startRow=1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
    CB.setRowData(title.row,colnames(table1),rowIndex=1)
    for (r in 1:nrow(table1)) {
      data.row <- CellBlock(area.sheets[[1]],startRow=r+1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
      CB.setRowData(data.row,table1[r,],rowIndex=1,showNA=FALSE)
    }
  }
  
  ## Table 2: County-level design values for the 2015 NAAQS  
  if (p == 12) {
    ## Get highest design value for each county
    sitedv <- subset(sitedv.2015,status != "I")
    dv.var <- paste("dv",(year-2),year,sep="_")
    sitedv[which(sitedv$status == "I"),dv.var] <- NA
    sitedv$fips <- substr(sitedv$site,1,5)
    county.list <- unique(sitedv$fips)
    temp <- NULL
    for (i in 1:length(county.list)) {
      t <- subset(sitedv,fips == county.list[i])
      tmax <- t[order(t[,dv.var],t[,paste("max4",year,sep="_")],decreasing=TRUE)[1],]
      temp <- rbind(temp,tmax)
    }
    ## Get current year exceedance days for each county
    t1 <- subset(daily,site %in% sitedv$site)
    t2 <- tapply(t1$o3,list(substr(t1$site,1,5),t1$date),max.na)
    temp[,paste("exc",year,sep="_")] <- apply(t2,1,function(x) sum(x > level,na.rm=TRUE))
    ## Format Table 2 for export
    temp$meets_naaqs <- sapply(temp$status,function(x) switch(x,A="Yes",V="No","Inc"))
    table2 <- temp[,c("state_name","county_name","cbsa_name",dv.var,"meets_naaqs",
      paste("exc",year,sep="_"),paste("max4",c((year-2):year),sep="_"),
      paste("critical",c(year:(year+1)),sep="_"))]
    colnames(table2) <- c("State","County","Core Based Statistical Area (CBSA)",
      paste("Preliminary",year-2,"-",year,"Design Value"),"Meets 2015 NAAQS?",
      paste(year,"Exceedance Days*"),paste((year-2):year,"4th Highest Daily Maximum**"),
      paste(year:(year+1),"Critical Value***"))
    ## Create Excel spreadsheet object
    title.row <- CellBlock(area.sheets[[2]],startRow=1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
    CB.setRowData(title.row,colnames(table2),rowIndex=1)
    for (r in 1:nrow(table2)) {
      data.row <- CellBlock(area.sheets[[2]],startRow=r+1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
      CB.setRowData(data.row,table2[r,],rowIndex=1,showNA=FALSE)
    }
  }
  
  ## Table 3: Design values in 2008 8-hour NAAQS nonattainment areas
  if (p == 11) {
    ## Get highest design value for each nonattainment area
    sitedv <- subset(sitedv.2008,naa_name %in% naa.list.2008)
    dv.var <- paste("dv",(year-2),year,sep="_")
    sitedv[which(sitedv$status == "I"),dv.var] <- NA
    temp <- NULL
    for (i in 1:length(naa.list.2008)) {
      t <- subset(sitedv,naa_name == naa.list.2008[i])
      tmax <- t[order(t[,dv.var],t[,paste("max4",year,sep="_")],decreasing=TRUE)[1],]
      temp <- rbind(temp,tmax)
    }
    ## Get current year exceedance days for each area
    t1 <- subset(daily,site %in% sitedv$site)
    t2 <- merge(sitedv[,c("site","naa_name")],t1,by="site",all.x=FALSE,all.y=TRUE)
    t3 <- tapply(t2$o3,list(t2$naa_name,t2$date),max.na)
    t4 <- apply(t3,1,function(x) sum(x > level,na.rm=TRUE))
    temp[,paste("exc",year,sep="_")] <- sapply(temp$naa_name,function(x)
      ifelse(x %in% names(t4),t4[which(names(t4) == x)],0))
    ## Format Table 3 for export
    temp$meets_naaqs <- sapply(temp$status,function(x) switch(x,A="Yes",V="No","Inc"))
    temp$status <- subset(table1b,naa_name %in% naa.list.2008)$status
    temp$classification <- subset(table1b,naa_name %in% naa.list.2008)$classification
    table3 <- temp[,c("naa_name","status","classification",dv.var,"meets_naaqs",
      paste("exc",year,sep="_"),paste("max4",c((year-2):year),sep="_"),
      paste("critical",c(year,(year+1)),sep="_"))]
    colnames(table3) <- c("2008 NAAQS Nonattainment Area","Status","Classification",
      paste("Preliminary",year-2,"-",year,"Design Value"),"Meets 2008 NAAQS?",
      paste(year,"Exceedance Days*"),paste((year-2):year,"4th Highest Daily Maximum**"),
      paste(year:(year+1),"Critical Value***"))
    ## Create Excel spreadsheet object
    title.row <- CellBlock(area.sheets[[3]],startRow=1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
    CB.setRowData(title.row,colnames(table3),rowIndex=1)
    for (r in 1:nrow(table3)) {
      data.row <- CellBlock(area.sheets[[3]],startRow=r+1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
      CB.setRowData(data.row,table3[r,],rowIndex=1,showNA=FALSE)
    }
  }
  
  ## Table 4: Design values in 1997 8-hour NAAQS nonattainment areas
  if (p == 10) {
    ## Get highest design value for each nonattainment area
    sitedv <- subset(sitedv.1997,naa_name %in% naa.list.1997)
    dv.var <- paste("dv",(year-2),year,sep="_")
    sitedv[which(sitedv$status == "I"),dv.var] <- NA
    temp <- NULL
    for (i in 1:length(naa.list.1997)) {
      t <- subset(sitedv,naa_name == naa.list.1997[i])
      tmax <- t[order(t[,dv.var],t[,paste("max4",year,sep="_")],decreasing=TRUE)[1],]
      temp <- rbind(temp,tmax)
    }
    ## Get current year exceedance days for each area
    t1 <- subset(daily,site %in% sitedv$site)
    t2 <- merge(sitedv[,c("site","naa_name")],t1,by="site",all.x=FALSE,all.y=TRUE)
    t3 <- tapply(t2$o3,list(t2$naa_name,t2$date),max.na)
    t4 <- apply(t3,1,function(x) sum(x > level,na.rm=TRUE))
    temp[,paste("exc",year,sep="_")] <- sapply(temp$naa_name,function(x)
      ifelse(x %in% names(t4),t4[which(names(t4) == x)],0))
    ## Format Table 4 for export
    temp$meets_naaqs <- sapply(temp$status,function(x) switch(x,A="Yes",V="No","Inc"))
    temp$status <- gsub(" (NAAQS revoked)","",
      subset(table1c,naa_name %in% naa.list.1997)$status,fixed=TRUE)
    temp$classification <- gsub("Subpart 2/","",
      subset(table1c,naa_name %in% naa.list.1997)$classification,fixed=TRUE)
    table4 <- temp[,c("naa_name","status","classification",dv.var,"meets_naaqs",
      paste("exc",year,sep="_"),paste("max4",c((year-2):year),sep="_"),
      paste("critical",c(year,(year+1)),sep="_"))]
    colnames(table4) <- c("1997 NAAQS Nonattainment Area","Status","Classification",
      paste("Preliminary",year-2,"-",year,"Design Value"),"Meets 1997 NAAQS?",
      paste(year,"Exceedance Days*"),paste((year-2):year,"4th Highest Daily Maximum**"),
      paste(year:(year+1),"Critical Value***"))
    ## Create Excel spreadsheet object
    title.row <- CellBlock(area.sheets[[4]],startRow=1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
    CB.setRowData(title.row,colnames(table4),rowIndex=1)
    for (r in 1:nrow(table4)) {
      data.row <- CellBlock(area.sheets[[4]],startRow=r+1,startColumn=1,noRows=1,noColumns=11,create=FALSE)
      CB.setRowData(data.row,table4[r,],rowIndex=1,showNA=FALSE)
    }
  }
  
  ## Table 5: Design values in 1979 1-hour NAAQS nonattainment areas
  if (p == 9) {
    ## Get highest design value for each area
    sitedv <- subset(sitedv.1979,naa_name %in% naa.list.1979)
    dv.var <- paste("dv",(year-2),year,sep="_")
    ee.var <- paste("ee",(year-2),year,sep="_")
    sitedv[which(sitedv$status == "I"),ee.var] <- NA
    temp <- NULL
    for (i in 1:length(naa.list.1979)) {
      t <- subset(sitedv,naa_name == naa.list.1979[i])
      tmax <- t[order(t[,ee.var],t[,dv.var],decreasing=TRUE)[1],]
      temp <- rbind(temp,tmax)
    }
    ## Format table 5 for export
    temp$meets_naaqs <- sapply(temp$status,function(x) switch(x,A="Yes",V="No","Inc"))
    table5 <- temp[,c("naa_name",dv.var,ee.var,"meets_naaqs",
      paste("ee",c((year-2):(year-1)),sep="_"),paste("exc",year,sep="_"))]
    colnames(table5) <- c("Former 1-hour NAAQS Nonattainment Area",
      paste("Preliminary",year-2,"-",year,"Design Value"),
      paste(year-2,"-",year,"Average Exceedances"),"Meets 1-hour NAAQS?",
      paste((year-2):year,c(rep("Estimated",2),"Observed"),"Exceedances"))
    ## Create Excel spreadsheet object
    title.row <- CellBlock(area.sheets[[5]],startRow=1,startColumn=1,noRows=1,noColumns=7,create=FALSE)
    CB.setRowData(title.row,colnames(table5),rowIndex=1)
    for (r in 1:nrow(table5)) {
      data.row <- CellBlock(area.sheets[[5]],startRow=r+1,startColumn=1,noRows=1,noColumns=7,create=FALSE)
      CB.setRowData(data.row,table5[r,],rowIndex=1,showNA=FALSE)
    }
    
    ## Create Excel spreadsheet object for 1-hour site-level DVs
    sheet <- createSheet(site.wb,sheetName="1HR_NAAQS")
    sitedv <- eval(parse(text=paste("sitedv",naaqs,sep=".")))
    nr <- nrow(sitedv); nc <- ncol(sitedv);
    colnames(sitedv) <- c("AQS Site ID","Site Name","Address","Latitude","Longitude",
      "EPA Region","State Name","County Name","CBSA Name","CSA Name","Nonattainment Area Name",
      paste("Preliminary ",year-2,"-",year,"Design Value (ppb)"),
      paste(year-2,"-",year,"Average Expected Exceedances"),
      "Status (A=Attaining, I=Incomplete, V=Violating)",
      paste(year-2,"-",year,"Average Percent Complete"),
      paste(c((year-2):year),"Percent Complete"),
      paste(c((year-2):(year-1)),"Expected Exceedance Count"),
      paste(year,"Observed Exceedance Count"),
      paste(rep(c((year-2):year),each=5),rep(c("1st","2nd","3rd","4th","5th"),3),"Highest Daily Max (ppb)"),
      paste(rep(c((year-2):year),each=5),rep(c("1st","2nd","3rd","4th","5th"),3),"Highest Date"))
    title <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_CENTER",
      vertical="VERTICAL_BOTTOM",wrapText=TRUE),font=Font(site.wb,isBold=TRUE))
    left <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_LEFT"))
    center <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_CENTER"))
    right <- CellStyle(site.wb,alignment=Alignment(horizontal="ALIGN_RIGHT"),
      dataFormat=DataFormat("##00.000000"))
    cs.list <- vector("list",nc); names(cs.list) <- 1:nc;
    for (i in c(2,3,7:11)) { cs.list[[i]] <- left }
    for (i in c(1,6,12:nc)) { cs.list[[i]] <- center }
    for (i in c(4:5)) { cs.list[[i]] <- right }
    addDataFrame(sitedv,sheet,row.names=FALSE,colStyle=cs.list,colnamesStyle=title); gc();
    createFreezePane(sheet,rowSplit=2,colSplit=1,startRow=2,startCol=1)
    setColumnWidth(sheet,colIndex=c(2:3,7:14),colWidth=15)
    setColumnWidth(sheet,colIndex=c(1,4:6,15:nc),colWidth=12)
  }
}

## Save daily data and preliminary site-level DVs for shiny application
save(curr.1997,curr.2008,curr.2015,
  file=paste("OzoneWatch/data/daily_",date.stamp,".Rdata",sep=""))
save(sitedv.1997,sitedv.2008,sitedv.2015,
  file=paste("OzoneWatch/data/sitedv_",date.stamp,".Rdata",sep=""))

## Write area-level and site-level tables to Excel files
saveWorkbook(area.wb,file=paste("OzoneWatch/out",year,"/Ozone_Watch_",date.stamp,".xlsx",sep=""))
saveWorkbook(site.wb,file=paste("OzoneWatch/out",year,"/sitedv_",date.stamp,".xlsx",sep=""))

## Copy shiny app data files for publication, remove previous data files
app.files <- list.files("OzoneWatch/OzoneWatch/data/")
old.daily.file <- app.files[grep("daily_",app.files)][1]
old.dv.file <- app.files[grep("sitedv_",app.files)][1]
system(paste("cp OzoneWatch/data/daily_",date.stamp,".Rdata ",
  "OzoneWatch/OzoneWatch/data/daily_",date.stamp,".Rdata",sep=""))
system(paste("cp OzoneWatch/data/sitedv_",date.stamp,".Rdata ",
  "OzoneWatch/OzoneWatch/data/sitedv_",date.stamp,".Rdata",sep=""))
unlink(paste("OzoneWatch/OzoneWatch/data/",old.daily.file,sep=""))
unlink(paste("OzoneWatch/OzoneWatch/data/",old.dv.file,sep=""))
