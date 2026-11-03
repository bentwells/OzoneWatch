###################################################################################
## Create ozone design value tables for annual regional review and airtrends update
## years = numeric vector of at least 3 consecutive years used to run dvs_step1.r  
###################################################################################
dv.tables <- function(years) {
  
  ## Custom functions called within main function
  max.na <- function(x) ifelse(any(!is.na(x)),max(x,na.rm=TRUE),NA)
  met.naaqs <- function(x) {
    y <- data.frame(A=sum(x == "A"),I=sum(x == "I"),V=sum(x == "V"))
    z <- ifelse(y$V > 0,"No",ifelse(y$A == 0,"Incomplete","Yes"))
    return(z)
  }
  get.naa.states <- function(df) {
    naa.names <- df$naa_name
    states <- unlist(sapply(naa.names,function(x) strsplit(x,split=", ")[[1]][2]))
    states <- unlist(sapply(states,function(x) strsplit(x,split="-")))
    codes <- unique(states[order(states)])
    state.codes <- c(state.abb,"DC","PR")
    state.names <- c(state.name,"District of Columbia","Puerto Rico")
    out <- state.names[match(codes,state.codes)]
    return(out)
  }
  
  ## Set up working environment
  require(plyr)
  source("code_dev/get_monitors.r")
  ny <- length(years); dvyr1 <- years[(ny-2)]; dvyr2 <- years[(ny-1)]; dvyr3 <- years[ny];
  dv.begin.date <- as.Date(paste(dvyr1,"01-01",sep="-"))
  cur <- paste(c("dv","ee","code"),dvyr1,dvyr3,sep=".")
  all <- paste(rep(c("dv","code"),times=(ny-2)),rep(paste(c(years[1]:dvyr1),
    c((years[1]+2):dvyr3),sep="."),each=2),sep=".")
    
  ## Load site-level DV files created by dvs_step1.r
  file.base <- paste("data_dev/dv_",years[1],"_",years[ny],sep="")
  load(paste(file.base,"HRC124.Rdata",sep="_")); dv.1979 <- out;
  load(paste(file.base,"IRC84.Rdata",sep="_")); dv.1997 <- out;
  load(paste(file.base,"PRC75.Rdata",sep="_")); dv.2008 <- out;
  load(paste(file.base,"URC70.Rdata",sep="_")); dv.2015 <- out;
  
  ## Retrieve nonattainment area info from AQS
  naa.1979 <- get.naa.info(par=44201,psid=9)
  naa.1997 <- get.naa.info(par=44201,psid=10)
  naa.2008 <- get.naa.info(par=44201,psid=11)
  naa.2015 <- get.naa.info(par=44201,psid=23)
  
  ## Table 0: Monitor metadata
  monitors <- get.monitors(par=44201,yr1=years[1],yr2=years[ny],all=TRUE)
  ids <- monitors$id[!duplicated(monitors$id)]
  agency <- monitors$reporting_agency[!duplicated(monitors$id)]
  base_info <- monitors[!duplicated(monitors$id),c("epa_region","state_name","county_name",
    "cbsa_name","csa_name","site_name","address","latitude","longitude",
    "naa_name_2015","naa_name_2008","naa_name_1997","naa_name_1979")]
  mbd <- get.unique.dates(monitors$monitor_begin_date,monitors$id,first=TRUE)
  med <- get.unique.dates(monitors$monitor_end_date,monitors$id,first=FALSE)
  lsd <- get.unique.dates(monitors$last_sample_date,monitors$id,first=FALSE)
  pbd <- get.unique.dates(monitors$primary_begin_date,monitors$id,first=TRUE)
  ped <- get.unique.dates(monitors$primary_end_date,monitors$id,first=FALSE)
  sbd <- substr(get.unique.codes(monitors$season_begin_date,monitors$id,monitors$season_begin_date),6,10)
  sed <- substr(get.unique.codes(monitors$season_begin_date,monitors$id,monitors$season_end_date),6,10)
  nbd <- get.unique.dates(monitors$nonreg_begin_date,monitors$id,first=TRUE)
  ned <- get.unique.dates(monitors$nonreg_end_date,monitors$id,first=FALSE)
  nrc <- get.unique.codes(monitors$nonreg_begin_date,monitors$id,monitors$nonreg_concur)
  mtc <- get.unique.codes(monitors$method_begin_date,monitors$id,monitors$method_code)
  msc <- get.unique.codes(monitors$monitor_begin_date,monitors$id,monitors$measurement_scale)
  obj <- get.unique.codes(monitors$monitor_begin_date,monitors$id,monitors$monitor_objective)
  cfr <- get.unique.codes(monitors$monitor_begin_date,monitors$id,monitors$collection_frequency)
  frm_fem <- non_ref <- rep(" ",length(ids)); check.dates <- rep(FALSE,length(ids));
  for (i in 1:length(ids)) {
    codes <- subset(monitors,id == ids[i],
      c("method_code","frm_code","method_begin_date","method_end_date"))
    codes <- codes[which(!duplicated(codes)),]
    codes <- codes[order(codes$method_code),]
    if (sum(codes$frm_code != " ") > 0) { 
      frm_fem[i] <- paste(unique(codes$method_code[which(codes$frm_code != " ")]),collapse=",")
    }
    if (sum(codes$frm_code == " ") > 0) { 
      non_ref[i] <- paste(unique(codes$method_code[which(codes$frm_code == " ")]),collapse=",")
    }
    if (non_ref[i] != " ") {
      nr <- subset(codes,frm_code == " ")
      nr$method_end_date <- sapply(nr$method_end_date,function(x) ifelse(x == " ",Sys.Date(),as.Date(x)))
      if (any(nr$method_end_date > dv.begin.date)) { check.dates[i] <- TRUE }
    }
  }
  monitors$monitor_type <- gsub("NON-REGULATORY"," ",gsub("OTHER"," ",monitors$monitor_type))
  types <- gsub(" ,","",tapply(monitors$monitor_type,list(monitors$id),function(x) 
    paste(unique(x[order(x)]),collapse=",")))
  if (any(types == " ")) { types[which(types == " ")] <- "OTHER" }
  monitors$network <- gsub("UNOFFICIAL ","",gsub("PROPOSED ","",monitors$network))
  nets <- tapply(monitors$network,list(monitors$id),function(x)
    paste(unique(x[order(x)]),collapse=","))
  combos <- subset(monitors,combo_site != " ",c("id","combo_site","combo_date"))
  combos <- combos[which(!duplicated(combos)),]
  combo_site <- sapply(substr(ids,1,9),function(x) ifelse(x %in% substr(combos$id,1,9),
    combos$combo_site[match(x,substr(combos$id,1,9))],ifelse(x %in% combos$combo_site,
    substr(combos$id[match(x,combos$combo_site)],1,9)," ")))
  combo_date <- sapply(substr(ids,1,9),function(x) ifelse(x %in% substr(combos$id,1,9),
    combos$combo_date[match(x,substr(combos$id,1,9))],ifelse(x %in% combos$combo_site,
    combos$combo_date[match(x,combos$combo_site)]," ")))
  table0 <- data.frame(parameter="44201",site=substr(ids,1,9),poc=substr(ids,10,10),base_info,
    monitor_begin_date=mbd,monitor_end_date=med,last_sample_date=lsd,primary_begin_date=pbd,
    primary_end_date=ped,nonreg_begin_date=nbd,nonreg_end_date=ned,nonreg_concur=nrc,frm_fem,
    non_ref,combo_site,combo_date,agency,collection_frequency=cfr,season_begin_date=sbd,
    season_end_date=sed,monitor_types=types,monitor_networks=nets,measurement_scale=msc,
    monitor_objective=obj,row.names=NULL)
  
  ## Table 1a: 2015 8-hour NAAQS nonattainment area status
  temp <- subset(dv.2015,naa_name_2015 != " ")[,c("naa_name_2015",all)]
  temp[,grep("dv",colnames(temp))] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),
    temp[,grep("dv",colnames(temp))],temp[,grep("code",colnames(temp))])
  vals <- tapply(temp[,cur[1]],list(temp$naa_name_2015),max.na)
  meets.naaqs <- tapply(temp[,cur[3]],list(temp$naa_name),met.naaqs)
  table1a <- data.frame(naa.2015[,c("naa_name","epa_regions","status","classification")],dv=vals/1000,
    meets_naaqs=meets.naaqs,naa.2015[,c("cdd_date","redesignation_date")],row.names=c(1:length(vals)))
  
  ## Table 1b: 2008 8-hour NAAQS nonattainment area status
  temp <- subset(dv.2008,naa_name_2008 != " ")[,c("naa_name_2008",all)]
  temp[,grep("dv",colnames(temp))] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),
    temp[,grep("dv",colnames(temp))],temp[,grep("code",colnames(temp))])
  vals <- tapply(temp[,cur[1]],list(temp$naa_name_2008),max.na)
  meets.naaqs <- tapply(temp[,cur[3]],list(temp$naa_name),met.naaqs)
  table1b <- data.frame(naa.2008[,c("naa_name","epa_regions","status","classification")],dv=vals/1000,
    meets_naaqs=meets.naaqs,naa.2008[,c("cdd_date","redesignation_date")],row.names=c(1:length(vals)))
  
  ## Table 1c: 1997 8-hour NAAQS nonattainment area status
  temp <- subset(dv.1997,naa_name_1997 != " ")[,c("naa_name_1997",all)]
  temp[,grep("dv",colnames(temp))] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),
    temp[,grep("dv",colnames(temp))],temp[,grep("code",colnames(temp))])
  vals <- tapply(temp[,cur[1]],list(temp$naa_name_1997),max.na)
  meets.naaqs <- tapply(temp[,cur[3]],list(temp$naa_name),met.naaqs)
  table1c <- data.frame(naa.1997[,c("naa_name","epa_regions","status","classification")],dv=vals/1000,
    meets_naaqs=meets.naaqs,naa.1997[,c("cdd_date","redesignation_date")],row.names=c(1:length(vals)))
  
  ## Table 1d: 1979 1-hour NAAQS nonattainment area status
  temp <- subset(dv.1979,naa_name_1979 != " ")[,c("naa_name_1979","epa_region",cur)]
  regions <- unlist(tapply(temp$epa_region,list(temp$naa_name_1979),
    function(x) paste(unique(x[order(x)]),collapse=",")))
  vals <- tapply(temp[,cur[1]],list(temp$naa_name_1979),max.na)
  exc <- tapply(temp[,cur[2]],list(temp$naa_name_1979),max.na)
  meets.naaqs <- sapply(exc,function(x) ifelse(is.na(x),"Incomplete",ifelse(x <= 1,"Yes","No")))
  table1d <- data.frame(naa_name=names(regions),region=regions,dv=vals/1000,
    ee=exc,meets_naaqs=meets.naaqs,row.names=c(1:length(regions)))
  
  ## Table 2: Violating monitors outside 2015 8-hour NAAQS nonattainment areas
  table2 <- dv.2015[which(dv.2015[,cur[3]] == "V" & dv.2015$naa_name_2015 == " "),
    c("state_name","county_name","epa_region","site",cur[1],"cbsa_name")]
  table2[,cur[1]] <- table2[,cur[1]]/1000
  colnames(table2) <- gsub(".","_",colnames(table2),fixed=TRUE)
  
  ## Table 3a: Trends in 2015 8-hour NAAQS nonattainment areas
  temp <- subset(dv.2015,naa_name_2015 != " ")[,c("naa_name_2015",all)]
  temp[,grep("dv",colnames(temp))] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),
    temp[,grep("dv",colnames(temp))],temp[,grep("code",colnames(temp))])
  vals <- apply(temp[,grep("dv",colnames(temp))],2,function(x)
    tapply(x,list(temp$naa_name_2015),max.na))
  table3a <- data.frame(naa.2015[,c("naa_name","epa_regions")],
    vals/1000,row.names=c(1:nrow(naa.2015)))
  
  ## Table 3b: Trends in 2008 8-hour NAAQS nonattainment areas
  temp <- subset(dv.2008,naa_name_2008 != " ")[,c("naa_name_2008",all)]
  temp[,grep("dv",colnames(temp))] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),
    temp[,grep("dv",colnames(temp))],temp[,grep("code",colnames(temp))])
  vals <- apply(temp[,grep("dv",colnames(temp))],2,function(x)
    tapply(x,list(temp$naa_name_2008),max.na))
  table3b <- data.frame(naa.2008[,c("naa_name","epa_regions")],
    vals/1000,row.names=c(1:nrow(naa.2008)))
  
  ## Table 3c: Trends in 1997 8-hour NAAQS nonattainment areas
  temp <- subset(dv.1997,naa_name_1997 != " ")[,c("naa_name_1997",all)]
  temp[,grep("dv",colnames(temp))] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),
    temp[,grep("dv",colnames(temp))],temp[,grep("code",colnames(temp))])
  vals <- apply(temp[,grep("dv",colnames(temp))],2,function(x)
    tapply(x,list(temp$naa_name_1997),max.na))
  table3c <- data.frame(naa.1997[,c("naa_name","epa_regions")],vals/1000,
    row.names=c(1:nrow(naa.1997)))
  
  ## Table 4: County-level design values for the 2015 8-hour NAAQS
  temp <- dv.2015[,c("site","state_name","county_name","epa_region","cbsa_name",cur[1],cur[3])]
  temp[,cur[1]] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),temp[,cur[1]],temp[,cur[3]])
  temp$fips <- substr(temp$site,1,5)
  temp <- temp[order(temp$fips,temp[,cur[1]],decreasing=TRUE),]
  temp <- temp[!duplicated(temp$fips),]
  temp <- temp[order(temp$fips),]
  table4 <- na.omit(data.frame(state_name=temp$state_name,county_name=temp$county_name,
    state_fips=substr(temp$fips,1,2),county_fips=substr(temp$fips,3,5),
    epa_region=temp$epa_region,site=temp$site,dv=temp[,cur[1]]/1000,cbsa_name=temp$cbsa_name))
  
  ## Table 5: Monitor-level design values for the 2015 8-hour NAAQS
  temp <- dv.2015[which(!is.na(dv.2015[,paste("pct",dvyr1,dvyr3,sep=".")])),
    c("state_name","county_name","cbsa_name","csa_name","naa_name_2015","epa_region",
    "site","site_name","address","latitude","longitude",cur[1],cur[3],
    paste("pct",dvyr1,dvyr3,sep="."),paste("pct",c(dvyr1:dvyr3),sep="."),
    paste("max4",c(dvyr1:dvyr3),sep="."),paste("exc",c(dvyr1:dvyr3),sep="."))]
  temp$val.dv <- mapply(function(dv,code) ifelse(code == "I",NA,dv),temp[,cur[1]],temp[,cur[3]])
  temp$inc.dv <- mapply(function(dv,code) ifelse(code == "I",dv,NA),temp[,cur[1]],temp[,cur[3]])
  ppb.cols <- c("val.dv","inc.dv",paste("max4",c(dvyr1:dvyr3),sep="."))
  temp[,ppb.cols] <- temp[,ppb.cols]/1000
  table5 <- temp[,c(1:11,24,25,14:23)]
  colnames(table5) <- gsub(".","_",colnames(table5),fixed=TRUE)
  
  ## Table 6: Trends in monitor-level design values for the 2015 8-hour NAAQS
  temp <- dv.2015[,c("state_name","county_name","cbsa_name","csa_name","naa_name_2015",
    "epa_region","site","site_name","address","latitude","longitude",all)]
  temp[,grep("dv",colnames(temp))] <- mapply(function(dv,code) ifelse(code == "I",NA,dv),
    temp[,grep("dv",colnames(temp))],temp[,grep("code",colnames(temp))])
  counts <- apply(temp[,grep("dv",colnames(temp))],1,function(x) sum(!is.na(x)))
  table6 <- temp[which(counts > 0),-c(grep("code",colnames(temp)))]
  table6[,grep("dv",colnames(table6))] <- table6[,grep("dv",colnames(table6))]/1000
  colnames(table6) <- gsub(".","_",colnames(table6),fixed=TRUE)
  
  ## Save final DV tables in .Rdata format
  file.out <- paste("data_dev/dvtables",dvyr1,dvyr3,format(Sys.Date(),"%Y%m%d"),sep="_")
  save(list=ls(pattern="table"),file=paste(file.out,".Rdata",sep=""))
}
