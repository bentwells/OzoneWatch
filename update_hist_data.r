###################################################
## Generate historical datasets used in Ozone Watch
## Last Updated: May 17, 2022
###################################################

## Set up working environment
if (dir.exists("C:/")) { 
  source("C:/Users/bwells01/Documents/R/get_monitors.r")
  source("C:/Users/bwells01/Documents/R/get_rawdata.r") 
}
if (dir.exists("/home/")) { 
  source("/home/bwells01/R/get_monitors.r")
  source("/home/bwells01/R/get_rawdata.r")
}
library(data.table); library(plyr); 
options(stringsAsFactors=FALSE)
date.stamp <- format(Sys.Date(),"%Y%m%d")
year <- as.numeric(substr(date.stamp,1,4))-(as.numeric(substr(date.stamp,5,6)) < 4)

## Retrieve hourly ozone data from AQS
cat("Retrieving hourly ozone data from AQS...\n")
for (y in (year-12):(year-1)) {
  get.raw.data(par=44201,year=y)
  cat(y,as.character(Sys.time()),"\n")
}

## Calculate design values for all ozone standards
dv.dir <- paste("OzoneDV/DV",year-3,"_",year-1,sep="")
source(paste(dv.dir,"/code/dvs_step1.r",sep=""))
source(paste(dv.dir,"/code/dvs_step2.r",sep=""))
today <- toupper(format(Sys.Date(),"%d%b%y"))
work.dir <- paste(dv.dir,today,sep="/")
if (!dir.exists(work.dir)) {
  cat("Calculating Ozone Design Values - 2015 NAAQS...\n")
  design.values(years=c((year-12):(year-1)),method="U",level=70,nonreg=FALSE,ee.type="C")
  cat("Calculating Ozone Design Values - 2008 NAAQS...\n")
  design.values(years=c((year-12):(year-1)),method="P",level=75,nonreg=FALSE,ee.type="C")
  cat("Calculating Ozone Design Values - 1997 NAAQS...\n")
  design.values(years=c((year-12):(year-1)),method="I",level=84,nonreg=FALSE,ee.type="C")
  cat("Calculating Ozone Design Values - 1979 NAAQS...\n")
  design.values(years=c((year-12):(year-1)),method="H",level=124,nonreg=FALSE,ee.type="C")
  cat("Creating historical DV tables...\n")
  dv.tables(years=c((year-12):(year-1)),step1.date=today,type="FINAL")
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

## Create historical daily data files for max4plot and tileplot apps
for (p in 10:12) {
  
  ## Get data objects used to populate historical data lists
  level <- ifelse(p == 10,84,ifelse(p == 11,75,70))
  method <- ifelse(p == 10,"I",ifelse(p == 11,"P","U"))
  naaqs <- ifelse(p == 10,"1997",ifelse(p == 11,"2008","2015"))
  cat("Creating historical daily data files -",naaqs,"NAAQS...\n")
  load(paste(work.dir,"/daily_",year-12,"_",year-1,"_",method,"RC",level,".Rdata",sep=""))
  load(paste(work.dir,"/dv_",year-12,"_",year-1,"_",method,"RC",level,".Rdata",sep=""))
  load(paste(work.dir,"/monitors_",year-12,"_",year-1,".Rdata",sep=""))
  daily$year <- substr(daily$date,1,4)
  daily.data <- ddply(daily,c("site","year"),summarize,
    day=substr(date,6,10),dmax=floor(dmax),max4=max4.cum(dmax))
  temp <- split(daily.data,daily.data$site)
  sites <- monitors; dvs <- out;
  sites$site <- substr(sites$id,1,9)
  sites <- subset(sites,site %in% unique(daily.data$site) & !duplicated(site))
  dvs <- subset(dvs,site %in% unique(daily.data$site) & !duplicated(site))
  
  ## Apply current year site combos, if any
  tm <- get.monitors(par=44201,yr1=year,yr2=year,all=FALSE)
  tm$site <- substr(tm$id,1,9)
  combos <- subset(tm,combo_site != " " & !duplicated(site),c("site","combo_site","combo_date"))
  if (nrow(combos) > 0) {
    for (i in 1:nrow(combos)) {
      old.site <- which(sites$site == combos$site[i])
      new.site <- which(tm$site == combos$combo_site[i])
      if (length(old.site) == 0 | length(new.site) == 0) { next }
      site.cols <- colnames(dvs)[1:5]
      temp[[old.site]]$site <- tm$site[new.site]
      sites[old.site,] <- tm[new.site,]
      dvs[old.site,site.cols] <- tm[new.site,site.cols]
    }
  }
  
  ## Populate historical data list objects for each site
  hist <- vector("list",length(temp))
  for (i in 1:length(temp)) {
    dvdf <- data.frame(site=rep(dvs$site[i],12),
      year=c((year-12):(year-1)),
        dv=c(NA,NA,t(dvs[i,grep("dv",colnames(dvs))])),
    status=c(NA,NA,t(dvs[i,grep("code",colnames(dvs))])),
    pct3yr=c(NA,NA,t(dvs[i,intersect(grep("pct",colnames(dvs)),which(nchar(colnames(dvs)) == 13))])),
       pct=c(t(dvs[i,intersect(grep("pct",colnames(dvs)),which(nchar(colnames(dvs)) == 8))])),
       exc=c(t(dvs[i,grep("exc",colnames(dvs))])),
     crit1=c(NA,NA,t(pmax(3*(level+1)-dvs[i,grep("max4",colnames(dvs))][1:10]-dvs[i,grep("max4",colnames(dvs))][2:11],0))),
     crit2=c(NA,NA,t(pmax(3*(level+1)-dvs[i,grep("max4",colnames(dvs))][2:11]-dvs[i,grep("max4",colnames(dvs))][3:12],0))),
      max1=c(t(dvs[i,grep("max1",colnames(dvs))])),max2=c(t(dvs[i,grep("max2",colnames(dvs))])),
      max3=c(t(dvs[i,grep("max3",colnames(dvs))])),max4=c(t(dvs[i,grep("max4",colnames(dvs))])),
      max5=c(t(dvs[i,grep("max5",colnames(dvs))])),date1=c(t(dvs[i,grep("date1",colnames(dvs))])),
      date2=c(t(dvs[i,grep("date2",colnames(dvs))])),date3=c(t(dvs[i,grep("date3",colnames(dvs))])),
      date4=c(t(dvs[i,grep("date4",colnames(dvs))])),date5=c(t(dvs[i,grep("date5",colnames(dvs))])))
    hist[[i]]$site <- sites$site[i]
    hist[[i]]$site_name <- sites$site_name[i]
    hist[[i]]$address <- sites$address[i]
    hist[[i]]$latitude <- sites$latitude[i]
    hist[[i]]$longitude <- sites$longitude[i]
    hist[[i]]$epa_region <- sites$epa_region[i]
    hist[[i]]$state_name <- sites$state_name[i]
    hist[[i]]$county_name <- sites$county_name[i]
    hist[[i]]$cbsa_name <- sites$cbsa_name[i]
    hist[[i]]$csa_name <- sites$csa_name[i]
    if (p == 10) { hist[[i]]$naa_name <- sites$naa_name_1997[i] }
    if (p == 11) { hist[[i]]$naa_name <- sites$naa_name_2008[i] }
    if (p == 12) { hist[[i]]$naa_name <- sites$naa_name_2015[i] }
    hist[[i]]$dv_data <- dvdf
    hist[[i]]$daily_data <- temp[[i]]
  }
  assign(paste("hist",naaqs,sep="."),hist)
  hist.file <- paste("OzoneWatch/data/hist_",naaqs,"_",date.stamp,".Rdata",sep="")
  save(list=paste("hist",naaqs,sep="."),file=hist.file)
}

## Copy historical data files for shiny app publication, remove previous files
app.files <- list.files("OzoneWatch/OzoneWatch/data/")
old.hist.files <- app.files[grep("hist_",app.files)]
system(paste("cp OzoneWatch/data/hist_1997_",date.stamp,".Rdata ",
  "OzoneWatch/OzoneWatch/data/hist_1997_",date.stamp,".Rdata",sep=""))
system(paste("cp OzoneWatch/data/hist_2008_",date.stamp,".Rdata ",
  "OzoneWatch/OzoneWatch/data/hist_2008_",date.stamp,".Rdata",sep=""))
system(paste("cp OzoneWatch/data/hist_2015_",date.stamp,".Rdata ",
  "OzoneWatch/OzoneWatch/data/hist_2015_",date.stamp,".Rdata",sep=""))
unlink(paste("OzoneWatch/OzoneWatch/data/",old.hist.files,sep=""))