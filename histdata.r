## Set up working environment
setwd("/work/YODA/users/bwells01/")
library (chron); library(data.table); library(plyr); 
options(stringsAsFactors=FALSE,chron.year.abb=FALSE)
date.stamp <- chron(as.character(Sys.Date()),format="y-m-d",out.format="ymd")
year <- ifelse(month(date.stamp+1) > 4,year(date.stamp+1),year(date.stamp+1)-1)

## Find subdirectory containing most recent DV calculations
dv.dir <- paste("OzoneDV/DV",year-3,"_",year-1,sep="")
dir.num <- which.max(sapply(list.files(dv.dir),function(x) ifelse(nchar(x) == 7,
  chron(paste(match(tolower(substr(x,3,5)),tolower(month.abb)),
  substr(x,1,2),year,sep="/")),NA)))
dv.subdir <- paste(dv.dir,list.files(dv.dir)[dir.num],sep="/")

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
  level <- ifelse(p == 10,84,ifelse(p == 11,75,70))
  method <- ifelse(p == 10,"I",ifelse(p == 11,"P","U"))
  naaqs <- ifelse(p == 10,"1997",ifelse(p == 11,"2008","2015"))
  load(paste(dv.subdir,"/daily_",year-12,"_",year-1,"_",method,"RC",level,".Rdata",sep=""))
  load(paste(dv.subdir,"/dv_",year-12,"_",year-1,"_",method,"RC",level,".Rdata",sep=""))
  daily$year <- substr(daily$date,1,4)
  data <- ddply(daily,c("site","year"),summarize,
    day=substr(date,6,10),dmax=floor(dmax),max4=max4.cum(dmax))
  if (p < 12) {
    out$id <- paste(out$site,out$poc,sep="")
    sites <- subset(out,!duplicated(id))[,c(ncol(out),3:12)]
    colnames(sites)[1] <- "site"
  }
  if (p == 12) {
    sites <- subset(out,!duplicated(site))[,1:10]
  }
  sites$state_name <- gsub("Of","of",sites$state_name)
  temp <- split(data,data$site)
  hist <- vector("list",length(temp))
  for (i in 1:length(temp)) {
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
    if (p < 12) { hist[[i]]$naa_name <- sites$naa_name[i] }
    hist[[i]]$data <- temp[[i]]
  }
  assign(paste("hist",naaqs,sep="."),hist)
  hist.file <- paste("OzoneWatch/data/hist_",naaqs,"_",date.stamp,".Rdata",sep="")
  save(list=paste("hist",naaqs,sep="."),file=hist.file)
}
