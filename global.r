## Set options, load required packages
options(stringsAsFactors=FALSE)
require(shiny,quietly=TRUE,warn.conflicts=FALSE)
require(DT,quietly=TRUE,warn.conflicts=FALSE)
require(pins,quietly=TRUE,warn.conflicts=FALSE)
require(plyr,quietly=TRUE,warn.conflicts=FALSE)
require(reshape2,quietly=TRUE,warn.conflicts=FALSE)
require(sf,quietly=TRUE,warn.conflicts=FALSE)

## Custom functions used in sub-applications
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
med.na <- function(x) { return(ifelse(all(is.na(x)),NA,median(x,na.rm=TRUE))) }
min.na <- function(x) { return(ifelse(all(is.na(x)),NA,min(x,na.rm=TRUE))) }
cummax.na <- function(x) { 
  not.na <- which(!is.na(x))
  x[not.na] <- cummax(x[not.na])
  return(x)
}
filter.na <- function(x,N=3) {
  if (length(x) < N) { return(rep(NA,length(x))) }
  return(floor(filter(x,rep(1/N,N),sides=1)))
}
get.max.site <- function(sites,vals) {
  return(ifelse(is.null(which.max(vals)),NA,sites[which.max(vals)]))
}

## Run source code for sub-applications
source("rmapfuns.r")
source("dvtables.r")
source("areamaps.r")
source("dvtrends.r")
source("tileplot.r")
source("max4plot.r")

## Read current year data from pin
board <- pins::board_connect()
curr.pin <- paste(Sys.getenv("app_owner"),"OzoneWatch_curr_data",sep="/")
curr.date <- as.Date(substr(pin_meta(board,name=curr.pin)$created,1,10))
curr.data <- pin_read(board,name=curr.pin)
curr.1997 <- curr.data$curr.1997; curr.2008 <- curr.data$curr.2008; curr.2015 <- curr.data$curr.2015;
sitedv.1997 <- curr.data$sitedv.1997; sitedv.2008 <- curr.data$sitedv.2008; sitedv.2015 <- curr.data$sitedv.2015;
curr.year <- as.numeric(substr(colnames(sitedv.2015)[grep("dv",colnames(sitedv.2015))],9,12))
word.date <- as.character(format(curr.date,"%B %e, %Y"))
this.year <- as.numeric(substr(Sys.Date(),1,4)) == curr.year
