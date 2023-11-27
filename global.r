## Set options, load required packages
options(stringsAsFactors=FALSE)
require(shiny,quietly=TRUE,warn.conflicts=FALSE)
require(DT,quietly=TRUE,warn.conflicts=FALSE)
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

## Load app files
files <- list.files("data")
load(paste("data",files[length(files)],sep="/"))
curr.year <- as.numeric(substr(colnames(sitedv.2015)[grep("dv",colnames(sitedv.2015))],9,12))
curr.date <- as.Date(gsub("sitedv_","",gsub(".Rdata","",files[length(files)])),format="%Y%m%d")
word.date <- as.character(format(curr.date,"%B %d, %Y"))
this.year <- as.numeric(substr(Sys.Date(),1,4)) == curr.year
