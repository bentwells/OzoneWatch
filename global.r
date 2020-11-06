## Set options, load required packages
setwd("D:/OzoneWatch/")
options(stringsAsFactors=FALSE)
require(shiny,quietly=TRUE,warn.conflicts=FALSE)
require(DT,quietly=TRUE,warn.conflicts=FALSE)
require(plyr,quietly=TRUE,warn.conflicts=FALSE)
require(reshape2,quietly=TRUE,warn.conflicts=FALSE)
require(sp,quietly=TRUE,warn.conflicts=FALSE)

## Custom functions used in sub-applications
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
med.na <- function(x) { return(ifelse(all(is.na(x)),NA,median(x,na.rm=TRUE))) }
min.na <- function(x) { return(ifelse(all(is.na(x)),NA,min(x,na.rm=TRUE))) }
cummax.na <- function(x) { 
  not.na <- which(!is.na(x))
  x[not.na] <- cummax(x[not.na])
  return(x)
}
get.max.site <- function(sites,vals) {
  return(ifelse(is.null(which.max(vals)),NA,sites[which.max(vals)]))
}

## Run source code for sub-applications
source("shinyApp/rmapfuns.r")
source("shinyApp/dvtables.r")
source("shinyApp/areamaps.r")
source("shinyApp/dvtrends.r")
source("shinyApp/tileplot.r")
source("shinyApp/max4plot.r")

## Load most recent site-level design values file
files <- list.files("data")
load(paste("data",files[length(files)],sep="/"))
curr.year <- as.numeric(substr(colnames(sitedv.2015)[grep("dv",colnames(sitedv.2015))],9,12))
curr.date <- as.Date(gsub("sitedv_","",gsub(".Rdata","",files[length(files)])),format="%Y%m%d")
word.date <- as.character(format(curr.date,"%B %d, %Y"))
this.year <- as.numeric(substr(Sys.Date(),1,4)) == curr.year
