## Function to generate AQI tile plots based on inputs
tile.plot <- function(naaqs,geo,state,out) {
  
  ## Exit if any required inputs are missing
  if (is.null(naaqs)) { return() }
  if (is.null(geo)) { return() }
  if (is.null(state)) { return() }
  if (is.null(out)) { return() }
  if (out == "Please make a selection!") { return() }
  
  ## Set constants based on naaqs input
  std <- substr(naaqs,1,4)
  lvl <- ifelse(std == "2015",70,ifelse(std == "2008",75,84))
  years <- c((curr.year-12):curr.year)
  title2 <- paste(std," Ozone NAAQS (",lvl," ppb)",sep="")
  if (std == "1997") { aqi.breaks <- c(0,64.9,84.9,104.9,124.9,374.9) }
  if (std == "2008") { aqi.breaks <- c(0,59.9,75.9,95.9,115.9,374.9) }
  if (std == "2015") { aqi.breaks <- c(0,54.9,70.9,85.9,105.9,200.9) }
  break.min <- c(0,floor(aqi.breaks[2:5])+1)
  break.max <- floor(aqi.breaks[2:6])
  
  ## Get current year and historical data based on naaqs input
  if (!exists(paste("hist",std,sep="."))) { assign(paste("hist",std,sep="."),
    pin_read(board,name=paste(Sys.getenv("app_owner"),"/OzoneWatch_hist",std,sep="")),envir=sys.frame(0)) }
  curr.data <- eval(parse(text=paste("curr",std,sep=".")))
  hist.data <- eval(parse(text=paste("hist",std,sep=".")))
  
  ## Get site-level data based on geo input
  if (geo == "AQS Site ID") {
    curr <- curr.data[sapply(curr.data,function(x) substr(x$site,1,9) == out)]
    hist <- hist.data[sapply(hist.data,function(x) substr(x$site,1,9) == out)]
    title1 <- paste("AQS Site ID =",out)
  }
  if (geo == "State/County") {
    curr <- curr.data[sapply(curr.data,function(x) x$state == state & x$county == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$state == state & x$county == out)]
    title1 <- paste(out,"County,",state)
  }
  if (geo == "Core Based Statistical Area (CBSA)") {
    curr <- curr.data[sapply(curr.data,function(x) x$cbsa_name == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$cbsa_name == out)]
    title1 <- paste(out,"CBSA")
  }
  if (geo == "Combined Statistical Area (CSA)") {
    curr <- curr.data[sapply(curr.data,function(x) x$csa_name == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$csa_name == out)]
    title1 <- paste(out,"CSA")
  }
  if (geo == "Nonattainment Area (NAA)") {
    curr <- curr.data[sapply(curr.data,function(x) x$naa_name == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$naa_name == out)]
    title1 <- paste(out,"Nonattainment Area")
  }
  
  ## Create time series for AQI tile plot
  if (length(curr) == 0) { 
    dates <- seq(from=as.Date(paste(curr.year,"01-01",sep="-")),
      to=as.Date(paste(curr.year,"12-31",sep="-")),by=1)
    temp <- data.frame(site=hist[[1]]$site,year=substr(dates,1,4),
      day=substr(dates,6,10),dmax=NA,max4=NA)
  }
  if (length(curr) > 0) { temp <- ldply(curr,function(x) rbind(x$daily_data)) }
  if (length(hist) > 0) { temp <- rbind(temp,ldply(hist,function(x) rbind(x$daily_data))) }
  temp <- recast(temp,year + day ~ site + variable,
    id.var=c("site","year","day"),measure.var="dmax")
  temp$dmax <- floor(apply(as.matrix(temp[,grep("dmax",colnames(temp))]),1,max.na))
  temp <- recast(temp,day ~ year + variable,id.var=c("day","year"),measure.var="dmax")
  if (nrow(temp) < 366) {
    feb29 <- data.frame(day="02-29",matrix(NA,nrow=1,ncol=ncol(temp)-1))
    colnames(feb29) <- colnames(temp)
    temp <- rbind(temp[1:59,],feb29,temp[60:nrow(temp),])
  }
  for (y in years) {
    if (length(grep(y,colnames(temp))) == 0) { 
      temp[,paste(y,"dmax",sep="_")] <- rep(NA,nrow(temp)) 
    }
  }
  temp <- temp[,c("day",paste(years,"dmax",sep="_"))]
  assign("tileplot.vals",temp,envir=sys.frame(0))
  
  ## Generate AQI tile plot
  par(mar=c(6,3.5,2,0.5),cex.axis=1.5,cex.lab=1.5,cex.main=1.4,xpd=TRUE)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,bg="gray75",xlim=c(0,366),xaxs="i",xlab="",
    ylim=c(curr.year+0.5,years[1]-0.5),yaxs="i",ylab="",
    main=paste(title1,title2,sep=" - "))
  polygon(x=par("usr")[c(1,2,2,1)],y=par("usr")[c(3,3,4,4)],col="#C0C0C0")
  image(x=c(0:366),y=years,z=as.matrix(tileplot.vals[,-1]),breaks=aqi.breaks,
    col=c("#009000","#FFFF00","#FFAA00","#FF0000","#9000AA"),add=TRUE)
  x.ticks <- c(0,cumsum(table(substr(tileplot.vals$day,1,2))))
  x.midpt <- x.ticks[1:12] + (x.ticks[2:13]-x.ticks[1:12])/2
  y.ticks <- c(years-0.5,curr.year+0.5)
  axis(side=1,at=x.ticks,labels=FALSE,tcl=-0.5)
  mtext(month.abb,side=1,line=0.5,at=x.midpt,las=0,cex=1.5)
  axis(side=2,at=y.ticks,labels=FALSE,tcl=-0.5)
  mtext(years,side=2,line=0.25,at=years,las=2,cex=1.5)
  box()
  rect(xleft=c(0,0.125,0.265,0.545,0.695,0.89)*366,ybottom=curr.year+1.2,
    xright=c(0.025,0.15,0.29,0.57,0.72,0.915)*366,ytop=curr.year+1.6,border="#000000",
    col=c("#009000","#FFFF00","#FFAA00","#FF0000","#9000AA","#C0C0C0"))
  text(labels=c("Good","Moderate","Unhealthy Sens. Groups","Unhealthy","Very Unhealthy",
    "No Data"),x=c(0.025,0.15,0.29,0.57,0.72,0.915)*366,y=curr.year+1.4,cex=1.5,pos=4)
  text(labels=paste(break.min,"-",break.max,"ppb"),
    x=c(0,0.125,0.265,0.545,0.695)*366,y=curr.year+2,cex=1.5,pos=4)
}
