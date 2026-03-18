## Function to create design value and 4th max trend plots based on inputs
dv.trends <- function(naaqs,geo,state,out) {
  
  ## Exit if any required inputs are missing
  if (is.null(naaqs)) { return() }
  if (is.null(geo)) { return() }
  if (is.null(state)) { return() }
  if (is.null(out)) { return() }
  if (out == "Please make a selection!") { return() }
  
  ## Set constants based on naaqs input
  std <- substr(naaqs,1,4)
  lvl <- ifelse(std == "2015",70,ifelse(std == "2008",75,84))
  vals <- eval(parse(text=paste("sitedv",std,sep=".")))
  title2 <- paste(std," Ozone NAAQS (",lvl," ppb)",sep="")
  years <- c((curr.year-12):curr.year); ny <- length(years);
  
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
  
  ## Create time series data.frame for trend plot
  if (length(hist) > 0) { 
    t1 <- ldply(hist,function(x) rbind(x$dv_data))[,c("site","year","dv","max4")]
    dmax <- recast(ldply(hist,function(x) rbind(x$daily_data)),year + day ~ site + variable,
      id.var=c("site","year","day"),measure.var="dmax")
    dmax$dmax <- apply(dmax[-c(1,2)],1,max.na)
    max.year <- ddply(t1,"year",summarize,dv=max.na(dv),max4=max.na(max4))
    year.exc <- ddply(dmax,"year",summarize,exc=sum(dmax >= 71,na.rm=TRUE))
    for (i in 1:nrow(max.year)) {
      temp <- subset(t1,year == max.year$year[i])
      exc <- subset(year.exc,year == max.year$year[i])
      max.year$exc[i] <- ifelse(nrow(exc) > 0,exc$exc[which(exc$year == max.year$year[i])],NA)
      max.year$dv_site[i] <- get.max.site(sites=temp$site,vals=temp$dv)
      max.year$max4_site[i] <- get.max.site(sites=temp$site,vals=temp$max4)
    }
    max.date <- ifelse(this.year,substr(curr.date,6,10),"12-31")
    t2 <- ldply(hist,function(x) rbind(subset(x$daily_data,day == max.date)))
    t <- merge(t1,t2,by=c("site","year"),all=TRUE,suffixes=c(".1",".2"))
    for (i in 3:nrow(t)) {
      if (t$site[i] != t$site[i-2]) { next }
      t$dv.2[i] <- floor((t$max4.1[i-2] + t$max4.1[i-1] + t$max4.2[i])/3)
    }
    max.curr <- ddply(t,"year",summarize,max4_curr=max.na(max4.2),dv_curr=max.na(dv.2))
    dmax <- recast(dmax,day ~ year + variable,id.var=c("day","year"),measure.var="dmax")
    temp <- apply(as.matrix(dmax[c(1:which(dmax$day == max.date)),-1]),2,
      function(x) sum(x >= 71,na.rm=TRUE))
    if (is.null(names(temp))) { names(temp) <- colnames(dmax)[-1] }
    temp <- data.frame(year=as.integer(substr(names(temp),1,4)),exc_curr=temp,
      row.names=1:length(temp))
    max.curr <- merge(max.curr,temp,by="year",all.x=TRUE,all.y=FALSE)
    trend.hist <- merge(max.year,max.curr,by="year",all=TRUE)
  }
  if (length(curr) == 0) { 
    trend.curr <- data.frame(year=curr.year,dv=NA,max4=NA,exc=NA,
      dv_site=NA,max4_site=NA,dv_curr=NA,max4_curr=NA,exc_curr=NA)
  }
  if (length(curr) > 0) { 
    temp <- ldply(curr,function(x) rbind(x$dv_data))[,c("site","year","dv","max4")]
    dmax <- recast(ldply(curr,function(x) rbind(x$daily_data)),year + day ~ site + variable,
      id.var=c("site","year","day"),measure.var="dmax")
    dmax$dmax <- apply(dmax[-c(1,2)],1,max.na)
    trend.curr <- data.frame(year=curr.year,
      dv=ifelse(this.year,NA,max.na(temp$dv)),
      max4=ifelse(this.year,NA,max.na(temp$max4)),
      exc=ifelse(this.year,NA,sum(dmax$dmax >= 71,na.rm=TRUE)),
      dv_site=get.max.site(sites=temp$site,vals=temp$dv),
      max4_site=get.max.site(sites=temp$site,vals=temp$max4),
      dv_curr=max.na(temp$dv),max4_curr=max.na(temp$max4),
      exc_curr=sum(dmax$dmax >= 71,na.rm=TRUE))
  }
  trend.data <- rbind(trend.hist,trend.curr)
  assign("dvtrends.vals",trend.data,envir=sys.frame(0))
  
  ## Generate trend plot
  xrange <- c(years[1]-0.5,years[ny]+0.5)
  ymax <- pmax(100,10*ceiling(max.na(trend.data[,c("max4","max4_curr")]/10)))
  max.exc <- pmax(20,10*ceiling(1.5*max.na(trend.data[,c("exc","exc_curr")]/10)))
  exc.hist <- trend.data$exc*(ymax/max.exc)
  exc.curr <- trend.data$exc_curr*(ymax/max.exc)
  y.grid <- seq(10,ymax-10,10); Ny <- length(y.grid);
  legend.hts <- ymax*c(-0.12,-0.16,-0.2)
  par(mar=c(9,4,2,4),mgp=c(2.5,0.8,0),cex.axis=1.5,cex.lab=1.5,las=2,xpd=TRUE)
  plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=xrange,xlab="",
    yaxs='i',ylim=c(0,ymax),ylab="Concentration (ppb)",main=paste(title1,title2,sep=" - "))
  axis(side=1,at=years,labels=years,las=2)
  axis(side=4,at=seq(0,ymax,length.out=6),labels=seq(0,max.exc,length.out=6))
  mtext("Number of Ozone Exceedance Days",side=4,line=2.5,at=ymax/2,las=0,cex=1.5)
  polygon(x=xrange[c(1,2,2,1)],y=c(0,0,ymax,ymax),col="gray85")
  segments(x0=rep(xrange[1],Ny),x1=rep(xrange[2],Ny),y0=y.grid,y1=y.grid,col="white")
  segments(x0=years,x1=years,y0=rep(0,13),y1=rep(ymax,13),col="white")
  lines(x=trend.data$year,y=trend.data$max4,lty=1,lwd=2,col="orange3")
  points(x=trend.data$year,y=trend.data$max4,col="orange3",pch=16,cex=2)
  lines(x=trend.data$year,y=trend.data$dv,lty=1,lwd=2,col="steelblue")
  points(x=trend.data$year,y=trend.data$dv,col="steelblue",pch=16,cex=2)
  lines(x=trend.data$year,y=exc.hist,lty=1,lwd=2,col="green3")
  points(x=trend.data$year,y=exc.hist,col="green3",pch=16,cex=2)
  if (this.year) {
    lines(x=trend.data$year,y=trend.data$max4_curr,lty=2,lwd=2,col="orange3")
    points(x=trend.data$year,y=trend.data$max4_curr,col="orange3",pch=18,cex=2)
    lines(x=trend.data$year,y=trend.data$dv_curr,lty=2,lwd=2,col="steelblue")
    points(x=trend.data$year,y=trend.data$dv_curr,col="steelblue",pch=18,cex=2)
    lines(x=trend.data$year,y=exc.curr,lty=2,lwd=2,col="green3")
    points(x=trend.data$year,y=exc.curr,col="green3",pch=18,cex=2)
  }
  segments(x0=xrange[1],x1=xrange[2],y0=lvl,y1=lvl,lty=1,lwd=2,col="black"); box();
  segments(x0=rep(xrange[1],3),x1=rep(xrange[1]+0.5,3),y0=legend.hts,y1=legend.hts,
    lty=1,lwd=2,col=c("orange3","steelblue","green3"))
  points(x=rep(xrange[1]+0.25,3),y=legend.hts,col=c("orange3","steelblue","green3"),pch=16,cex=2)
  text(x=rep(xrange[1]+0.5,3),y=legend.hts,labels=c("Annual 4th High","Design Value",
    "Exceedance Days"),pos=4,cex=1.25)
  if (this.year) {
    segments(x0=rep(xrange[1]+3.5,3),x1=rep(xrange[1]+4,3),y0=legend.hts,y1=legend.hts,
      lty=2,lwd=2,col=c("orange3","steelblue","green3"))
    points(x=rep(xrange[1]+3.75,3),y=legend.hts,col=c("orange3","steelblue","green3"),pch=18,cex=2)
    text(labels=c(paste("Annual 4th High as of",substr(word.date,1,nchar(word.date)-6)),
      paste("Design Value as of",substr(word.date,1,nchar(word.date)-6)),
      paste("Exceedance Days as of",substr(word.date,1,nchar(word.date)-6))),
      x=rep(xrange[1]+4,3),y=legend.hts,pos=4,cex=1.25)
    segments(x0=xrange[1]+9,x1=xrange[1]+9.5,y0=legend.hts[1],y1=legend.hts[1],col="black",lwd=2)
    text(x=xrange[1]+9.5,y=legend.hts[1],labels=paste(std,"NAAQS Level"),pos=4,cex=1.25)
  }
  if (!this.year) {
    segments(x0=xrange[1]+3.5,x1=xrange[1]+4,y0=legend.hts[1],y1=legend.hts[1],col="black",lwd=2)
    text(x=xrange[1]+4,y=legend.hts[1],labels=paste(std,"NAAQS Level"),pos=4,cex=1.25)
  }
}