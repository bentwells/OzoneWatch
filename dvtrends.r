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
  curr.file <- paste("data",rev(files[grep("daily",files)])[1],sep="/")
  hist.file <- paste("data",rev(files[grep(paste("hist",std,sep="_"),files)])[1],sep="/")
  if (!exists(paste("curr",std,sep="."))) { load(curr.file,envir=sys.frame(0)) }
  if (!exists(paste("hist",std,sep="."))) { load(hist.file,envir=sys.frame(0)) }
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
    max.year <- ddply(t1,"year",summarize,dv=max.na(dv),max4=max.na(max4))
    for (i in 1:nrow(max.year)) {
      temp <- subset(t1,year == max.year$year[i])
      max.year$dv_site[i] <- get.max.site(sites=temp$site,vals=temp$dv)
      max.year$max4_site[i] <- get.max.site(sites=temp$site,vals=temp$max4)
    }
    t2 <- ldply(hist,function(x) rbind(subset(x$daily_data,
      day == ifelse(this.year,substr(curr.date,6,10),"12-31"))))
    t2$dv <- unlist(tapply(t2$max4,list(t2$site),filter.na))
    max.curr <- ddply(t2,"year",summarize,max4_curr=max.na(max4),dv_curr=max.na(dv))
    trend.hist <- merge(max.year,max.curr)
  }
  if (length(curr) == 0) { 
    trend.curr <- data.frame(site=hist[[1]]$site,year=curr.year,dv=NA,max4=NA,
      dv_site=NA,max4_site=NA,max4_curr=NA)
  }
  if (length(curr) > 0) { 
    temp <- ldply(curr,function(x) rbind(x$dv_data))[,c("site","year","dv","max4")]
    dv.site <- get.max.site(sites=temp$site,vals=temp$dv)
    max4.site <- get.max.site(sites=temp$site,vals=temp$max4)
    trend.curr <- data.frame(year=curr.year,dv=ifelse(this.year,NA,max.na(temp$dv)),
      max4=ifelse(this.year,NA,max.na(temp$max4)),dv_site=dv.site,max4_site=max4.site,
      max4_curr=max.na(temp$max4),dv_curr=max.na(temp$dv))
  }
  trend.data <- rbind(trend.hist,trend.curr)
  assign("dvtrends.vals",trend.data,envir=sys.frame(0))
  
  ## Generate trend plot
  xrange <- c(years[1]-0.5,years[ny]+0.5)
  legend.txt <- c("Annual 4th High","Design Value","NAAQS Level",
    paste("Annual 4th High as of",substr(word.date,1,nchar(word.date)-6)),
    paste("Design Value as of",substr(word.date,1,nchar(word.date)-6)))
  legend.col <- c("orange3","steelblue","black","orange3","steelblue")
  legend.lty <- c(1,1,1,2,2); legend.pch <- c(16,16,NA,18,18);
  if (!this.year) { legend.txt <- legend.txt[1:3]; legend.col <- legend.col[1:3];
    legend.lty <- legend.lty[1:3]; legend.pch <- legend.pch[1:3]; }
  par(mar=c(4,4,2,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5,las=2)
  plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=xrange,xlab="",
    yaxs='i',ylim=c(0,120),ylab="Concentration (ppb)",main=paste(title1,title2,sep=" - "))
  axis(side=1,at=years,labels=years,las=2)
  polygon(x=xrange[c(1,2,2,1)],y=c(0,0,120,120),col="gray85")
  abline(h=seq(10,110,10),v=years,col="white")
  lines(x=trend.data$year,y=trend.data$max4,lty=1,lwd=2,col="orange3")
  points(x=trend.data$year,y=trend.data$max4,col="orange3",pch=16,cex=2)
  lines(x=trend.data$year,y=trend.data$dv,lty=1,lwd=2,col="steelblue")
  points(x=trend.data$year,y=trend.data$dv,col="steelblue",pch=16,cex=2)
  if (this.year) {
    lines(x=trend.data$year,y=trend.data$max4_curr,lty=2,lwd=2,col="orange3")
    points(x=trend.data$year,y=trend.data$max4_curr,col="orange3",pch=18,cex=2)
    lines(x=trend.data$year,y=trend.data$dv_curr,lty=2,lwd=2,col="steelblue")
    points(x=trend.data$year,y=trend.data$dv_curr,col="steelblue",pch=18,cex=2)
  }
  abline(h=lvl,lty=1,lwd=2,col="black"); box();
  legend("bottomleft",legend=legend.txt,col=legend.col,lty=legend.lty,pch=legend.pch,
    lwd=2,ncol=ifelse(this.year,2,1),cex=1.5,pt.cex=2,bty='n')
}