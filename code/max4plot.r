## Function to generate cumulative 4th max plots based on inputs
max4.plot <- function(naaqs,geo,state,out) {
  
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
  
  ## Get current year and historical data based on naaqs input
  if (!exists(paste("hist",std,sep="."))) { assign(paste("hist",std,sep="."),
    pin_read(board,name=paste(Sys.getenv("app_owner"),"/OzoneWatch_hist",std,sep="")),envir=sys.frame(0)) }
  curr.data <- eval(parse(text=paste("curr",std,sep=".")))
  hist.data <- eval(parse(text=paste("hist",std,sep=".")))
  
  ## Get site-level data based on geo input
  if (geo == "AQS Site ID") {
    curr <- curr.data[sapply(curr.data,function(x) x$site == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$site == out)]
    vals <- subset(vals,vals$site == out)
    title1 <- paste("AQS Site ID =",out)
  }
  if (geo == "State/County") {
    curr <- curr.data[sapply(curr.data,function(x) x$state_name == state & x$county_name == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$state_name == state & x$county_name == out)]
    vals <- subset(vals,state_name == state & county_name == out)
    title1 <- paste(out,"County,",state)
  }
  if (geo == "Core Based Statistical Area (CBSA)") {
    curr <- curr.data[sapply(curr.data,function(x) x$cbsa_name == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$cbsa_name == out)]
    vals <- subset(vals,cbsa_name == out)
    title1 <- paste(out,"CBSA")
  }
  if (geo == "Combined Statistical Area (CSA)") {
    curr <- curr.data[sapply(curr.data,function(x) x$csa_name == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$csa_name == out)]
    vals <- subset(vals,csa_name == out)
    title1 <- paste(out,"CSA")
  }
  if (geo == "Nonattainment Area (NAA)") {
    curr <- curr.data[sapply(curr.data,function(x) x$naa_name == out)]
    hist <- hist.data[sapply(hist.data,function(x) x$naa_name == out)]
    vals <- subset(vals,naa_name == out)
    title1 <- paste(out,"Nonattainment Area")
  }
  
  ## Create current year 4th max time series
  if (length(curr) == 0) { 
    dates <- as.character(seq(from=as.Date(paste(curr.year,"01-01",sep="-")),
      to=as.Date(paste(curr.year,"12-31",sep="-")),by=1))
    temp <- data.frame(day=substr(dates,6,10),max4=NA,exc=NA)
    if (nrow(temp) < 366) { temp <- rbind(temp[1:59,],temp[59,],temp[60:nrow(temp),]) }
  }
  if (length(curr) > 0) {
    temp <- ldply(curr,function(x) rbind(x$daily_data))
    temp <- recast(temp,day ~ site + variable,
      id.var=c("site","day"),measure.var=c("max4","exc"))
    temp$max4 <- apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,max.na)
    temp$exc <- apply(as.matrix(temp[,grep("exc",colnames(temp))]),1,sum,na.rm=TRUE)
    temp$exc <- cumsum(diff(c(0,temp$exc)) > 0)
    if (nrow(temp) < 366) { temp <- rbind(temp[1:59,],temp[59,],temp[60:nrow(temp),]) }
    if (this.year) {
      today <- paste(substr(curr.date,6,7),substr(curr.date,9,10),sep="-")
      curr.row <- match(today,temp$day)
      temp$max4[curr.row:nrow(temp)] <- NA
      temp$exc[curr.row:nrow(temp)] <- NA
    }
  }
  curr.vals <- temp[,c("day","max4","exc")]
  
  ## Create historical 4th max time series
  if (length(hist) == 0) { assign("max4plot.vals",curr.vals,envir=sys.frame(0)) }
  if (length(hist) > 0) {
    temp <- ldply(hist,function(x) rbind(x$daily_data))
    temp <- recast(temp,year + day ~ site + variable,
      id.var=c("site","year","day"),measure.var=c("max4","exc"))
    temp$max4 <- apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,max.na)
    temp$exc <- apply(as.matrix(temp[,grep("exc",colnames(temp))]),1,sum,na.rm=TRUE)
    years <- range(as.numeric(temp$year))
    temp <- recast(temp,day ~ year + variable,
      id.var=c("day","year"),measure.var=c("max4","exc"))
    temp[60,which(is.na(temp[60,]))] <- temp[59,which(is.na(temp[60,]))]
    temp[,grep("exc",colnames(temp))] <- apply(as.matrix(temp[,grep("exc",colnames(temp))]),2,
      function(x) cumsum(diff(c(0,x)) > 0))
    temp <- data.frame(day=temp$day,
      min.val=cummax.na(apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,min.na)),
      med.val=cummax.na(apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,med.na)),
      max.val=cummax.na(apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,max.na)),
      avg.exc=round(apply(as.matrix(temp[,grep("exc",colnames(temp))]),1,mean.na),2))
    if (nrow(temp) < 366) { temp <- rbind(temp[1:59,],temp[59,],temp[60:nrow(temp),]) }
    all.vals <- data.frame(temp,max4=curr.vals$max4,exc=curr.vals$exc)
    assign("max4plot.vals",all.vals,envir=sys.frame(0))
  }
  
  ## Generate cumulative 4th max plot
  cval <- min(c(vals[,paste("critical",curr.year,sep="_")],120),na.rm=TRUE)
  ymax <- pmax(100,10*ceiling(max.na(max4plot.vals[,c("max.val","max4")]/10)))
  max.exc <- pmax(20,10*ceiling(1.5*max.na(max4plot.vals[,c("avg.exc","exc")]/10)))
  exc.hist <- max4plot.vals$avg.exc*(ymax/max.exc)
  exc.curr <- max4plot.vals$exc*(ymax/max.exc)
  x.ticks <- c(0,cumsum(table(substr(max4plot.vals$day,1,2))))
  x.midpt <- x.ticks[1:12] + (x.ticks[2:13] - x.ticks[1:12])/2
  y.grid <- seq(10,ymax-10,10); Ny <- length(y.grid);
  lab.hts <- ymax*c(-0.12,-0.09,-0.06)
  par(mar=c(6,4,2,4),mgp=c(2.4,0.8,0),cex.axis=1.5,cex.lab=1.5,cex.main=1.5,xpd=TRUE)
  plot(x=NULL,y=NULL,xaxt='n',xlim=c(0,366),xaxs="i",xlab="",ylim=c(0,ymax),yaxs="i",
    ylab="Annual 4th Highest Daily Maximum Value (ppb)",main=paste(title1,title2,sep=" - "))
  polygon(x=par("usr")[c(1,2,2,1)],y=par("usr")[c(3,3,4,4)],col="#E0E0E0")
  segments(x0=rep(0,Ny),x1=rep(366,Ny),y0=y.grid,y1=y.grid,col="#FFFFFF")
  segments(x0=x.ticks[2:12],x1=x.ticks[2:12],y0=0,y1=ymax,col="#FFFFFF")
  if (length(hist) > 0) {
    polygon(x=c(seq(0.5,nrow(max4plot.vals)-0.5,1),seq(nrow(max4plot.vals)-0.5,0.5,-1)),
      y=c(max4plot.vals$min.val,rev(max4plot.vals$max.val)),border="#80FFFF",col="#80FFFF")
    segments(x0=rep(0,Ny),x1=rep(366,Ny),y0=y.grid,y1=y.grid,col="#FFFFFF")
    segments(x0=x.ticks[2:12],x1=x.ticks[2:12],y0=0,y1=ymax,col="#FFFFFF")
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$min.val,col="#0000FF",lwd=2)
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$med.val,col="#0000FF",lwd=2)
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$max.val,col="#0000FF",lwd=2)
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=exc.hist,col="#FF8C00",lwd=3)
  }
  axis(side=1,at=x.ticks,labels=FALSE,tcl=-0.5)
  mtext(month.abb,side=1,line=0.5,at=x.midpt,las=0,cex=1.5)
  axis(side=4,at=seq(0,ymax,length.out=6),labels=seq(0,max.exc,length.out=6))
  mtext("Number of Ozone Exceedance Days",side=4,line=2.5,at=ymax/2,las=0,cex=1.5)
  segments(x0=0,x1=366,y0=cval,y1=cval,lty=3,lwd=2,col="#FF5555")
  segments(x0=0,x1=366,y0=lvl,y1=lvl,lty=2,lwd=2,col="#000000")
  if (length(curr) > 0) {
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$max4,col="#00AA00",lwd=3)
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=exc.curr,col="#FF00FF",lwd=3)
  }
  box()
  segments(x0=rep(0,3),x1=rep(20,3),y0=lab.hts,y1=lab.hts,
    lty=c(2,1,3),lwd=c(2,3,2),col=c("#000000","#00AA00","#FF5555"))
  text(labels=c(paste(std,"NAAQS Level"),paste(curr.year,c("Observed 4th High",
    "Critical Value"))),x=rep(20,3),y=lab.hts,pos=4,cex=1.25)
  segments(x0=220,x1=240,y0=lab.hts[3],y1=lab.hts[3],col="#FF00FF",lwd=3)
  text(labels=paste(curr.year,"Observed Exceedance Days"),x=240,y=lab.hts[3],pos=4,cex=1.25)
  if (length(hist) > 0) {
    polygon(x=c(100,120,120,100),y=lab.hts[c(1,1,3,3)],border="#80FFFF",col="#80FFFF")
    segments(x0=rep(100,3),x1=rep(120,3),y0=lab.hts,y1=lab.hts,col=rep("#0000FF",3),lwd=2)
    text(labels=paste(years[1],"-",years[2],c("Maximum","Median","Minimum"),"4th High"),
      x=rep(120,3),y=lab.hts,pos=4,cex=1.25)
    segments(x0=220,x1=240,y0=lab.hts[2],y1=lab.hts[2],col="#FF8C00",lwd=3)
    text(labels=paste(years[1],"-",years[2],"Average Exceedance Days"),x=240,y=lab.hts[2],
      pos=4,cex=1.25)
  }
}
