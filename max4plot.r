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
    vals <- subset(vals,substr(vals$site,1,9) == out)
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
    dates <- seq(from=as.Date(paste(curr.year,"01-01",sep="-")),
      to=as.Date(paste(curr.year,"12-31",sep="-")),by=1)
    temp <- data.frame(day=substr(dates,6,10),max4=NA)
  }
  if (length(curr) > 0) {
    temp <- ldply(curr,function(x) rbind(x$daily_data))
    temp <- recast(temp,day ~ site + variable,
      id.var=c("site","day"),measure.var="max4")
    temp$max4 <- apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,max.na)
    if (nrow(temp) < 366) { temp <- rbind(temp[1:59,],temp[59,],temp[60:nrow(temp),]) }
    if (this.year) {
      today <- paste(substr(curr.date,6,7),substr(curr.date,9,10),sep="-")
      curr.row <- match(today,temp$day)
      temp$max4[curr.row:nrow(temp)] <- NA
    }
  }
  curr.vals <- temp

  ## Create historical 4th max time series
  if (length(hist) == 0) { assign("max4plot.vals",curr.vals,envir=sys.frame(0)) }
  if (length(hist) > 0) {
    temp <- ldply(hist,function(x) rbind(x$daily_data))
    temp <- recast(temp,year + day ~ site + variable,
      id.var=c("site","year","day"),measure.var="max4")
    temp$max4 <- apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,max.na)
    years <- range(as.numeric(temp$year))
    temp <- recast(temp,day ~ year + variable,
      id.var=c("day","year"),measure.var="max4")
    temp <- data.frame(day=temp$day,
      min.val=cummax.na(apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,min.na)),
      med.val=cummax.na(apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,med.na)),
      max.val=cummax.na(apply(as.matrix(temp[,grep("max4",colnames(temp))]),1,max.na)))
    if (nrow(temp) < 366) { temp <- rbind(temp[1:59,],temp[59,],temp[60:nrow(temp),]) }
    all.vals <- data.frame(temp,max4=curr.vals$max4)
    assign("max4plot.vals",all.vals,envir=sys.frame(0))
  }
  
  ## Generate cumulative 4th max plot
  cval <- min(c(vals[,paste("critical",curr.year,sep="_")],120),na.rm=TRUE)
  x.ticks <- c(0,cumsum(table(substr(max4plot.vals$day,1,2))))
  x.midpt <- x.ticks[1:12] + (x.ticks[2:13] - x.ticks[1:12])/2
  lab.ht <- 2.5+c(10,5,0)
  par(mar=c(2,4,2,0.5),mgp=c(2.4,0.8,0),cex.axis=1.5,cex.lab=1.5,cex.main=1.4)
  plot(x=NULL,y=NULL,xaxt='n',xlim=c(0,366),xaxs="i",xlab="",ylim=c(0,120),yaxs="i",
    ylab="Annual 4th Highest Daily Maximum Value (ppb)",main=paste(title1,title2,sep=" - "))
  polygon(x=par("usr")[c(1,2,2,1)],y=par("usr")[c(3,3,4,4)],col="#E0E0E0")
  abline(h=seq(0,120,10),v=x.ticks,col="#FFFFFF")
  if (length(hist) > 0) {
    polygon(x=c(seq(0.5,nrow(max4plot.vals)-0.5,1),seq(nrow(max4plot.vals)-0.5,0.5,-1)),
      y=c(max4plot.vals$min.val,rev(max4plot.vals$max.val)),border="#80FFFF",col="#80FFFF")
    polygon(x=c(335,360,360,335),y=lab.ht[c(3,3,1,1)],border="#80FFFF",col="#80FFFF")
    abline(h=seq(0,120,10),v=x.ticks,col="#FFFFFF")
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$min.val,col="#0000FF",lwd=2)
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$med.val,col="#0000FF",lwd=2)
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$max.val,col="#0000FF",lwd=2)
    segments(x0=rep(335,3),x1=rep(360,3),y0=lab.ht,y1=lab.ht,col=rep("#0000FF",3),lwd=2)
    text(labels=paste(years[1],"-",years[2],c("Maximum","Median","Minimum")),
      x=335,y=lab.ht,pos=2,cex=1.5)
  }
  axis(side=1,at=x.ticks,labels=FALSE,tcl=-0.5)
  mtext(month.abb,side=1,line=0.5,at=x.midpt,las=0,cex=1.5)
  abline(h=cval,lty=1,lwd=2,col="#FF5555")
  abline(h=lvl,lty=2,lwd=2,col="#000000")
  if (length(curr) > 0) {
    lines(x=seq(0.5,nrow(max4plot.vals)-0.5,1),y=max4plot.vals$max4,col="#00AA00",lwd=2)
    segments(x0=rep(215,3),x1=rep(240,3),y0=lab.ht,y1=lab.ht,
      lty=c(2,1,1),lwd=2,col=c("#000000","#00AA00","#FF5555"))
    text(labels=c("NAAQS Level",paste(curr.year,c("Observed","Critical Value"))),
      x=215,y=lab.ht,pos=2,cex=1.5)
  }
  box()
}
