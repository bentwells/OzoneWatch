## Function to display maps of design values based on inputs
area.maps <- function(naaqs,type,geo,region,state,out,year,value) {
  
  ## Exit if any required inputs are missing
  if (is.null(naaqs)) { return() }
  if (is.null(geo)) { return() }
  if (is.null(region)) { return() }
  if (is.null(state)) { return() }
  if (is.null(year)) { return() }
  if (is.null(value)) { return() }
  if (type == "Site-level Design Values") {
    if (is.null(out)) { return() }
    if (out == "Please select a State first!") { return() }
  }
  
  ## Get site-level DVs and set constants based on naaqs input
  std <- substr(naaqs,1,4)
  lvl <- ifelse(std == "2015",70,ifelse(std == "2008",75,84))
  dv.year <- as.numeric(year); years <- c((dv.year-2):dv.year);
  val.cols <- c(paste("dv",years[1],years[3],sep="_"),paste("max4",years,sep="_"))
  
  ## Set constants based on type input
  dv.type <- tolower(substr(type,1,4))
  local.map <- ifelse(dv.type == "area",FALSE,ifelse(out == "All Sites",FALSE,TRUE))
  
  ## Set constants based on geo input
  area.type <- switch(substr(geo,1,3),Sta="county",Cor="cbsa",Com="csa",Non="naa")
  area.title <- ifelse(area.type == "county","County",toupper(area.type))
  name.col <- paste(area.type,"name",sep="_")
  
  ## Set constants based on region and state inputs
  national.map <- ifelse(local.map,FALSE,ifelse(region == "National" & state == "All States",TRUE,FALSE))
  hi.res <- ifelse(local.map,TRUE,ifelse(state %in% c("All States","Alaska"),FALSE,TRUE))
  if (!hi.res & !exists("map.data.20m")) { load("data/map_data_20m.Rdata",envir=sys.frame(0)) }
  if (hi.res & !exists("map.data.500k")) { load("data/map_data_500k.Rdata",envir=sys.frame(0)) }
  map.data <- switch(tolower(hi.res),true=map.data.500k,false=map.data.20m)
  
  ## Get nonattainment area information, if applicable
  if (area.type == "naa") {
    if (!exists(paste("naa.shp",std,sep="."))) {
      load(paste("data/O3_naa_",std,".Rdata",sep=""),envir=sys.frame(0))
    }
    assign("naa.dbf",eval(parse(text=paste("naa.dbf",std,sep="."))))
    assign("naa.shp",eval(parse(text=paste("naa.shp",std,sep="."))))
  }
  
  ## Get list of states to draw based on region and state inputs, for non-local maps
  map.states <- draw.map("state",hires=hi.res,plot=FALSE)
  temp <- eval(parse(text=paste("sitedv",std,sep=".")))
  if (!local.map) {
    if (region != "National" & state == "All States") {
      map.states$epa_region <- temp$epa_region[which(!duplicated(temp$state_name))]
      map.states <- subset(map.states,epa_region == substr(region,12,13))
      if (region == "EPA Region 2") { map.states <- subset(map.states,name != "Puerto Rico") }
      if (region == "EPA Region 9") { map.states <- subset(map.states,name != "Hawaii") }
      if (region == "EPA Region 10") { map.states <- subset(map.states,name != "Alaska") }
    }
    if (state != "All States") { map.states <- subset(map.states,name == state) }
  }
  
  ## Get site-level design values based on current year data
  if (dv.year == curr.year) {
    vals <- temp[,c("site","site_name","address","latitude","longitude",
      "epa_region","state_name",name.col,val.cols)]
  }
  
  ## Get site-level design values based on historical data
  if (dv.year < curr.year) {
    
    ## Load historical data from file based on naaqs input
    hist.file <- paste("data",rev(files[grep(paste("hist",std,sep="_"),files)])[1],sep="/")
    if (!exists(paste("hist",std,sep="."))) { load(hist.file,envir=sys.frame(0)) }
    hist.data <- eval(parse(text=paste("hist",std,sep=".")))
    
    ## Get design values for all sites based on year input
    dvs <- lapply(hist.data,function(x) x$dv_data[which(x$dv_data$year %in% years),])
    vals <- data.frame(site=unlist(lapply(hist.data,function(x) x$site)),
      site_name=unlist(lapply(hist.data,function(x) x$site_name)),
      address=unlist(lapply(hist.data,function(x) x$address)),
      latitude=unlist(lapply(hist.data,function(x) x$latitude)),
      longitude=unlist(lapply(hist.data,function(x) x$longitude)),
      epa_region=unlist(lapply(hist.data,function(x) x$epa_region)),
      state_name=unlist(lapply(hist.data,function(x) x$state_name)))
    if (area.type == "county") { 
      vals$county_name <- unlist(lapply(hist.data,function(x) x$county_name)) }
    if (area.type == "cbsa") { 
      vals$cbsa_name <- unlist(lapply(hist.data,function(x) x$cbsa_name)) }
    if (area.type == "csa") { 
      vals$csa_name <- unlist(lapply(hist.data,function(x) x$csa_name)) }
    if (area.type == "naa") { 
      vals$naa_name <- unlist(lapply(hist.data,function(x) x$naa_name)) }
    vals[,val.cols[1]] <- unlist(lapply(dvs,function(x) x$dv[3]))
    vals[,val.cols[2]] <- unlist(lapply(dvs,function(x) x$max4[1]))
    vals[,val.cols[3]] <- unlist(lapply(dvs,function(x) x$max4[2]))
    vals[,val.cols[4]] <- unlist(lapply(dvs,function(x) x$max4[3]))
    if (grepl("dv",value)) {
      dv.status <- unlist(lapply(dvs,function(x) x$status[3]))
      vals[,val.cols[1]] <- mapply(function(dv,status) ifelse(status == "I",NA,dv),
        dv=vals[,val.cols[1]],status=dv.status)
    }
    pct3yr <- unlist(lapply(dvs,function(x) x$pct3yr[3]))
    vals <- vals[which(!is.na(pct3yr)),]
  }
  
  ## Get design values and polygons for area-level maps
  if (dv.type == "area") {
    if (area.type == "county") {
      vals <- subset(vals,state_name %in% map.states$name)
      vals$county_fips <- substr(vals$site,1,5)
      vals <- vals[order(vals$county_fips,vals[,value],decreasing=TRUE),]
      vals <- vals[!duplicated(vals[,c("county_fips")]),]
      vals <- vals[order(vals$county_fips),]
      area.info <- map.data$county.info[match(vals$county_fips,map.data$county.info$fips),]
      area.shape <- subset(map.data$county.shape,fips %in% area.info$fips)
    }
    if (area.type != "county") {
      temp <- subset(vals[which(vals[,name.col] != " "),],state_name %in% map.states$name)
      vals <- vals[which(vals[,name.col] %in% unique(temp[,name.col])),]
      vals <- vals[order(vals[,name.col],vals[,value],decreasing=TRUE),]
      vals <- vals[!duplicated(vals[,name.col]),]
      vals <- vals[order(vals[,name.col]),]
      if (area.type == "cbsa") {
        area.info <- map.data$cbsa.info[match(vals$cbsa_name,map.data$cbsa.info$name),]
        area.shape <- subset(map.data$cbsa.shape,fips %in% area.info$fips)
      }
      if (area.type == "csa") {
        area.info <- map.data$csa.info[match(vals$csa_name,map.data$csa.info$name),]
        area.shape <- subset(map.data$csa.shape,fips %in% area.info$fips)
      }
      if (area.type == "naa") {
        area.info <- naa.dbf[match(vals$naa_name,naa.dbf$name),]
        area.shape <- subset(naa.shp,fips %in% area.info$fips)
      }
    }
    assign("map.shape",st_sf(vals[order(area.info$fips),],
      geometry=lapply(split(area.shape[,c("id","lon","lat")],area.shape$fips),function(poly) 
      st_multipolygon(lapply(split(poly[,c("lon","lat")],poly$id),function(x) 
      st_polygon(list(as.matrix(x)))))),crs=4326),envir=sys.frame(0))
  }
  
  ## Get design values and polygons for non-local site-level maps
  if (dv.type == "site" & !local.map) {
    vals <- subset(vals[order(vals[,value],na.last=FALSE),],state_name %in% map.states$name)
    if (area.type == "county") {
      area.info <- subset(map.data$county.info,code %in% map.states$code)
      area.shape <- subset(map.data$county.shape,fips %in% area.info$fips)
    }
    if (area.type == "cbsa") {
      area.info <- map.data$cbsa.info[which(lapply(
        sapply(map.data$cbsa.info$code,strsplit,split="-",USE.NAMES=FALSE),
        function(x) sum(!is.na(match(x,map.states$code)))) > 0),]
      area.shape <- subset(map.data$cbsa.shape,fips %in% area.info$fips)
    }
    if (area.type == "csa") {
      area.info <- map.data$csa.info[which(lapply(
        sapply(map.data$csa.info$code,strsplit,split="-",USE.NAMES=FALSE),
        function(x) sum(!is.na(match(x,map.states$code)))) > 0),]
      area.shape <- subset(map.data$csa.shape,fips %in% area.info$fips)
    }
    if (area.type == "naa") {
      area.info <- naa.dbf[match(vals$naa_name,naa.dbf$name),]
      area.shape <- subset(naa.shp,fips %in% area.info$fips)
    }
  }
  
  ## Get polygons for local site-level maps
  if (local.map) {
    if (area.type == "county") { 
      temp <- subset(vals,state_name == state & county_name == out)[1,]
      area.info <- subset(map.data$county.info,fips == substr(temp$site,1,5))
      area.shape <- subset(map.data$county.shape,fips == area.info$fips)
    }
    if (area.type == "cbsa") {
      area.info <- subset(map.data$cbsa.info,name == out)
      area.shape <- subset(map.data$cbsa.shape,fips == area.info$fips)
    }
    if (area.type == "csa") {
      area.info <- subset(map.data$csa.info,name == out)
      area.shape <- subset(map.data$csa.shape,fips == area.info$fips)
    }
    if (area.type == "naa") {
      area.info <- subset(naa.dbf,name == out)
      area.shape <- subset(naa.shp,fips == area.info$fips)
    }
  }
  
  ## Get plotting area boundaries
  if (national.map) {
    north <- 49.5; south <- 24.5; east <- -66.7; west <- -125;
  }
  if (!national.map & !local.map) {
    state.shape <- subset(map.data$state.shape,fips %in% map.states$fips)
    north <- max(c(state.shape$lat,area.shape$lat))+0.1
    south <- min(c(state.shape$lat,area.shape$lat))-0.1
    east <- max(c(state.shape$lon,area.shape$lon))+0.1
    west <- min(c(state.shape$lon,area.shape$lon))-0.1
  }
  if (local.map) {
    north <- max(area.shape$lat)+0.5; south <- min(area.shape$lat)-0.5;
    east <- max(area.shape$lon)+0.5; west <- min(area.shape$lon)-0.5;
  }
  
  ## Add buffers to plot boundaries to maintain aspect ratio
  if (!national.map) {
    ns.diff <- north - south; ew.diff <- east - west;
    add <- abs(ns.diff - 2/3*ew.diff)/2
    if (ns.diff > 2/3*ew.diff) { east <- east + add; west <- west - add; }
    if (ns.diff < 2/3*ew.diff) { north <- north + add; south <- south - add; }
  }
  
  ## Get list of states/counties to draw and site-level DVs for local maps
  if (local.map) {
    map.states <- unique(subset(map.data$state.shape,
      lat < north & lat > south & lon < east & lon > west)$fips)
    map.counties <- unique(subset(map.data$county.shape,
      lat < north & lat > south & lon < east & lon > west)$fips)
    vals <- subset(vals[order(vals[,value],na.last=FALSE),],
      latitude < north & latitude > south & longitude < east & longitude > west)
  }
  assign("map.vals",vals,envir=sys.frame(0))
  
  ## Assign colors to each area based on DVs
  color.range <- c(lvl-32.5,lvl+33.5)
  color.vals <- c(rgb(rep(0,33),seq(0,1,1/32),rep(1,33)),
                  rgb(rep(1,33),seq(1,0,-1/32),rep(0,33)))
  map.colors <- assign.colors(pmin(pmax(vals[,value],color.range[1]),
    color.range[2]),range=color.range,palette=color.vals,
    na.col=ifelse(dv.type == "area","#C0C0C0","#FFFFFF"))
  
  ## Create legend label and figure title
  value.lab <- ifelse(substr(value,1,2) == "dv",ifelse(dv.year == curr.year,
    paste("Preliminary",years[1],"-",years[3],"Design Value"),
    paste(years[1],"-",years[3],"Design Value")),
    paste(substr(value,6,9),"4th Highest Daily Maximum Value"))
  legend.lab <- ifelse(grepl(curr.year,value),paste(value.lab,"(ppb) as of",
    word.date),paste(value.lab,"(ppb)"))
  title1 <- paste(value.lab,"s for the ",std," Ozone NAAQS (",lvl," ppb)",sep="")
  title2 <- ifelse(dv.type == "area",paste("by",area.title),ifelse(local.map,
    paste("in",out,ifelse(area.type == "county",paste("County,",state),"")),
    ifelse(national.map,"",paste("with",area.title,"Boundaries"))))
  title3 <- ifelse(region == "National",
    ifelse(state == "All States","",paste("in",state)),
    ifelse(state == "All States",paste("in",region),paste("in",state)))
  title <- ifelse(national.map,ifelse(dv.type == "area",paste(title1,title2),title1),
    ifelse(local.map,paste(title1,title2),ifelse(dv.type == "area",
    paste(title1,title2,title3),paste(title1,title3,title2))))
  cex.title <- ifelse(nchar(title) <= 100,1.5,ifelse(nchar(title) <= 120,1.25,1.1))
  
  ## Create design value maps
  par(mar=c(5.5,0.5,2,0.5),cex.axis=1.5,cex.lab=1.5,cex.main=cex.title)
  plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs="i",xlab="",xlim=c(west,east),
    yaxs="i",ylab="",ylim=c(south,north),main=title)
  if (national.map) {
    if (dv.type == "area") {
      for (i in 1:nrow(area.info)) {
        t <- subset(area.shape,fips == area.info$fips[i])
        add.layer(type="polygon",x=t$lon,y=t$lat,id=t$id,col=map.colors[i])
      }
      draw.map("state",map.states$fips,id="fips",add=TRUE,lwd=2)
    }
    if (dv.type == "site") {
      draw.map("state",add=TRUE,lwd=2)
      add.layer(type="points",x=vals$longitude,y=vals$latitude,
        pch=21,bg=map.colors,cex=2)
    }
  }
  if (!national.map & !local.map) {
    if (dv.type == "area") {
      draw.map("county",map.states$code,id="code",add=TRUE,
        hires=hi.res,rescale=FALSE)
      if (nrow(area.info) > 0) {
        for (i in 1:nrow(area.info)) {
          t <- subset(area.shape,fips == area.info$fips[i])
          add.layer(type="polygon",x=t$lon,y=t$lat,id=t$id,
            rescale=FALSE,col=map.colors[i])
        }
      }
      draw.map("state",map.states$fips,id="fips",add=TRUE,
        hires=hi.res,rescale=FALSE,lwd=2)
    }
    if (dv.type == "site") {
      if (nrow(area.info) > 0) {
        for (i in 1:nrow(area.info)) {
          t <- subset(area.shape,fips == area.info$fips[i])
          add.layer(type="polygon",x=t$lon,y=t$lat,id=t$id,
            rescale=FALSE,border="#000000",col="#F0F0F0")
        }
      }
      draw.map("state",map.states$fips,id="fips",add=TRUE,
        hires=hi.res,rescale=FALSE,lwd=2)
      add.layer(type="points",x=vals$longitude,y=vals$latitude,
        rescale=FALSE,pch=21,bg=map.colors,cex=2)
    }
  }
  if (local.map) {
    add.layer(type="polygon",x=area.shape$lon,y=area.shape$lat,
      id=area.shape$id,rescale=FALSE,col="#F0F0F0",lwd=2)
    if (length(map.states) > 0) {
      draw.map("state",map.states,add=TRUE,hires=TRUE,rescale=FALSE,lwd=2)
    }
    if (length(map.counties) > 0) {
      draw.map("county",map.counties,add=TRUE,hires=TRUE,rescale=FALSE)
    }
    add.layer(type="polygon",x=area.shape$lon,y=area.shape$lat,
      id=area.shape$id,rescale=FALSE,border="#0000FF",lwd=2)
    add.layer(type="points",x=vals$longitude,y=vals$latitude,
      rescale=FALSE,pch=21,bg=map.colors,cex=2)
  }
  image.plot(zlim=color.range,col=color.vals,add=TRUE,legend.only=TRUE,horizontal=TRUE,
    axis.args=list(mgp=c(2.5,1,0)),legend.cex=1.5,legend.lab=legend.lab,legend.line=2.5,
    legend.mar=3.75,legend.width=1.5,legend.shrink=1)
}
