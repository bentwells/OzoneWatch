## Function to display design value tables based on inputs
dv.tables <- function(naaqs,type,geo,region,state,out,year,maxes) {
  
  ## Exit if any required inputs are missing
  if (is.null(naaqs)) { return() }
  if (is.null(geo)) { return() }
  if (is.null(region)) { return() }
  if (is.null(state)) { return() }
  if (is.null(year)) { return() }
  if (type == "Area-level Design Values") {
    if (is.null(maxes)) { return() }
  }
  if (type == "Site-level Design Values") {
    if (is.null(out)) { return() }
    if (out == "Please select a State first!") { return() }
  }
  
  ## Set constants based on naaqs, geo, and year inputs
  std <- substr(naaqs,1,4)
  lvl <- ifelse(std == "2015",70,ifelse(std == "2008",75,84))
  dv.year <- as.numeric(year); years <- c((dv.year-2):dv.year);
  dv.col <- paste("dv",(dv.year-2),dv.year,sep="_")
  area.type <- switch(substr(geo,1,3),Sta="county",Cor="cbsa",Com="csa",Non="naa")
  name.col <- paste(area.type,"name",sep="_")
  
  ## Create DV tables based on current year data
  if (dv.year == curr.year) { 
    vals <- eval(parse(text=paste("sitedv",std,sep=".")))
    vals <- vals[which(vals[,name.col] != " "),]
    if (region != "National" & state == "All States") {
      region.num <- substr(region,12,13)
      if (area.type == "county") { vals <- subset(vals,epa_region == region.num) }
      if (area.type != "county") { 
        area.list <- unique(vals[which(vals$epa_region == region.num),name.col])
        vals <- vals[which(vals[,name.col] %in% area.list),]
      }
    }
    if (state != "All States") {
      if (area.type == "county") { vals <- subset(vals,state_name == state) }
      if (area.type != "county") { 
        area.list <- unique(vals[which(vals$state_name == state),name.col])
        vals <- vals[which(vals[,name.col] %in% area.list),]
      }
    }
    if (type == "Site-level Design Values") { if (out != "All Sites") {
      vals <- vals[which(vals[,name.col] == out),] }}
  }
  
  ## Create DV tables based on historical data
  if (dv.year < curr.year) { 
    
    ## Read historical data from pin based on naaqs input, if needed
    if (!exists(paste("hist",std,sep="."))) { assign(paste("hist",std,sep="."),
      pin_read(board,name=paste(Sys.getenv("app_owner"),"/OzoneWatch_hist",std,sep="")),envir=sys.frame(0)) }
    hist.data <- eval(parse(text=paste("hist",std,sep=".")))
    
    ## Subset data based on inputs
    hist <- hist.data[sapply(hist.data,function(x) x[name.col] != " ")]
    if (region != "National" & state == "All States") {
      region.num <- substr(region,12,13)
      if (area.type == "county") { 
        hist <- hist[sapply(hist,function(x) x$epa_region == region.num)] }
      if (area.type != "county") {
        temp <- hist[sapply(hist,function(x) x$epa_region == region.num)]
        area.list <- unique(sapply(temp,function(x) x[name.col]))
        hist <- hist[sapply(hist,function(x) x[name.col] %in% area.list)]
      }
    }
    if (state != "All States") {
      if (area.type == "county") {
        hist <- hist[sapply(hist,function(x) x$state_name == state)] }
      if (area.type != "county") {
        temp <- hist[sapply(hist,function(x) x$state_name == state)]
        area.list <- unique(sapply(temp,function(x) x[name.col]))
        hist <- hist[sapply(hist,function(x) x[name.col] %in% area.list)]
      }
    }
    if (type == "Site-level Design Values") { if (out != "All Sites" & length(hist) > 0) {
      hist <- hist[sapply(hist,function(x) x[name.col] == out)]
    }}
    
    ## If no sites remain, create an empty data.frame
    if (length(hist) == 0) { 
      vals <- subset(eval(parse(text=paste("sitedv",std,sep="."))),1 == 0)
      colnames(vals) <- gsub((curr.year-2),years[1],colnames(vals))
      colnames(vals) <- gsub((curr.year-1),years[2],colnames(vals))
      colnames(vals) <- gsub(curr.year,years[3],colnames(vals))
      colnames(vals) <- gsub((curr.year+1),dv.year+1,colnames(vals))
    }
    
    ## Get design values for all remaining sites based on year input
    if (length(hist) > 0) {
      dvs <- lapply(hist,function(x) x$dv_data[which(x$dv_data$year %in% years),])
      vals <- data.frame(site=unlist(lapply(hist,function(x) x$site)),
        site_name=unlist(lapply(hist,function(x) x$site_name)),
        address=unlist(lapply(hist,function(x) x$address)),
        latitude=unlist(lapply(hist,function(x) x$latitude)),
        longitude=unlist(lapply(hist,function(x) x$longitude)),
        epa_region=unlist(lapply(hist,function(x) x$epa_region)),
        state_name=unlist(lapply(hist,function(x) x$state_name)),
        county_name=unlist(lapply(hist,function(x) x$county_name)),
        cbsa_name=unlist(lapply(hist,function(x) x$cbsa_name)),
        csa_name=unlist(lapply(hist,function(x) x$csa_name)),
        naa_name=unlist(lapply(hist,function(x) x$naa_name)))
      vals[,paste("dv",years[1],years[3],sep="_")] <- unlist(lapply(dvs,function(x) x$dv[3]))
      vals$status <- unlist(lapply(dvs,function(x) x$status[3]))
      vals[,paste("pct",years[1],years[3],sep="_")] <- unlist(lapply(dvs,function(x) x$pct3yr[3]))
      vals[,paste("pct",years[1],sep="_")] <- unlist(lapply(dvs,function(x) x$pct[1]))
      vals[,paste("pct",years[2],sep="_")] <- unlist(lapply(dvs,function(x) x$pct[2]))
      vals[,paste("pct",years[3],sep="_")] <- unlist(lapply(dvs,function(x) x$pct[3]))
      vals[,paste("exc",years[1],sep="_")] <- unlist(lapply(dvs,function(x) x$exc[1]))
      vals[,paste("exc",years[2],sep="_")] <- unlist(lapply(dvs,function(x) x$exc[2]))
      vals[,paste("exc",years[3],sep="_")] <- unlist(lapply(dvs,function(x) x$exc[3]))
      vals[,paste("critical",years[3],sep="_")] <- unlist(lapply(dvs,function(x) x$crit1[3]))
      vals[,paste("critical",dv.year+1,sep="_")] <- unlist(lapply(dvs,function(x) x$crit2[3]))
      for (y in years) { for (i in 1:5) {
        vals[,paste("max",i,"_",y,sep="")] <- unlist(lapply(dvs,
          function(x) x[(y-dv.year+3),paste("max",i,sep="")])) }}
      for (y in years) { for (i in 1:5) {
        vals[,paste("date",i,"_",y,sep="")] <- unlist(lapply(dvs,
          function(x) x[(y-dv.year+3),paste("date",i,sep="")])) }}
      vals <- vals[which(!is.na(vals[,paste("pct",years[1],years[3],sep="_")])),]
    }
  }
  
  ## Generate Area-level Design Value table
  if (type == "Area-level Design Values") {
    
    ## Generate additional columns and set column names
    vals$meets_naaqs <- sapply(vals[,dv.col],function(x) ifelse(x > lvl,"No","Yes"))
    val.cols <- c(dv.col,"meets_naaqs",paste("max4",years[1:3],sep="_"),
      paste("critical",years[2:3]+1,sep="_"))
    cnames <- c(ifelse(dv.year == curr.year,
      paste("Preliminary",years[1],"-",years[3],"Design Value (ppb)"),
      paste(years[1],"-",years[3],"Design Value (ppb)")),
      paste("Meets ",std," NAAQS? (",lvl," ppb)",sep=""),
      paste(years[1:3],"4th Max Value (ppb)"),
      paste(years[2:3]+1,"Critical Value (ppb)"))
    cval <- paste("critical",years[3]+1,sep="_")
    
    ## County-level design values
    if (area.type == "county") {
      vals <- vals[order(vals$state_name,vals$county_name,vals[,dv.col],decreasing=TRUE),
        c("state_name","county_name","cbsa_name",val.cols)]
      critical <- vals[,c("state_name","county_name",cval)]
      if (maxes == "Site with Highest Design Value") {
        vals <- vals[!duplicated(vals[,c("state_name","county_name")]),c(1:(ncol(vals)-1))]
        table.out <- vals[order(vals$state_name,vals$county_name),]
      }
      if (maxes == "Site with Highest Annual Value") {
        temp <- vals[!duplicated(vals[,c("state_name","county_name")]),1:5]
        table.out <- temp[order(temp$state_name,temp$county_name),]
        for (y in years) {
          table.out[,paste("max4",y,sep="_")] <- c(tapply(vals[,paste("max4",y,sep="_")],
            list(paste(vals$state_name,vals$county_name)),max.na))
        }
        table.out[,paste("critical",years[3],sep="_")] <- c(tapply(vals[,paste("critical",
          years[3],sep="_")],list(paste(vals$state_name,vals$county_name)),min.na))
      }
      table.out[,cval] <- c(tapply(critical[,cval],
        list(paste(critical$state_name,critical$county_name)),min.na))
      colnames(table.out) <- c("State","County","Core Based Statistical Area (CBSA)",cnames)
    }
    
    ## CBSA, CSA, or NAA-level design values
    if (area.type != "county") {
      vals <- vals[order(vals[,name.col],vals[,dv.col],decreasing=TRUE),c(name.col,val.cols)]
      critical <- vals[,c(name.col,cval)]
      if (maxes == "Site with Highest Design Value") {
        vals <- vals[!duplicated(vals[,name.col]),c(1:(ncol(vals)-1))]
        table.out <- vals[order(vals[,name.col]),]
      }
      if (maxes == "Site with Highest Annual Value") {
        temp <- vals[!duplicated(vals[,name.col]),1:3]
        table.out <- temp[order(temp[,name.col]),]
        for (y in years) {
          table.out[,paste("max4",y,sep="_")] <- c(tapply(vals[,paste("max4",y,sep="_")],
            list(vals[,name.col]),max.na))
        }
        table.out[,paste("critical",years[3],sep="_")] <- c(tapply(vals[,paste("critical",
          years[3],sep="_")],list(vals[,name.col]),min.na))
      } 
      table.out[,cval] <- c(tapply(critical[,cval],list(critical[,name.col]),min.na))
      colnames(table.out) <- c(geo,cnames)
      if (area.type == "naa") { colnames(table.out)[1] <- paste(std,"NAAQS Nonattainment Area") }
    }
  }
  
  ## Generate Site-level Design Value table
  if (type == "Site-level Design Values") {
    if (out == "All Sites") { 
      table.out <- switch(area.type,county=vals[order(vals$site),],
        vals[order(vals[,name.col],vals$site),])
    }
    if (out != "All Sites") { table.out <- vals[order(vals$site),] }
    colnames(table.out) <- c("AQS Site ID","Site Name","Address","Latitude","Longitude",
      "EPA Region","State Name","County Name","CBSA Name","CSA Name","NAA Name",
      paste(ifelse(dv.year == curr.year,"Preliminary",""),years[1],"-",years[3],"Design Value (ppb)"),
      "Status (A=Attaining, V=Violating, I=Incomplete)",
      paste(years[1],"-",years[3],"Average Data Completeness (%)"),
      paste(years[1:3],"Data Completeness (%)"),
      paste(years[1:3],"Exceedance Days"),
      paste(years[2:3]+1,"Critical Value (ppb)"),
      paste(years[1],c("1st","2nd","3rd","4th","5th"),"Max Value (ppb)"),
      paste(years[2],c("1st","2nd","3rd","4th","5th"),"Max Value (ppb)"),
      paste(years[3],c("1st","2nd","3rd","4th","5th"),"Max Value (ppb)"),
      paste(years[1],c("1st","2nd","3rd","4th","5th"),"Max Date"),
      paste(years[2],c("1st","2nd","3rd","4th","5th"),"Max Date"),
      paste(years[3],c("1st","2nd","3rd","4th","5th"),"Max Date"))
  }
  
  ## Return DV table
  return(table.out)
}
