## Function to display design value tables based on inputs
dv.tables <- function(naaqs,type,geo,region,state,out) {
  
  ## Exit if any required inputs are missing
  if (is.null(naaqs)) { return() }
  if (is.null(geo)) { return() }
  if (is.null(region)) { return() }
  if (is.null(state)) { return() }
  if (type == "Site-level Design Values") {
    if (is.null(out)) { return() }
    if (out == "Please select a State first!") { return() }
  }
  
  ## Get site-level DVs and set constants based on naaqs input
  std <- substr(naaqs,1,4)
  lvl <- ifelse(std == "2015",70,ifelse(std == "2008",75,84))
  vals <- eval(parse(text=paste("sitedv",std,sep=".")))
  dv.col <- colnames(vals)[grep("dv",colnames(vals))]
  years <- as.numeric(substr(colnames(vals)[grep("max4",colnames(vals))],6,9))
  
  ## Set variables based on geo input
  area.type <- switch(substr(geo,1,3),Sta="county",Cor="cbsa",Com="csa",Non="naa")
  if (std == "2015" & area.type == "naa") { return() }
  name.col <- paste(area.type,"name",sep="_")
  fix.areas <- ifelse(type == "Area-level Design Values",ifelse(area.type == "county",
    FALSE,TRUE),ifelse(out == "All Sites",FALSE,ifelse(area.type == "county",FALSE,TRUE)))
  
  ## Subset data based on region and state inputs  
  temp <- vals[which(vals[,name.col] != " "),]
  if (region != "National") { temp <- subset(temp,epa_region == substr(region,12,13)) }
  if (state != "All States") { temp <- subset(temp,state_name == state) }
  vals <- switch(tolower(fix.areas),true=vals[which(vals[,name.col] %in%
    unique(temp[,name.col])),],false=temp)
  
  ## Get Area-level Design Values
  if (type == "Area-level Design Values") {
    
    ## Generate additional columns and set column names for area-level tables
    vals$meets_naaqs <- sapply(vals[,dv.col],function(x) ifelse(x > lvl,"No","Yes"))
    vals[,paste("critical",years[3],sep="_")] <- 3*(lvl+1) - 
      vals[,paste("max4",years[2],sep="_")] - vals[,paste("max4",years[1],sep="_")]
    vals[,paste("critical",years[3]+1,sep="_")] <- 3*(lvl+1) - 
      vals[,paste("max4",years[3],sep="_")] - vals[,paste("max4",years[2],sep="_")]
    val.cols <- c(dv.col,"meets_naaqs",paste("max4",years[1:3],sep="_"),
      paste("critical",years[2:3]+1,sep="_"))
    cnames <- c(paste("Preliminary",years[1],"-",years[3],"Design Value (ppb)"),
      paste("Meets ",std," NAAQS? (",lvl," ppb)",sep=""),
      paste(years[1:3],"4th Max Value (ppb)"),
      paste(years[2:3]+1,"Critical Value (ppb)"))
    
    ## County-level design values
    if (area.type == "county") {
      vals <- vals[order(vals$state_name,vals$county_name,vals[,dv.col],decreasing=TRUE),
        c("state_name","county_name","cbsa_name",val.cols)]
      vals <- vals[!duplicated(vals[,c("state_name","county_name")]),]
      table.out <- vals[order(vals$state_name,vals$county_name),]
      colnames(table.out) <- c("State","County","Core Based Statistical Area (CBSA)",cnames)
    }
    
    ## CBSA, CSA, or NAA-level design values
    if (area.type != "county") {
      vals <- vals[order(vals[,name.col],vals[,dv.col],decreasing=TRUE),c(name.col,val.cols)]
      vals <- vals[!duplicated(vals[,name.col]),]
      table.out <- vals[order(vals[,name.col]),]
      colnames(table.out) <- c(geo,cnames)
      if (area.type == "naa") { colnames(table.out)[1] <- paste(std,"NAAQS Nonattainment Area") }
    }
  }
  
  ## Get Site-level Design Values
  if (type == "Site-level Design Values") {
    
    ## All sites in the U.S., an EPA region, or a State
    if (out == "All Sites") { 
      table.out <- switch(area.type,county=vals[order(vals$site),],
        vals[order(vals[,name.col],vals$site),])
    }
    
    ## All sites in a particular county, CBSA, CSA, or NAA
    if (out != "All Sites") {
      vals <- vals[which(vals[,name.col] == out),]
      table.out <- vals[order(vals$site),]
    }
  }
  
  ## Return DV table
  return(table.out)
}
