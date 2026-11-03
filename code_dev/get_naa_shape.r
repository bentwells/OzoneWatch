options(stringsAsFactors=FALSE,warn=-1)
library(data.table); library(pins); library(plyr); library(sf);
naaqs <- c("Ozone_8-hr_(1997_standard)","Ozone_8-hr_(2008_standard)","Ozone_8-hr_(2015_standard)",
  "Lead_(2008_standard)","SO2_1-hr_(2010_standard)","PM2_5_24hr_(2006_standard)",
  "PM2_5_Annual_(1997_standard)","PM2_5_Annual_(2012_standard)","PM10_(1987_standard)",
  "CO_(1971_standard)","Ozone_1-hr_(1979_standard-revoked)","NO2_(1971_standard)")
state.name <- c(state.name,"District of Columbia","Guam","Puerto Rico","Virgin Islands")
state.abb <- c(state.abb,"DC","GU","PR","VI")
for (i in 1:length(naaqs)) {
  ## Read in NAA shapefile, merge multistate areas, output coordinates and metadata
  naa <- st_read(dsn=paste("Census/shapeNAA/",naaqs[i],".shp",sep=""),quiet=TRUE)
  naa <- naa[order(naa$area_name,naa$population,decreasing=TRUE),]
  naa$state_suff <- state.abb[match(naa$state_name,state.name)]
  if (i == 3) { 
    naa$area_name <- gsub("-Revised","",naa$area_name,fixed=TRUE)
    naa$composite_ <- gsub("_Revised","",naa$composite_,fixed=TRUE)
    naa$composite_ <- gsub("2008","2015",naa$composite_,fixed=TRUE)
  }
  if (i <= 3) {
    naa.names <- ddply(naa,"composite_",summarize,
      naa_name=paste(area_name[1],paste(state_suff,collapse="-"),sep=", "))$naa_name
    naa.match <- charmatch(naa$area_name,naa.names)
    if (i == 1) { naa.match[which(naa.match == 0)] <- rev(grep("Greene County Area",naa.names)) }
    naa$naa_name <- naa.names[naa.match]
    naa.comb <- aggregate(st_geometry(naa),list(naa$naa_name),st_combine)
  }
  if (i >= 4) { naa.comb <- aggregate(st_geometry(naa),list(naa$composite_),st_combine) }
  naa.union <- st_make_valid(st_simplify(st_union(st_as_sf(naa.comb),by_feature=TRUE),dTolerance=10))
  polys <- st_cast(st_cast(naa.union,"MULTIPOLYGON"),"POLYGON")
  ids <- sapply(rownames(polys),function(x) strsplit(x,".",fixed=TRUE))
  polys$fips <- sapply(ids,function(x) as.numeric(x[1]))
  polys$id <- sapply(ids,function(x) ifelse(length(x) == 1,0,as.numeric(x[2]))+1)
  polys$area <- as.numeric(st_area(polys))
  keep <- subset(polys,id == 1 | area >= 1e6) ## Keep 1st polygon and polygons > 1 km^2
  keep$new_id <- unlist(tapply(keep$id,list(keep$fips),function(x) cumsum(!is.na(x))))
  naa.shp <- subset(as.data.frame(st_coordinates(st_transform(st_cast(naa.union,"MULTIPOLYGON"),
    crs=4326))),L1 == 1,c("L3","L2","X","Y")) ## Setting L1 == 1 removes holes
  colnames(naa.shp) <- c("fips","id","lon","lat")
  naa.shp <- subset(naa.shp,paste(fips,id) %in% paste(keep$fips,keep$id))
  naa.shp$id <- keep$new_id[match(paste(naa.shp$fips,naa.shp$id),paste(keep$fips,keep$id))]
  naa.dbf <- ddply(naa,"composite_",summarize,
    naa_name=paste(area_name[1],paste(state_suff,collapse="-"),sep=", "),
    epa_regions=paste(unique(epa_region[order(epa_region)]),collapse=","),
    status=max(ifelse(is.na(current_st)," ",gsub(" (NAAQS revoked)","",current_st,fixed=TRUE))),
    classification=max(ifelse(is.na(classifi_4)," ",gsub(" (Rural Transport)","",classifi_4,fixed=TRUE))),
    designation_date=max(ifelse(year(designat_1) < 1900," ",as.character(designat_1))),
    cdd_date=max(ifelse(year(cdd_effect) < 1900," ",as.character(cdd_effect))),
    daa_date=max(ifelse(year(daa_effect) < 1900," ",as.character(daa_effect))),
    redesignation_date=max(ifelse(year(redesign_4) < 1900," ",as.character(redesign_4))))
  naa.dbf$naa_name <- gsub("MD-VA-DC","DC-MD-VA",naa.dbf$naa_name,fixed=TRUE)
  naa.dbf <- naa.dbf[order(naa.dbf$naa_name),]; colnames(naa.dbf)[1] <- "id";
  ## Fixes specific to ozone nonattainment areas, output files for Ozone Watch
  if (grepl("Ozone_8-hr",naaqs[i])) {
    std <- gsub("Ozone_8-hr_(","",gsub("_standard)","",naaqs[i],fixed=TRUE),fixed=TRUE)
    pechanga <- grep("Pechanga",naa.dbf$naa_name) ## Shorten Pechanga area name
    if (length(pechanga) > 0) {
      naa.dbf$naa_name[pechanga] <- "Pechanga Band of Luiseno Mission Indians, CA"
    }
    if (std == "2015") {
      door <- grep("Door County",naa.dbf$naa_name) ## Combine 2015 Door County areas, fix dates
      if (length(door) > 0) { 
        naa.dbf$naa_name[door] <- "Door County, WI"
        naa.dbf$designation_date[door] <- "2018-06-04"
        naa.dbf$redesignation_date[door] <- naa.dbf$cdd_date[door] <- naa.dbf$daa_date[door] <- " "
        naa.dbf <- naa.dbf[which(!duplicated(naa.dbf$naa_name)),]
      }
      elpaso <- grep("El Paso",naa.dbf$naa_name) ## Fix issue with dates for 2015 El Paso area
      if (length(elpaso) > 0) {
        naa.dbf$designation_date[elpaso] <- "2018-06-04"
        naa.dbf$redesignation_date[elpaso] <- naa.dbf$cdd_date[elpaso] <- naa.dbf$daa_date[elpaso] <- " "
      }
    }
    if (std %in% c("2015","2008")) {
      naa.dbf$naa_name[grep("Cecil County",naa.dbf$naa_name)] <- "Cecil County, MD"
      naa.dbf$naa_name[grep("New Castle County",naa.dbf$naa_name)] <- "New Castle County, DE"
      naa.dbf$naa_name[grep("Philadelphia",naa.dbf$naa_name)] <- "Philadelphia-Wilmington-Atlantic City, PA-NJ"
    }
    if (std == "2008") {
      seaford <- grep("Seaford",naa.dbf$naa_name)
      if (length(seaford) > 0) {
        naa.dbf$naa_name[seaford] <- "Seaford, DE"
      }
    }
    if (std == "1997") {
      naa.dbf$naa_name <- gsub("PA-NJ-DE-MD","PA-NJ-MD-DE",naa.dbf$naa_name,fixed=TRUE)
    }
    assign(paste("naa.shp",std,sep="."),naa.shp)
    assign(paste("naa.dbf",std,sep="."),data.frame(fips=1:nrow(naa.dbf),name=naa.dbf$naa_name))
    save(list=paste("naa",c("dbf","shp"),std,sep="."),
      file=paste("OzoneWatch/OzoneWatch/data/O3_naa_",std,".Rdata",sep=""))
  }
  save(list=c("naa.dbf","naa.shp"),file=paste("Census/shapeNAA/",gsub(" ","_",naaqs[i]),".Rdata",sep=""))
  cat(naaqs[i],"\n")
}

## Write Ozone Watch mapping data to a pin on DMAP staging server
load("OzoneWatch/OzoneWatch/data/map_data_20m.Rdata")
load("OzoneWatch/OzoneWatch/data/map_data_500k.Rdata")
board <- board_connect()
data.list <- list(map.data.20m=map.data.20m,map.data.500k=map.data.500k,
  naa.dbf.1997=naa.dbf.1997,naa.shp.1997=naa.shp.1997,naa.dbf.2008=naa.dbf.2008,
  naa.shp.2008=naa.shp.2008,naa.dbf.2015=naa.dbf.2015,naa.shp.2015=naa.shp.2015)
pin_write(board,x=data.list,name="OzoneWatch_map_data",type="rds",title="OzoneWatch - mapping data",
  description=paste("Last updated on",Sys.Date()),versioned=FALSE,force_identical_write=TRUE)
