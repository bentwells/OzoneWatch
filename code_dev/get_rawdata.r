## Retrieve hourly AQS criteria pollutant data for one parameter/year
source("code_dev/get_aqs_data.r")

get.raw.data <- function(par,year) {
  options(stringsAsFactors=FALSE)
  poll.name <- switch(paste("p",par,sep=""),p14129="pb",p42101="co",
    p42401="so2",p42600="noy",p42601="no",p42602="no2",p42603="nox",
    p44201="o3",p81102="pm10",p88101="pm25")
  dt.begin <- ifelse(par == 42101,paste(year-1,"12-31 17:00:00"),
    paste(year,"01-01 00:00:00",sep="-"))
  dt.end <- ifelse(par == 44201,paste(year+1,"01-01 06:00:00",sep="-"),
    paste(year,"12-31 23:00:00",sep="-"))
  
  aqs.data <- subset(get.aqs.data(paste("SELECT DISTINCT
          rd.state_code || rd.county_code || rd.site_id || rd.poc AS id,
          TO_CHAR(rd.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
          GREATEST(rd.standard_sample_value,0) AS ",poll.name,",
          rd.method_code AS method,
          COALESCE(rd.event_code || rd.null_data_code,' ') AS flag,
          COALESCE(rd.event_concurence_indicator || rd.null_code_concurrence,' ') AS concur
     FROM raw_data_concurrences rd
    WHERE rd.duration_code = '1'
      AND rd.parameter_code = '",par,"'
      AND rd.sampling_begin_datetime >= TO_DATE('",dt.begin,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.sampling_begin_datetime <= TO_DATE('",dt.end,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.state_code NOT IN ('80','CC')
    ORDER BY 1,2",sep="")),substr(dt,15,19) == "00:00")
  if (par == 14129) { aqs.data$pb <- floor(100*aqs.data$pb)/100 }
  if (par == 42101) { aqs.data$co <- floor(10*aqs.data$co)/10 }
  if (par == 42401) { aqs.data$so2 <- floor(10*aqs.data$so2)/10 }
  if (par == 42600) { aqs.data$no2 <- floor(10*aqs.data$noy)/10 }
  if (par == 42601) { aqs.data$no2 <- floor(10*aqs.data$no)/10 }
  if (par == 42602) { aqs.data$no2 <- floor(10*aqs.data$no2)/10 }
  if (par == 42603) { aqs.data$no2 <- floor(10*aqs.data$nox)/10 }
  if (par == 44201) { aqs.data$o3 <- floor(aqs.data$o3*1000) }
  if (par == 81102) { aqs.data$pm10 <- floor(10*aqs.data$pm10)/10 }
  if (par == 88101) { aqs.data$pm25 <- floor(10*aqs.data$pm25)/10 }
  
  ## Fix issue with duplicate exceptional event concurrence indicators
  if (any(duplicated(aqs.data[,c("id","dt")]))) {
    dup.ind <- which(duplicated(aqs.data[,c("id","dt")]))
    dup.ind <- c((dup.ind-1),dup.ind)
    dup.ind <- dup.ind[order(dup.ind)]
    vals.rm <- dup.ind[which(aqs.data$concur[dup.ind] == "N")]
    aqs.data <- aqs.data[-c(vals.rm),]
  }
  
  all.ids <- as.character(unique(aqs.data$id))
  all.dts <- unique(aqs.data$dt)[order(unique(aqs.data$dt))]
  all.hrs <- data.frame(id=rep(all.ids,each=length(all.dts)),
                        dt=rep(all.dts,times=length(all.ids)))
  out <- merge(all.hrs,aqs.data,all.x=TRUE,all.y=FALSE)
  out$method <- replace(out$method,which(is.na(out$method))," ")
  out$flag <- replace(out$flag,which(is.na(out$flag))," ")
  out$concur <- replace(out$concur,which(is.na(out$concur))," ")
  assign(poll.name,out)
  save(list=poll.name,file=paste("data_dev/",poll.name,"_",year,".Rdata",sep=""))
}
