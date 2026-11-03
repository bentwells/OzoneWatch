options(stringsAsFactors=FALSE,warn=-1)
source("code_dev/get_aqs_data.r")

## Function to get earliest or latest date values by monitor
## dt = column with date or datetime values
## id = monitor id variable
## first = use earliest value (TRUE) or latest value (FALSE)?
get.unique.dates <- function(dt,id,first) {
  if (any(dt == " ")) { dt[which(dt == " ")] <- as.character(Sys.Date()) }
  if (first) { unique.dates <- tapply(dt,list(id),min) }
  if (!first) { unique.dates <- tapply(dt,list(id),max) }
  out <- sapply(unique.dates,function(x) ifelse(x == Sys.Date(),
    " ",as.character(x)))
  return(out)
}

## Function to get codes associated with most recent begin dates
## dt = column with date or datetime values
## id = monitor id variable
## code = column with code values
get.unique.codes <- function(dt,id,code) {
  ids <- unique(id)
  n <- length(ids)
  out <- rep(" ",n)
  for (i in 1:n) {
    t <- which(id == ids[i])
    if (length(t) == 1) {
      out[i] <- code[t]
      next
    }
    if (any(dt[t] == " ")) {
      j <- which(dt[t] == " ")[1]
      out[i] <- code[t][j]
      next
    }
    out[i] <- code[t][which.max(as.Date(dt[t]))]
  }    
  return(out)
}

## Function to get all method codes associated with a given pollutant
get.methods <- function(par) {
  methods <- get.aqs.data(paste("SELECT DISTINCT
    methods.methodology_code AS method_code,
    COALESCE(methods.reference_method_id,' ') AS frm_fem_id,
    COALESCE(methods.sample_anal_desc,' ') AS method_desc,
    COALESCE(methods.equivalent_method_desc,' ') AS manufacturer,
    methods.fed_mdl AS mdl
    FROM sampling_methodologies methods
   WHERE methods.pa_parameter_code = '",par,"'
     AND methods.status_ind = 'P'
   ORDER BY 1",sep=""))
  methods$mdl <- as.numeric(methods$mdl)*ifelse(par == 44201,1000,1)
  if (par == 44201) { 
    ## Fix issue with AIRS legacy FRM method code
    methods$frm_fem_id[which(methods$method_code == '011')] <- "RFOA-0176-011"
    methods$manufacturer[which(methods$method_code == '011')] <- "AIRS LEGACY CODE"
  }
  return(methods)
}

## Function to get ozone monitoring seasons for a range of years
## yr1 = get seasons with begin dates on or after this year
## yr2 = get seasons with end dates on or before this year
get.seasons <- function(yr1,yr2) {
  seasons <- get.aqs.data(paste("SELECT DISTINCT
        COALESCE(seasons.cn_stt_state_code,' ') as state,
        COALESCE(seasons.cn_county_code,' ') as county,
        COALESCE(seasons.site_id,' ') as site_id,
        COALESCE(seasons.monitoring_season_begin_month,1) as begin_month,
        COALESCE(seasons.monitoring_season_begin_day,1) as begin_day,
        COALESCE(seasons.monitoring_season_begin_year,",yr1,") as begin_year,
        COALESCE(seasons.monitoring_season_end_month,12) as end_month,
        COALESCE(seasons.monitoring_season_end_day,31) as end_day,
        COALESCE(seasons.monitoring_season_end_year,",yr2,") as end_year
   FROM monitoring_seasons seasons
  WHERE seasons.pa_parameter_code = '44201'
    AND seasons.monitoring_season_begin_year <= '",yr2,"'
    AND (seasons.monitoring_season_end_year >= '",yr1,"'
     OR seasons.monitoring_season_end_year IS NULL)
    AND seasons.cn_stt_state_code NOT IN ('80','CC')
  ORDER BY 1,2,3",sep=""))
  return(seasons)
}

## Function to get nonattainment area information from AQS
get.naa.info <- function(par,psid) {
  require(plyr);
  naa <- get.aqs.data(paste("SELECT
    naas.area_name || ', ' || naas.state_suffix AS naa_name,
    TO_NUMBER(states.epar_epa_region_code,'99') AS epa_region,
    COALESCE(naas.current_status,' ') AS status,
    COALESCE(naas.classification,' ') AS classification,
    COALESCE(TO_CHAR(naas.designation_effective_date+1/24,'YYYY-MM-DD'),' ') AS designation_date,
    COALESCE(TO_CHAR(naas.cdd_effective_date+1/24,'YYYY-MM-DD'),' ') AS cdd_date,
    COALESCE(TO_CHAR(naas.daa_effective_date+1/24,'YYYY-MM-DD'),' ') AS daa_date,
    COALESCE(TO_CHAR(naas.redesignation_effective_date+1/24,'YYYY-MM-DD'),' ') AS redesignation_date
   FROM naas naas, states states
  WHERE SUBSTR(naas.composite_id_w_state,-2,2) = states.state_abbr
    AND naas.parameter_code = '",par,"'
    AND naas.ps_id = '",psid,"'
  ORDER BY 1",sep=""))
  if (psid %in% c(10,11,23)) {
    naa$naa_name[grep("Pechanga",naa$naa_name)] <- "Pechanga Band of Luiseno Mission Indians, CA"
  }
  if (psid %in% c(11,23)) {
    naa$naa_name[grep(", MD, MD",naa$naa_name)] <- "Cecil County, MD"
    naa$naa_name[grep(", DE, DE",naa$naa_name)] <- "New Castle County, DE"
  }
  if (psid == 11) {
    naa$naa_name[grep("Seaford",naa$naa_name)] <- "Seaford, DE"
  }
  if (psid == 18) {
    naa$naa_name[grep("San Joaquin",naa$naa_name)] <- "San Joaquin Valley, CA"
  }
  if (psid == 23) {
    naa$naa_name[grep("-Revised",naa$naa_name)] <- "Door County, WI"
  }
  naa$status <- gsub(" (NAAQS revoked)","",naa$status,fixed=TRUE)
  naa$classification <- gsub(" (Rural Transport)","",naa$classification,fixed=TRUE)
  naa.out <- ddply(naa,"naa_name",summarize,
    epa_regions=paste(unique(epa_region[order(epa_region)]),collapse=","),
    status=max(status),classification=max(classification),cdd_date=max(cdd_date,daa_date),
    redesignation_date=ifelse(any(redesignation_date == " ")," ",max(redesignation_date)))
  if (par == 42401) { 
    naa.out$designation_date <- ddply(naa,"naa_name",summarize,
      designation_date=max(designation_date))$designation_date
  }
  return(naa.out)
}

## Function to retrieve monitor metadata from AQS
## par = Get monitors with this AQS parameter code
## yr1 = Get monitors stopping after/during this year
## yr2 = Get monitors starting before/during this year
## all = Return all results (TRUE) or unique ids only (FALSE)?
get.monitors <- function(par,yr1,yr2,all=FALSE) {
  poll.table <- get.aqs.data(paste("SELECT DISTINCT
    psids.pollutant_standard_id AS psid,
    psids.pa_parameter_code AS par,
    psids.short_description AS standard
    FROM pollutant_standards psids
    WHERE psids.pa_parameter_code = '",par,"'
      AND psids.pollutant_standard_id NOT IN (3,5,6,7,18,21,24)
    ORDER BY 1,2",sep=""))
  poll.table$year <- sapply(poll.table$standard,function(x) substr(x,nchar(x)-3,nchar(x)))
  if (par == 12128) { poll.table$year <- 2009 }
  if (par == 88101) { poll.table$psid[3] <- 18 }
  t <- get.aqs.data(paste("SELECT DISTINCT
    sites.state_code || sites.county_code || sites.site_id || monitors.poc AS id,
    COALESCE(INITCAP(sites.local_site_name),' ') AS site_name,
    COALESCE(INITCAP(sites.street_address),' ') AS address,
    ROUND(COALESCE(sites.standard_latitude,sites.user_latitude),6) AS latitude,
    ROUND(COALESCE(sites.standard_longitude,sites.user_longitude),6) AS longitude,
    tz.gmt_diff AS gmt_offset,
    TO_NUMBER(regions.epa_region_code,'99') AS epa_region,
    states.state_name AS state_name,
    counties.county_name AS county_name,
    COALESCE(TRIM(cbsas.cbsa_name),' ') AS cbsa_name,
    COALESCE(TRIM(cbsas.csa_title),' ') AS csa_name,",
    if (nrow(poll.table) > 0) { 
      paste("COALESCE(
        (SELECT area_name || ', ' || state_suffix
           FROM site_naa
          WHERE sites.si_id = site_naa.si_id(+)
            AND parameter_code = '",poll.table$par[1],"'
            AND ps_id = '",poll.table$psid[1],"'),' ')
        AS naa_name_",poll.table$year[1],",",sep="")
    },
    if (nrow(poll.table) > 1) { 
      paste("COALESCE(
        (SELECT area_name || ', ' || state_suffix
           FROM site_naa
          WHERE sites.si_id = site_naa.si_id(+)
            AND parameter_code = '",poll.table$par[2],"'
            AND ps_id = '",poll.table$psid[2],"'),' ')
        AS naa_name_",poll.table$year[2],",",sep="")
    },
    if (nrow(poll.table) > 2) { 
      paste("COALESCE(
        (SELECT area_name || ', ' || state_suffix
           FROM site_naa
          WHERE sites.si_id = site_naa.si_id(+)
            AND parameter_code = '",poll.table$par[3],"'
            AND ps_id = '",poll.table$psid[3],"'),' ')
        AS naa_name_",poll.table$year[3],",",sep="")
    },
    if (nrow(poll.table) > 3) { 
      paste("COALESCE(
        (SELECT area_name || ', ' || state_suffix
           FROM site_naa
          WHERE sites.si_id = site_naa.si_id(+)
            AND parameter_code = '",poll.table$par[4],"'
            AND ps_id = '",poll.table$psid[4],"'),' ')
        AS naa_name_",poll.table$year[4],",",sep="")
    },
    "COALESCE(agencies.agency_desc,' ') AS reporting_agency,
    COALESCE(INITCAP(monitors.ms_measurement_scale),' ') AS measurement_scale,
    COALESCE(INITCAP(objectives.mot_monitor_obj_type),' ') AS monitor_objective,
    COALESCE(TO_CHAR(sp.sampling_begin_date+1/24,'YYYY-MM-DD'),' ') AS monitor_begin_date,
    COALESCE(TO_CHAR(sp.sampling_end_date-1/24,'YYYY-MM-DD'),' ') AS monitor_end_date,
    COALESCE(TO_CHAR(monitors.last_sampling_date+1/24,'YYYY-MM-DD'),' ') AS last_sample_date,
    COALESCE(TO_CHAR(exclusions.begin_date+1/24,'YYYY-MM-DD'),' ') AS nonreg_begin_date,
    COALESCE(TO_CHAR(exclusions.end_date-1/24,'YYYY-MM-DD'),' ') AS nonreg_end_date,
    COALESCE(exclusions.concurrence_flag,' ') AS nonreg_concur,
    COALESCE(methods.method_code,' ') AS method_code,
    COALESCE(sm.reference_method_id,' ') AS frm_code,
    COALESCE(TO_CHAR(methods.begin_date+1/24,'YYYY-MM-DD'),' ') AS method_begin_date,
    COALESCE(TO_CHAR(methods.end_date-1/24,'YYYY-MM-DD'),' ') AS method_end_date,
    COALESCE(mta.mt_monitor_type,' ') AS monitor_type,
    COALESCE(TO_CHAR(mta.monitor_type_begin_date+1/24,'YYYY-MM-DD'),' ') AS type_begin_date,
    COALESCE(TO_CHAR(mta.monitor_type_end_date-1/24,'YYYY-MM-DD'),' ') AS type_end_date,
    COALESCE(mna.monitor_network_code,' ') AS network,
    COALESCE(TO_CHAR(mna.begin_date+1/24,'YYYY-MM-DD'),' ') AS network_begin_date,
    COALESCE(TO_CHAR(mna.end_date-1/24,'YYYY-MM-DD'),' ') AS network_end_date,
    COALESCE(TO_CHAR(pmp.begin_date+1/24,'YYYY-MM-DD'),' ') AS primary_begin_date,
    COALESCE(TO_CHAR(pmp.end_date-1/24,'YYYY-MM-DD'),' ') AS primary_end_date,
    COALESCE(INITCAP(cf.coll_freq_desc),' ') AS collection_frequency,
    COALESCE(TO_CHAR(rcf.req_coll_freq_begin_date+1/24,'YYYY-MM-DD'),' ') AS frequency_begin_date,
    COALESCE(TO_CHAR(rcf.req_coll_freq_end_date-1/24,'YYYY-MM-DD'),' ') AS frequency_end_date,
    COALESCE(combos.second_site_state || combos.second_site_county || 
      combos.second_site_number,' ') AS combo_site,
    COALESCE(TO_CHAR(combos.cutover_date+1/24,'YYYY-MM-DD'),' ') AS combo_date,
    COALESCE(seasons.monitoring_season_begin_year,",yr1,") || '-' ||
      COALESCE(TO_CHAR(seasons.monitoring_season_begin_month,'FM09'),'01') || '-' ||
      COALESCE(TO_CHAR(seasons.monitoring_season_begin_day,'FM09'),'01') AS season_begin_date,
    COALESCE(seasons.monitoring_season_end_year,",yr2,") || '-' ||
      COALESCE(TO_CHAR(seasons.monitoring_season_end_month,'FM09'),'12') || '-' ||
      COALESCE(TO_CHAR(seasons.monitoring_season_end_day,'FM09'),'31') AS season_end_date
  FROM agencies agencies,
    cbsa_mapping cbsas,
    collection_frequencies cf,
    counties counties,
    epa_regions regions,
    linked_sites combos,
    measurement_scales scales,
    monitor_exclusions exclusions,
    monitor_methods methods,
    monitor_networks networks,
    monitor_network_affiliations mna,
    monitor_objectives objectives,
    monitor_objective_types mot,
    monitor_types types,
    monitor_type_assignments mta,
    monitoring_seasons seasons,
    monitors monitors,
    primary_monitor_periods pmp,
    req_coll_frequencies rcf,
    sample_periods sp,
    sampling_methodologies sm,
    site_basic sites,
    site_time_zone stz,
    states states,
    time_zones tz
  WHERE agencies.agency_code = monitors.monitoring_agency
    AND methods.method_code = sm.methodology_code
    AND methods.parameter_code = sm.pa_parameter_code
    AND mna.monitor_network_code = networks.monitor_network_code(+)
    AND monitors.mo_id = exclusions.mo_id(+)
    AND monitors.mo_id = methods.mo_id(+)
    AND monitors.mo_id = mna.mo_id(+)
    AND monitors.mo_id = mta.mo_mo_id(+)
    AND monitors.mo_id = objectives.mo_mo_id(+)
    AND monitors.mo_id = pmp.mo_id(+)
    AND monitors.mo_id = rcf.mo_mo_id(+)
    AND monitors.mo_id = sp.mo_mo_id(+)
    AND monitors.ms_measurement_scale = scales.measurement_scale(+)
    AND monitors.si_si_id = sites.si_id
    AND mta.mt_monitor_type = types.monitor_type(+)
    AND objectives.mot_monitor_obj_type = mot.monitor_obj_type(+)
    AND rcf.cf_coll_freq_code = cf.coll_freq_code(+)
    AND regions.epa_region_code = states.epar_epa_region_code
    AND sites.county_code = cbsas.county_code(+)
    AND sites.county_code = counties.county_code
    AND sites.si_id = combos.first_si_id(+)
    AND sites.state_code = cbsas.state_code(+)
    AND sites.state_code = counties.stt_state_code
    AND sites.state_code = states.state_code
    AND stz.si_id = sites.si_id
    AND tz.time_zone_name = stz.time_zone_name
    AND combos.parameter_code(+) = '",par,"'
    AND EXTRACT(YEAR FROM combos.cutover_date(+)) >= '",yr1,"'
    AND EXTRACT(YEAR FROM combos.cutover_date(+)) <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM exclusions.begin_date(+)),",yr1,") <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM exclusions.end_date(+)),",yr2,") >= '",yr1,"'
    AND COALESCE(EXTRACT(YEAR FROM methods.begin_date(+)),",yr1,") <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM methods.end_date(+)),",yr2,") >= '",yr1,"'
    AND COALESCE(EXTRACT(YEAR FROM mna.begin_date(+)),",yr1,") <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM mna.end_date(+)),",yr2,") >= '",yr1,"'
    AND COALESCE(EXTRACT(YEAR FROM monitors.last_sampling_date),",yr2,") >= '",yr1,"'
    AND monitors.last_sampling_date IS NOT NULL
    AND monitors.pa_parameter_code = '",par,"'
    AND monitors.status_ind = 'P'
    AND COALESCE(EXTRACT(YEAR FROM mta.monitor_type_begin_date(+)),",yr1,") <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM mta.monitor_type_end_date(+)),",yr2,") >= '",yr1,"'
    AND COALESCE(EXTRACT(YEAR FROM pmp.begin_date(+)),",yr1,") <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM pmp.end_date(+)),",yr2,") >= '",yr1,"'
    AND COALESCE(EXTRACT(YEAR FROM rcf.req_coll_freq_begin_date(+)),",yr1,") <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM rcf.req_coll_freq_end_date(+)),",yr2,") >= '",yr1,"'
    AND sites.state_code NOT IN ('80','CC')
    AND sites.status_ind = 'P'
    AND sm.pa_parameter_code = '",par,"'
    AND COALESCE(EXTRACT(YEAR FROM sp.sampling_begin_date(+)),",yr1,") <= '",yr2,"'
    AND COALESCE(EXTRACT(YEAR FROM sp.sampling_end_date(+)),",yr2,") >= '",yr1,"'
    AND sites.state_code = seasons.cn_stt_state_code(+)
    AND (sites.county_code = seasons.cn_county_code",
        ifelse(par == 44201," OR seasons.cn_county_code IS NULL)","(+))"),"
    AND (sites.site_id = seasons.site_id",
        ifelse(par == 44201," OR seasons.site_id IS NULL)","(+))"),"
    AND COALESCE(seasons.monitoring_season_begin_year(+),",yr1,") <= '",yr2,"'
    AND COALESCE(seasons.monitoring_season_end_year(+),",yr2,") >= '",yr1,"'
    AND seasons.pa_parameter_code(+) = '",par,"'
  ORDER BY 1",sep=""))
  ## Fix issue where lat/lon coordinates are returned as character fields
  t$latitude <- as.numeric(t$latitude); t$longitude <- as.numeric(t$longitude);
  ## Fix issues with ozone state/county/CBSA/CSA/NAA names
  if (par == 44201) {
    ## Pechanga - shorten ridiculously long name
    pechanga <- grep("Pechanga",t$naa_name_2015)
    t$naa_name_2015[pechanga] <- t$naa_name_2008[pechanga] <- t$naa_name_1997[pechanga] <-
      "Pechanga Band of Luiseno Mission Indians, CA"
    ## Seaford, DE - Fix naming issue in 2008 shapefile
    seaford <- grep("Seaford",t$naa_name_2008)
    t$naa_name_2008[seaford] <- "Seaford, DE"
    ## Allegan County, MI - fix misclassified monitor near county line
    allegan <- which(substr(t$id,1,5) == "26005")
    t$naa_name_1997[allegan] <- "Allegan County Area, MI"
    ## New York Metro Area - coastline issues for 1997 NAAQS
    nyc <- which(substr(t$id,1,5) == "09001")
    t$naa_name_1997[nyc] <- "New York-N. New Jersey-Long Island Area, NY-NJ-CT"
    ## Portland, ME - coastline issues for 1997 NAAQS
    portland <- which(substr(t$id,1,5) == "23031")
    t$naa_name_1997[portland] <- "Portland Area, ME"
    ## Fix Cecil County, MD NAA name
    cecil <- which(substr(t$id,1,5) == "24015")
    t$naa_name_2015[cecil] <- t$naa_name_2008[cecil] <- "Cecil County, MD"
    ## Fix New Castle County, DE NAA name
    newcastle <- which(substr(t$id,1,5) == "10003")
    t$naa_name_2015[newcastle] <- t$naa_name_2008[newcastle] <- "New Castle County, DE"
    ## Fix Philadelphia NAA name for 2015 and 2008 NAAQS
    phil <- grep("Philadelphia",t$naa_name_2015)
    t$naa_name_2015[phil] <- t$naa_name_2008[phil] <- "Philadelphia-Wilmington-Atlantic City, PA-NJ"
    ## Fixes to state/county/CBSA/CSA names for Ozone Watch
    t$state_name <- gsub("Of","of",t$state_name)
    t$csa_name <- gsub("AL-TN","AL",t$csa_name,fixed=TRUE)
    fix <- which(substr(t$id,1,5) == "72097")
    if (length(fix) > 0) {
      t$county_name[fix] <- "Mayaguez"
      t$cbsa_name[fix] <- "Mayaguez, PR"
      t$csa_name[fix] <- "Mayaguez-San German, PR"
    }
    fix2 <- which(substr(t$id,1,5) == "08101")
    t$csa_name[fix2] <- "Pueblo-Canyon City, CO"
  }
  if (par == 88101) {
    sjv <- grep("San Joaquin",t$naa_name_1997)
    t$naa_name_1997[sjv] <- "San Joaquin Valley, CA"
  }
  if (all) { monitors <- t }
  if (!all) {
    monitors <- t[which(!duplicated(t$id)),]
    first <- paste(c("monitor","nonreg","method","type","network","primary","frequency"),"begin_date",sep="_")
    last <- paste(c("monitor","nonreg","method","type","network","primary","frequency"),"end_date",sep="_")
    monitors[,first] <- lapply(t[,first],get.unique.dates,id=t$id,first=TRUE)
    monitors[,last] <- lapply(t[,last],get.unique.dates,id=t$id,first=FALSE)
    monitors$last_sample_date <- get.unique.dates(t$last_sample_date,t$id,first=FALSE)
    monitors$nonreg_concur <- get.unique.codes(t$nonreg_begin_date,t$id,t$nonreg_concur)
    monitors$method_code <- get.unique.codes(t$method_begin_date,t$id,t$method_code)
    monitors$frm_code <- get.unique.codes(t$method_begin_date,t$id,t$frm_code)
    monitors$monitor_type <- get.unique.codes(t$type_begin_date,t$id,t$monitor_type)
    monitors$network <- get.unique.codes(t$network_begin_date,t$id,t$network)
    monitors$season_begin_date <- substr(get.unique.codes(t$season_begin_date,t$id,t$season_begin_date),6,10)
    monitors$season_end_date <- substr(get.unique.codes(t$season_begin_date,t$id,t$season_end_date),6,10)
    monitors$season_begin_date[which(monitors$network == "NCORE")] <- "01-01"
    monitors$season_end_date[which(monitors$network == "NCORE")] <- "12-31"
  }
  return(monitors)
}
