##########################################################
## Server function for the Ozone Watch R shiny application
##########################################################
shinyServer(function(input,output) {
  
  #######################################################################
  ## Logic controlling reactive drop-down menus in the left-hand UI panel
  #######################################################################
  
  ## Create a switch for area-level vs. site-level design values
  output$ui.type <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (!input$app.select %in% c("Design Value Tables","Design Value Maps")) { return() }
    radioButtons(inputId="type.select",label=NULL,
      choices=c("Area-level Design Values","Site-level Design Values"),
      selected="Area-level Design Values")
  })
  
  ## Create a geography selection input
  output$ui.geo <- renderUI({
    if (is.null(input$app.select)) { return() }
    geo.choices <- c("AQS Site ID","State/County","Core Based Statistical Area (CBSA)",
      "Combined Statistical Area (CSA)","Nonattainment Area (NAA)")
    if (input$app.select %in% c("Design Value Tables","Design Value Maps")) {
      geo.choices <- geo.choices[geo.choices != "AQS Site ID"]
    }
    selectInput(inputId="geo.select",label="Select a Geographic Area Type:",
      choices=geo.choices,selected="Nonattainment Area (NAA)")
  })
  
  ## Create a state selection input
  output$ui.state <- renderUI({
    if (is.null(input$region.select)) { return() }
    state.choices <- c(state.name[1:8],"District of Columbia",
      state.name[9:50],"Puerto Rico")
    if (input$region.select != "National") {
      naaqs <- substr(input$naaqs.select,1,4)
      sites <- subset(eval(parse(text=paste("sitedv",naaqs,sep="."))),
        epa_region == substr(input$region.select,12,13),c("epa_region","state_name"))
      state.choices <- unique(sites$state_name)
    }
    selectInput(inputId="state.select",label="Select a State:",
      choices=c("All States",state.choices),
      selected="All States")
  })
  
  ## Create an area-specific or site-specific selection input
  output$ui.out <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (is.null(input$naaqs.select)) { return() }
    if (is.null(input$geo.select)) { return () }
    if (is.null(input$region.select)) { return() }
    if (is.null(input$state.select)) { return() }
    if (input$app.select %in% c("Design Value Tables","Design Value Maps")) {
      if (input$type.select == "Area-level Design Values") { return() }
    }
    naaqs <- substr(input$naaqs.select,1,4)
    sites <- eval(parse(text=paste("sitedv",naaqs,sep=".")))[,1:11]
    if (input$region.select != "National") {
      sites <- subset(sites,epa_region == substr(input$region.select,12,13))
    }
    if (input$state.select != "All States") {
      sites <- subset(sites,state_name == input$state.select)
    }
    ## Site-level selection
    if (input$geo.select == "AQS Site ID") {
      if (input$app.select %in% c("Design Value Tables","Design Value Maps")) { return() }
      out.label <- "Select a Site:"
      if (input$state.select == "All States") {
        out.choices <- "Please select a State first!"
      }
      if (input$state.select != "All States") {
        vals <- subset(sites,!duplicated(substr(site,1,9)),
          c("site","site_name","address"))
        display.name <- mapply(function(x,y) ifelse(x == " ",y,x),
          x=vals$site_name,y=vals$address)
        out.choices <- substr(vals$site,1,9)
        names(out.choices) <- paste(out.choices,display.name,sep=" - ")
      }
    }
    ## County-level selection
    if (input$geo.select == "State/County") {
      out.label <- "Select a County:"
      if (input$state.select == "All States") { 
        out.choices <- "Please select a State first!"
      }
      if (input$state.select != "All States") {
        vals <- subset(sites,!duplicated(substr(site,1,5)),"county_name")
        out.choices <- vals$county_name[order(vals$county_name)]
      }
    }
    ## CBSA-level selection
    if (input$geo.select == "Core Based Statistical Area (CBSA)") {
      out.label <- "Select a CBSA:"
      vals <- subset(sites,cbsa_name != " " & !duplicated(cbsa_name),"cbsa_name")
      out.choices <- vals$cbsa_name[order(vals$cbsa_name)]
    }
    ## CSA-level selection
    if (input$geo.select == "Combined Statistical Area (CSA)") {
      out.label <- "Select a CSA:"
      vals <- subset(sites,csa_name != " " & !duplicated(csa_name),"csa_name")
      out.choices <- vals$csa_name[order(vals$csa_name)]
    }
    ## NAA-level selection
    if (input$geo.select == "Nonattainment Area (NAA)") {
      out.label <- "Select a NAA:"
      vals <- subset(sites,naa_name != " " & !duplicated(naa_name),"naa_name")
      out.choices <- vals$naa_name[order(vals$naa_name)]
    }
    out.default <- ifelse(input$app.select %in% c("Design Value Tables","Design Value Maps"),
      "All Sites","Please Make a Selection!")
    selectInput(inputId="out.select",label=out.label,
      choices=c(out.default,out.choices),selected=out.default)
  })
  
  ## Select which years to display in the Design Value Maps and Tables applications
  output$ui.year <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (!input$app.select %in% c("Design Value Tables","Design Value Maps")) { return() }
    year.choices <- c(curr.year:(curr.year-10))
    names(year.choices) <- paste(year.choices-2,year.choices,sep=" - ")
    selectInput(inputId="year.select",label="Select which years to display:",
      choices=year.choices,selected=year.choices[1])
  })
  
  ## Select which values to display in the "Design Value Maps" application
  output$ui.value <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (is.null(input$year.select)) { return() }
    if (input$app.select != "Design Value Maps") { return() }
    years <- as.numeric(input$year.select)-c(2:0)
    val.choices <- c(paste("dv",years[1],years[3],sep="_"),paste("max4",years,sep="_"))
    names(val.choices) <- c(ifelse(years[3] == curr.year,
      paste("Preliminary",years[1],"-",years[3],"Design Value"),
      paste(years[1],"-",years[3],"Design Value")),
      paste(years,"4th Highest Daily Maximum Value"))
    selectInput(inputId="value.select",label="Select which values to display:",
      choices=val.choices,selected=val.choices[1])
  })
  
  ## Choose how area 4th maxes are displayed in the "Design Value Tables" Application
  output$ui.maxes <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (input$app.select != "Design Value Tables") { return() }
    if (is.null(input$type.select)) { return() }
    if (input$type.select == "Site-level Design Values") { return() }
    radioButtons(inputId="maxes.select",label="Area 4th Highest Values based on:",
      choices=c("Site with Highest Design Value","Site with Highest Annual Value"),
      selected="Site with Highest Design Value")
  })
  
  #############################################################
  ## Logic controlling download links in the left-hand UI panel
  #############################################################
  
  ## Download tables from the "Design Value Tables" application as CSV files
  output$download.dvtables <- downloadHandler(
    filename=function() { return(paste("Ozone_Watch_",
    gsub(" ","_",gsub("-","",gsub(":","",Sys.time()))),".csv",sep="")) },
    content=function(file) { write.csv(
      dv.tables(naaqs=input$naaqs.select,type=input$type.select,geo=input$geo.select,
        region=input$region.select,state=input$state.select,out=input$out.select,
        year=input$year.select,maxes=input$maxes.select),file,row.names=FALSE,na="")
  })
  
  ## Download images from the "Design Value Maps" application as PNG files
  output$download.areamaps <- downloadHandler(
    filename=function() { return(paste("Ozone_Watch_",
    gsub(" ","_",gsub("-","",gsub(":","",Sys.time()))),".png",sep="")) },
    content=function(file) {
      png(file,width=960,height=720)
      area.maps(naaqs=input$naaqs.select,type=input$type.select,geo=input$geo.select,
        region=input$region.select,state=input$state.select,out=input$out.select,
        year=input$year.select,value=input$value.select)
      dev.off()
  })
  
  ## Download images from the "Design Value Trends" application as PNG files
  output$download.dvtrends <- downloadHandler(
    filename=function() { return(paste("Ozone_Watch_",
    gsub(" ","_",gsub("-","",gsub(":","",Sys.time()))),".png",sep="")) },
    content=function(file) {
      png(file,width=960,height=720)
      dv.trends(naaqs=input$naaqs.select,geo=input$geo.select,
        state=input$state.select,out=input$out.select)
      dev.off()
  })
  
  ## Download images from the "Daily AQI Tile Plots" application as PNG files
  output$download.tileplot <- downloadHandler(
    filename=function() { return(paste("Ozone_Watch_",
    gsub(" ","_",gsub("-","",gsub(":","",Sys.time()))),".png",sep="")) },
    content=function(file) {
      png(file,width=960,height=720)
      tile.plot(naaqs=input$naaqs.select,geo=input$geo.select,
        state=input$state.select,out=input$out.select)
      dev.off()
  })
  
  ## Download images from the "Cumulative 4th Max Plots" application as PNG files
  output$download.max4plot <- downloadHandler(
    filename=function() { return(paste("Ozone_Watch_",
    gsub(" ","_",gsub("-","",gsub(":","",Sys.time()))),".png",sep="")) },
    content=function(file) {
      png(file,width=960,height=720)
      max4.plot(naaqs=input$naaqs.select,geo=input$geo.select,
        state=input$state.select,out=input$out.select)
      dev.off()
  })
  
  ####################################################################
  ## Logic controlling tables and plots displayed in the main UI panel
  ####################################################################
  
  ## Display tables in the "Design Value Tables" application
  output$display.dvtables <- DT::renderDataTable({
    input$go.dvtables
    inputs <- isolate({
      list(naaqs=input$naaqs.select,type=input$type.select,geo=input$geo.select,
        region=input$region.select,state=input$state.select,out=input$out.select,
        year=input$year.select,maxes=input$maxes.select)
    })
    withProgress({
      table.out <- dv.tables(naaqs=inputs$naaqs,type=inputs$type,geo=inputs$geo,
        region=inputs$region,state=inputs$state,out=inputs$out,year=inputs$year,
        maxes=inputs$maxes)
      area.col <- grep("Area",colnames(table.out))-1
      val.col <- grep("Design Value",colnames(table.out))-1
      meet.col <- grep("Meets",colnames(table.out))-1
      col.types <- unlist(lapply(table.out,class),use.names=FALSE)
      col.types[(meet.col+1)] <- "numeric"
      char.cols <- which(col.types == "character")-1
      num.cols <- which(col.types %in% c("integer","numeric"))-1
      datatable(table.out,
        options=list(autoWidth=TRUE,info=FALSE,paging=FALSE,searching=FALSE,
          columnDefs=list(list(width="200px",targets=area.col),
                          list(width="100px",targets=val.col),
                          list(width="80px",targets=meet.col),
                          list(className='dt-left',targets=char.cols),
                          list(className='dt-center',targets=num.cols))),
        rownames=FALSE,class="compact hover order-column row-border stripe")
    },message="Loading...",value=NULL,detail=NULL)
  })
  
  ## Display plots in the "Design Value Maps" application
  output$display.areamaps <- renderPlot({
    input$go.areamaps
    inputs <- isolate({
      list(naaqs=input$naaqs.select,type=input$type.select,geo=input$geo.select,
        region=input$region.select,state=input$state.select,out=input$out.select,
        year=input$year.select,value=input$value.select)
    })
    withProgress({
      area.maps(naaqs=inputs$naaqs,type=inputs$type,geo=inputs$geo,region=inputs$region,
        state=inputs$state,out=inputs$out,year=inputs$year,value=inputs$value)
    },message="Loading...",value=NULL,detail=NULL)
  },width=960,height=720)
  
  ## Display plots in the "Design Value Trends" application
  output$display.dvtrends <- renderPlot({
    input$go.dvtrends
    inputs <- isolate({
      list(naaqs=input$naaqs.select,geo=input$geo.select,
        state=input$state.select,out=input$out.select)
    })
    withProgress({
      dv.trends(naaqs=inputs$naaqs,geo=inputs$geo,state=inputs$state,out=inputs$out)
    },message="Loading...",value=NULL,detail=NULL)
  },width=960,height=720)
  
  ## Display plots in the "Daily AQI Tile Plots" application
  output$display.tileplot <- renderPlot({
    input$go.tileplot
    inputs <- isolate({
      list(naaqs=input$naaqs.select,geo=input$geo.select,
        state=input$state.select,out=input$out.select)
    })
    withProgress({
      tile.plot(naaqs=inputs$naaqs,geo=inputs$geo,state=inputs$state,out=inputs$out)
    },message="Loading...",value=NULL,detail=NULL)
  },width=960,height=720)
  
  ## Display plots in the "Cumulative 4th Max Plots" application
  output$display.max4plot <- renderPlot({
    input$go.max4plot
    inputs <- isolate({
      list(naaqs=input$naaqs.select,geo=input$geo.select,
        state=input$state.select,out=input$out.select)
    })
    withProgress({
      max4.plot(naaqs=inputs$naaqs,geo=inputs$geo,state=inputs$state,out=inputs$out)
    },message="Loading...",value=NULL,detail=NULL)
  },width=960,height=720)
  
  ##################################################################
  ## Logic controlling text displayed in the panel beneath the plots
  ##################################################################
  
  ## Hover to display values in the "Design Value Maps" application
  output$text.areamaps <- renderPrint({
    if (input$app.select != "Design Value Maps") { return(cat("")) }
    if (is.null(input$geo.select)) { return(cat("")) }
    if (is.null(input$year.select)) { return(cat("")) }
    if (input$go.areamaps == 0) { return(cat("")) }
    years <- as.numeric(input$year.select)-c(2:0)
    area.type <- switch(substr(input$geo.select,1,3),
      Sta="county",Cor="cbsa",Com="csa",Non="naa")
    area.title <- ifelse(area.type == "county","County",toupper(area.type))
    name.col <- paste(area.type,"name",sep="_")
    
    ## Hover text for area-level design values
    if (input$type.select == "Area-level Design Values") {
      char1 <- ifelse(area.type == "county","State Name:",paste(area.title,"Name:"))
      char2 <- ifelse(area.type == "county","County Name:","")
      char3 <- ifelse(years[3] == curr.year,
        paste("Preliminary",years[1],"-",years[3],"Design Value (ppb):"),
        paste(years[1],"-",years[3],"Design Value (ppb):"))
      char4 <- paste(years[1],"4th Highest Daily Maximum Value (ppb):")
      char5 <- paste(years[2],"4th Highest Daily Maximum Value (ppb):")
      char6 <- paste(years[3],"4th Highest Daily Maximum Value (ppb):")
      ntab <- ((47 - nchar(c(char1,char2))) %/% 8) + 1
      char.out <- c(paste(char1,paste(rep("\t",ntab[1]),collapse=""),char3,sep=""),
        paste(char2,paste(rep("\t",ntab[2]),collapse=""),char4,sep=""),
        paste(paste(rep("\t",6),collapse=""),char5,sep=""),
        paste(paste(rep("\t",6),collapse=""),char6,sep=""))
      if (is.null(input$areamaps.hover$x)) { return(cat(char.out,sep="\n")) }
      lon <- input$areamaps.hover$x; lat <- input$areamaps.hover$y;
      area.index <- as.integer(st_within(st_point(c(lon,lat)),map.shape))
      if (is.na(area.index)) { return(cat(char.out,sep="\n")) }
      area.hover <- as.data.frame(map.shape[area.index,])
      out1 <- paste(char1,ifelse(area.type == "county",area.hover$state_name,
        substr(area.hover[,name.col],1,36)))
      out2 <- paste(char2,ifelse(area.type == "county",area.hover$county_name,""))
      out3 <- paste(char3,area.hover[,grep("dv",colnames(area.hover))])
      out4 <- paste(char4,area.hover[,paste("max4",years[1],sep="_")])
      out5 <- paste(char5,area.hover[,paste("max4",years[2],sep="_")])
      out6 <- paste(char6,area.hover[,paste("max4",years[3],sep="_")])
      ntab <- ((47 - nchar(c(out1,out2))) %/% 8) + 1
      char.out <- c(paste(out1,paste(rep("\t",ntab[1]),collapse=""),out3,sep=""),
        paste(out2,paste(rep("\t",ntab[2]),collapse=""),out4,sep=""),
        paste(paste(rep("\t",6),collapse=""),out5,sep=""),
        paste(paste(rep("\t",6),collapse=""),out6,sep=""))
      return(cat(char.out,sep="\n"))
    }
    
    ## Hover text for site-level design values
    if (input$type.select == "Site-level Design Values") {
      char1 <- "AQS Site ID:"; char2 <- "Site Name:";
      char3 <- "State Name:"; char4 <- paste(area.title,"Name:");
      char5 <- ifelse(years[3] == curr.year,
        paste("Preliminary",years[1],"-",years[3],"Design Value (ppb):"),
        paste(years[1],"-",years[3],"Design Value (ppb):"))
      char6 <- paste(years[1],"4th Highest Daily Maximum Value (ppb):")
      char7 <- paste(years[2],"4th Highest Daily Maximum Value (ppb):")
      char8 <- paste(years[3],"4th Highest Daily Maximum Value (ppb):")
      ntab <- ((47 - nchar(c(char1,char2,char3,char4))) %/% 8) + 1
      char.out <- c(paste(char1,paste(rep("\t",ntab[1]),collapse=""),char5,sep=""),
        paste(char2,paste(rep("\t",ntab[2]),collapse=""),char6,sep=""),
        paste(char3,paste(rep("\t",ntab[3]),collapse=""),char7,sep=""),
        paste(char4,paste(rep("\t",ntab[4]),collapse=""),char8,sep=""))
      if (is.null(input$areamaps.hover$x)) { return(cat(char.out,sep="\n")) }
      box <- unlist(input$areamaps.hover$domain)
      lon <- input$areamaps.hover$x; lat <- input$areamaps.hover$y;
      tol.x <- 0.007*diff(box[1:2]); tol.y <- 0.01*diff(box[3:4]);
      pts <- subset(map.vals,abs(longitude - lon) < tol.x & abs(latitude - lat) < tol.y)
      if (nrow(pts) == 0) { return(cat(char.out,sep="\n")) }
      if (nrow(pts) > 1) {
        dist <- sqrt((pts$longitude - lon)^2 + (pts$latitude - lat)^2)
        pts <- pts[which.min(dist),]
      }
      out1 <- paste(char1,pts$site)
      out2 <- paste(char2,substr(ifelse(pts$site_name == "",pts$address,pts$site_name),1,36))
      out3 <- paste(char3,pts$state_name)
      out4 <- paste(char4,substr(pts[,name.col],1,36))
      out5 <- paste(char5,pts[,grep("dv",colnames(pts))])
      out6 <- paste(char6,pts[,paste("max4",years[1],sep="_")])
      out7 <- paste(char7,pts[,paste("max4",years[2],sep="_")])
      out8 <- paste(char8,pts[,paste("max4",years[3],sep="_")])
      ntab <- ((47 - nchar(c(out1,out2,out3,out4))) %/% 8) + 1
      char.out <- c(paste(out1,paste(rep("\t",ntab[1]),collapse=""),out5,sep=""),
        paste(out2,paste(rep("\t",ntab[2]),collapse=""),out6,sep=""),
        paste(out3,paste(rep("\t",ntab[3]),collapse=""),out7,sep=""),
        paste(out4,paste(rep("\t",ntab[4]),collapse=""),out8,sep=""))
      return(cat(char.out,sep="\n"))
    }
  })
  
  ## Hover to display values in the "Design Value Trends" application
  output$text.dvtrends <- renderPrint({
    if (input$app.select != "Design Value Trends") { return(cat("")) }
    if (input$go.dvtrends == 0) { return(cat("")) }
    cdate <- substr(word.date,1,(nchar(word.date)-6))
    char1 <- "Year:"
    char2 <- "Design Value (ppb):"
    char3 <- "4th High Value (ppb):"
    char4 <- paste("Design Value as of",cdate,"(ppb):")
    char5 <- paste("4th High Value as of",cdate,"(ppb):")
    char6 <- "Max Design Value Site:"
    char7 <- "Max 4th High Site:"
    ntab <- ((47 - nchar(c(char1,char2))) %/% 8) + 1
    char.out <- c(paste(char1,paste(rep("\t",ntab[1]),collapse=""),char6,sep=""),
      paste(char2,paste(rep("\t",ntab[2]),collapse=""),char7,sep=""),char3,char4,char5)
    if (!this.year) { char.out <- char.out[1:3] }
    if (is.null(input$dvtrends.hover$x)) { return(cat(char.out,sep="\n")) }
    year <- floor(input$dvtrends.hover$x + 0.5)
    row <- year - curr.year + nrow(dvtrends.vals)
    dv <- dvtrends.vals$dv[row]
    max4 <- dvtrends.vals$max4[row]
    dv.curr <- dvtrends.vals$dv_curr[row]
    max4.curr <- dvtrends.vals$max4_curr[row]
    dv.site <- dvtrends.vals$dv_site[row]
    max4.site <- dvtrends.vals$max4_site[row]
    out1 <- paste(char1,ifelse(length(year) > 0,year,""))
    out2 <- paste(char2,ifelse(length(dv) > 0,dv,""))
    out3 <- paste(char3,ifelse(length(max4) > 0,max4,""))
    out4 <- paste(char4,ifelse(length(dv.curr) > 0,dv.curr,""))
    out5 <- paste(char5,ifelse(length(max4.curr) > 0,max4.curr,""))
    out6 <- paste(char6,ifelse(length(dv.site) > 0,dv.site,""))
    out7 <- paste(char7,ifelse(length(max4.site) > 0,max4.site,""))
    ntab <- ((47 - nchar(c(out1,out2))) %/% 8) + 1
    char.out <- c(paste(out1,paste(rep("\t",ntab[1]),collapse=""),out6,sep=""),
      paste(out2,paste(rep("\t",ntab[2]),collapse=""),out7,sep=""),out3,out4,out5)
    if (!this.year) { char.out <- char.out[1:3] }
    return(cat(char.out,sep="\n"))
  })
  
  ## Hover to display values in the "Daily AQI Tile Plots" application
  output$text.tileplot <- renderPrint({
    if (input$app.select != "Daily AQI Tile Plots") { return(cat("")) }
    if (is.null(input$naaqs.select)) { return(cat("")) }
    if (input$go.tileplot == 0) { return(cat("")) }
    char1 <- "Date:"; char2 <- "AQI Value:";
    char3 <- "Daily Max 8-hour Value (ppb):"
    if (is.null(input$tileplot.hover$x)) { 
      return(cat(char1,char2,char3,sep="\n")) 
    }
    naaqs <- substr(input$naaqs.select,1,4)
    aqi.vals <- c(0,50,100,150,200,300)
    if (naaqs == "1997") { aqi.breaks <- c(0,64.9,84.9,104.9,124.9,374.9) }
    if (naaqs == "2008") { aqi.breaks <- c(0,59.9,75.9,95.9,115.9,374.9) }
    if (naaqs == "2015") { aqi.breaks <- c(0,54.9,70.9,85.9,105.9,200.9) }
    year <- floor(input$tileplot.hover$y + 0.5)
    row <- floor(input$tileplot.hover$x + 1)
    col <- year - as.numeric(substr(colnames(tileplot.vals)[2],1,4)) + 2
    date <- paste(tileplot.vals$day[row],year,sep="-")
    ppb.val <- tileplot.vals[row,col]
    aqi.break <- sum(aqi.breaks < ppb.val)
    aqi.val <- round((aqi.vals[(aqi.break + 1)] - aqi.vals[aqi.break]) / 
      (aqi.breaks[(aqi.break + 1)] - aqi.breaks[aqi.break]) *
      (ppb.val - aqi.breaks[aqi.break]) + aqi.vals[aqi.break])
    out1 <- paste(char1,ifelse(length(date) > 0,date,""))
    out2 <- paste(char2,ifelse(length(aqi.val) > 0,aqi.val,""))
    out3 <- paste(char3,ifelse(length(ppb.val) > 0,ppb.val,""))
    return(cat(out1,out2,out3,sep="\n"))
  })
  
  ## Hover to display values in the "Cumulative 4th Max Plots" application
  output$text.max4plot <- renderPrint({
    if (input$app.select != "Cumulative 4th Max Plots") { return(cat("")) }
    if (input$go.max4plot == 0) { return(cat("")) }
    char1 <- "Date:"
    char2 <- paste(curr.year,"4th Highest Value (ppb):")
    char3 <- "Historical Minimum 4th High (ppb):"
    char4 <- "Historical Median 4th High (ppb):"
    char5 <- "Historical Maximum 4th High (ppb):"
    if (is.null(input$max4plot.hover$x)) { 
      return(cat(char1,char2,char3,char4,char5,sep="\n")) 
    }
    row <- floor(input$max4plot.hover$x + 1)
    day <- format(as.POSIXlt(paste("2000",max4plot.vals$day[row],sep="-")),"%B %d")
    max4.val <- max4plot.vals$max4[row]; min.val <- max4plot.vals$min.val[row];
    med.val <- max4plot.vals$med.val[row]; max.val <- max4plot.vals$max.val[row];
    out1 <- paste(char1,ifelse(length(day) > 0,day,""))
    out2 <- paste(char2,ifelse(length(max4.val) > 0,max4.val,""))
    out3 <- paste(char3,ifelse(length(min.val) > 0,min.val,""))
    out4 <- paste(char4,ifelse(length(med.val) > 0,med.val,""))
    out5 <- paste(char5,ifelse(length(max.val) > 0,max.val,""))
    return(cat(out1,out2,out3,out4,out5,sep="\n"))
  })
  
  ## Clear global workspace upon exit
  onSessionEnded(function() { rm(list=ls(),envir=sys.frame(0)) })
})
