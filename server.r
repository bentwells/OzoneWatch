##########################################################
## Server function for the Ozone Watch R shiny application
##########################################################
options(stringsAsFactors=FALSE)
require(shiny,quietly=TRUE,warn.conflicts=FALSE)
require(DT,quietly=TRUE,warn.conflicts=FALSE)

shinyServer(function(input,output) {
  ## Load most recent site-level design values file
  files <- isolate(list.files("data"))
  load(paste("data",rev(files[grep("sitedv",files)])[1],sep="/"),envir=sys.frame(0))
  
  #######################################################################
  ## Logic controlling reactive drop-down menus in the left-hand UI panel
  #######################################################################
  
  ## Create a switch for area-level vs. site-level design values
  output$ui.type <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (!grepl("Design Value",input$app.select)) { return() }
    radioButtons(inputId="type.select",label=NULL,
      choices=c("Area-level Design Values","Site-level Design Values"),
      selected="Area-level Design Values")
  })
  
  ## Create an Ozone NAAQS selection input
  output$ui.naaqs <- renderUI({
    if (is.null(input$app.select)) { return() }
    naaqs.default <- "2015 8-hour (70 ppb)"
    if (grepl("Design Value",input$app.select)) {
      naaqs.default <- "2008 8-hour (75 ppb)"
    }
    selectInput(inputId="naaqs.select",label="Select an Ozone NAAQS:",
      choices=c("2015 8-hour (70 ppb)","2008 8-hour (75 ppb)","1997 8-hour (84 ppb)"),
      selected=naaqs.default)
  })
  
  ## Create a geography selection input
  output$ui.geo <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (is.null(input$naaqs.select)) { return() }
    geo.choices <- c("AQS Site ID","State/County","Core Based Statistical Area (CBSA)",
      "Combined Statistical Area (CSA)","Nonattainment Area (NAA)")
    geo.default <- "Nonattainment Area (NAA)"
    if (grepl("Design Value",input$app.select)) {
      geo.choices <- geo.choices[geo.choices != "AQS Site ID"]
    }
    if (input$naaqs.select == "2015 8-hour (70 ppb)") {
      geo.choices <- geo.choices[geo.choices != "Nonattainment Area (NAA)"]
      geo.default <- "State/County"
    }
    selectInput(inputId="geo.select",label="Select a Geographic Area Type:",
      choices=geo.choices,selected=geo.default)
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
    if (grepl("Design Value",input$app.select)) {
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
      if (grepl("Design Value",input$app.select)) { return() }
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
      if (naaqs == "2015") { return() }
      out.label <- "Select a NAA:"
      vals <- subset(sites,naa_name != " " & !duplicated(naa_name),"naa_name")
      out.choices <- vals$naa_name[order(vals$naa_name)]
    }
    out.default <- switch(substr(input$app.select,1,6),Design="All Sites",
      "Please make a selection!")
    selectInput(inputId="out.select",label=out.label,
      choices=c(out.default,out.choices),selected=out.default)
  })
  
  ## Select which values to display in the "Design Value Maps" application
  output$ui.value <- renderUI({
    if (is.null(input$app.select)) { return() }
    if (is.null(input$naaqs.select)) { return() }
    if (input$app.select != "Design Value Maps") { return() }
    naaqs <- substr(input$naaqs.select,1,4)
    cnames <- colnames(eval(parse(text=paste("sitedv",naaqs,sep="."))))
    years <- as.numeric(substr(cnames[grep("max4",cnames)],6,9))
    val.choices <- c(paste("dv",years[1],years[3],sep="_"),paste("max4",years,sep="_"))
    names(val.choices) <- c(paste("Preliminary",years[1],"-",years[3],"Design Value"),
        paste(years,"4th Highest Daily Maximum Value"))
    selectInput(inputId="value.select",label="Select which values to display:",
      choices=val.choices,selected=val.choices[1])
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
        region=input$region.select,state=input$state.select,out=input$out.select),
      file,row.names=FALSE,na="")
  })
  
  ## Download images from the "Design Value Maps" application as PNG files
  output$download.areamaps <- downloadHandler(
    filename=function() { return(paste("Ozone_Watch_",
    gsub(" ","_",gsub("-","",gsub(":","",Sys.time()))),".png",sep="")) },
    content=function(file) {
      png(file,width=960,height=720)
      area.maps(naaqs=input$naaqs.select,type=input$type.select,geo=input$geo.select,
        region=input$region.select,state=input$state.select,out=input$out.select,
        value=input$value.select)
      dev.off()
  })
  
  ## Download images from the "Cumulative 4th Max Plot" application as PNG files
  output$download.max4plot <- downloadHandler(
    filename=function() { return(paste("Ozone_Watch_",
    gsub(" ","_",gsub("-","",gsub(":","",Sys.time()))),".png",sep="")) },
    content=function(file) {
      png(file,width=960,height=720)
      max4.plot(naaqs=input$naaqs.select,geo=input$geo.select,
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
  
  ####################################################################
  ## Logic controlling tables and plots displayed in the main UI panel
  ####################################################################
  
  ## Display tables in the "Design Value Tables" application
  output$display.dvtables <- DT::renderDataTable({
    input$go.dvtables
    inputs <- isolate({
      list(naaqs=input$naaqs.select,type=input$type.select,geo=input$geo.select,
        region=input$region.select,state=input$state.select,out=input$out.select)
    })
    withProgress({
      if (!exists("dv.tables")) { source("dvtables.r",local=sys.frame(0)) }
      table.out <- dv.tables(naaqs=inputs$naaqs,type=inputs$type,geo=inputs$geo,
        region=inputs$region,state=inputs$state,out=inputs$out)
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
        value=input$value.select)
    })
    withProgress({
      if (!exists("area.maps")) { source("areamaps.r",local=sys.frame(0)) }
      area.maps(naaqs=inputs$naaqs,type=inputs$type,geo=inputs$geo,region=inputs$region,
        state=inputs$state,out=inputs$out,value=inputs$value)
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
      if (!exists("max4.plot")) { source("max4plot.r",local=sys.frame(0)) }
      max4.plot(naaqs=inputs$naaqs,geo=inputs$geo,state=inputs$state,out=inputs$out)
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
      if (!exists("tile.plot")) { source("tileplot.r",local=sys.frame(0)) }
      tile.plot(naaqs=inputs$naaqs,geo=inputs$geo,state=inputs$state,out=inputs$out)
    },message="Loading...",value=NULL,detail=NULL)
  },width=960,height=720)
  
  ##################################################################
  ## Logic controlling text displayed in the panel beneath the plots
  ##################################################################

  ## Hover to display values in the "Design Value Maps" application
  output$text.areamaps <- renderPrint({
    if (input$app.select != "Design Value Maps") { return(cat("")) }
    if (is.null(input$naaqs.select)) { return(cat("")) }
    if (is.null(input$geo.select)) { return(cat("")) }
    if (input$go.areamaps == 0) { return(cat("")) }
    naaqs <- substr(input$naaqs.select,1,4)
    cnames <- colnames(eval(parse(text=paste("sitedv",naaqs,sep="."))))
    years <- as.numeric(substr(cnames[grep("max4",cnames)],6,9))
    area.type <- switch(substr(input$geo.select,1,3),
      Sta="county",Cor="cbsa",Com="csa",Non="naa")
    area.title <- ifelse(area.type == "county","County",toupper(area.type))
    if (naaqs == "2015" & area.type == "naa") { return() }
    name.col <- paste(area.type,"name",sep="_")
    
    ## Hover text for area-level design values
    if (input$type.select == "Area-level Design Values") {
      char1 <- ifelse(area.type == "county","State Name:",paste(area.title,"Name:"))
      char2 <- ifelse(area.type == "county","County Name:","")
      char3 <- paste("Preliminary",years[1],"-",years[3],"Design Value (ppb):")
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
      area.hover <- over(SpatialPoints(matrix(c(lon,lat),ncol=2)),map.shape)
      if (is.na(area.hover[,name.col])) { return(cat(char.out,sep="\n")) }
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
      char5 <- paste("Preliminary",years[1],"-",years[3],"Design Value (ppb):")
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
  
  ## Hover to display values in the "Cumulative 4th Max Plots" application
  output$text.max4plot <- renderPrint({
    if (input$app.select != "Cumulative 4th Max Plots") { return(cat("")) }
    if (is.null(input$naaqs.select)) { return(cat("")) }
    if (input$go.max4plot == 0) { return(cat("")) }
    naaqs <- substr(input$naaqs.select,1,4)
    cnames <- colnames(eval(parse(text=paste("sitedv",naaqs,sep="."))))
    dv.year <- as.numeric(substr(cnames[grep("dv",cnames)],9,12))
    char1 <- "Date:"
    char2 <- paste(dv.year,"4th Highest Value (ppb):")
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
    out3 <- paste(char3,ifelse(length(min.val) > 0, min.val,""))
    out4 <- paste(char4,ifelse(length(med.val) > 0, med.val,""))
    out5 <- paste(char5,ifelse(length(max.val) > 0, max.val,""))
    return(cat(out1,out2,out3,out4,out5,sep="\n"))
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
    hi <- which(aqi.breaks > ppb.val)[1]
    lo <- sum(aqi.breaks < ppb.val)
    aqi.val <- round((aqi.vals[hi] - aqi.vals[lo])/(aqi.breaks[hi] - aqi.breaks[lo]) *
      (ppb.val - aqi.breaks[lo]) + aqi.vals[lo])
    out1 <- paste(char1,ifelse(length(date) > 0,date,""))
    out2 <- paste(char2,ifelse(length(aqi.val) > 0,aqi.val,""))
    out3 <- paste(char3,ifelse(length(ppb.val) > 0,ppb.val,""))
    return(cat(out1,out2,out3,sep="\n"))
  })
})
