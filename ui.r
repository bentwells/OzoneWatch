######################################################
## UI function for the Ozone Watch R shiny application
######################################################

## Function to display introductory text in opening window for each application
intro.text <- function() {
  div(p("Ozone Watch is designed to help users keep track of ozone concentrations reported during the
       current ozone season and understand the possible implications for attainment of the National
       Ambient Air Quality Standards (NAAQS) for ozone during the current 3-year design value period.
       The information in Ozone Watch is based on ozone concentration data reported to the EPA's Air
       Quality System (AQS) database, combined with real-time ozone concentration data reported to the
       EPA's AirNow database on days where the data have not yet been submitted to AQS."),
    p("The ozone concentration data for the current ozone season are refreshed each morning, beginning
       on May 1st and continuing throughout the year. Ozone concentration data for prior years are
       retrieved from AQS and refreshed on a monthly basis."),
    p(strong("This preliminary data is intended for informational use only. Data and figures obtained
       from Ozone Watch should NOT be used for regulatory or other official purposes.")),
    p("Ozone Watch consists of five applications, which are listed in the first drop-down menu on the
       left. Additional information specific to each application will be shown beneath this paragraph
       the first time it is selected in the drop-down menu. Users can return to this screen at any
       time by clicking the 'View Intro' button that will appear after content is generated."))
}

## Main UI function
shinyUI(fluidPage(title="Ozone Watch",
    
  ## Apply HTML styles to various parts of the page
  tags$head(tags$style(HTML("#heading-panel { background-color:#80E0FF; padding-left:10px; }"))),
  tags$head(tags$style(HTML("#control-panel { background-color:#E0E0E0; padding:10px; }"))),
  tags$head(tags$style(HTML("#display-panel { background-color:#FFFFFF; }"))),
  tags$head(tags$style(HTML("#dvtables-intro { padding:10px; }"))),
  tags$head(tags$style(HTML("#areamaps-intro { padding:10px; }"))),
  tags$head(tags$style(HTML("#dvtrends-intro { padding:10px; }"))),
  tags$head(tags$style(HTML("#tileplot-intro { padding:10px; }"))),
  tags$head(tags$style(HTML("#max4plot-intro { padding:10px; }"))),
  tags$head(tags$style(HTML("h5 { margin-top: 0px; margin-bottom: 0px; }"))),
  tags$head(tags$style(HTML(".btn-default { color: #fff; background-color: #005ea2; font-weight: 700; }"))),
  tags$head(tags$style(HTML(".shiny-text-output { font-family:'Lucida Console'; font-size:16px; }"))),
  
  ## Header panel showing last updated date (top of page)
  absolutePanel(id="heading-panel",h1(paste("Last Updated:",word.date)),
    top="0px",left="0px",height="80px",width="100%"),
  
  ## Control panel with menus, buttons, and download links (left side)
  absolutePanel(id="control-panel",
    
    ## Drop down menu inputs
    selectInput(inputId="app.select",label=h5(strong("Select an Application:")),
      choices=c("Design Value Tables","Design Value Maps","Design Value Trends",
        "Daily AQI Tile Plots","Cumulative 4th Max Plots"),
      selected="Design Value Tables",width="100%"),
    uiOutput("ui.type"),
    selectInput(inputId="naaqs.select",label=h5(strong("Select an Ozone NAAQS:")),
      choices=c("2015 8-hour (70 ppb)","2008 8-hour (75 ppb)","1997 8-hour (84 ppb)"),
      selected="2015 8-hour (70 ppb)",width="100%"),
    uiOutput("ui.geo"),
    selectInput(inputId="region.select",label=h5(strong("Select an EPA Region:")),
      choices=c("National",paste("EPA Region",c(1:10))),
      selected="National",width="100%"),
    uiOutput("ui.state"),
    uiOutput("ui.out"),
    uiOutput("ui.year"),
    uiOutput("ui.value"),
    uiOutput("ui.maxes"),
    
    ## Add 'Go!' button beneath the menus for each application
    fluidRow(column(width=1,offset=0,
      conditionalPanel(condition="input['app.select'] == 'Design Value Tables'",
        actionButton("go.dvtables","Go!",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Design Value Maps'",
        actionButton("go.areamaps","Go!",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Design Value Trends'",
        actionButton("go.dvtrends","Go!",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Daily AQI Tile Plots'",
        actionButton("go.tileplot","Go!",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Cumulative 4th Max Plots'",
        actionButton("go.max4plot","Go!",width="100px"))),
    
    ## Add 'View Intro' button to the right of the 'Go!' button
    column(width=1,offset=4,
      conditionalPanel(condition="input['app.select'] == 'Design Value Tables' & !output['reset.dvtables']",
        actionButton("reset.dvtables","View Intro",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Design Value Maps' & !output['reset.areamaps']",
        actionButton("reset.areamaps","View Intro",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Design Value Trends' & !output['reset.dvtrends']",
        actionButton("reset.dvtrends","View Intro",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Daily AQI Tile Plots' & !output['reset.tileplot']",
        actionButton("reset.tileplot","View Intro",width="100px")),
      conditionalPanel(condition="input['app.select'] == 'Cumulative 4th Max Plots' & !output['reset.max4plot']",
        actionButton("reset.max4plot","View Intro",width="100px")))),
    br(),
    
    ## Links to download design value tables and images
    conditionalPanel(condition="input['app.select'] == 'Design Value Tables' & !output['reset.dvtables']",
      downloadLink("download.dvtables","Download this Table (CSV)")),
    conditionalPanel(condition="input['app.select'] == 'Design Value Maps' & !output['reset.areamaps']",
      downloadLink("download.areamaps","Download this Image (PNG)"),br(),br(),
      downloadLink("table.areamaps","Download the Data (CSV)")),
    conditionalPanel(condition="input['app.select'] == 'Design Value Trends' & !output['reset.dvtrends']",
      downloadLink("download.dvtrends","Download this Image (PNG)"),br(),br(),
      downloadLink("table.dvtrends","Download the Data (CSV)")),
    conditionalPanel(condition="input['app.select'] == 'Daily AQI Tile Plots' & !output['reset.tileplot']",
      downloadLink("download.tileplot","Download this Image (PNG)"),br(),br(),
      downloadLink("table.tileplot","Download the Data (CSV)")),
    conditionalPanel(condition="input['app.select'] == 'Cumulative 4th Max Plots' & !output['reset.max4plot']",
      downloadLink("download.max4plot","Download this Image (PNG)"),br(),br(),
      downloadLink("table.max4plot","Download the Data (CSV)")),
  top="80px",left="0px",height="840px",width="25%"),
  
  ## Main panel displaying tables and plots
  absolutePanel(id="display-panel",
    
    ## Main panel for "Design Value Tables" application
    conditionalPanel(id="dvtables-intro",
      condition="input['app.select'] == 'Design Value Tables' & output['reset.dvtables']",
      intro.text(),
      p(strong("Design Value Tables Application")),
      p("This application displays tables of preliminary design value information for the current 3-year
         period based on the data available to-date, or design values for previous 3-year periods. The
         first set of buttons on the left allow users to switch between area-level summary information
         based on the ozone monitoring site with the highest design value in each geographic area, and
         more detailed information for each ozone monitoring site."),
      p("The remaining menus allow users to choose which ozone NAAQS the design values will be compared
         with, to choose a geographic area delineation for calculating summary statistics, to subset the
         results to areas or sites within a particular EPA Region or State, or choose which 3-year data
         period to display."),
      p("When the 'Area-level Design Values' option is selected, a final set of buttons allows the user
         to toggle between showing annual 4th highest daily maximum values based on either the monitoring
         site with the highest design value, or the monitoring site with the highest annual value in each
         year. When the 'Site-level Design Values' option is selected, a final menu allows the user to
         further subset the results to a single geographic area such as a county or CBSA."),
      p("Once selections have been made and the 'Go!' button has been pressed, a table will appear in this
         window, and a link will appear beneath the 'Go!' button allowing the user to download the table
         displayed on the screen in a .csv file. Each table may be sorted by clicking the arrow buttons to
         the right of each column heading."),
      p("The 'Critical Value' columns in each area-level table represent the ozone concentration value in
         parts per billion (ppb) which would cause that area to exceed the NAAQS, based on the 4th highest
         daily maximum values from the previous two years. If an area's 4th highest daily maximum value 
         reaches or exceeds the critical value, that area has exceeded the NAAQS.")),
    conditionalPanel(condition="input['app.select'] == 'Design Value Tables' & !output['reset.dvtables']",
      DT::dataTableOutput("display.dvtables")),
    
    ## Main panel for "Design Value Maps" application
    conditionalPanel(id="areamaps-intro",
      condition="input['app.select'] == 'Design Value Maps' & output['reset.areamaps']",
      intro.text(),
      p(strong("Design Value Maps Application")),
      p("This application generates maps of preliminary ozone design values and annual 4th highest daily
         maximum values for the current 3-year period, or for previous 3-year periods. The buttons on the
         left allow the user to switch between maps showing area boundaries filled with colors based on
         the highest monitored value in each area, and maps showing values at individual monitoring sites,
         which are represented by colored dots."),
      p("The remaining menus allow users to choose which ozone NAAQS the design values will be compared
         with, to choose which type of geographic area boundaries will be shown on the map, to display
         localized maps of EPA Regions or States, or to choose which 3-year data period to display."),
      p("When the 'Site-level Design Values' option is selected, an final menu appears which can generate
         localized maps of the site-level values for a single geographic area, such as a county or CBSA.
         Selecting the 'All Sites' option will generate a map of all monitoring sites in an EPA Region or
         State, with all geographic area boundaries of the selected type shaded in light grey."),
      p("Once a selection has been made and the 'Go!' button has been pressed, a map will appear in this
         window, and two links will appear beneath the 'Go!' button. The first link allows the user to
         download the image displayed on the screen as a .png file, and the second link allows the user
         to download the data displayed in the figure as a .csv file. Additionally, the user may hover
         over individual areas or monitoring sites on the map with the mouse, and the values for each
         area or site will appear in a text box beneath the map."),
      p("The blue to cyan colors on the left-hand side of the legend at the bottom of each map represent
         values that are at or below the selected NAAQS, and the yellow to red colors on the right-hand
         side of the legend represent values that are exceeding the NAAQS.")),
    conditionalPanel(condition="input['app.select'] == 'Design Value Maps' & !output['reset.areamaps']",
      plotOutput("display.areamaps",height="720px",width="960px",hover="areamaps.hover")),
    
    ## Main panel for "Design Value Trends" application
    conditionalPanel(id="dvtrends-intro",
      condition="input['app.select'] == 'Design Value Trends' & output['reset.dvtrends']",
      intro.text(),
      p(strong("Design Value Trends Application")),
      p("This application generates plots showing trends in the ozone design value and 4th highest daily
         maximum value for a particular geographic area or monitoring site."),
      p("The menus on the left allow users to choose which ozone NAAQS the values will be compared with,
         to choose which type of geographic area will be assessed in each plot, and to select a specific
         area or monitoring site. The list of options in the final menu can be quite lengthy, so it may
         be helpful to select an EPA Region and/or State first to shorten the list."),
      p("Once a selection has been made and the 'Go!' button has been pressed, a plot will appear in this
         window, and two links will appear beneath the 'Go!' button. The first link allows the user to
         download the image displayed on the screen as a .png file, and the second link allows the user
         to download the data displayed in the figure as a .csv file. Additionally, the user may hover
         over the plot with the mouse, and the design value and 4th highest daily maximum value for that
         year will appear in a text box beneath the plot."),
      p("The solid orange line on the plot represents the 4th highest daily maximum value for each year,
         and the solid blue line on the plot represents the design value for the 3-year period ending in
         that year. The dotted orange line represents the 4th highest daily maximum value for each year
         based on only the days on or before the current update. For example, if the date at the top of
         the screen is June 30th, the line represents the 4th highest value as of June 30th for each year.
         Over the course of the ozone season, the dotted orange line will move closer to the solid orange
         line. At the end of the ozone season, the dotted and solid lines will be identical."),
      p("Similarly, the dotted blue line represents the design value as of the last update for each year.
         This is calculated as the average of the dotted orange line for the current year and the solid
         orange line for the previous two years. This line will also move closer to the solid blue line
         as the ozone season progresses. Finally, the solid black line on the plot represents the level
         of the selected ozone NAAQS.")),
    conditionalPanel(condition="input['app.select'] == 'Design Value Trends' & !output['reset.dvtrends']",
      plotOutput("display.dvtrends",height="720px",width="960px",hover="dvtrends.hover")),
    
    ## Main panel for "Daily AQI Tile Plots" application
    conditionalPanel(id="tileplot-intro",
      condition="input['app.select'] == 'Daily AQI Tile Plots' & output['reset.tileplot']",
      intro.text(),
      p(strong("Daily AQI Tile Plots Application")),
      p("This application generates 'tile plots', or a grid of colored tiles, where the color of each
         tile represents the ozone Air Quality Index (AQI) color based on the 8-hour daily maximum value
         for that day. These plots can be useful for identifying seasonal patterns or long-term trends
         in the daily data for a geographic area or monitoring site, or identifying which days had the
         worst ozone air quality in a given year. The rows in each tile plot represent calendar years,
         and the columns represent individual days within the year. The daily AQI values for geographic
         areas with multiple ozone monitoring sites are determined based on the site with the highest
         daily maximum 8-hour value for that day."),
      p("The menus on the left allow users to choose which ozone NAAQS will be used to determine the AQI
         breakpoints, to choose which type of geographic area will be assessed in the plot, and to select
         a specific area or monitoring site. The list of options in the final menu can be quite lengthy,
         so it may be helpful to select an EPA Region and/or State first to shorten the list. Most users
         will want to keep the default ozone NAAQS selection of '2015 8-hour (70 ppb)', since the current
         ozone AQI breakpoints are based on that standard."),
      p("Once a selection has been made and the 'Go!' button has been pressed, a plot will appear in this
         window, and two links will appear beneath the 'Go!' button. The first link allows the user to
         download the image displayed on the screen as a .png file, and the second link allows the user
         to download the data displayed in the figure as a .csv file. Additionally, the user may hover over
         the plot with the mouse, and the daily maximum 8-hour ozone concentration value and associated AQI
         value for the selected date will appear in a text box benath the plot.")),
    conditionalPanel(condition="input['app.select'] == 'Daily AQI Tile Plots' & !output['reset.tileplot']",
      plotOutput("display.tileplot",height="720px",width="960px",hover="tileplot.hover")),
    
    ## Main panel for "Cumulative 4th Max Plots" application
    conditionalPanel(id="max4plot-intro",
      condition="input['app.select'] == 'Cumulative 4th Max Plots' & output['reset.max4plot']",
      intro.text(),
      p(strong("Cumulative 4th Max Plots Application")),
      p("This application generates plots showing how the 4th highest 8-hour daily maximum value for a
         particular geographic area or monitoring site has changed throughout the course of the year.
         This information can be useful during the spring and summer months for determining whether or
         not a particular site or area is likely to exceed the NAAQS before the end of the year. For
         example, if the design value for a given area is already close to the level of the NAAQS in May,
         then it is likely that the area will exceed the NAAQS before the end of the year, whereas if an
         area is still well below the NAAQS in September, then that area is unlikely to exceed the NAAQS
         this year."),
      p("The menus on the left allow users to choose which ozone NAAQS the values will be compared with,
         to choose which type of geographic area will be assessed in the plot, and to select a specific
         area or monitoring site. The list of options in the final menu can be quite lengthy, so it may
         be helpful to select an EPA Region and/or State first to shorten the list."),
      p("Once a selection has been made and the 'Go!' button has been pressed, a plot will appear in this
         window, and two links will appear beneath the 'Go!' button. The first link allows the user to
         download the image displayed on the screen as a .png file, and the second link allows the user
         to download the data displayed in the figure as a .csv file. Additionally, the user may hover
         over the plot with the mouse, and the 4th highest daily maximum value for the associated day of
         the year will appear in a text box beneath the plot."),
      p("The green line on the plot represents the 4th highest daily maximum value for the area or site
         for each day to-date for the current year, while the blue lines with shaded interior represent
         the historical range of the 4th highest daily maximum value over the past 12 years. This makes
         for an easy comparison of the 4th highest daily maximum value for the current year with historical
         fluctuations. Note that for geographic areas, the green line is based on the highest monitoring
         site in the area on each day."),
      p("The dotted black line on each plot represents the level of the selected NAAQS, and the red line
         represents the 'Critical Value' for that area or site. If the green line reaches or crosses the
         red line, that means the area or site has exceeded the NAAQS for the current 3-year period.")),
    conditionalPanel(condition="input['app.select'] == 'Cumulative 4th Max Plots' & !output['reset.max4plot']",
      plotOutput("display.max4plot",height="720px",width="960px",hover="max4plot.hover")),
    top="80px",left="25%",height="720px",width="75%"),
  
  ## Hover text panel (bottom of page)
  absolutePanel(id="text-panel",
    conditionalPanel(condition="input['app.select'] == 'Design Value Maps' & !output['reset.areamaps']",
      verbatimTextOutput("text.areamaps")),
    conditionalPanel(condition="input['app.select'] == 'Design Value Trends' & !output['reset.dvtrends']",
      verbatimTextOutput("text.dvtrends")),
    conditionalPanel(condition="input['app.select'] == 'Daily AQI Tile Plots' & !output['reset.tileplot']",
      verbatimTextOutput("text.tileplot")),
    conditionalPanel(condition="input['app.select'] == 'Cumulative 4th Max Plots' & !output['reset.max4plot']",
      verbatimTextOutput("text.max4plot")),
    top="800px",left="25%",height="120px",width="960px")
))