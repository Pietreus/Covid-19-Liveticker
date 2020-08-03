#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html map
#https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases/resource/fc5fddb7-2f59-4809-a023-8ed33c41012a datasets
#
library(shiny)
library(tictoc)
library(leaflet)
library(maps)
library(dplyr)
library(rgdal)
library(openxlsx)
library(RColorBrewer)
library(htmltools)
library(countrycode)
library(plotly)
library(lubridate)
library(tidyr)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  tic("getData")
  covidData <- getData()
  toc()
  
  tic("drawMap")
  output$worldMap <-
    renderLeaflet(drawMap(input$dataType, covidData))
  toc()
  
  tic("UpdateMaps events")
  observeEvent(input$worldMap_shape_click, updateMaps(input$plotType))
  observeEvent(input$scaleType, updateMaps(input$plotType))
  observeEvent(input$plotType, updateMaps(input$plotType))
  observeEvent(input$selectedCountry, updateMaps(input$plotType,TRUE))
  toc()
  
  # update the location selectInput on map clicks
  updateMaps <- function(displayType,dropDown=FALSE) {
    tic("inside updateMaps")
    if(dropDown){
      p <- data.frame(id = countrycode(input$selectedCountry, origin = "country.name", destination = "iso3c"))
    }else{
      if (!is.null(input$worldMap_shape_click)) {
        p <- input$worldMap_shape_click
      } else{
        p <- data.frame(id = "AUT")
      }
      if(!p$id %in% covidData$ISO3){return()}
      print(p$id)
      output$countryVector <- renderUI({
        selectInput('selectedCountry',
                    'Country',
                    countrycode(unique(covidData$Country.Region),origin = "country.name", destination = "country.name"),
                    selected = countrycode(p$id, origin = "iso3c", destination = "country.name"))})
    }
    output$summary <- renderText(countrySummary(covidData,p$id))
    # options(repr.plot.width = 1, repr.plot.height = 10)
    detailPlot <- filter(covidData,ISO3 == p$id & amount >=1 & mode == displayType) %>%
      ggplot(aes(x = date, y = amount)) +
      labs(
        title = paste0(displayType," Covid-19 cases in ",countrycode(p$id, origin = "iso3c", destination = "country.name")),
        subtitle = paste0("data as of ", today()))
    if (displayType == "Total") {
      detailPlot <- detailPlot + geom_line(aes(color = type))
      if (input$scaleType == "log") {
        detailPlot <- detailPlot + scale_y_log10()
      }
    }else{
      detailPlot <- detailPlot +
        geom_bar(aes(fill = type),
                 stat = 'identity',
                 position = position_dodge2())
    }
    output$detailPlot <- renderPlotly({
      ggplotly(detailPlot, dynamicTicks = !(input$scaleType == "log" & displayType=="Total")) %>% layout(hovermode = 'compare')
    })
    toc()
  }
  
})

getData <- function(){
  #cache data
  if (file.exists("./data/covid.RData")) {
    load("./data/covid.RData")
    if (max(covidData$date) == today()-days(1)) {
      return(covidData)
    }
  }
  
  #download raw data from johns hopkins and reformat
  
  casesraw <- 
    read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/
             csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
    pivot_longer(-c(Province.State,Country.Region,Lat,Long),names_to = "date") %>% mutate(type = "cases")
  
  deathsraw <- 
    read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/
                        csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
    pivot_longer(-c(Province.State,Country.Region,Lat,Long),names_to = "date") %>% mutate(type = "deaths")
  
  recovraw <- 
    read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/
                       csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>% 
    pivot_longer(-c(Province.State,Country.Region,Lat,Long),names_to = "date") %>% mutate(type = "recovered")
  
  
  totalData <- bind_rows(casesraw,deathsraw,recovraw) %>% 
    mutate(ISO3 = countrycode(Country.Region, origin = "country.name", destination = "iso3c")) %>%
    #get one column for every country
    group_by(ISO3,Country.Region,date,type) %>%
    summarise(amount = sum(value)) %>%
    ungroup() %>%
    mutate(date = mdy(substring(date,2)),mode = "Total")
  
  #calculate daily cases
  dailyData <- totalData %>% 
    group_by(Country.Region,ISO3,type) %>%
    mutate(amount = amount - lead(amount, 1, default = 0, order_by = desc(date)),mode = "Daily") %>%
    ungroup()
  
  #active cases = cases - deaths - recovered, data is presorted so simple diff is possible
  activeData <- totalData %>%
    group_by(Country.Region,ISO3,date) %>%
    summarise(amount = amount[1] - amount[2] - amount[3],type = "active", mode = "Active")
  
  
  covidData <- bind_rows(dailyData,totalData,activeData)
  #save for caching
  save('covidData', file = "./data/covid.RData")
  return(covidData)
}

drawMap <- function(datatype, covidData) {
  tic("inside drawmap")
  #read cached data if possible
  if (file.exists("./data/worldData.RData")) {
    load("./data/worldData.RData")
  }else{
    #use wider format for geojson data
    covid_plot <- 
      group_by(covidData, ISO3,type) %>%
      filter(mode != "Daily",row_number() == n()) %>%
      transmute( 
        ISO3 = ISO3,
        labelText = Country.Region,
        amount = amount,
        type = type) %>% pivot_wider(names_from = type,values_from = amount)
    
    #download geojsons and add covid data
    download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="data/world_shape_file.zip")
    system("unzip data/world_shape_file.zip")
    world_spdf <- readOGR(
      dsn = paste0(getwd(), "/data/") ,
      layer = "TM_WORLD_BORDERS_SIMPL-0.3",
      verbose = FALSE
    )
    world_spdf@data <-
      left_join(world_spdf@data, covid_plot, by = c("ISO3" = "ISO3"))
    world_spdf@data$labelText[is.na(world_spdf@data$labelText)] <- "data missing"
    save('world_spdf', file = "./data/worldData.RData")
  }
  
  if (datatype == "Cases") {
    paletteString <- "YlOrBr"
    domainData <- world_spdf@data$cases
  } else if (datatype == "Deaths") {
    paletteString <- "Greys"
    domainData <- world_spdf@data$deaths
  } else{
    paletteString <- "YlGnBu"
    domainData <- world_spdf@data$active
  }
  #calculate nice upper limit for the map
  
  maxval <- max(domainData,na.rm = TRUE)
  power <- floor(log10(maxval))
  
  mybins <- c(0,signif(quantile(domainData,seq(0.175,0.975,0.2), na.rm = T),digits = 1),ceiling(maxval/10^power)*10^power)
  # mybins <- c(0, 10000, 20000, 50000, 100000, 500000, Inf)
  mypalette <-
    colorBin(
      palette = paletteString,
      domain = domainData,
      na.color = "transparent",
      bins = mybins
    )
  #renderPlot({
  map <- leaflet(world_spdf) %>%
    setView(lat = 10, lng = 0 , zoom = 1) %>%
    addTiles() %>%
    addPolygons(
      stroke = F,
      layerId = world_spdf@data$ISO3,
      fillOpacity = 0.8,
      smoothFactor = 0.5,
      color = ~ mypalette(domainData),
      label = lapply(world_spdf@data$labelText, htmltools::HTML)
    ) %>%
    addLegend(
      pal = mypalette,
      values =  ~ domainData,
      opacity = 0.9,
      title = datatype,
      position = "bottomleft"
    )
  toc()
  return(map)
  
}

countrySummary <- function(data,ISO3,date=today()-days(1)){
  relevantdata <- data[data$ISO3 == ISO3,] %>% group_by(type) %>% filter(row_number()==n(),mode!="Daily")
  paste0(
    "<table>
    <caption>",ISO3,"</caption>
    <tr>
    <td style='padding-right: 10px;'>Total Cases </td>
    <td>",relevantdata$amount[relevantdata$type=="cases"],"</td>
    </tr>
    <tr>
    <td style='padding-right: 10px;'>Recovered</td>
    <td>",relevantdata$amount[relevantdata$type=="recovered"],"</td>
    </tr>
    <tr>
    <td style='padding-right: 10px;'>Active Cases</td>
    <td>",relevantdata$amount[relevantdata$type=="active"],"</td>
    </tr>
    <tr>
    <td style='padding-right: 10px;'>Total Deaths  </td>
    <td>",relevantdata$amount[relevantdata$type=="deaths"],"</td>
    </table>"
  )
}
