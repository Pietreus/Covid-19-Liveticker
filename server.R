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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    covidData <- getData()
    
    covid_plot <- group_by(covidData,ISO3) %>% 
        filter(row_number() == 1) %>%
        summarise(cases = casesCum,  
                  deaths = deathsCum,
                  active = active,
                  labelText = paste("<p>", ISO3, "<br>Total Cases: ", casesCum, " <br>Deaths: ", deathsCum,"<br>Active Cases: ", active, "</p>", sep= ""))
    covid_long <- pivot_longer(data = covidData, cols = -c(ISO3,date),names_to = "Type",values_to = "Amount",values_drop_na = TRUE)
    # covid_long$Type <- reorder(as.factor(covid_long$Type),c("deathCum","casesCum","recovCum","deathNew","casesNew","recovNew","active","testsCum","testsNew"))
    # covid_long$Type <- relevel(as.factor(covid_long$Type),"recovCum")
    # covid_long$Type <- relevel(as.factor(covid_long$Type),"recovNew")
    # covid_long$Type <- relevel(as.factor(covid_long$Type),"deathsCum")
    # covid_long$Type <- relevel(as.factor(covid_long$Type),"deathsNew")
    covid_long_cum <- covid_long %>% filter(Type %in% c("casesCum","deathsCum","recovCum"))
    covid_long_new <- covid_long %>% filter(Type %in% c("casesNew","deathsNew","recovNew"))
    
    world_spdf <- readOGR(
      dsn = paste0(getwd(),"/data/") , 
      layer="TM_WORLD_BORDERS_SIMPL-0.3",
      verbose=FALSE)
    world_spdf@data <- left_join(world_spdf@data, covid_plot, by = c("ISO3" = "ISO3"))
    
    output$worldMap <- renderLeaflet(drawMap(input$dataType,world_spdf))
    
    observeEvent(input$worldMap_shape_click, updateMaps())
    observeEvent(input$scaleType, updateMaps())
    
    #output$worldMap <- renderLeaflet(myMap)
    output$countryVector <- renderText(as.character(world_spdf@data$NAME))

      # update the location selectInput on map clicks
      
      updateMaps <- function(){
      
        if(!is.null(input$worldMap_shape_click)){
          p <- input$worldMap_shape_click
        }else{
          p <- data.frame(id = "AUT")
        }
        output$country <- renderText(p$id) #p$id
        print(p)
      cplot <- filter(covid_long_cum, ISO3 == p$id & Amount >= 1) %>%
        ggplot(aes(x = date, y = Amount, color = Type)) +
        geom_line()
      if(input$scaleType=="log"){
        cplot <- cplot + scale_y_log10()
      }
      output$cumPlot <- renderPlotly({
        ggplotly(cplot,dynamicTicks = TRUE) %>% layout(hovermode = 'compare')
      })
      nplot <- filter(covid_long_new, ISO3 == p$id & Amount >= 10) %>%
        ggplot() +
        geom_bar(aes(x = date,y = Amount, fill = Type),stat='identity',position = 'dodge')
      output$dayPlot <- renderPlotly({
        ggplotly(nplot,dynamicTicks = TRUE) %>% layout(hovermode = 'compare')
      })
    }  
})

getData <- function(){
    if(file.exists("./data/covid.RData")){
        load("./data/covid.RData")
        if(max(covidData$date)==ymd(Sys.Date())){
            return(covidData)
        }
    }
    
    dataRaw <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
      mutate(date = ymd(date),ISO3 = as.character(iso_code)) #cases,deaths,tests,new and cumsum, abs and per mil/tsd
    datarecov <- read.csv("https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv") %>% 
        transmute(ISO3 = countrycode(Country.Region, origin = "country.name", destination = "iso3c"),
                  date = ymd(Date),
                  recovCum = Recovered) %>%
        group_by(ISO3,date) %>%
        filter(row_number() == 1) %>% 
        ungroup() %>%
        group_by(ISO3) %>%
        mutate(recovNew = recovCum - lead(recovCum,1,default = 0, order_by = desc(date)))
    covidData <- left_join(dataRaw,datarecov, by = c("ISO3" = "ISO3","date" = "date")) %>%
        fill(recovCum, .direction = "up") %>%
        transmute(ISO3 = iso_code, date = ymd(date),
                  casesCum = total_cases, casesNew = new_cases,
                  deathsCum = total_deaths, deathsNew = new_deaths,
                  recovCum, recovNew,
                  testsCum = total_tests, testsNew = new_tests,
                  active = total_cases - recovCum - deathsCum) %>%
        arrange(desc(date))
    save('covidData',file = "./data/covid.RData")
}

drawMap <- function(dataType,world_spdf){
  
  if(dataType=="Cases"){
    paletteString <- "YlOrBr"
    domainData <- world_spdf@data$cases
  }else if(dataType=="Deaths"){
    paletteString <- "Greys"
    domainData <- world_spdf@data$deaths
  }else{
    paletteString <- "YlGnBu"
    domainData <- world_spdf@data$active
  }
  
  mybins <- c(0,10000,20000,50000,100000,500000,Inf)
  mypalette <- colorBin(palette=paletteString, domain=domainData, na.color="transparent", bins=mybins)
  #renderPlot({
  leaflet(world_spdf) %>%
    setView( lat=10, lng=0 , zoom=1) %>%
    addTiles() %>% 
    addPolygons(stroke = FALSE,
                layerId = world_spdf@data$ISO3,
                fillOpacity = 0.8, 
                smoothFactor = 0.5,
                color = ~mypalette(domainData),
                label = lapply(world_spdf@data$labelText, htmltools::HTML)) %>%
    addLegend(pal = mypalette, values=~domainData, opacity=0.9, title = dataType, position = "bottomleft")
  
}
