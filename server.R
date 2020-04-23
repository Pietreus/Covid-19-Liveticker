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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    observeEvent(input$worldMap_shape_click, { # update the location selectInput on map clicks
        p <- input$worldMap_shape_click
        output$country <- renderText(p$id) #p$id 
        print(p)
        output$cumPlot <- renderPlotly({
            plot_ly(type = "scatter", data = covidData[covidData$ISO3 == p$id & covidData$casesCum >= 10 ,], x = ~date, y= ~casesCum, mode = 'lines', name = 'cases') %>%
                add_trace(x = ~date, y = ~deathsCum, name = 'deaths') %>%
                add_trace(x = ~date, y = ~active, name = 'active Cases') %>%
                layout(plot, yaxis = list(type = "log"))
        })
        output$dayPlot <- renderPlotly({
            plot_ly(type = "bar", data = covidData[covidData$ISO3 == p$id & covidData$casesCum >= 10 ,], x = ~date, y= ~casesNew, mode = 'lines', name = 'cases') %>%
                add_trace(x = ~date, y = ~deathsNew, name = 'deaths')
                
        })
    })
    #observe({ # update the location selectInput on map clicks
    #    p <- input$worldMap_shape_click
    #    output$country <- p$id
    #    print(p)
    #})
    covidData <- getData()
    
     
    covid_plot <- group_by(covidData,ISO3) %>% 
        filter(row_number() == 1) %>%
        summarise(cases = casesCum,  
                  deaths = deathsCum,
                  active = active,
                  labelText = paste("<p>", ISO3, "<br>Total Cases: ", casesCum, " <br>Deaths: ", deathsCum,"<br>Active Cases: ", active, "</p>", sep= ""))
    
    world_spdf <- readOGR( 
        dsn= paste0(getwd(),"/data/") , 
        layer="TM_WORLD_BORDERS_SIMPL-0.3",
        verbose=FALSE)
    world_spdf@data <- left_join(world_spdf@data, covid_plot, by = c("ISO3" = "ISO3"))
    
    mybins <- c(0,10000,20000,50000,100000,500000,Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$cases, na.color="transparent", bins=mybins)
        #renderPlot({
    myMap <- leaflet(world_spdf) %>%
        setView( lat=10, lng=0 , zoom=1) %>%
        addTiles() %>% 
        addPolygons(stroke = FALSE,
                    layerId = world_spdf@data$ISO3,
                    fillOpacity = 0.8, 
                    smoothFactor = 0.5,
                    color = ~mypalette(cases),
                    label = lapply(world_spdf@data$labelText, htmltools::HTML)) %>%
        addLegend(pal = mypalette, values=~cases, opacity=0.9, title = "Cases", position = "bottomleft")
        
    output$worldMap <- renderLeaflet(myMap)
    output$countryVector <- renderText(as.character(world_spdf@data$NAME))
    
    
    
    output$cumPlot <- renderPlotly({
        plot_ly(type = "scatter", data = covidData[covidData$ISO3 == "AUT" &covidData$casesCum > 0 ,], x = ~date, y= ~casesCum)
    })    
        

})

getData <- function(){
    #casesDataRaw <- read.csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&merge-replace02=on&merge-overwrite02=on&filter03=explode&explode-header-att03=date&explode-value-att03=value&filter04=rename&rename-oldtag04=%23affected%2Bdate&rename-newtag04=%23date&rename-header04=Date&filter05=rename&rename-oldtag05=%23affected%2Bvalue&rename-newtag05=%23affected%2Binfected%2Bvalue%2Bnum&rename-header05=Value&filter06=clean&clean-date-tags06=%23date&filter07=sort&sort-tags07=%23date&sort-reverse07=on&filter08=sort&sort-tags08=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv",
    #                         comment.char = "#")
    #deathDataRaw <- read.csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&merge-replace02=on&merge-overwrite02=on&filter03=explode&explode-header-att03=date&explode-value-att03=value&filter04=rename&rename-oldtag04=%23affected%2Bdate&rename-newtag04=%23date&rename-header04=Date&filter05=rename&rename-oldtag05=%23affected%2Bvalue&rename-newtag05=%23affected%2Binfected%2Bvalue%2Bnum&rename-header05=Value&filter06=clean&clean-date-tags06=%23date&filter07=sort&sort-tags07=%23date&sort-reverse07=on&filter08=sort&sort-tags08=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv",
    #                         comment.char = "#")
    #recoveredDataRaw <- read.csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_recovered_global_narrow.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&merge-replace02=on&merge-overwrite02=on&filter03=explode&explode-header-att03=date&explode-value-att03=value&filter04=rename&rename-oldtag04=%23affected%2Bdate&rename-newtag04=%23date&rename-header04=Date&filter05=rename&rename-oldtag05=%23affected%2Bvalue&rename-newtag05=%23affected%2Binfected%2Bvalue%2Bnum&rename-header05=Value&filter06=clean&clean-date-tags06=%23date&filter07=sort&sort-tags07=%23date&sort-reverse07=on&filter08=sort&sort-tags08=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv",
    #                             comment.char = "#")
    dataRaw <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% mutate(date = ymd(date),ISO3 = as.character(iso_code)) #cases,deaths,tests,new and cumsum, abs and per mil/tsd
    datarecov <- read.csv("https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv") %>% 
        transmute(ISO3 = countrycode(Country.Region, origin = "country.name", destination = "iso3c"),
                  date = ymd(Date),
                  recovCum = Recovered) %>%
        group_by(ISO3,date) %>%
        filter(row_number() == 1) %>% 
        ungroup() %>%
        group_by(ISO3) %>%
        mutate(recovNew = recovCum - (lead(recovCum,1,default = 0, order_by = desc(date))))
    covidData <- left_join(dataRaw,datarecov, by = c("ISO3" = "ISO3","date" = "date")) %>%
        fill(recovCum, .direction = "down") %>%
        transmute(ISO3 = iso_code, date = ymd(date),
                  casesCum = total_cases, casesNew = new_cases,
                  deathsCum = total_deaths, deathsNew = new_deaths,
                  recovCum, recovNew,
                  testsCum = total_tests, testsNew = new_tests,
                  active = total_cases - recovCum) %>%
        arrange(desc(date))
    
    #Deaths,recovered,cases, no ISO3
    #probably use datahelp for recovered data and calculate active, and recovered per day from there
    #simply rename the data for convenience, problematic countries: "US","Taiwan*"."Korea, South" and "Czechia"
    #maybe with that :)
    #countrycode::countrycode(sourcevar = "Korea,Sourth",origin = "country.name",destination = "iso3c")
    #https://github.com/CSSEGISandData/COVID-19 #wide format
    #head(datahelp[datahelp$Date == "2020-04-21",],20)
#    dataRaw <- inner_join(
#        inner_join(casesDataRaw, deathDataRaw, by = setdiff(names(casesDataRaw),"Value"), suffix = c(".cases",".deaths")),
#        recoveredDataRaw, by = setdiff(names(recoveredDataRaw),c("Value"))) %>% 
#        select(ISO3 = ISO.3166.1.Alpha.3.Codes, date = Date, casesCum = Value.cases, deathsCum = Value.deaths, recovCum = Value) %>%
    
    # dataRaw <- read.csv("https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv") %>%
    #     select(ISO3 = ISO.3166.1.Alpha.3.Codes, date = Date, casesCum = Value.cases, deathsCum = Value.deaths, recovCum = Value) %>%
    #     mutate(date = ymd(date)) %>%
    #     arrange(desc(casesCum),desc(deathsCum)) %>%
    #     group_by(ISO3,date) %>%
    #     filter(row_number() == 1) %>% 
    #     ungroup() %>%
    #     group_by(ISO3) %>%
    #     mutate(casesNew = casesCum - (lead(casesCum,1,default = 0, order_by = desc(date))),
    #            deathsNew = deathsCum - (lead(deathsCum,1,default = 0, order_by = desc(date))),
    #            recovNew = recovCum - (lead(recovCum,1,default = 0, order_by = desc(date))),
    #            active = casesCum - recovCum)
        
        
        
}


