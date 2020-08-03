#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(leaflet)
library(plotly)

# Define UI for application that draws a histogram
shinyUI({
  fluidPage(title ="Covid-19 Liveticker",
    tabsetPanel(id = "dataType",
                tabPanel("Total Infections",value = "Cases"),
                tabPanel("Active Cases",value = "Active"),
                tabPanel("Total Deaths",value = "Deaths")),
    sidebarLayout(
        sidebarPanel(
          uiOutput("countryVector"),
          htmlOutput("summary"),
          radioButtons("plotType","Plottype",c("Total","Daily"), selected = "Total"),
          conditionalPanel(
            condition = "input.plotType != 'Daily'",radioButtons("scaleType","Scale",list("linear","log"), selected="log"))),
        mainPanel(
            leafletOutput("worldMap"),
            plotlyOutput("detailPlot"))))
  })