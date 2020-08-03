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
library(shinyBS)
# Define UI for application that draws a histogram
shinyUI({
  fluidPage(title ="Covid-19 Liveticker",
            tipify(tabsetPanel(id = "dataType",
                               tabPanel("Total Infections",value = "Cases"),
                               tabPanel("Active Cases",value = "Active"),
                               tabPanel("Total Deaths",value = "Deaths")),
                   title = "change heatmap coloring"),
            sidebarLayout(
              sidebarPanel(
                tipify(uiOutput("countryVector"),title = "select country via map or dropdown menu"),
                htmlOutput("summary"),
                tipify(radioButtons("plotType","Displayed Plot",c("Total","Daily"), selected = "Total"),
                       title = "Change the type of covid-19 cases displayed in the Plot"),
                tipify(conditionalPanel(
                  condition = "input.plotType != 'Daily'",radioButtons("scaleType","y-axis scaling",list("linear","log"), selected="log")),title = "Change the y-axis scaling")),
              mainPanel(
                tipify(leafletOutput("worldMap"),
                       title = "select Country for Information"),
                plotlyOutput("detailPlot"))))
})