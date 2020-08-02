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
shinyUI(fluidPage("Covid-19 Liveticker",

    # Application title
    tabsetPanel(id = "dataType",
                tabPanel("Total Infections",value = "Cases"),
                tabPanel("Active Cases",value = "Active"),
                tabPanel("Total Deaths",value = "Deaths")
    ),
    #idee für Datendarstellung:
    #Fallzahlen, Land,etc mit Slider für Datum
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        # Show a plot of the generated distribution
        sidebarPanel(
          uiOutput("countryVector"),
          htmlOutput("summary"),
          radioButtons("plotType","Plottype",c("Total","Daily"), selected = "Total"),
          
          
            
            #p(h3("Hello"),
          conditionalPanel(condition = "input.plotType != 'Daily'",radioButtons("scaleType","Scale",list("linear","log"), selected="log"))
            
        ),
        mainPanel(
            #p(outputOptions("countryVector")),
            # p(plotlyOutput("cumPlot")),
            # p(plotlyOutput("dayPlot")),
            leafletOutput("worldMap"),
            plotlyOutput("detailPlot")#,
            # fluidRow(
            #   column(4,
            #          uiOutput("countryVector")
            #          ),
            #   column(4,
            #          # selectInput("scaleType", "Scale",list("linear","log"), selected="log"),
            #          #radioButtons("scaleType","Scale",list("linear","log"), selected="log")
            #          ),
            #   column(4,
            #          #radioButtons("plotType","Plottype",c("Total","Daily"), selected = "Total")
            #          )
            )
        )
    )
    )
