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
shinyUI(fluidPage(

    # Application title
    titlePanel("Covid-19 Liveticker"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("worldMap")#,
            #p(h3("Hello"),
            #    h3(verbatimTextOutput("country")),
            #h3("Bye"))
        ),
        sidebarPanel(
            #p(outputOptions("countryVector")),
            p(selectInput("selectedCountry", "Country",textOutput("countryVector"), selected="")),
            p(plotlyOutput("cumPlot")),
            p(plotlyOutput("dayPlot")),
            p(selectInput("plotType","Plottype",c("log","not log?"), selected = "not log?"))
        )
    )
))
