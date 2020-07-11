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
shinyUI(navbarPage("Covid-19 Liveticker",

    # Application title
    tabsetPanel(id = "tabs",
                tabPanel("Total Cases",value = "total"),
      tabPanel("New Cases",value = "new")
    ),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        # Show a plot of the generated distribution
        sidebarPanel(
            leafletOutput("worldMap")#,
            #p(h3("Hello"),
            #    h3(verbatimTextOutput("country")),
            #h3("Bye"))
        ),
        mainPanel(
            #p(outputOptions("countryVector")),
            p(plotlyOutput("cumPlot")),
            p(plotlyOutput("dayPlot")),
            fluidRow(
              column(4,
                     selectInput("selectedCountry", "Country",textOutput("countryVector"), selected="")
                     ),
              column(4,
                     selectInput("scaleType", "Scale",list("linear","log"), selected="log")
                     ),
              column(4,
                     selectInput("dataType","Plottype",c("Cases","Active","Deaths"), selected = "Cases")
              )
            )
        )
    )
))
