
suppressMessages({
  library(shiny)
  library(tidyverse)
  library(ggplot2)
  library(shinythemes)
  library(plotly)
  library(DT)
  library(dict)
  library(shinycssloaders)
})

## read in cleaned wine data from csv file 
suppressWarnings(suppressMessages(wine_dat <- readr::read_csv("clean_winedata.csv")))

wine_dat <- rename(wine_dat, 
                   Country = country, Continent = continent, Description = description, Designation = designation, Price = price,
                   Name = title, Variety = variety, Winery = winery, Points = points)

## shiny app dev 
shinyUI(fluidPage(
  theme=shinytheme("paper"),
  
  titlePanel("Wine For You and Wine For Me"),
  h4("Find a wine for any occasion and budget"),
  
  sidebarLayout(
    sidebarPanel(
      width=3,
      h6("Modify the options below to find your ideal wine. For further details, click the Wine Details tab."),
      uiOutput("continent"),
      h6("Note: Figure will not display until at least one country, or Select All Countries, is selected."),
      uiOutput("countries"),
      uiOutput("allcountries"), 
      uiOutput("points"),
      uiOutput("grape"),
      uiOutput("pricerange")
    ),
    mainPanel(
      ## source: https://groups.google.com/forum/#!topic/shiny-discuss/hZNaJ_q1VbU
      tabsetPanel(type="tab",
        tabPanel(title= "Wine Results",
          br(), 
          suppressWarnings(suppressMessages(plotlyOutput("plot1", height="500px"))) %>% withSpinner(),
          uiOutput("colourby")
        ), 
        tabPanel(title="Wine Details",
          suppressWarnings(suppressMessages(uiOutput("sortfulltableby"))),
          suppressWarnings(suppressMessages(dataTableOutput("viewfull")))
        )
      )
    )
  )
))
