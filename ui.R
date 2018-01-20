
suppressMessages({
  library(shiny)
  library(tidyverse)
  library(ggplot2)
  library(shinythemes)
  library(plotly)
})

## read in cleaned wine data from csv file 
suppressWarnings(suppressMessages(wine_dat <- readr::read_csv("clean_winedata.csv")))

wine_dat <- rename(wine_dat, 
                   Country = country, Continent = continent, Description = description, Designation = designation, Price = price,
                   Name = title, Variety = variety, Winery = winery, Points = points)

## shiny app dev 
shinyUI(fluidPage(
  theme=shinytheme("paper"),
  
  titlePanel("Find a Great Wine!"),
  
  sidebarLayout(
    sidebarPanel(
      h6("Modify the options below to find a wine for any occasion and budget."), 
      uiOutput("continent"),
      uiOutput("countries"),
      uiOutput("points"),
      uiOutput("pricerange")
    ),
    mainPanel(
      ## source: https://groups.google.com/forum/#!topic/shiny-discuss/hZNaJ_q1VbU
      tabsetPanel(type="tab",
        tabPanel(title= "Plot",
          suppressWarnings(suppressMessages(plotlyOutput("plot1"))),
          suppressWarnings(suppressMessages(uiOutput("sortby"))), 
          suppressWarnings(suppressMessages(tableOutput("viewtable")))
        ), 
        tabPanel(title="Full Table",
          suppressWarnings(suppressMessages(uiOutput("sortfulltableby"))),
          suppressWarnings(suppressMessages(tableOutput("viewfull")))
        )
      )
    )
  )
))
