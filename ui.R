
suppressMessages({
  library(shiny)
  library(tidyverse)
  library(ggplot2)
  library(shinythemes)
  library(plotly)
  library(DT)
  library(dict)
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
      uiOutput("allcountries"), 
      uiOutput("countries"),
      uiOutput("points"),
      uiOutput("grape"),
      uiOutput("pricerange")
    ),
    mainPanel(
      ## source: https://groups.google.com/forum/#!topic/shiny-discuss/hZNaJ_q1VbU
      tabsetPanel(type="tab",
        tabPanel(title= "Wine Results",
          br(), 
          suppressWarnings(suppressMessages(plotlyOutput("plot1")))
        ), 
        tabPanel(title="Wine Details",
          suppressWarnings(suppressMessages(uiOutput("sortfulltableby"))),
          suppressWarnings(suppressMessages(dataTableOutput("viewfull")))
        )
      )
    )
  )
))
