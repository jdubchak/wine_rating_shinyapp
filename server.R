library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)
library(dict)

## read in cleaned wine data from csv file 
suppressWarnings(suppressMessages(wine_dat <- readr::read_csv("clean_winedata.csv")))

suppressWarnings({wine_dat <- wine_dat %>% 
  mutate(price = as.numeric(price))})

wine_dat <- rename(wine_dat, 
       Country = country, Continent = continent, Description = description, Designation = designation, Price = price,
       Name = title, Variety = variety, Winery = winery, Points=points)

country_dict <- dict()
country_dict[["Africa"]] <- c("Egypt", "Morocco", "South Africa")
country_dict[["Asia"]] <- c("China", "India", "Israel", "Lebanon", "Turkey")
country_dict[["North America"]] <- c("Canada", "Mexico", "US")
country_dict[["South America"]] <- c("Argentina", "Brazil", "Chile", "Peru", "Uruguay")
country_dict[["Oceania"]] <- c("Australia", "New Zealand")

min_price <- 4

max_price <- 3300

shinyServer(function(input, output) { 
  
  output$continent <- renderUI({
    checkboxGroupInput("continent2", "Continent", choices = sort(unique(wine_dat$Continent)), selected = "Africa")
  })
  
  output$points <- renderUI({
    selectInput("points2", label = "Wine Rating", choices = c("95-100 : Classic", "90-94 : Exceptional", "85-89 : Very Good", 
                                                             "80-84 : Good", "70-79 : Tastes Average", 
                                                             "60-69 : Flawed and Not Recommended", "50-59 : Flawed and Undrinkable"),
                selected = "85-89 : Very Good")
  })
  
  output$countries <- renderUI({
    selectInput("countries2", label = "Select Country\nNote: Figure will not display until at least one country is selected.", 
                choices = sort(unique(filter(wine_dat, Continent %in% input$continent2)$Country)), 
                                                                multiple = TRUE)
  })
  
  output$pricerange <- renderUI({ 
    sliderInput("pricerange", label = h3("Select Price Range"), min =min_price, max = max_price, value = c(40, 150), step = 200)
  })
  
  output$allcountries <- renderUI({checkboxInput("allcountries", label = "Select All Countries")})
  
  points <- reactive({
    if(input$points2 == "95-100 : Classic"){
      point_vals = c(95,100)
    }else if(input$points2 =="90-94 : Exceptional"){
      point_vals = c(90,94)
    }else if(input$points2 =="85-89 : Very Good"){
      point_vals = c(85,89)
    }else if(input$points2 =="80-84 : Good"){
      point_vals = c(80,84)
    }else if(input$points2 =="70-79 : Tastes Average"){
      point_vals = c(70,79)
    }else if(input$points2 =="60-69 : Flawed and Not Recommended"){
      point_vals = c(60,69)
    }else{
      point_vals = c(50,59)
    }
    point_vals
  })
  
  wine_data <- reactive({
  if(input$allcountries%%2==1){
    wine_dat %>%
    filter(Continent %in% input$continent2) %>% 
    filter(Country %in% unlist(lapply(input$continent2, function(x) country_dict[[x]]))) %>% 
    filter(Price>=input$pricerange[1]) %>%
    filter(Price<=input$pricerange[2]) %>%
    filter(Points>=points()[1]) %>%
    filter(Points<=points()[2])    
  } else{
    wine_dat %>%
    filter(Continent %in% input$continent2) %>%
    filter(Country %in% input$countries2) %>%
    filter(Price>=input$pricerange[1]) %>%
    filter(Price<=input$pricerange[2]) %>%
    filter(Points>=points()[1]) %>%
    filter(Points<=points()[2]) 
  }
  })

  output$plot1 <- renderPlotly({
      g <- ggplot(wine_data(), aes(Name=Name)) + 
      geom_jitter(aes(Price, Points, col=Country)) +
      labs(x="Price (USD)", y="Wine Rating", title="Comparison of Wine Quality and Price by Country") +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size=20), axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15))
      ## plotly_build source: https://stackoverflow.com/a/45316028/8666137
      pl <- plotly_build(g)
  })

  wine_fulltable <- reactive({
      wine_data() %>% 
        select(Name, Price, Points, Description, Country, Variety) %>% 
        arrange(desc(Points))
  })
  
  output$viewfull <- renderDataTable({
    wine_fulltable()
  })

})
