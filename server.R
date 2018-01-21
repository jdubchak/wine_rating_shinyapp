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
country_dict[["Europe"]] <- c("Italy", "Portugal", "Spain", "France", "Germany","Austria","Hungary","Greece",
                              "Romania","Czech Republic","Slovenia","Luxembourg","Croatia","Georgia","England",
                              "Serbia","Moldova","Bulgaria", "Cyprus","Armenia", "Switzerland","Bosnia and Herzegovina",
                              "Ukraine","Slovakia","Macedonia" )

min_price <- 4

max_price <- 3300

shinyServer(function(input, output){ 
  
  output$continent <- renderUI({
    checkboxGroupInput("continent2", "Continent", choices = sort(unique(wine_dat$Continent))[-7], selected = "Africa")
  })
  
  output$points <- renderUI({
    selectInput("points2", label = "Wine Rating", choices = c("95-100 : Classic", "90-94 : Exceptional", "85-89 : Very Good", 
                                                             "80-84 : Good", "70-79 : Tastes Average", 
                                                             "60-69 : Flawed and Not Recommended", "50-59 : Flawed and Undrinkable"),
                selected = "85-89 : Very Good")
  })
  
  output$countries <- renderUI({
    selectInput("countries2", label = "Select Country", 
                choices = sort(unique(filter(wine_dat, Continent %in% input$continent2)$Country)), 
                                                                multiple = TRUE)
  })
  
  output$grape <- renderUI({
    selectInput("grape", label = "Select Grape Varietal", selected = "All",
                choices = c("Red", "White", "All"))
  })
  
  output$pricerange <- renderUI({ 
    sliderInput("pricerange", label = h6("Select Price Range"), min =min_price, max = max_price, value = c(40, 150), step = 200)
  })
  
  output$allcountries <- renderUI({checkboxInput("allcountries", label = "Select All Countries")})
  
  output$colourby <- renderUI({
    selectInput("colourby", label = "Colour Points By", selected = "Country",
                choices = c("Country", "Continent", "Grape Variety"))
  })
    
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
  
  grape_type <- reactive({
    if(input$grape == "All"){
      unique(wine_dat$Grape)
    }else{
      input$grape
    }
  })
  
  colour <- reactive({
    if(input$colourby=="Country"){
      wine_data()$Country
    }else if(input$colourby=="Continent"){
      wine_data()$Continent
    }else{
      wine_data()$Grape
    }
  })
  
  wine_data <- reactive({
  if(input$allcountries%%2==1){
    wine_dat %>%
    filter(Continent %in% input$continent2) %>% 
    filter(Country %in% unlist(lapply(input$continent2, function(x) country_dict[[x]]))) %>%
    filter(Grape %in% grape_type()) %>% 
    filter(Price>=input$pricerange[1]) %>%
    filter(Price<=input$pricerange[2]) %>%
    filter(Points>=points()[1]) %>%
    filter(Points<=points()[2])    
  } else{
    wine_dat %>%
    filter(Continent %in% input$continent2) %>%
    filter(Country %in% input$countries2) %>%
    filter(Grape %in% grape_type()) %>% 
    filter(Price>=input$pricerange[1]) %>%
    filter(Price<=input$pricerange[2]) %>%
    filter(Points>=points()[1]) %>%
    filter(Points<=points()[2]) 
  }
  })

  output$plot1 <- renderPlotly({
      g <- ggplot(wine_data(), aes(Name=Name)) + 
      geom_jitter(aes(Price, Points, col=colour()), alpha=0.6) +
      labs(x="Price (USD)", y="Wine Rating", title="Comparison of Wine Quality and Price by Country", colour="Colours") +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size=18), axis.title.x = element_text(size=13),
            axis.title.y = element_text(size=13)) + scale_fill_viridis(option = "magma")
      ## plotly_build source: https://stackoverflow.com/a/45316028/8666137
      pl <- plotly_build(g)
  })

  wine_fulltable <- reactive({
      wine_data() %>% 
        select(Name, Price, Points, Description, Country, Variety, Grape) %>% 
        arrange(desc(Points))
  })
  
  output$viewfull <- renderDataTable({
    wine_fulltable()
  })

})
