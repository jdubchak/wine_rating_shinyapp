## Jordan Dubchak January 2018
##
## This file cleans the wine dataset by replacing na values with "Unknown",
## dropping the region_2, and 2 columns relating to the taster, 
## and derives a column of continents from the countries column 

## load required library tidyberse
suppressMessages(library(tidyverse))

## load original wine data csv 
suppressWarnings(suppressMessages(wine_dat <- readr::read_csv("winemag-data-130k-v2.csv")))

## drop unnecessary region_2, and 2 taster columns 
wine_dat <- wine_dat %>% 
  select(-c(region_2, taster_twitter_handle, taster_name))

## convert na values to "Unknown"
## function source: https://stackoverflow.com/a/31034685/8666137
wine_dat_unknowns <- apply(wine_dat, 2, function(x){
  x[is.na(x)] <- "Unknown"
  return(x)
})

## convert matrix back to a dataframe 
wine_dat <- as_data_frame(wine_dat_unknowns)

## Countries/Continents from https://old.datahub.io/dataset/countries-continents/resource/aa08c34c-57e8-4e15-bd36-969bee26aba5
##
## use a dataframe of countries and continents to map continent values by country in wine data 
suppressMessages(countr_conts <- readr::read_csv("Countries-Continents-csv.csv") %>% select(c(Continent, Country)))
colnames(countr_conts) <- c("continent", "country")
countr_conts <- semi_join(countr_conts, wine_dat, by = "country")
countr_conts <- rbind(countr_conts, c("North America", "US"))
countr_conts <- rbind(countr_conts, c("England", "Europe"))

## these two functions map continent values by country in wine data 
## if the country from wine data is in the dataset of countries and continents, save the continent name 
get_cont <- function(x){
  row <- filter(countr_conts, country==x)
  return(row$continent)
}
## if the country from wine data is in the dataset of countries and continents, use get_cont function,
## if not, save continent value as unknown 
check_filter <- function(x){
  ifelse(x %in% countr_conts$country, get_cont(x), return("Unknown"))
}

## execute the above 2 functions on the wine data country variable 
## warning: LONG runtime 
continent <- lapply(wine_dat$country, check_filter)

## save all continent names as a variable in the wine data dataframe
wine_dat$continent <- unlist(continent)

## save wine data dataframe as a csv file 
write_csv(wine_dat, "clean_winedata.csv")