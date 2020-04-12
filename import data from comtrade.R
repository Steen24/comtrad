
# Packages    ----------------------------------------------------------------

# install.packages("comtradr")
# install.packages('dpylr')
# install.packages('openxlsx')
library(tidyverse)
library(magrittr)
library(rjson)
library("rjson")
library(comtradr)
library(gmdata)
library(Gabegit/gmdata)
library(tidyverse)
library(openxlsx)
library(readr)
library(readxl)
# install.packages('xlsx')
library(xlsx)

# import data -------------------------------------------------------------

# files
    codes_raw <- read_excel( 'C:/Users/SW/Desktop/HS17toHS07_BIO_DATA.xlsx', sheet = 3 ,skip = 1  )   %>% dplyr::rename( HS2017 = `010310...1` )%>% distinct( HS2017)
    codes_raw <- codes_raw[1:40,]
    codes <- codes_raw 
    countries_raw <- read_excel( 'C:/Users/SW/Desktop/Comtrade Country Code and ISO list.xlsx', sheet = 1  ) %>% select(`Country Name, Full`)
    countries <- unique(countries_raw$`Country Name, Full`)
    
# parameters for loop
    step =  round(nrow(codes_raw)/10)
    round <-  seq(1,step)*10 
    codes_raw$id <-  as.numeric(row.names(codes_raw))

# initiate df
    trade <- data.frame()
    
for( c in countries[c(276,53,58)]){
    
  codes <- codes_raw 
  
    for(i in round ){
      
        codes_download <- codes %>% filter( id <= i)
        
        tryCatch(
            query_trade <- ct_search(reporters = c , 
                                   partners = "World", 
                                   trade_direction = "all",
                                   start_date = 2012, end_date = 2012,
                                     commod_codes = codes_download$HS2017
                                     ) %>%
                        select(classification,year,aggregate_level,trade_flow,reporter,commodity_code,commodity,qty,trade_value_usd) %>% 
                        as_tibble()
            , error = function(e) e, finally = print( paste0( c," Error message ignored "))  
            )
        
        codes <-  codes %>% filter( id > i)
        
        if( exists('query_trade') ){
            trade <- rbind( trade, query_trade)
            rm(query_trade)
        }
        
    } # i loop
} # c loop 

trade %>%  distinct(reporter,commodity_code )    
    
    