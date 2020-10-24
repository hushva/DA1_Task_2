# File:    pizza_restaurants_analysis.R
# Course:  Data Analysis 1: Data Exploration
# Group: C
# Date:    2020-10-21

# INSTALL AND LOAD PACKAGES ################################

# CLEAR THE ENVIRONMENT
rm(list=ls())

# INSTALL pacman if needed
if (!require("pacman")) install.packages("pacman")

# USE pacman TO LOAD ADD-ON PACKAGES AS DESIRED
pacman::p_load(pacman, dplyr, GGally, ggplot, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, moments) 
# CALL PACKAGES
library(tidyverse)
library(xtable)


# IMPORTING THE DATASET WITH RIO ##########################
# CSV
rest_data <- import("~/Desktop/Coding_1_R/pizza_restaurants/DA1_Task_2/raw/pizza_restaurants_raw.csv")
head(rest_data)

# IMPORTING THE CSV FILE (ALTERNATIVELY)
data_in <- "~/Desktop/Coding_1_R/pizza_restaurants/DA1_Task_2/"
rest_data <- read.csv(paste0(data_in,"raw/pizza_restaurants_raw.csv"))


# DATA VIEWER ############################################
View(rest_data)


# CHANGE NAMING CONVENTION OF COLUMNS
rest_data <- rename(rest_data,
                    margherita_price = `Margherita Price (HUF)`,
                    pizza_only = `Pizza only (binary)`,
                    cola_price = `0.5L Cola Price (HUF)`,
                    online_rating = `Online rating`,
                    distance = `Distance to CEU (KM)`)


# CHECK VARIABLE DATA TYPES #############################
# It seems some columns do not contain numeric values, we should check that:
typeof(rest_data$margherita_price)
typeof(rest_data$cola_price)
typeof(rest_data$online_rating)
typeof(rest_data$pizza_only)
typeof(rest_data$distance)


# CHANGE VARIABLE DATA TYPE
# REPLACE THE COMMA SEPERATOR WITH POINT
rest_data$online_rating <- as.double(gsub(",", ".", gsub("\\.", "", rest_data$online_rating))) 
rest_data$distance <- as.double(gsub(",", ".", gsub("\\.", "", rest_data$distance)))
           

## FILTER MISSING VALUES FROM THE "cola_price" COLUMN
## ASSIGN THE FILTERED THE DATASET TO A NEW VARIABLE CALLED 'f_rest_data'
f_rest_data <- filter(rest_data,!is.na(cola_price))


# DATA VIEWER
View(f_rest_data)


# COMPUTER BASIC STATISTICS FOR 'margherita_price'
summary(f_rest_data$margherita_price)

# COMPUTE SUMMARY STATISTICS FOR 'margherita_price'
rest_summary <- f_rest_data %>%  summarise( 
      mean = mean( margherita_price ),
      median = median( margherita_price ),
      sd = sd( margherita_price  ),
      min = min( margherita_price ),
      max = max( margherita_price ),
      iq_range = IQR( margherita_price ),
      skew = skewness(margherita_price), # skewness function using moments package
      numObs = sum( !is.na( margherita_price )))


# COMPUTE BASIC STATISTICS FOR 'cola price'
summary(f_rest_data$cola_price )

# COMPUTE SUMMARY STATISTICS FOR 'cola_price'
cola-price-histogram
cola_stat <- f_rest_data %>% summarise( 
      mean = mean( cola_price ),
      median = median( cola_price ),
      sd = sd( cola_price  ),
      min = min( cola_price ),
      max = max( cola_price ),
      iq_range = IQR( cola_price ),
      skew = ((mean(cola_price)-median(cola_price))/sd(cola_price)),
      numObs = sum( !is.na( cola_price )) )

 master

# JOIN SUMMARY STATISTICS TABLES FOR 'margherita_price' & 'cola_price' (NEED TO ADD ROW NAMES: pizza & cola)
stat_table <- rest_summary %>% add_row( cola_stat )
stat_table
 
 
# format the table and print
xt_cola <- xtable(cola_stat,caption = "Summary Table: Cola Prices",align='llccccccc', digits = c(2,0,2,0,0,0,0,3,0))
names(xt_cola) <- c('Mean','Median','Std.dev.','Min','Max','IQ range','Skewness', 'Observations' )
print(xt_cola, type = "latex", comment = getOption("xtable.comment", FALSE))


xt_pizza <- xtable(rest_summary,caption = "Summary Table: Margherita Prices",align='llccccccc', digits = c(2,0,2,0,0,0,0,3,0))
names(xt_pizza) <- c('Mean','Median','Std.dev.','Min','Max','IQ range','Skewness', 'Observations' )
print(xt_pizza, type = "latex", comment = getOption("xtable.comment", FALSE))


# BASIC HISTOGRAMS #########################################

# Put graphs in 2 rows and 1 column
par(mfrow = c(2, 1))

cola-price-histogram
## Histogram for cola prices
f_rest_data %>% 
   ggplot(aes(x=cola_price)) +
   geom_histogram()+ 
   theme_gray()+
   labs(x='Price', y='Frequency', title = 'Cola Price Distribution')

 master

pizza_price-histogram
##Histogram for pizza prices
 f_rest_data %>% 
    ggplot(aes(x = margherita_price)) +
    geom_histogram()+ 
    theme_gray()+
    labs(x='Price', y='Frequency', title = 'Margherita Pizza Price Distribution')
 
 master

 # RESTORE GRAPHIC PARAMETER
 par(mfrow=c(1, 1))
 
 
 # HISTOGRAM BY GROUP #############################
 
 # Put graphs in 3 rows and 1 column
 par(mfrow = c(3, 1))
 
 # Histogram
 # Margherita price based on region
 ggplot(data = f_rest_data , aes( x = margherita_price , fill = Region ) ) +
    geom_histogram( aes( y = ..density.. ), alpha =0.5 ) +
    labs( x = "Price" , y = 'Relative Frequency'  ,
          fill = 'Region' )
 
 # BOX PLOT #####################################
 # Margherita price based on region
 ggplot(f_rest_data, aes(x = Region , y = margherita_price)) + 
    geom_boxplot(varwidth = T)
 
 # DENSITY PLOT #################################
 # Margherita price based on region
 ggplot(f_rest_data, aes(x = margherita_price , fill = Region)) + 
    geom_density(col = NA , alpha = 0.35) + 
    labs( x = "Margherita Price (HUF)" , y = "Density")
 
 # Density plot for pizza and cola prices based on region
 ggplot( data = f_rest_data ) +
    geom_density( aes( x = margherita_price ) , color = 'blue'  , alpha = 0.1 ) +
    geom_density( aes( x = f_rest_data$cola_price )  , color = 'red' , alpha = 0.1 ) +
    labs(x = "Price in HUF",
         y = "Relative Frequency" )
 
 
 # RESTORE GRAPHIC PARAMETER
 par(mfrow=c(1, 1))
 
 
 # Price of pizza in the capital
 hist(cola_price$margherita_price[cola_price$Region == "Capital"],
      main = "Pizza Prices in the capital")
 
 # Price of pizza in the countryside
 hist(cola_price$margherita_price[cola_price$Region == "Countryside"],
      main = "Price of pizza in the countryside")
 
 
 
# HISTOGRAM BY GROUP (REGION = Capital) ################

# Put graphs in 2 rows and 1 column
par(mfrow = c(2, 1))

# Histogram for price of pizza in the capital
hist(f_rest_data$margherita_price [f_rest_data$Region == "Capital"],
     xlim = c(0, 2500),
     main = "Distribution of Pizza Prices in the Capital",
     xlab = "Price in HUF",
     col = "#83BFFF") |+
   

# Histogram for price of cola in the capital
hist(f_rest_data$cola_price [f_rest_data$Region == "Capital"],
     xlim = c(0, 1500),
     main = "Distribution of PBeverage Prices in the Capital",
     xlab = "Price in HUF",
     col = "#FF402F")



# PLOTS ##############################################

# Basic X-Y plot for pizza and cola price variables
plot(cola_price$margherita_price , cola_price$cola_price, 
     col = "#cc0000",  # Hex code for red
     pch = 19,         # Solid points
     main = "Pizza Restaurants: Prices of Pizza Vs. Price of Cola",
     xlab = "Pizza Price (HUF)",
     ylab = "Cola Price (HUF)")

# PLot for pizza prices based on region 
ggplot(cola_price, aes(x = Region , y = margherita_price)) +
   geom_point(position = position_jitter(0.2)) +
   xlab("Region") + 
   ylab("Margherita Pizza Price (HUF)")


ggplot(f_rest_data, aes(x = Region , y = margherita_price)) +
   geom_point(position = position_jitter(0.2)) +
   xlab("Region") + 
   ylab("Margherita Pizza Price (HUF)")


# CLEAN UP ###########################################

# RESTORE GRAPHIC PARAMETER
par(mfrow=c(1, 1))

# Clear environment
rm(list = ls()) 

# Clear packages and ad-ons
p_unload(all) 


# Clear packages
detach("package:datasets", unload = TRUE) #For base

# Clear plots
dev.off()

# Clear console
cat("\014")
