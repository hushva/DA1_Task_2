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
library(ggplot2)


# IMPORTING THE CSV FILE (ALTERNATIVELY)
data_in <- "~/Documents/CEU/Courses/2020_Fall/Mandatory/DA1/Task2/DA1_Task_2/"
rest_data <- read.csv(paste0(data_in,"/raw/pizza_restaurants_raw.csv"))


# DATA VIEWER ############################################
View(rest_data)


# CHANGE NAMING CONVENTION OF COLUMNS
rest_data <- rename(rest_data,
                    margherita_price = `Margherita.Price..HUF.`,
                    pizza_only = `Pizza.only..binary.`,
                    cola_price = `X0.5L.Cola.Price..HUF.`,
                    online_rating = `Online.rating`,
                    distance = `Distance.to.CEU..KM.`)


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

#WRITE OUT CLEANED DATA TABLE
write_csv( f_rest_data , paste0( data_in,
                                  "clean/pizza_restaurants_clean.csv"))

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
cola_stat <- f_rest_data %>% summarise( 
      mean = mean( cola_price ),
      median = median( cola_price ),
      sd = sd( cola_price  ),
      min = min( cola_price ),
      max = max( cola_price ),
      iq_range = IQR( cola_price ),
      skew = ((mean(cola_price)-median(cola_price))/sd(cola_price)),
      numObs = sum( !is.na( cola_price )) )
 
 
# format the table and print
xt_cola <- xtable(cola_stat,caption = "Summary Table: Cola Prices",align='llccccccc', digits = c(2,0,2,0,0,0,0,3,0))
names(xt_cola) <- c('Mean','Median','Std.dev.','Min','Max','IQ range','Skewness', 'Observations' )
print(xt_cola, type = "latex", comment = getOption("xtable.comment", FALSE))


xt_pizza <- xtable(rest_summary,caption = "Summary Table: Margherita Prices",align='llccccccc', digits = c(2,0,2,0,0,0,0,3,0))
names(xt_pizza) <- c('Mean','Median','Std.dev.','Min','Max','IQ range','Skewness', 'Observations' )
print(xt_pizza, type = "latex", comment = getOption("xtable.comment", FALSE))


# JOIN SUMMARY STATISTICS TABLES FOR 'margherita_price' & 'cola_price'
stat_table <- xt_pizza %>% add_row( xt_cola )
stat_table



# BASIC HISTOGRAMS #########################################

# Put graphs in 2 rows and 1 column
par(mfrow = c(2, 1))

## Histogram for cola prices
f_rest_data %>% 
  ggplot(aes(x=cola_price)) +
  geom_histogram()+ 
  theme_gray()+
  labs(x='Price', y='Frequency', title = 'Cola Price Distribution')

##Histogram for pizza prices
f_rest_data %>% 
  ggplot(aes(x = margherita_price)) +
  geom_histogram()+ 
  theme_gray()+
  labs(x='Price', y='Frequency', title = 'Margherita Pizza Price Distribution')


# PLOTS ##############################################

# Basic X-Y plot for pizza and cola price variables
plot(f_rest_data$margherita_price , f_rest_data$cola_price, 
     col = "#cc0000",  # Hex code for red
     pch = 19,         # Solid points
     main = "Pizza Restaurants: Prices of Pizza Vs. Price of Cola",
     xlab = "Pizza Price (HUF)",
     ylab = "Cola Price (HUF)")


# PLot for pizza prices based on region 
ggplot(f_rest_data, aes(x = Region , y = margherita_price)) +
   geom_point(position = position_jitter(0.15)) +
   xlab("Region") + 
   ylab("Margherita Pizza Price (HUF)")

# PLot for cola prices based on region
ggplot(f_rest_data, aes(x = Region , y = cola_price)) +
   geom_point(position = position_jitter(0.15)) +
   xlab("Region") + 
   ylab("Cola Price (HUF)")

 
# HISTOGRAM BY GROUP (REGION = Capital) ################
 
# Put graphs in 2 rows and 1 column
 par(mfrow = c(2, 1))
 
# Histogram for price of pizza in the capital
 hist(f_rest_data$margherita_price [f_rest_data$Region == "Capital"],
      xlim = c(0, 2500),
      main = "Distribution of Pizza Prices in the Capital",
      xlab = "Price of Pizza in HUF",
      col = "#83BFFF")
   
   
# Histogram for price of cola in the capital
  hist(f_rest_data$cola_price [f_rest_data$Region == "Capital"],
        xlim = c(0, 1500),
        main = "Distribution of Beverage Prices in the Capital",
        xlab = "Price of Cola in HUF",
        col = "#FF8784")
 

# HISTOGRAM BY GROUP (REGION = Countryside) ################
  
# Put graphs in 2 rows and 1 column
  par(mfrow = c(2, 1))
  
# Histogram for price of pizza in the countryside
  hist(f_rest_data$margherita_price [f_rest_data$Region == "Countryside"],
       xlim = c(0, 2500),
       main = "Distribution of Pizza Prices in the Countryside",
       xlab = "Price of Pizza in HUF",
       col = "#83BFFF")
  
  
# Histogram for price of cola in the countrysude
  hist(f_rest_data$cola_price [f_rest_data$Region == "Countryside"],
       xlim = c(0, 1500),
       main = "Distribution of Beverage Prices in the Countryside",
       xlab = "Price of Cola in HUF",
       col = "#FF8784")
    
 
# RESTORE GRAPHIC PARAMETER
 par(mfrow=c(1, 1))
 
 
# HISTOGRAMS GROUPED BY REGION #################
 
# Put graphs in 3 rows and 1 column
 par(mfrow = c(3, 1))
 
# Histogram
# Margherita price based on region
 ggplot(data = f_rest_data , aes( x = margherita_price , fill = Region ) ) +
    geom_histogram( aes( y = ..density.. ), alpha =0.5 ) +
    labs( x = "Price" , y = 'Relative Frequency'  ,
          fill = 'Region' )

 # Cola price based on region
 ggplot(data = f_rest_data , aes( x = cola_price , fill = Region ) ) +
   geom_histogram( aes( y = ..density.. ), alpha =0.5 ) +
   labs( x = "Price" , y = 'Relative Frequency'  ,
         fill = 'Region' ) 
 
  
# BOX PLOT #####################################
# Margherita price based on region
 ggplot(f_rest_data, aes(x = Region , y = margherita_price)) + 
    geom_boxplot(varwidth = T)
 
 # Cola price based on region
 ggplot(f_rest_data, aes(x = Region , y = cola_price)) + 
   geom_boxplot(varwidth = T)
  
 
 # DENSITY PLOT ###############################
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
 
 
#Multiple test to determine whether the marghertia pizza prices in the Capital are the same as in the Countryside
testing <- f_rest_data %>% 
   select(Region, margherita_price) %>% 
   group_by(Region) %>% 
   summarise(mean_margherita_price = mean(margherita_price),
             se_margherita_price =1/sqrt(n())*sd(margherita_price),
             num_obs=n())
 testing <- mutate(testing, t_stat=mean_margherita_price / se_margherita_price)
 
 #Check results of the multiple test
 testing

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

