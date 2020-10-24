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
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
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
data_in <- "C:/Users/ADMIN/Desktop/CEU/R_codes/Task2/"
rest_data <- read.csv(paste0(data_in,"pizza_restaurants_raw.csv"))


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
cola_price <- filter(rest_data,!is.na(cola_price))

# DATA VIEWER
View(cola_price)


# COMPUTER BASIC STATISTICS FOR 'margherita_price'
summary(cola_price$margherita_price)

# COMPUTE SUMMARY STATISTICS FOR 'margherita_price'
rest_summary <- cola_price %>%  summarise( 
      mean = mean( margherita_price ),
      median = median( margherita_price ),
      sd = sd( margherita_price  ),
      min = min( margherita_price ),
      max = max( margherita_price ),
      iq_range = IQR( margherita_price ),
      skew = skewness(margherita_price), # skewness function using moments package
      numObs = sum( !is.na( margherita_price )))


# COMPUTE BASIC STATISTICS FOR 'cola price'
summary(cola_price$cola_price )

# COMPUTE SUMMARY STATISTICS FOR 'cola_price'
#cola-price-histogram
cola_summarystats <- cola_price %>% summarise( 
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
stat_table <- rest_summary %>% add_row( cola_summarystats )
stat_table
 
 
 
# format the table and print
xt_cola <- xtable(cola_summarystats,caption = "Summary Table: Cola Prices",align='llccccccc', digits = c(2,0,2,0,0,0,0,3,0))
names(xt_cola) <- c('Mean','Median','Std.dev.','Min','Max','IQ range','Skewness', 'Observations' )
print(xt_cola, type = "latex", comment = getOption("xtable.comment", FALSE))


xt_pizza <- xtable(rest_summary,caption = "Summary Table: Margherita Prices",align='llccccccc', digits = c(2,0,2,0,0,0,0,3,0))
names(xt_pizza) <- c('Mean','Median','Std.dev.','Min','Max','IQ range','Skewness', 'Observations' )
print(xt_pizza, type = "latex", comment = getOption("xtable.comment", FALSE))


# PLOTS #######################################

# Separate plots for pizza prices based on region 
ggplot(cola_price, aes(x = Region , y = margherita_price)) +
   geom_point(position = position_jitter(0.2)) +
   xlab("Region") + 
   ylab("Margherita Pizza Price (HUF)")


# Plot Pizza Price vs. Cola Price
plot(cola_price$margherita_price , cola_price$cola_price, 
     col = "#cc0000",  # Hex code for red
     pch = 19,         # Solid points
     main = "Pizza Restaurants: Prices of Pizza Vs. Price of Cola",
     xlab = "Pizza Price (HUF)",
     ylab = "Cola Price (HUF)")





# Need a table with frequencies for each category
pizza_price <- table(cola_price$margherita_price)  # Create table
barplot(pizza_price)              # Bar chart




# BASIC HISTOGRAMS #########################################

cola-price-histogram
##Histogram for cola prices
cola_price %>% 
   ggplot(aes(x=cola_price)) +
   geom_histogram()+ 
   theme_gray()+
   labs(x='Price', y='Count', title = 'Cola Price Distribution')

 master

 
pizza_price-histogram
##Histogram for pizza prices
 cola_price %>% 
    ggplot(aes(x = margherita_price)) +
    geom_histogram()+ 
    theme_gray()+
    labs(x='Price', y='Count', title = 'Margherita Pizza Price Distribution')
 
 master
 
 

# HISTOGRAM BY GROUP #######################################

# Put graphs in 2 rows and 1 column
par(mfrow = c(2, 1))

# Histogram for price of pizza in the capital
hist(cola_price$margherita_price [cola_price$Region == "Capital"],
     xlim = c(0, 2500),
     breaks = 27,
     main = "Distribution of the pizza price in the capital",
     xlab = "",
     col = "red")

hist(cola_price$cola_price [cola_price$Region == "Capital"],
     xlim = c(0, 1500),
     ##breaks = 27,
     main = "Distribution of the pizza price in the capital",
     xlab = "",
     col = "blue")



# PLOTS ####################################################

# Good to first check univariate distributions
hist(cola_price$margherita_price)
hist(cola_price$cola_price)

# Basic X-Y plot for two quantitative variables
plot(cola_price$cola_price, cola_price$margherita_price)

# Add some options
plot(cola_price$cola_price, cola_price$margherita_price,
     pch = 19,         # Solid circle
     cex = 1.5,        # Make 150% size
     col = "#cc0000",  # Red
     main = "Scatterplot for price: Pizza vs. Beverage",
     xlab = "Beverage Price",
     ylab = "Pizza Price")


# Restore graphic parameter
par(mfrow=c(1, 1))


# HISTOGRAM ################################################

# Default
hist(lynx)

# Add some options
hist(cola_price$margherita_price,
     breaks = 48,          # "Suggests" 14 bins
     freq   = FALSE,       # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Prices of pizza"),
     xlab   = "Price of pizza")

# Add a normal distribution
curve(dnorm(x, mean = mean(cola_price$margherita_price), sd = sd(cola_price$margherita_price)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph

# Add two kernel density estimators
lines(density(cola_price$margherita_price, col = "blue", lwd = 2))
lines(density(cola_price$margherita_price, adjust = 3, col = "purple", lwd = 2))

# Add a rug plot
rug(cola_price$margherita_price, lwd = 2, col = "gray")



# Use pacman to load add-on packages as desired
pacman::p_load(pacman, psych) 


# SELECT BY CATEGORY #######################################

# Price of pizza in the capital
hist(cola_price$margherita_price [cola_price$Region == "Capital"],
     main = "Pizza Prices in the capital")

# Price of pizza in the countryside
hist(cola_price[cola_price$Region == "Countryside"],
     main = "Price of pizza in the countryside")


#Multiple test to determine whether the marghertia pizza prices in the Capital are the same as in the Countryside
testing <- cola_price %>% 
   select(Region, margherita_price) %>% 
   group_by(Region) %>% 
   summarise(mean_margherita_price = mean(margherita_price),
             se_margherita_price =1/sqrt(n())*sd(margherita_price),
             num_obs=n())
testing <- mutate(testing, t_stat=mean_margherita_price / se_margherita_price)

#Check results of the multiple test
testing

# CLEAN UP #################################################

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

