# File:    pizza_restaurants_analysis.R
# Course:  Data Analysis 1: Data Exploration
# Group: C
# Date:    2020-10-21

# INSTALL AND LOAD PACKAGES ################################


# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 
# Clear the environment
rm(list=ls())

library(tidyverse)

# IMPORTING THE DATASET WITH RIO ##########################
# CSV
rest_data <- import("~/raw/pizza_restaurants_raw.csv")
head(rest_data)

#Import the .csv file
data_in <- "~/Documents/CEU/Courses/2020_Fall/Mandatory/DA1/Task2/DA1_Task_2/"
rest_data <- read.csv(paste0(data_in,"raw/pizza_restaurants_raw.csv"))


#We take a look how our table looks like
View(rest_data)

# Column names are not align with naming conventions
rest_data <- rename(rest_data,
                    margherita_price = Margherita.Price..HUF.,
                    pizza_only = Pizza.only..binary.,
                    cola_price = X0.5L.Cola.Price..HUF.,
                    online_rating = Online.rating,
                    distance = Distance.to.CEU..KM.)


# It seems some columns do not contain numeric values, we should check that:
typeof(rest_data$Margherita.Price..HUF.)
typeof(rest_data$X0.5L.Cola.Price..HUF.)
typeof(rest_data$Online.rating)
typeof(rest_data$Pizza.only..binary.)
typeof(rest_data$Distance.to.CEU..KM.)


## Change data type and remove commas
rest_data$online_rating <- as.double(gsub(",", ".", gsub("\\.", "", rest_data$online_rating)))
rest_data$distance <- as.double(gsub(",", ".", gsub("\\.", "", rest_data$distance)))
           
## Dropping NAs from cola_price column
cola_price <- filter(rest_data,!is.na(cola_price))


View(cola_price)



summary(cola_price$`Margherita Price (HUF)`)
# Create descriptive table
rests_stat <- summarise( cola_price , 
                              mean = mean( cola_price$`Margherita Price (HUF)` ),
                              median = median( cola_price$`Margherita Price (HUF)` ),
                              std = sd( cola_price$`Margherita Price (HUF)` ),
                              min = min( cola_price$`Margherita Price (HUF)` ),
                              max = max( cola_price$`Margherita Price (HUF)` ) )



#Check basic stats for cola prices in the dataset
summary(cola_price$cola_price )

# Create descriptive table
cola_summarystats <- summarise( cola_price , 
                         mean = mean(cola_price ),
                         median = median( cola_price  ),
                         std = sd( cola_price  ),
                         min = min( cola_price  ),
                         max = max( cola_price  ),
                         iq_range = IQR(cola_price),
                         numObs   = sum( !is.na( cola_price ) ) )




# Plot Pizza Price vs. Beverage Price

plot(cola_price$`Margherita Price (HUF)`, cola_price$`0.5L Cola Price (HUF)`,
     col = "#cc0000",  # Hex code for red
     pch = 19,         # Solid points
     main = "Pizza Restaurants: Prices of Pizza Vs. Price of Beverage",
     xlab = "Pizza Price (HUF)",
     ylab = "Beverage Price (HUF)")



# Need a table with frequencies for each category
pizza_price <- table(cola_price$`Margherita Price (HUF)`)  # Create table
barplot(pizza_price)              # Bar chart
plot(pizza_price)                 # Default X-Y plot (lines)





# BASIC HISTOGRAMS #########################################

hist(cola_price$`Margherita Price (HUF)`)
hist(cola_price$`0.5L Cola Price (HUF)`) ## missing value error

# HISTOGRAM BY GROUP #######################################

# Put graphs in 2 rows and 1 column
par(mfrow = c(2, 1))

# Histogram for price of pizza in the capital
hist(cola_price$`Margherita Price (HUF)` [cola_price$Region == "Capital"],
     xlim = c(0, 2500),
     breaks = 27,
     main = "Distribution of the pizza price in the capital",
     xlab = "",
     col = "red")

hist(cola_price$`0.5L Cola Price (HUF)` (HUF)` [cola_price$Region == "Capital"],
     xlim = c(0, 1500),
     ##breaks = 27,
     main = "Distribution of the pizza price in the capital",
     xlab = "",
     col = "blue")


# PLOTS ####################################################

# Good to first check univariate distributions
hist(cola_price$`Margherita Price (HUF)`)
hist(cola_price$`0.5L Cola Price (HUF)`)

# Basic X-Y plot for two quantitative variables
plot(cola_price$`0.5L Cola Price (HUF)`, cola_price$`Margherita Price (HUF)`)

# Add some options
plot(cola_price$`0.5L Cola Price (HUF)`, cola_price$`Margherita Price (HUF)`,
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
hist(cola_price$`Margherita Price (HUF)`,
     breaks = 48,          # "Suggests" 14 bins
     freq   = FALSE,       # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Prices of pizza"),
     xlab   = "Price of pizza")

# Add a normal distribution
curve(dnorm(x, mean = mean(cola_price$`Margherita Price (HUF)`), sd = sd(cola_price$`Margherita Price (HUF)`)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph

# Add two kernel density estimators
lines(density(cola_price$`Margherita Price (HUF)`), col = "blue", lwd = 2)
lines(density(cola_price$`Margherita Price (HUF)`, adjust = 3), col = "purple", lwd = 2)

# Add a rug plot
rug(cola_price$`Margherita Price (HUF)`, lwd = 2, col = "gray")


# SUMMARY() ################################################

summary(cola_price$`Online rating`)       # Categorical variable
summary(cola_price$`Margherita Price (HUF)`)  # Quantitative variable
summary(cola_price)               # Entire data frame

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, psych) 

# Get info on package
p_help(psych)           # Opens package PDF in browser
p_help(psych, web = F)  # Opens help in R Viewer

# DESCRIBE() ###############################################

# For quantitative variables only.

describe(cola_price$`Margherita Price (HUF)`)  # One quantitative variable
describe(cola_price)               # Entire data frame


# SELECT BY CATEGORY #######################################

# Price of pizza in the capital
hist(cola_price$`Margherita Price (HUF)`[cola_price$Region == "Capital"],
     main = "Pizza Prices in the capital")

# Price of pizza in the countryside
hist(cola_price[cola_price$Region == "Countryside"],
     main = "Price of pizza in the countryside")






# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons


# Clear packages
detach("package:datasets", unload = TRUE) #For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

