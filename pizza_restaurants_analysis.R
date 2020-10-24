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

<<<<<<< HEAD
#Import the .csv file
data_in <- "/Users/steve_j/Downloads"
rest_data <- read.csv(paste0(data_in,"/pizza_restaurants_raw.csv"))
=======
# IMPORTING THE CSV FILE (ALTERNATIVELY)
data_in <- "C:/Users/ADMIN/Desktop/CEU/R_codes/DA1_Task_2/raw/"
rest_data <- read.csv(paste0(data_in,"pizza_restaurants_raw.csv"))
>>>>>>> dc9586f7657788524bd818731d1003654623fc1e


#We take a look how our table looks like
View(rest_data)

# CHANGE VARIABLE DATA TYPE
# REPLACE THE COMMA SEPERATOR WITH POINT
rest_data$Online.rating<- as.double(gsub(",", ".", gsub("\\.", "", rest_data$online_rating))) 
rest_data$distance <- as.double(gsub(",", ".", gsub("\\.", "", rest_data$distance)))


# Column names are not align with naming conventions
rest_data <- rename(rest_data,
<<<<<<< HEAD
                    margherita_price = Margherita.Price..HUF.,
                    pizza_only = Pizza.only..binary.,
                    cola_price = X0.5L.Cola.Price..HUF.,
                    online_rating = Online.rating,
                    distance = Distance.to.CEU..KM.)
=======
                    margherita_price = `Margherita.Price..HUF.`,
                    pizza_only = `Pizza.only..binary.`,
                    cola_price = `X0.5L.Cola.Price..HUF.`,
                    online_rating = `Online.rating`,
                    distance = `Distance.to.CEU..KM.`)
>>>>>>> dc9586f7657788524bd818731d1003654623fc1e


# It seems some columns do not contain numeric values, we should check that:
typeof(rest_data$Margherita.Price..HUF.)
typeof(rest_data$X0.5L.Cola.Price..HUF.)
typeof(rest_data$Online.rating)
typeof(rest_data$Pizza.only..binary.)
typeof(rest_data$Distance.to.CEU..KM.)




View(rest_data)

colnames(rest_data)[9] <- "pizza_only"
#As it turned out, Online rating and Distance to CEU are character types, need to replace , with .

rest_data$Online.rating <- as.integer(rest_data$Online.rating)







summary(pizza_dataset$`Margherita Price (HUF)`)
# Create descriptive table
rests_stat <- summarise( pizza_dataset , 
                              mean = mean( pizza_dataset$`Margherita Price (HUF)` ),
                              median = median( pizza_dataset$`Margherita Price (HUF)` ),
                              std = sd( pizza_dataset$`Margherita Price (HUF)` ),
                              min = min( pizza_dataset$`Margherita Price (HUF)` ),
                              max = max( pizza_dataset$`Margherita Price (HUF)` ) )


vienna_sum_stat

summary(pizza_dataset$`0.5L Cola Price (HUF)`)
# Clone the dataset and assign it to a variable




# Plot Pizza Price vs. Beverage Price

plot(pizza_dataset$`Margherita Price (HUF)`, pizza_dataset$`0.5L Cola Price (HUF)`,
     col = "#cc0000",  # Hex code for red
     pch = 19,         # Solid points
     main = "Pizza Restaurants: Prices of Pizza Vs. Price of Beverage",
     xlab = "Pizza Price (HUF)",
     ylab = "Beverage Price (HUF)")



# Need a table with frequencies for each category
pizza_price <- table(pizza_dataset$`Margherita Price (HUF)`)  # Create table
barplot(pizza_price)              # Bar chart
plot(pizza_price)                 # Default X-Y plot (lines)





# BASIC HISTOGRAMS #########################################

hist(pizza_dataset$`Margherita Price (HUF)`)
hist(pizza_dataset$`0.5L Cola Price (HUF)`) ## missing value error

# HISTOGRAM BY GROUP #######################################

# Put graphs in 2 rows and 1 column
par(mfrow = c(2, 1))

# Histogram for price of pizza in the capital
<<<<<<< HEAD
hist(pizza_dataset$`Margherita Price (HUF)` [pizza_dataset$Region == "Capital"],
     xlim = c(0, 2500),
     breaks = 27,
     main = "Distribution of the pizza price in the capital",
     xlab = "",
     col = "red")

hist(pizza_dataset$`0.5L Cola Price (HUF)` (HUF)` [pizza_dataset$Region == "Capital"],
     xlim = c(0, 1500),
     ##breaks = 27,
     main = "Distribution of the pizza price in the capital",
     xlab = "",
     col = "blue")


# PLOTS ####################################################

# Good to first check univariate distributions
hist(pizza_dataset$`Margherita Price (HUF)`)
hist(pizza_dataset$`0.5L Cola Price (HUF)`)

# Basic X-Y plot for two quantitative variables
plot(pizza_dataset$`0.5L Cola Price (HUF)`, pizza_dataset$`Margherita Price (HUF)`)

# Add some options
plot(pizza_dataset$`0.5L Cola Price (HUF)`, pizza_dataset$`Margherita Price (HUF)`,
     pch = 19,         # Solid circle
     cex = 1.5,        # Make 150% size
     col = "#cc0000",  # Red
     main = "Scatterplot for price: Pizza vs. Beverage",
     xlab = "Beverage Price",
     ylab = "Pizza Price")


# Restore graphic parameter
=======
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
>>>>>>> dc9586f7657788524bd818731d1003654623fc1e
par(mfrow=c(1, 1))


# HISTOGRAM ################################################

# Default
hist(lynx)

# Add some options
hist(pizza_dataset$`Margherita Price (HUF)`,
     breaks = 48,          # "Suggests" 14 bins
     freq   = FALSE,       # Axis shows density, not freq.
     col    = "thistle1",  # Color for histogram
     main   = paste("Histogram of Prices of pizza"),
     xlab   = "Price of pizza")

# Add a normal distribution
curve(dnorm(x, mean = mean(pizza_dataset$`Margherita Price (HUF)`), sd = sd(pizza_dataset$`Margherita Price (HUF)`)),
      col = "thistle4",  # Color of curve
      lwd = 2,           # Line width of 2 pixels
      add = TRUE)        # Superimpose on previous graph

# Add two kernel density estimators
lines(density(pizza_dataset$`Margherita Price (HUF)`), col = "blue", lwd = 2)
lines(density(pizza_dataset$`Margherita Price (HUF)`, adjust = 3), col = "purple", lwd = 2)

# Add a rug plot
rug(pizza_dataset$`Margherita Price (HUF)`, lwd = 2, col = "gray")


# SUMMARY() ################################################

summary(pizza_dataset$`Online rating`)       # Categorical variable
summary(pizza_dataset$`Margherita Price (HUF)`)  # Quantitative variable
summary(pizza_dataset)               # Entire data frame

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, psych) 

# Get info on package
p_help(psych)           # Opens package PDF in browser
p_help(psych, web = F)  # Opens help in R Viewer

# DESCRIBE() ###############################################

# For quantitative variables only.

describe(pizza_dataset$`Margherita Price (HUF)`)  # One quantitative variable
describe(pizza_dataset)               # Entire data frame


# SELECT BY CATEGORY #######################################

# Price of pizza in the capital
hist(pizza_dataset$`Margherita Price (HUF)`[pizza_dataset$Region == "Capital"],
     main = "Pizza Prices in the capital")

# Price of pizza in the countryside
hist(pizza_dataset[pizza_dataset$Region == "Countryside"],
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

