# set up working directory
setwd("~/R/R_Projects/BIOL_653")

# Set up needed libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# set up data set being used
library(gapminder)




## PART I - Make these plots!

# Plot 1 - use points and color to identify continents

ggplot(data= gapminder, aes(x= gdpPercap, y= lifeExp, color= continent)) + 
  geom_point(aes(shape= continent))

 

# Plot 2 - Since the per capita gpd has a very large range,
#   use a transformation to linearise the data.

ggplot(data= gapminder, aes(x= log(gdpPercap), y= lifeExp, color= continent)) + 
    geom_point(aes(shape= continent)) +
    xlab("gdpPercap")


# Plot 3 - include a simple linear fit to the transformed data

ggplot(data= gapminder, aes(x= log(gdpPercap), y= lifeExp, color= continent)) + 
  geom_point(aes(shape= continent)) +
  xlab("gdpPercap") +
  # geom_smooth(color = "blue", method= "lm", se= FALSE)
  geom_smooth(color = "blue", method= "lm")



# Plot 4 - plot the density functions of life expectancy for each continent

ggplot(data= gapminder, aes(x= lifeExp,y= , color= continent, fill= continent)) + 
 # geom_density(aes(y= log(gdpPercap))) +
  geom_density(color = "black", alpha = 0.6) + 
  facet_wrap(~year) 



# Plot 5 - for a bonus, fix the bottom continent labels

ggplot(data= gapminder, aes(x= continent,y= lifeExp, color= continent)) + 
  geom_boxplot() + 
  facet_wrap(~year) 

ggplot(data= gapminder, aes(x= continent,y= lifeExp, color= continent)) + 
  geom_boxplot() + 
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 50, size=10, vjust = 0.5))


# PART TWO - Make these plots and then some!

# use dplyr to calculate summary statistics
# the function subset() may be useful


# Plot 6

ggplot(data= gapminder, aes(x= lifeExp,y= , color= continent, fill= continent)) + 
  # geom_density(aes(y= log(gdpPercap))) +
  geom_density(color = "black", alpha = 0.6)



# Plot 7 - Plot 7 - plot the mean life expectancy on a density plot for Asia

asia_data <- filter(gapminder, continent == "Asia")

ggplot(data= asia_data,
       aes(x = lifeExp,y = , color = continent, fill = continent)) + 
  geom_density(color = "black", alpha = 0.6, fill = "green") +
  geom_vline(xintercept = mean(asia_data$lifeExp)) +
  ggtitle("Life Expectancy in Asia")

  

# 8) Plot 8
# a. Create a data.frame of the mean life expectancies for each continent.
#     You should end up with a data.frame like this
# '''{r}

meanLife <- gapminder %>%
  group_by(continent) %>%
  summarise(avg_life= mean(lifeExp))
meanLife


# b. Plot the density plot of life expectancies for each continent
#     and draw a vertical line to mark the mean life expectancy
#     for each continent.

    
meanLife <- gapminder %>%
  group_by(continent) %>%
  summarise(avg_life= mean(lifeExp))
  
ggplot(data = gapminder, aes(x=lifeExp, color = continent, fill = continent)) +
  geom_density(color = "black", alpha = 0.6) +
  facet_wrap(~continent) +
  geom_vline(data=meanLife, aes(xintercept = avg_life))
    
 


# Part III - Troubleshooting practice

hw_gapminder <- read.csv('./hw_gapminder.csv')
# Problem here is a typo of an added "e" on field name lifeExp
# mean_lifeExp <- mean(hw_gapminder$lifeExpe)
# Corrected code line:
mean_lifeExp <- mean(hw_gapminder$lifeExp)
mean_lifeExp

# Problem here is missing "c" before second string providing the "y" values for
# the list array
small_set <- hw_gapminder[c(1, 2, 3, 4, 1300:1304), ('country', 'continent', 'year')]
small_set <- hw_gapminder[c(1, 2, 3, 4, 1300:1304), c('country', 'continent', 'year')]
small_set


# Problem here is that there are NA values in the dataset so in the default
# na.rm = FALSE so the NAs are not stripped out.
mean_gdp <- mean(hw_gapminder$gdpPercap)
# Changed argument na.rm = TRUE
mean_gdp <- mean(hw_gapminder$gdpPercap, na.rm = TRUE)
mean_gdp



# Problem here is missing second "=" 
max_country <- hw_gapminder$country[which(hw_gapminder$lifeExp = max(hw_gapminder$lifeExp))]
max_country <- hw_gapminder$country[which(hw_gapminder$lifeExp == max(hw_gapminder$lifeExp))]
max_country
# The desired outcomes are:
#   mean_lifeExp
## [1] 59.47444
#small_set
##                    country continent year
## 1              Afghanistan      Asia 1952
## 2              Afghanistan      Asia 1957
## 3              Afghanistan      Asia 1962
## 4              Afghanistan      Asia 1967
## 1300 Sao Tome and Principe    Africa 1967
## 1301 Sao Tome and Principe    Africa 1972
## 1302 Sao Tome and Principe    Africa 1977
## 1303 Sao Tome and Principe    Africa 1982
## 1304 Sao Tome and Principe    Africa 1987
#mean_gdp
## [1] 7210.019
#max_country
## [1] "Japan"

