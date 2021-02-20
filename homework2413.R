# DS 413/613 HOMEWORK 2 
# Instructions:  For this homework assignment, produce an Rmarkdown file
# and a Word file as you have done for previous labs and the first homework assignment. 
# For homework 2, however push your Word file to Github using the required commands from 
# the Bash terminal.  Create a new repository. Use your first name and your last name as 
# a name of your new repository. When your push is complete and your Word file is 
# registered in the new repository, email me the link
# (in the address bar at the top of the page) for your new repository.

# Homework 2 problems
# 1) Enter and run USArrests. What type of information is shown in the data table USArrests? 
#  (Three or four sentences)
library(tidyverse)
USArrests
# This is a data set of statistics. It contains teh arrests per 100,000 residents for assault,
# murder and rape in every US state during the year of 1973. It also contains the percent 
# of the population living in urban areas. 



# 2) Use and show R coding that features a map function to show maximum values for all 
# variables of the USArerests data frame.  Which State has the largest number of Assaults 
# according to the USAressts data frame ?

map_dbl(USArrests, max)

# North Carolina has the highest number of assaults accoting to the data frame. 
  
# 3) Install the nycflights13 package: 
install.packages("nycflights13") 
# call the following library:  
library(nycflights13)
#, and then enter 
flights 
#(this will produce the flights data table)


# 4) Use and show R code that will indicate how many rows and how many columns 
# the flights data has.  Review and revisit your notes from STAT 412/612. 

nrow(flights)
ncol(flights)



# Describe a tibble (two or three sentences). Now use and show R code
# that verifies that flights is a tibble

# a tibble is a modern data frame. it is more concise and lazier in arguments. it is easier and 
# more managable.

tibble(flights)


# 5) Now Use and show R code (featuring a map function) that will output
# the type of each column of the flights tibble.

map(flights, typeof)


# 6)  Use and show R coding that features usage of a map function 
# to find the slope and the intercept of models for the different levels 
# of the Species variable of the iris data frame. 
# For each model, Sepal.Width predicts Sepal.Length.

iris


library(purrr)
library(dplyr)

factor(iris$species)


iris %>%
  split(.$species) %>%
  map(~lm(sepal_width ~ sepal_length, data = .))


# 7)
V <- list(12, 22, 27, 31.5, NA, 39, "east")
V
# For the given list above;
# 7a) Use and show R code to find the length of the list

length(V)

# 7b) Use and show R code that will extract the missing value
V[is.na(U)]

# 7c) Use and show R code that will extract the third object
V[3]

# 7d) Use and show R code that will extract the character string and the minimum number.
V[c(1,7)]

# 7e) Use one line of code to show that the seventh object is a character object.
str(V[7])
