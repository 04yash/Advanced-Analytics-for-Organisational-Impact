# 1. Import the necessary package
# Import the tidyverse package.
library('tidyverse')

# Import the data set.
# You can choose how to import the CSV file.
turtle_sales <- read.csv(file.choose(), header=T)

# View the data frame.
head(turtle_sales)

# Print the data frame.
turtle_sales 

# creating a new data frame from the subset of sales data frame.

# Remove the '' and 'region' columns.
turtle_sales <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# Check the new data frame.
head(turtle_sales)

# Use the summary() function.
summary(turtle_sales)


# Create a scatterplot
# Specify X as EU_Sales, y as Global_Sales , and turtle_sales as the data source 
# (the x-axis variable is passed first, followed by the y-axis,
#  and then the source of the data is specified).
qplot(EU_Sales, Global_Sales, data=turtle_sales)
qplot(Product, Platform, data=turtle_sales)

# Create a scatterplot with a -variable.
# Assign wage to the y variable, followed by the data source.
qplot(y=EU_Sales, data=turtle_sales)
qplot(y=Global_Sales, data=turtle_sales)

# Create a histogram
# First pass the x-variable, then specify the data source. 
qplot(Platform, data=turtle_sales)
qplot(Platform, bins=5, data=turtle_sales)

# Boxplot
qplot(Product, Platform, data=turtle_sales, geom='boxplot')


# Any relationship between Platform, EU_Sales, and NA_Sales?
# View the third plot.
qplot(Product,
      Platform,
      colour=Global_Sales,
      data=turtle_sales,
      geom='boxplot')
  

# Observations and Insights from initial data.

# we were able to eliminate various other columns which were non usable 
# the initial outlook suggest that x360 sales were highest
# the ps3 and pc games sales were in good proximity and were 2nd highest
# the other gaming units giving fights were wii and DS
# boxplot was able to deduce that sales of pc and  had product in 7500 range.

  


