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

# Remove the 'Ranking' and 'region' columns.
turtle_sales <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)



# Use the summary() function.
summary(turtle_sales)

turtle_sales[is.na(turtle_sales)]

# Sense-check the new data set.
# The top six rows of the data frame.
head(turtle_sales)

sapply(turtle_sales, mode)



###############################################################################

# 3. Identify relationships between the two variables - Global_Sales and Product.



# Plot the relationship with base R graphics.
plot(turtle_sales$Global_Sales, turtle_sales$Product)


###############################################################################

# 4. Fit a simple linear regression model.

# Create a model with only one x variable.
model1 <- lm(Global_Sales~Product,
             data=turtle_sales)


# View the model.
model1



# View more outputs for the model - the full regression table.
summary(model1)


# 5. Plot the model.

# View residuals on a plot.
plot(model1$residuals)



# Plot the relationship with base R graphics.
plot(turtle_sales$Global_Sales, turtle_sales$Product)
coefficients(model1)


# Add line-of-best-fit.
abline(coefficients(model1))

# 6. Create a log transformation.

# Complete a log transformation with dplyr's mutate() function.
turtle_sales <- mutate(turtle_sales, 
              logProduct=log(Product))


# View new object with new variable.
head(turtle_sales)


# Create a new model using logProduct.
model2 <- lm(logProduct~Global_Sales,
             data=turtle_sales)


# View full regression table.
summary(model2)



# Plot the relationship between Global_Sales and logProduct.
plot(turtle_sales$Global_Sales, turtle_sales$logProduct)


# Add a line-of-best fit to existing plot.
abline(coefficients(model2))

turtle_sales <-mutate(turtle_sales,logindex=log(Product))

head(turtle_sales)

model12<-lm(logindex~Product,data=turtle_sales)

summary(model12)
plot(turtle_sales$Product,turtle_sales$logindex)
abline(coefficients(model12))
tail(turtle_sales)
predict(model12,turtle_sales)


# multiple linear regression 

# 1. Prepare your workstation

# Import the tidyverse package.
library(tidyverse)


# Import the data set (turtle_sales.csv). 
stock <- read.csv('turtle_sales.csv', header=TRUE)
stock <- read.csv(file.choose(), header=T)


# Remove the 'Ranking' and 'region' columns.
stock <- select(stock, -Ranking, -Year, -Genre, -Publisher, -Platform)

# Sense check using the summary() function.
head(stock)
summary(stock)

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (stock) and set 
# character size (cex=2).
corPlot(stock, cex=2)


# Create a new object and 
# specify the lm function and the variables.
modela = lm(Product~NA_Sales+EU_Sales, data=stock)

# Print the summary statistics.
summary(modela)

# Load the new data file (turtle_sales.csv) and view its structure.
turtle_sales <- read.csv(file.choose(), header=TRUE)


# Add new variables.
modelb = lm(Product~NA_Sales+EU_Sales+Global_Sales,
            data=stock)

# Change the model name.
summary(modelb)

# Load the new data file (turtle_Sales.csv) and view its structure.
stock1 <- read.csv(file.choose(), header=TRUE)

# View the data.
str(stock1)

# Create a new object and specify the predict function.
predictTest = predict(modelb, newdata=stock1,
                      interval='confidence')

# Print the object.
predictTest
turtle_sales

#  Predictions based on given values
# as it can be seen that modela is  better then modellb 
# we can see that global sales was a significant variable in these regressions 
# the correlation plot had a good insight to offer
# the linear regression model helped us understand a that global sales were affected by the product range and it was scattring.

 
# 5. Observations and insights
# we were able to observe closely how the north american sales and eurepean sales were different audience market compared to global scale.
# some new platform sales in all the markets is a good sign as it shows that customers are transitioning towards new experiences.
# when used the linear regression on product numbers the data suggested that p value was very small and hence the product was a signifcant variable.
# we used the logindex function on product to better adjust new r value value which changed from 19.44% to 42.11%
# after mutating the logproduct value the new adjusted value came up to 79.83% and p value became even smaller
# the abline on the new logproduct was adjusted and had all the data points around the line 
# after doing multiple regression on the combined data set of sales data against the product we could se that first value was 2829 which after prediction was in lwr range 2966 close to the actual value and the first predicted value was 3549 uper value was in 4132 and as follows
# the new plotted abcoeffecintline on model 12 shows the adjusted line and linear data.
# the turtle games has a new front in terms of exploring the overall market base with new platforms doing really good and especially targeting certain platforms in certain market zones such as NES,Wii,GB,GBA,SNES doing really well in north american market space.
# but with the relative results from regressions and numbers reflecting that the  north american  market is something where they did good
# but the european market needs some figuring out as the sales such vast continent can bring some good business 

# NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
#NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
#NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
#NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
#NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.


