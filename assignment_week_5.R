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

turtle_sales[is.na(turtle_sales)]

# Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

aggregate(Product~Global_Sales+EU_Sales+NA_Sales,turtle_sales,mean)
aggregate(Product~Global_Sales+EU_Sales+NA_Sales,turtle_sales,sum)

# Determine the unique values in each column
unique(turtle_sales$Product)

unique(turtle_sales$Platform)

unique(turtle_sales$Global_Sales)

#  bar plot is the best to compare the game sales.

# 4. Visualise data

# Create barplots.

barplot(table(turtle_sales$Platform),
        main='Product',
        xlab='Platform',
        ylab='Global_Sales',
        col='blue')

# Boxplot
qplot(Platform,Product, data=turtle_sales, colour=I('red'), geom='boxplot')

# View the histogram.
plot(hist(turtle_sales$Global_Sales))
plot(hist(turtle_sales$EU_Sales))
plot(hist(turtle_sales$NA_Sales))




# 3. Distribution of the data.

# Specify boxplot function.
boxplot(turtle_sales$Product)


# Specify histogram function.
hist(turtle_sales$Product)

# 4. Determine normality of data.

# Specify qqnorm function (draw a qqplot).
qqnorm(turtle_sales$Product)
qqnorm(turtle_sales$Global_Sales)
qqnorm(turtle_sales$EU_Sales)
qqnorm(turtle_sales$NA_Sales)


# Specify qqline function.
qqline(turtle_sales$Product) 
qqline(turtle_sales$Global_Sales) 
qqline(turtle_sales$EU_Sales) 
qqline(turtle_sales$NA_Sales)




## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(turtle_sales$Product)
shapiro.test(turtle_sales$Global_Sales)
shapiro.test(turtle_sales$EU_Sales)
shapiro.test(turtle_sales$NA_Sales)
## Skewness and Kurtosis
# Install the moments package and load the library.
install.packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
skewness(turtle_sales$Product) 
kurtosis(turtle_sales$Product)

# Determine the correlation

# Specify the cor function.
# Set the first and second variables

cor(turtle_sales$Product, turtle_sales$Global_Sales)
cor(turtle_sales$EU_Sales, turtle_sales$NA_Sales) 

# 2. Creating ggplot for better insight.

# Create a scatterplot to view result.
ggplot(turtle_sales, 
       mapping=aes(x= Global_Sales, y=Product, col=Platform)) +
  geom_point() + scale_x_continuous(breaks=seq(0, 70, 5), "Global Sales") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Number of products")


# Create a scatterplot to view result.
ggplot(turtle_sales, 
       mapping=aes(x= EU_Sales, y=Product, col=Platform)) +
  geom_point() + scale_x_continuous(breaks=seq(0, 70, 5), "European Sales") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Number of Product")

# Create a scatterplot to view result.
ggplot(turtle_sales, 
       mapping=aes(x= NA_Sales, y=Product, col=Platform)) +
  geom_point() + scale_x_continuous(breaks=seq(0, 70, 5), "North american Sales") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Number of Product")



# We have chosen the Platform vs Product plot.
ggplot(turtle_sales, aes(x=Platform, y=Product,)) +
  geom_boxplot(fill='yellow',
               notch=TRUE,
               outlier.color='blue')


# 2c) Create three new scatterplots


# Change colours, adjust size and alpha of points.
ggplot(turtle_sales,
       mapping=aes(x=NA_Sales, y=EU_Sales,)) +
  geom_point(color='purple',
             alpha=0.75,
             size=2.5) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 70, 5), "North American Sales") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "European Sales") +
  # Add a title and subtitle.
  labs(title="Relationship between north american and european data on various platform",
       subtitle=" the sales number are not that different") +
  # Facet by product.
  facet_wrap(~Platform)


# Change colours, adjust size and alpha of points.
ggplot(turtle_sales,
       mapping=aes(x=NA_Sales, y=Product)) +
  geom_point(color='red',
             alpha=0.75,
             size=2.5) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 70, 5), "North American Sales") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Product") +
  # Add a title and subtitle.
  labs(title="Relationship between north american sales and product purchase on various platform",
       subtitle=" the product purchase across north america ") +
  # Facet by product.
  facet_wrap(~Platform)

# Change colours, adjust size and alpha of points.
ggplot(turtle_sales,
       mapping=aes(x=Global_Sales, y=Product)) +
  geom_point(color='orange',
             alpha=0.75,
             size=2.5) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 70, 5), "Global  Sales") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Product") +
  # Add a title and subtitle.
  labs(title="Relationship between Global sales and product purchase across the platform",
       subtitle=" the product purchase across  the globe ") +
  # Facet by product.
  facet_wrap(~Platform)

# Change colours, adjust size and alpha of points.
ggplot(turtle_sales,
       mapping=aes(x=EU_Sales, y=Product)) +
  geom_point(color='green',
             alpha=0.75,
             size=2.5) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 70, 5), "European Sales") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Product") +
  # Add a title and subtitle.
  labs(title="Relationship between European sales and product purchase on various gaming platform",
       subtitle=" the product purchase across European Sales ") +
  # Facet by product.
  facet_wrap(~Platform)


# Any relationship between Platform, EU_Sales, and NA_Sales?
# View the third plot.
qplot(NA_Sales,
      EU_Sales,
      colour=Platform,
      data=turtle_sales,
      geom='boxplot') 
     
ggplot(data = turtle_sales, aes(x = Product , y =Platform , size = NA_Sales,colour=EU_Sales,)) +
  geom_line()


# Any relationship between Platform, Global_Sales, and NA_Sales?
# View the third plot.
qplot(NA_Sales,
      Global_Sales,
      colour=Platform,
      data=turtle_sales,
      geom='boxplot') 

ggplot(data = turtle_sales, aes(x = Product , y =Platform , size = Global_Sales,colour=NA_Sales,)) +
  geom_line()
ggplot(data = turtle_sales, aes(x = Product , y =Platform , size = NA_Sales,colour=Global_Sales)) +
  geom_line()


# Observations and Insights

# the first thing we notice when we sum and compare  the data points of north american sales and european sales is that the number are very much in proximity.
# we can establish that turtle games have a good north american sales in comparison to european market.
# the global scale market is huge and bring in lot of numbers
# the product numbers in global sales will go higher but in different platforms
# people are experimenting with technology and new fronts of enjoyment are being explored by everyone as can be seen in scatterplots with various outlooks
# overall the market for pc and ps4 and x360  is not going anywhere they have there sales and need to capitalized
# overall the data sales suggest the sales data in over 40 years has changed a lot of new variation of consoles and other enjoyable tech is available at hand 
# the growth of gaming industry seems to be splattering in global sales hence the turtle team needs to focus on the market space and their requirement.
# overall we can see wii sales did really well in on the markets be it global , north american, european.
# the comparison between north american and global sales the platforms sales of Wii,NES,GB,DS, performed better in north american market in comparsion to their sales in north america
# while comparing platforms sales in north american and European market the Wii platform had bigger sales number in european market in comparison to north american market.
# Sales number of gaming consoles platforms such as gameboy, nintendo DS, SNES,GBA were also surprising across all the market and hence these platforms are becoming a strong competition for established platforms like playstation,x360 and pc. 