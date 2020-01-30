# QQ plot 
EPI = EPI_data$EPI

# do.points means black dots; vertical means verticle connection lines 
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
plot(ecdf(EPI), do.points=TRUE, verticals=TRUE)
plot(ecdf(EPI), do.points=FALSE, verticals=FALSE) 

help(qqnorm)
par(pty="s")
qqnorm(EPI) # plot 
qqline(EPI) # line 

# Make a Q-Q plot against the generating distribution by
x = seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
# 
# xlab means x label, same for y 
qqline(x)
help(qqplot)
help(seq) # generate regular sequences 
help(qt) # quantile function for the t distribution with df degrees of freedom
help(ppoints) # Generates the sequence of probability points
ppoints(10)

# With another column 
DALY = EPI_data$DALY
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 
qqplot(EPI, DALY)

# Comparing two distributions 
boxplot(EPI_data$EPI,EPI_data$DALY)
boxplot(EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM)

# -----------------------------------------------------------------------------------
# Linear basis and least-squares constraints
multivariate = read.csv("/Users/wangfeng/Desktop/ITWS 6600 Data Analytics/R Inclass Demos/multivariate.csv")
attach(multivariate)
View(multivariate)
mm = lm(Homeowners~Immigrant) # lm is used to fit linear models
mm
help(lm)
summary(mm)$coef 

plot(Homeowners~Immigrant) # scatter plot y~x
help(abline) # adds one or more straight lines through the current plot.
abline(mm) 
abline(mm,col = 'red', lwd=1) # lwd : The line width, a positive number, defaulting to 1
# col: color, can be numbers but I like color names 

# newImmigrantdata = data.frame(Immigrant = c(0, 20))
# mm %>% predict(newImmigrantdata)

mm$coefficients

# -----------------------------------------------------------------------------------
# Creating plots 
plot(mtcars$wt, mtcars$mpg)
View(mtcars)

# Install tidyverse or ggplot
install.packages("tidyverse")
library(ggplot2)
qqplot(mtcars$wt, mtcars$mpg)
# or like this 
qqplot(wt, mpg, data = mtcars)

# ggplot
ggplot(mtcars, aes(x = wt, y = mpg))+ geom_point()
help(ggplot)
plot(pressure$temperature, pressure$pressure, type = 'l')
plot(pressure$temperature, pressure$pressure, type = 'b')
# "l" for lines, 'b' for both
help(plot)
# Or use this to add points 
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = 'red')
points(pressure$temperature, pressure$pressure/2, col = 'blue')
qplot(pressure$temperature, pressure$pressure, geom = 'line')
help(qplot) # Quick plot
# similarly the x and y can be assigned as 
qplot(temperature, pressure, data = pressure, geom = 'line')

ggplot(pressure, aes(x = temperature, y = pressure)) + geom_line() + geom_point()


# Creating bar plots 
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl) # cyl is continuous
qplot(factor(mtcars$cyl)) # factor to make it discrete
qplot(factor(cyl), data = mtcars) # same 
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()


# Creating histograms 
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5)

# Box plot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth) # same
boxplot(len ~ supp + dose, data = ToothGrowth)

qplot(supp, len, data = ToothGrowth, geom = 'boxplot')
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot()

qplot(interaction(supp, dose), len, data = ToothGrowth, geom = 'boxplot')
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = 'boxplot')
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()


# Install Dplyr # Like python
install.packages('dplyr')
library(dplyr)

install.packages("nycflights13")
library(nycflights13)
dim(flights)
flights

filter(flights, month == 1, day == 1)

arrange(flights, year, month, day)
arrange(flights, desc(arr_delay)) # descending order 

select(flights, year, month, day) # select columns
select(flights, year:day)
select(flights, -(year:day)) # except these
select(flights, tail_num = tailnum)

rename(flights, tail_num = tailnum)

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
) # add new column by mutate 

transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
) # only keep the new variables

summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)

sample_n(flights, 10)
sample_frac(flights, 0.01)

# Grouped operations
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)
