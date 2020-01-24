# Creating a dataframe
# Example: RPI weather dataframe 

days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')

help("data.frame")

RPI_Weather_Week <- data.frame(days, temp, snowed)

RPI_Weather_Week

head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,] # Showing the first row
RPI_Weather_Week[,1] # Showing the first column 
RPI_Weather_Week[1:5, c('days', 'temp')]
RPI_Weather_Week$temp

subset(RPI_Weather_Week, subset=snowed==TRUE)

# Sort the snowed column 
sorted.snowed <- order(RPI_Weather_Week['snowed']) # Return the row indices as integer
# Show the sorted in F, T 
sorted.snowed 
RPI_Weather_Week[sorted.snowed,] # Show the dataframe in the order of the sorted indices

# Show in the descending order of the temperature
dec.snow = order(-RPI_Weather_Week$temp)
dec.snow
RPI_Weather_Week[dec.snow,]

#------------------------------------------------------------------
# Creating dataframes
empty.dataframe = data.frame()  # Creating an empty dataframe
v1 = 1:10
letters # Built-in 26 letters 
v2 = letters[1:10]
df = data.frame(col.name.1 = v1, col.name.2 = v2)
df
df_another = data.frame(v1, v2)
df_another

# Writing to csv
write.csv(df, file = 'df_0123.csv')
df_read = read.csv('df_0123.csv')
df_read

GPW3 = read.csv(file.choose(),header = TRUE)
GPW3
head(GPW3)
summary(GPW3$Mean.Extent..sq.km.)
hist(GPW3$Mean.Extent..sq.km.) # Some NA exist
GPW3_NA = is.na(GPW3$Mean.Extent..sq.km.)
GPW3_meanex_noNA = GPW3$Mean.Extent..sq.km.[!GPW3_NA]
hist(GPW3_meanex_noNA)  # Still cannot 

# Read excel pacakge
install.packages("readxl")
library(readxl)
EPI = read_xls(file.choose())
head(EPI)

EPI_data = read.csv(file.choose(), header = TRUE, skip = 1)
View(EPI_data) # Open it 
attach(EPI_data)
EPI_data$EPI

# Use is.na to filter na 
tf = is.na(EPI_data$EPI)
# Filter out na 
E = EPI_data$EPI[!tf]
summary(E)
fivenum(E)

# Stem and leaf plot 
stem(E)
hist(E)
help(hist)
hist(E, seq(30., 95., 1.0), prob=TRUE)

help(lines)
lines(density(E,na.rm=TRUE,bw=1.))
lines(density(E,na.rm=TRUE,bw='SJ'))

help(rug)
rug(E)

plot(ecdf(E), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(E)
qqline(E)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

EPI_Land = E[!Landlock]
ELand = EPI_Land[!is.na(EPI_Land)] # Filter out na 
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

EPI_water = E[!No_surface_water]
EPI_water = EPI_water[!is.na(EPI_water)]
boxplot(EPI_water)

# Filter according to regions
EPI_Europe = E[EPI_regions == 'Europe']
EPI_Europe = EPI_Europe[!is.na(EPI_Europe)]
hist(EPI_Europe)
