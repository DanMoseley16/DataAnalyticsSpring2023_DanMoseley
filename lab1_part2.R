install.packages("magrittr")
install.packages("ggplot2")
library("magrittr")
library("ggplot2")

plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)
plot(ecdf(EPI), do.points = TRUE, verticals = TRUE)

help("qqnorm")
help("qqplot")
par(pty = "s")

qqnorm(EPI)
qqline(EPI)

x <- seq(30,95,1)
x
x2 <- seq(30,95,2)
x2
x2 <- seq(30,96,2)
x2
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

#DALY
plot(ecdf(DALY), do.points = FALSE, verticals = TRUE)
plot(ecdf(DALY), do.points = TRUE, verticals = TRUE)

qqnorm(DALY)
qqline(DALY)

#WATER_H
plot(ecdf(WATER_H), do.points = FALSE, verticals = TRUE)
plot(ecdf(WATER_H), do.points = TRUE, verticals = TRUE)

qqnorm(WATER_H)
qqline(WATER_H)

boxplot(EPI, DALY)
boxplot(EPI, ENVHEALTH)
boxplot(ECOSYSTEM, DALY)
boxplot(ENVHEALTH, AIR_H)
boxplot(WATER_H, AIR_H)
boxplot(AIR_H, BIODIVERSITY)
boxplot(EPI, BIODIVERSITY)

#Multivariate
multivariate <- read.csv("B:/School Work/Data Analytics/multivariate.csv")
head(multivariate)
attach(multivariate)

help(lm)

mm <- lm(Homeowners ~ Immigrant)
mm
summary(mm)$coef

plot(Homeowners ~ Immigrant)
help(abline)
abline(mm)
abline(mm, col = 2, lwd = 3)

newImmigrantdata <- data.frame(Immigrant = c(0, 20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm, col = 3, lwd = 3)
attributes(mm)
mm$coefficients

plot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")
qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x = temperature, y = pressure)) + geom_line() + geom_point()

#Creating Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generates a table of counts
qplot(mtcars$cyl) #cyl is continuous here
qplot(factor(mtcars$cyl)) #treat cyl as discrete
#Bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()

#Creating Histograms
#View the distribution of one-dimensional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) #specify approximate # of bins with breaks
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5)

#Creating Box-Plot
plot(ToothGrowth$supp, ToothGrowth$len)
#Formula Syntax
boxplot(len ~ supp, data = ToothGrowth)
#This syntax you can combine variables on the x-axis
#Put interaction of two variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth)
#with ggplot2 you can get the same results as above
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
#if the two vectors are in the same dataframe, you can use the following syntax
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
#in ggplot2, the above is equivalent to:
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot()
#Using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
#You can write the same thing above, get the columns from the dataframe
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
#using ggplot() you can do the same thing and it is equivalent to:
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()
