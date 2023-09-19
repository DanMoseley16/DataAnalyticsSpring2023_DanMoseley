#EPI_data <- read.csv(”<path>/2010EPI_data.csv")
#or
#install.packages('readxl')
library('readxl')

EPI_data <- read.csv("C:/Users/moseld2/Dropbox/School Work/Fall 2023/Data Analytics/Labs/Lab 1/2010EPI_data.csv")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
#
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

#EPI
summary(EPI)
fivenum(EPI, na.rm = TRUE)
stem(EPI)
hist(EPI, main = 'Simple EPI Histogram')
hist(EPI, seq(30., 95., 1.0), prob = TRUE, main = 'EPI Histogram Data')
lines(density(EPI, na.rm = TRUE, bw = 1.))
lines(density(EPI, na.rm = TRUE, bw = "SJ"))
rug(EPI)


plot(ecdf(EPI), do.points=FALSE, verticals=TRUE, main = 'EPI Plot')
par(pty="s")
qqnorm(EPI)
qqline(EPI)
qqplot(qt(ppoints(250), df = 5), EPI, xlab = "Q-Q plot for t dsn")

#DALY
summary(DALY)
fivenum(DALY, na.rm = TRUE)
stem(DALY)
hist(DALY, main = 'Simple DALY Histogram')
lines(density(DALY, na.rm = TRUE, bw = 1.))
lines(density(DALY, na.rm = TRUE, bw = "SJ"))
rug(DALY)

plot(ecdf(DALY), do.points = FALSE, verticals = TRUE, main = 'DALY Plot')
qqnorm(DALY)
qqline(DALY)
qqplot(qt(ppoints(250), df = 5), DALY, xlab = "Q-Q plot for t dsn")

#WATER_H
summary(WATER_H)
fivenum(WATER_H, na.rm = TRUE)
stem(WATER_H)
hist(WATER_H, main = 'Simple Water Histogram')
lines(density(WATER_H, na.rm = TRUE, bw = 1.))
lines(density(WATER_H, na.rm = TRUE, bw = "SJ"))
rug(WATER_H)

plot(ecdf(WATER_H), do.points = FALSE, verticals = TRUE, main = 'Water Plot')
qqnorm(WATER_H)
qqline(WATER_H)
qqplot(qt(ppoints(250), df = 5), WATER_H, xlab = "Q-Q plot for t dsn")

#Comparison
boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(EPI, ENVHEALTH)
qqplot(EPI, ENVHEALTH)

boxplot(EPI, ECOSYSTEM)
qqplot(EPI, ECOSYSTEM)

boxplot(EPI, AIR_H)
qqplot(EPI, AIR_H)

boxplot(EPI, WATER_H)
qqplot(EPI, WATER_H)

boxplot(EPI, AIR_E)
qqplot(EPI, AIR_E)

boxplot(EPI, WATER_E)
qqplot(EPI, WATER_E)

boxplot(EPI, BIODIVERSITY)
qqplot(EPI, BIODIVERSITY)

help("distributions")

#Filtering
EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob = TRUE)
lines(density(ELand, na.rm = TRUE, bw = 1.))
lines(density(ELand, na.rm = TRUE, bw = "SJ"))
rug(ELand)

plot(ecdf(ELand), do.points = FALSE, verticals = TRUE, main = 'Water Plot')
qqnorm(ELand)
qqline(ELand)
qqplot(qt(ppoints(250), df = 5), ELand, xlab = "Q-Q plot for t dsn")

#Filtering Surface Water
EPISurfaceWater <- EPI[!No_surface_water]
ESurfaceWater <- EPISurfaceWater[!is.na(EPISurfaceWater)]
hist(ESurfaceWater)
hist(ESurfaceWater, seq(30., 95., 1.0), prob = TRUE)
lines(density(ESurfaceWater, na.rm = TRUE, bw = 1.))
lines(density(ESurfaceWater, na.rm = TRUE, bw = "SJ"))
rug(ESurfaceWater)

plot(ecdf(ESurfaceWater), do.points = FALSE, verticals = TRUE, main = 'Water Plot')
qqnorm(ESurfaceWater)
qqline(ESurfaceWater)
qqplot(qt(ppoints(250), df = 5), ESurfaceWater, xlab = "Q-Q plot for t dsn")

#Filtering Desert
EPIDesert <- EPI[!Desert]
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob = TRUE)
lines(density(EDesert, na.rm = TRUE, bw = 1.))
lines(density(EDesert, na.rm = TRUE, bw = "SJ"))
rug(EDesert)

plot(ecdf(EDesert), do.points = FALSE, verticals = TRUE, main = 'Water Plot')
qqnorm(EDesert)
qqline(EDesert)
qqplot(qt(ppoints(250), df = 5), EDesert, xlab = "Q-Q plot for t dsn")

#Filtering Population Density
EPIPopulationDensity <- EPI[!High_Population_Density]
EPopulationDensity <- EPIPopulationDensity[!is.na(EPIPopulationDensity)]
hist(EPopulationDensity)
hist(EPopulationDensity, seq(30., 95., 1.0), prob = TRUE)
lines(density(EPopulationDensity, na.rm = TRUE, bw = 1.))
lines(density(EPopulationDensity, na.rm = TRUE, bw = "SJ"))
rug(EPopulationDensity)

plot(ecdf(EPopulationDensity), do.points = FALSE, verticals = TRUE, main = 'Water Plot')
qqnorm(EPopulationDensity)
qqline(EPopulationDensity)
qqplot(qt(ppoints(250), df = 5), EPopulationDensity, xlab = "Q-Q plot for t dsn")

southAsia <- c('South Asia')
EPI_South_Asia <- which(EPI[GEO_subregion] %in% southAsia)
EPI_South_Asia

#other data
GRUMP_data <- read.csv("C:/Users/moseld2/Dropbox/School Work/Fall 2023/Data Analytics/Labs/Lab 1/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GRUMP_data)

attach(GRUMP_data)
fix(GRUMP_data)
Area
tf <- is.na(Area)
A <- Area[!tf] 

summary(Area)
fivenum(Area, na.rm = TRUE)
stem(Area)
hist(Area, main = 'Simple Area Histogram')
hist(Area, seq(30., 95., 1.0), prob = TRUE, main = 'Area Histogram Data')
lines(density(Area, na.rm = TRUE, bw = 1.))
lines(density(Area, na.rm = TRUE, bw = "SJ"))
rug(Area)


plot(ecdf(Area), do.points=FALSE, verticals=TRUE, main = 'Area Plot')
par(pty="s")
qqnorm(Area)
qqline(Area)
qqplot(qt(ppoints(250), df = 5), Area, xlab = "Q-Q plot for t dsn")
