### set the working directory correct and read the data 
### This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
str(NEI)
unique(NEI$fips)
unique(NEI$SCC)
unique(NEI$Pollutant)
unique(NEI$type)
unique(NEI$year)
str(SCC)
library(reshape2)
library(plyr)
### plot 1
### Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
### Using the base plotting system, make a plot showing the total PM2.5 emission from 
### all sources for each of the years 1999, 2002, 2005, and 2008.

dat.sum <- ddply(NEI, "year", summarize, total = sum(Emissions))

### Another simple way to do it!
### dat.sum <- tapply(NEI$Emissions, NEI$year, FUN = sum)
png(file="plot1.png", width=600, height=600, bg="white") 
with(dat.sum, barplot(names.arg = year, total, main = "US PM 2.5 Emissions",
                       ylab = "PM 2.5 emissions in tons",
                       xlab = "Year",
                       ylim = c(0, max(total)*1.05)))

dev.off()

### plot2
### Have total emissions from PM2.5 decreased in the Baltimore City, 
### Maryland (fips == 24510) from 1999 to 2008? Use the base plotting system 
### to make a plot answering this question.
Baltimore <- subset(NEI, fips == "24510")
Baltimore.sum <- ddply(Baltimore, "year", summarize, total = sum(Emissions))
png(file="plot2.png", width=600, height=600, bg="white") 
with(Baltimore.sum, barplot(names.arg = year, total, main = "Baltimore City PM 2.5 Emissions",
                       ylab = "PM 2.5 emissions in tons",
                       xlab = "Year",
                       ylim = c(0, max(total)*1.05)))
dev.off()

### plot3
### Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
### variable, which of these four sources have seen decreases in emissions from 1999–2008 
### for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the 
### ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
Baltimore <- subset(NEI, fips == "24510")
Baltimore.sep <- ddply(Baltimore, c("year", "type"), summarize, total = sum(Emissions))
# qplot with different panel
# qplot(year, total, data=Baltimore.sep, facets = .~type, binwidth = 2) + geom_smooth()
# g <- ggplot(data=Baltimore.sep, aes(year, total)) 
# g + facet_grid(. ~ type) + geom_bar(stat="identity", aes(fill=factor(year))) + 
#  xlab("Year") + ylab("PM 2.5 emissions in tons") + 
#  ggtitle("different types, PM 2.5 Emissions in Baltimore City, MD")
### start the plot
png(file="plot3.png", width=600, height=600, bg="white")
g <- ggplot(Baltimore.sep, aes(year, total, group = type, color = type))
g + geom_line(lwd = 2) + labs(title = " Different types, Baltimore City PM 2.5 Emissions") + 
  labs(y = "PM 2.5 emissions in tons")
dev.off()

### plot4
### Across the United States, how have emissions from coal 
### combustion-related sources changed from 1999–2008?
str(SCC)
### find the coal consumption but without coal mining
index <- grepl("[Cc]oal", SCC$Short.Name) & !grepl("[Cc]oal Mining", SCC$Short.Name)
coal <- SCC[index, 1] ### find the coal consumption SCC in NEI, length(coal) = 172
coal <- as.character(coal)
### find the required data 
coal.NEI <- NEI[NEI$SCC %in% coal, ]
### NOTE if using "coal %in% NEI$SCC" rather than "NEI$SCC %in% coal", then in big trouble

### sum the values for different years, can use aggregate, tapply, and ddply
### dat <- aggregate(Emissions ~ year, data=coal.NEI, sum)
dat <- ddply(coal.NEI, "year", summarize, total = sum(Emissions))
png(file="plot4.png", width=600, height=600, bg="white") 
with(dat, barplot(names.arg = year, total, main = "coal combustion-related sources, US PM 2.5 Emissions",
                      ylab = "PM 2.5 emissions in tons",
                      xlab = "Year",
                      ylim = c(0, max(total)*1.05)))

dev.off()

### plot5
### How have emissions from motor vehicle sources changed 
### from 1999–2008 in Baltimore City?
Baltimore.veh <- subset(NEI, fips == "24510" & type == "ON-ROAD")
dat <- ddply(Baltimore.veh, "year", summarize, total = sum(Emissions))
png(file="plot5.png", width=600, height=600, bg="white") 
with(dat, barplot(names.arg = year, total, main = "Baltimore motor vehicle, PM 2.5 Emissions",
                  ylab = "PM 2.5 emissions in tons",
                  xlab = "Year",
                  ylim = c(0, max(total)*1.05)))

dev.off()

### plot6
### Compare emissions from motor vehicle sources in Baltimore City 
### with emissions from motor vehicle sources in Los Angeles County, 
### California (fips == "06037"). Which city has seen greater changes 
### over time in motor vehicle emissions?
compared.dat <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD")
dat <- ddply(compared.dat, c("year", "fips"), summarize, total = sum(Emissions))
dat[dat$fip=="06037" ,  'fips'] <- "Los Angeles, California"
dat[dat$fip=="24510" ,  'fips'] <- "Baltimore City"
names(dat)[2] <- "county"
# qplot(year, total, data= dat, facets = .~fips, binwidth = 2) + geom_smooth()
png(file="plot6.png", width=600, height=600, bg="white")
g <- ggplot(dat, aes(year, total, group = county, color = county))
g + geom_point(colour = "black",  size = 2) +geom_smooth(linetype = 2, lwd = 1) + labs(title = "Motor vehicle, 
    Los Angeles, California VS Baltimore City PM, 2.5 Emissions") +
    labs(y = "PM 2.5 emissions in tons")
dev.off()



