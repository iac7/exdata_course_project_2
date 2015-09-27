# zip files download from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# store in working diretory

#reading data
nei <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

##prepare the data
# change variable names to lower cases for cleaner data
names(nei) <- tolower(names(nei))

# subset the coal data
library(dplyr)
scc_short <- select(scc, SCC, EI.Sector)
merge <- merge(nei, scc_short, by.x = "scc", by.y = "SCC")

motor <- grepl("Vehicles", merge$EI.Sector)
vehicles <- filter(merge, EI.Sector = motor)
#or this command can be used: vehicles <- filter (merge, EI.Sector %in% c("Mobile - On-Road Gasoline Light Duty Vehicles", "Mobile - On-Road Gasoline Heavy Duty Vehicles", "Mobile - On-Road Diesel Light Duty Vehicles", "Mobile - On-Road Diesel Heavy Duty Vehicles"))

balti_la <- subset(vehicles, fips == c("24510", "06037"))

by_year <- group_by(balti_la, year, fips)
sums <- summarise(by_year, sum(emissions))
names(sums) <- c("year", "fips", "emissions")
sums <- transform(sums, fips = factor(fips))

by_year2 <- group_by(balti_la, fips, year)
sums2 <- summarise(by_year2, sum(emissions))
names(sums2) <- c("fips", "year", "emissions")
sums2 <- transform(sums2, fips = factor(fips))



##build a plot
library("ggplot2")
qplot(year, emissions, data = sums, color = fips, geom = "line")+xlab("")


g <- ggplot(sums2, aes(year, emissions, color = fips))
g+geom_line()+facet_wrap(~fips)+geom_point(size =4)+labs(title="Emissions from motor vehicles in Baltimore and Los Angeles", x= "", color="Counties")


#copy plot to png file (default is 480x480)
dev.copy(png, file = "plot6.png")
dev.off()


