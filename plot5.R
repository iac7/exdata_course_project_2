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

baltimore <- subset(vehicles, fips == "24510")

by_year <- group_by(baltimore, year)
sums <- summarise(by_year, sum(emissions))
names(sums) <- c("year", "emissions")

##build a plot
library("ggplot2")
g <- ggplot(sums, aes(year, emissions))
g+geom_line(color ="blue", alpha=1/3, size=2)+geom_point()+ ggtitle("Emissions from motor vehicle sources changed in Baltimore City")+xlab("")

#copy plot to png file (default is 480x480)
dev.copy(png, file = "plot5.png")
dev.off()


