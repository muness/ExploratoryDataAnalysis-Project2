## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(NEI)

class(NEI)

library(reshape2)
NEI_melt_by_year <- melt(NEI, id.vars = "year", measure.vars = "Emissions") 

head(NEI_melt_by_year)
NEI_pivot_by_year <- dcast(NEI_melt_by_year, year ~ variable, sum) 

plot(NEI_pivot_by_year, type="p", main="Emission by year")
# this shows that there has been a drop in emmissions from 1999 to 2008

