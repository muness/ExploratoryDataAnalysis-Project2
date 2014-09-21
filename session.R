NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(reshape2)


# 1.Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
NEI_melt_by_year <- melt(NEI, id.vars="year", measure.vars="Emissions") 
NEI_pivot_by_year <- dcast(NEI_melt_by_year, year ~ variable, sum) 

png("plot1.png")
plot(NEI_pivot_by_year, type="p", main="Emission by year")
# this shows that there has been a drop in emmissions from 1999 to 2008
dev.off()


# 2.Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
NEI_pivot_balt_by_year <- recast(subset(NEI, fips=="24510"), year ~ variable, sum, id.var="year", measure.var= "Emissions")

png("plot2.png")
plot(NEI_pivot_balt_by_year, type="p", main="Emission by year in baltimore")
# this shows that there has been a drop in emmissions from 1999 to 2008 in Baltimore (despite a temp increase from 2002 to 2005)
dev.off()


# 3.Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
NEI_balt_melt_by_year_and_type <- melt(subset(NEI, fips=="24510"), id.vars=c("year","type"), measure.vars="Emissions")
NEI_balt_cast_year_and_type <- dcast(NEI_balt_melt_by_year_and_type, year + type ~ variable, sum)

library(ggplot2)
png("plot3.png")
p<-ggplot(NEI_balt_cast_year_and_type, aes(x=year, y=Emissions)) + geom_line()
p + facet_wrap(~ type, scales="free_y")
# Between 1999 and 2008, non-road, on-road, nonpoint have seen decreases in emissions. Point has seen an increase.
dev.off()

# 4.Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
SCC_coal<-subset(SCC, grepl("Coal", EI.Sector))
NEI_coal_by_year <- recast(subset(NEI, SCC %in% SCC_coal$SCC), year ~ variable, sum, id.var="year", measure.var= "Emissions")
png("plot4.png")
plot(NEI_coal_by_year, type="p", main="Coal emissions by year")
# this shows that there has been a drop in Coal emmissions from 1999 to 2008
dev.off()

# 5.How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? 

