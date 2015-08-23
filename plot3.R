#Exploratory Data Analysis
#Project 2 - Plot 3
#21 August 2015
#Datasource: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
#_______________________________________________________________________________
#Make sure these two files ("summarySCC_PM25.rds" & "Source_Classification_Code.rds") are in your working 
#directory

#Load library
library(dplyr)
library(ggplot2)

#Read the two RDS files into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Use Dplyr and the chain "%>%" to do it all in one line grouping by year and type 
baltimoresummarise <- tbl_df(NEI) %>% filter(fips=="24510") %>% group_by(year,type) %>% summarise("TotalEmissions"=sum(Emissions))
baltimore_ggplot<-ggplot(baltimoresummarise,aes(x=year,y=TotalEmissions))
bplot<-baltimore_ggplot+geom_point(colour="black")+facet_grid(.~type)+
        scale_x_continuous(breaks = c(1999,2002,2005,2008))+theme(axis.text.x = element_text(angle = -90))+
        stat_smooth(method="lm",se=FALSE,colour="blue") +
        labs(x = "Year", y = "Total Emissions [Tons]",title = "PM2.5 Total Emissions in Baltimore by different sources")
png(filename = "plot3.png", width = 800, height = 800)
print(bplot)
dev.off()