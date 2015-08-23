#Exploratory Data Analysis
#Project 2 - Plot 5
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
#Merge the two datasets into one dataset on SCC
NEISCC<-merge(NEI,SCC,by.x="SCC",by.y="SCC")
#Change the data to a dpyl tbl, filter on baltimore and group by Year and SSC level
baltimore <- tbl_df(NEISCC) %>% filter(fips=="24510") %>% filter(grepl("Mobile - On-Road",EI.Sector)) %>% 
        group_by(year,SCC.Level.Three) %>% summarise("TotalEmissions"=sum(Emissions))
#Change the data to a dpyl tbl, filter on baltimore and group by Year only. This will then be a total
#rolled up value for Baltimore
baltimoreTotal<-tbl_df(NEISCC) %>% filter(fips=="24510") %>% filter(grepl("Mobile - On-Road",EI.Sector)) %>% 
             group_by(year) %>% summarise("TotalEmissions"=sum(Emissions)) %>% mutate("SCC.Level.Three"="All motor vehicle sources")
#Combine the total with the summary of each
BaltimoreAll<-rbind(baltimore,baltimoreTotal)
baltimoreggplot<-ggplot(BaltimoreAll,aes(x=year,y=TotalEmissions))
bplot<- baltimoreggplot+geom_point()+labs(y="Total Emissions [Tons]",title="PM2.5 Total emissions from motor vehicle sources in Baltimore City")+
        facet_wrap(~SCC.Level.Three,ncol=2,scales = "free_y")+scale_x_continuous(breaks = c(1999,2002,2005,2008))+
        stat_smooth(method="lm",se=FALSE,colour="blue")
png(filename = "plot5.png", width = 800, height = 800)
print(bplot)
dev.off()