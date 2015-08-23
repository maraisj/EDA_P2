#Exploratory Data Analysis
#Project 2 - Plot 6
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
#Change the data to a dpyl tbl, filter on baltimore and Los angeles group by Year and SSC level and fips
filtered<- tbl_df(NEISCC) %>% filter(fips=="24510"|fips=="06037") %>% filter(grepl("Mobile - On-Road",EI.Sector)) %>% 
        group_by(year,SCC.Level.Three,fips) %>% summarise("TotalEmissions"=sum(Emissions)) %>%
        mutate("City"=ifelse(fips=="06037","Los Angeles County","Baltimore City"))
#Get the totals of both for the All motors Vehicle part of the plot
filteredTotals<- tbl_df(NEISCC) %>% filter(fips=="24510"|fips=="06037") %>% filter(grepl("Mobile - On-Road",EI.Sector)) %>% 
        group_by(year,fips) %>% summarise("TotalEmissions"=sum(Emissions)) %>%
        mutate("City"=ifelse(fips=="06037","Los Angeles County","Baltimore City")) %>% 
        mutate("SCC.Level.Three"="All motor vehicle sources")
#Bind the totals level tbl with the SCC 
filteredAll<-rbind(filtered,filteredTotals)
#Create the ggplot
g<- ggplot(filteredAll,aes(x=year,y=TotalEmissions,colour=City))
p<-g+geom_point()+facet_wrap(~SCC.Level.Three,ncol=3,scales = "free_y")+stat_smooth(method="lm",se=FALSE)+
        scale_x_continuous(breaks = c(1999,2002,2005,2008))+
        labs(y="Total Emissions [Tons]",title="PM2.5 Total emissions comparison of motor vehicle sources in Two Cities")
png(filename = "plot6.png", width = 1050, height = 1000)
print(p)
dev.off()
