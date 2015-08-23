#Exploratory Data Analysis
#Project 2 - Plot 4
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
#Filter on anything that contains "Coal" or "coal" in the EI.Sector Variable
coalNEISCC<-filter(NEISCC, grepl("coal|Coal",EI.Sector))
#Group by EI.Sector and year and then summarise on the total of Emissions using dplyr and the chain operator
coalNEISCCNSummary<-group_by(coalNEISCC,EI.Sector,year) %>% summarise("TotalEmissions"=sum(Emissions))
#Use ggplot to plot the data
coalgg<-ggplot(coalNEISCCNSummary,aes(x=year,y=TotalEmissions))
coalplot<- coalgg+ geom_point()+facet_wrap(~EI.Sector,ncol=1,scales = "free_y")+
        labs(y="Total Emissions [Tons]",title="PM2.5 Total Emissions from coal combusion sources in the US")+
        stat_smooth(method="lm",se=FALSE,colour="blue")+scale_x_continuous(breaks = c(1999,2002,2005,2008))
#Create the graphic
png(filename = "plot4.png", width = 800, height = 800)
print(coalplot)
dev.off()