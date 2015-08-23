#Exploratory Data Analysis
#Project 2 - Plot 1
#21 August 2015
#Datasource: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
#_______________________________________________________________________________
#Make sure these two files ("summarySCC_PM25.rds" & "Source_Classification_Code.rds") are in your working 
#directory

#Load library
library(dplyr)

#Read the two RDS files into NEI and SCC
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##Use Dplyr to format the data
#create a dplyr tbl dataframe
dNEI<-tbl_df(NEI)
#Do a group by on the data set by year
dNEIg<- group_by(dNEI,year)
#Sum the emmisions using dplyr
totalsPerYear<-summarise(dNEIg,sum(Emissions))
#Create the PNG file
png(filename = "plot1.png", width = 800, height = 800)
plot(totalsPerYear$year,totalsPerYear$`sum(Emissions)`,type='l',xlab = "Year",
     ylab = "Total Emissions [Tons]",main="PM2.5 Total Emissions in the USA",xaxt='n',col="red")
#change the ticks on the x axis to show the year from 1999 to 2008
axis(1,at=c(1999,2002,2005,2008))
dev.off()