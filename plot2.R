#Exploratory Data Analysis
#Project 2 - Plot 2
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
#Filter on Baltimore
baltimore<-filter(dNEI,fips=="24510")
#Group by and summarise the data
baltimoreGroupBy<-group_by(baltimore,year)
baltimoreSummary<- summarise(baltimoreGroupBy,sum(Emissions))

#Construct a linear model to fit the data
linModel<-lm(baltimoreSummary$`sum(Emissions)`~baltimoreSummary$year)
#Get everything set to write to the png file
png(filename = "plot2.png", width = 800, height = 800)
par(mar=c(6, 6, 4, 2) + 0.1)
#Do  a base plot of the data without the x axis tick marks
plot(baltimoreSummary$year,baltimoreSummary$`sum(Emissions)`,xlab = "Year",
    ylab = "Total Emissions [Tons]",main="PM2.5 Total Emissions in Baltimore City, Maryland",xaxt="n",col="red",type="l",cex.lab=1.5, cex.main=1.5)
#Add the x Axis
axis(1,at=c(1999,2002,2005,2008),cex.lab=1.5)
# Calculate and add the linear model
a<-lm(baltimoreSummary$`sum(Emissions)`~baltimoreSummary$year)
abline(a)
# Add the legend
legend("topright",legend = c("Total Emissions","Linear Model of Total Emissions"),lty=c(1,1,1),col = c("red","black"), pt.cex = 1,cex=1.5)
dev.off()