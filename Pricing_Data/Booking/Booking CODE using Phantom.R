library(rvest)
library(ggplot2)
library(gridExtra)
library(RSelenium)
library(wdman)

mainurl<-"http://www.booking.com/index.html?label=gen173nr-1FCAEoggJCAlhYSDNiBW5vcmVmcgV1c19tb4gBAZgBMbgBB8gBDNgBAegBAfgBAqgCAw;sid=96686802ac9fd5657590aaff5fbbae00;click_from_logo=1"
pDrv <- phantomjs(port = 4567L)
remDr <- remoteDriver(browserName = "phantomjs", port = 4567L)
remDr$open(silent=TRUE)
remDr$navigate(mainurl)
Sys.sleep(10+rnorm(1))
mainresults <- read_html(remDr$getPageSource()[[1]])%>%
        html_nodes("#usp_choice") %>%
        html_text()
mainreviews_count<-read_html(remDr$getPageSource()[[1]]) %>%
        html_nodes("#usp_review h3") %>%
        html_text()
# Close session
remDr$close()
pDrv$stop()

#Cleanup data
mainresults_edited<-gsub(",",'',mainresults)
mainresults_edited<-regmatches(mainresults_edited,gregexpr('[0-9]+',
                                                           mainresults_edited))
mainresults_edited<-do.call(rbind.data.frame, mainresults_edited)
names(mainresults_edited)<-c("Properties_Worldwide","Vacation_Rentals","Destinations","Countries")
mainresults_edited$Date<-Sys.Date()



mainreviews_count_edited<-gsub(",",'',mainreviews_count)
mainreviews_count_edited<-regmatches(mainreviews_count_edited,
                                     gregexpr('[0-9]+',mainreviews_count_edited))
mainreviews_count_edited$Date<-Sys.Date()


#allcountries
countriesurl<-"https://www.booking.com/country.html?label=gen173nr-1FCAEoggJCAlhYSDNiBW5vcmVmcgV1c19tb4gBAZgBMbgBB8gBDNgBAegBAfgBAqgCAw;sid=96686802ac9fd5657590aaff5fbbae00"
pDrv <- phantomjs(port = 4567L)
remDr <- remoteDriver(browserName = "phantomjs", port = 4567L)
remDr$open(silent=TRUE)
remDr$navigate(countriesurl)
Sys.sleep(10+rnorm(1))
countriesresults<-read_html(remDr$getPageSource()[[1]]) %>%
        html_nodes(".block_header") %>%
        html_text()
# Close session
remDr$close()
pDrv$stop()


countriesresults_edited<-gsub(pattern="\n",replacement="",countriesresults)
temp1<-gsub('[[:digit:]]+', '', countriesresults_edited)
temp1<-gsub(pattern="  hotels",'',temp1)
temp2<-regmatches(countriesresults,gregexpr('[0-9]+',countriesresults))
#countriesresults_edited<-data.frame(cbind(temp1,temp2))
countriesresults_edited<-data.frame(cbind(temp1,do.call("rbind", temp2)))
names(countriesresults_edited)<-c("Country","Hotels")
countriesresults_edited$Date<-Sys.Date()

allbookingdata<-merge(mainresults_edited,mainreviews_count_edited,by.x="Date",by.y="Date")
allbookingdata_names<-names(allbookingdata)
allbookingdata_names[6]<-"Review_Count"
names(allbookingdata)<-allbookingdata_names

#Save data- created so we can't overwrite previous day's data (but can overwrite today's data)
setwd("/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Booking/")
filename1<-paste("Country DATA/Country Data",Sys.Date(),".csv")
write.csv(countriesresults_edited,file=filename1)
filename2<-paste("Main Results DATA/Main Results",Sys.Date(),".csv")
write.csv(allbookingdata,file=filename2)

#Combine the files into one data frame: Main Data First
wd<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Booking/Main Results DATA"
setwd(wd)
files<-list.files(path=wd, full.names=TRUE)
alldata_main<-data.frame(NULL)
len<-length(files)
for(i in 1:len){
        alldata_main<-rbind(alldata_main,read.csv(files[i]))
}

#Combine the files into one data frame: Country Data First
wd<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Booking/Country DATA"
setwd(wd)
files<-list.files(path=wd, full.names=TRUE)
alldata_country<-data.frame(NULL)
len<-length(files)
for(i in 1:len){
        alldata_country<-rbind(alldata_country,read.csv(files[i]))
}

#Eliminate the erroneous first column
alldata_main<-alldata_main[,-1]
alldata_country<-alldata_country[,-1]

#Set the date class & country character class
alldata_main$Date<-as.POSIXct(alldata_main$Date,format="%Y-%m-%d")
alldata_country$Date<-as.POSIXct(alldata_country$Date,format="%Y-%m-%d")
alldata_country$Country<-as.character(alldata_country$Country)

#Get continent mapping data
continents<-read.csv("/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Booking/Country to Continent List.csv")
continents$Country<-as.character(continents$Country)

#Curaçao fails (due to special symbol) so manually enter it.
continents<-rbind(continents,c("Curaçao","South America"))

#Map the countries to their continents in the alldata_country data
mergedcountrydata<-merge(alldata_country,continents,all.x=TRUE,by.x="Country",
                         by.y="Country")

#Save the combined data
filepath1<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Booking/ALLDATA_MAIN.csv"
filepath2<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Booking/ALLDATA_COUNTRIES.csv"
write.csv(alldata_main,file=filepath1)
write.csv(mergedcountrydata,file=filepath2)

#Graphing the data from the main webpage
plot1<-ggplot(alldata_main,aes(x=Date,y=Properties_Worldwide))+geom_line()+
        ggtitle("Booking.com: Number of Properties Worldwide")+
        xlab(NULL)+ylab(NULL)+
        theme(plot.title=element_text(hjust=0.5))
plot2<-ggplot(alldata_main,aes(x=Date,y=Vacation_Rentals))+geom_line()+
        ggtitle("Booking.com: Number of Vacation Rentals")+
        xlab(NULL)+ylab(NULL)+
        theme(plot.title=element_text(hjust=0.5))
plot3<-ggplot(alldata_main,aes(x=Date,y=Destinations))+geom_line()+
        ggtitle("Booking.com: Number of Destinations")+
        xlab(NULL)+ylab(NULL)+
        theme(plot.title=element_text(hjust=0.5))
plot4<-ggplot(alldata_main,aes(x=Date,y=Review_Count))+geom_line()+
        ggtitle("Booking.com: Number of Customer Reviews")+
        xlab(NULL)+ylab(NULL)+
        theme(plot.title=element_text(hjust=0.5))

setwd("/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Booking")
pdf("Summary Stats.pdf",paper="a4r",width=10,height=7)
grid.arrange(plot1,plot2,plot3,plot4,ncol=2,widths=unit(c(5,5),"inches"),
             heights=unit(c(3,3),"inches"))
dev.off()
