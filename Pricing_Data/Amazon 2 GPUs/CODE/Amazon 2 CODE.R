library(rvest)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(stringr)
library(plyr)
library(quantmod)

#Nvidia Quadro_lists first 60 results
scraping<-read_html("https://www.amazon.com/s/ref=sr_nr_p_n_condition-type_0?fst=as%3Aoff&rh=n%3A172282%2Cn%3A541966%2Cn%3A193870011%2Cn%3A284822%2Ck%3Agraphics+card%2Cp_n_feature_keywords_five_browse-bin%3A6147188011%2Cp_85%3A2470955011%2Cp_n_condition-type%3A2224371011&keywords=graphics+card&ie=UTF8&qid=1491416500&rnid=2224369011&lo=computers")
quadro<-scraping %>%
        html_nodes(".s-item-container") %>%
        html_text()

#Clean data
quadro_name<-sapply(strsplit(quadro, "\n"), `[[`, 1) #gets name by grabbing everything before \n
quadro_price<-sapply(strsplit(quadro, "\\$"), "[", 2) #grabs all the characters after the first $ (and before the second $)
quadro_price<-sapply(strsplit(quadro_price, "Prime"), "[", 1) #grabs all the characters before "Prime"
quadro_price<-sapply(strsplit(quadro_price, "Only"), "[", 1) #grabs all the characters before "Only"
quadro_price<-gsub("\n                    ",".",quadro_price) #puts the decimal place in
quadro_price<-gsub("\n","",quadro_price) #removes \n
quadro_price<-str_trim(quadro_price,"right") #removes ending white spaces

#Put into Data Frame
quadrodata<-data.frame(quadro_name,quadro_price,"Nvidia Quadro")
names(quadrodata)<-c("Product_Name","Price","Brand")



#Nvidia GeForce_lists first 60 results
scraping<-read_html("https://www.amazon.com/s/ref=sr_nr_p_n_condition-type_0?fst=as%3Aoff&rh=n%3A172282%2Cn%3A541966%2Cn%3A193870011%2Cn%3A284822%2Ck%3Agraphics+card%2Cp_n_feature_keywords_five_browse-bin%3A6147187011%2Cp_85%3A2470955011%2Cp_n_condition-type%3A2224371011&keywords=graphics+card&ie=UTF8&qid=1491416489&rnid=2224369011&lo=computers")
geforce<-scraping %>%
        html_nodes(".s-item-container") %>%
        html_text()

#Clean data
geforce_name<-sapply(strsplit(geforce, "\n"), `[[`, 1) #gets name by grabbing everything before \n
geforce_price<-sapply(strsplit(geforce, "\\$"), "[", 2) #grabs all the characters after the first $ (and before the second $)
geforce_price<-sapply(strsplit(geforce_price, "Prime"), "[", 1) #grabs all the characters before "Prime"
geforce_price<-sapply(strsplit(geforce_price, "Only"), "[", 1) #grabs all the characters before "Only"
geforce_price<-gsub("\n                    ",".",geforce_price) #puts the decimal place in
geforce_price<-gsub("\n","",geforce_price) #removes \n
geforce_price<-str_trim(geforce_price,"right") #removes ending white spaces

#Put into Data Frame
geforcedata<-data.frame(geforce_name,geforce_price,"Nvidia GeForce")
names(geforcedata)<-c("Product_Name","Price","Brand")



#AMD Radeon_lists first 60 results
scraping<-read_html("https://www.amazon.com/s/ref=sr_nr_p_n_condition-type_0?fst=as%3Aoff&rh=n%3A172282%2Cn%3A541966%2Cn%3A193870011%2Cn%3A284822%2Ck%3Agraphics+card%2Cp_n_feature_keywords_five_browse-bin%3A8259091011%2Cp_85%3A2470955011%2Cp_n_condition-type%3A2224371011&keywords=graphics+card&ie=UTF8&qid=1491416483&rnid=2224369011&lo=computers")
radeon<-scraping %>%
        html_nodes(".s-item-container") %>%
        html_text()

#Clean data
radeon_name<-sapply(strsplit(radeon, "\n"), `[[`, 1) #gets name by grabbing everything before \n
radeon_price<-sapply(strsplit(radeon, "\\$"), "[", 2) #grabs all the characters after the first $ (and before the second $)
radeon_price<-sapply(strsplit(radeon_price, "Prime"), "[", 1) #grabs all the characters before "Prime"
radeon_price<-sapply(strsplit(radeon_price, "Only"), "[", 1) #grabs all the characters before "Only"
radeon_price<-gsub("\n                    ",".",radeon_price) #puts the decimal place in
radeon_price<-gsub("\n","",radeon_price) #removes \n
radeon_price<-str_trim(radeon_price,"right") #removes ending white spaces

#Put into Data Frame
radeondata<-data.frame(radeon_name,radeon_price,"AMD Radeon")
names(radeondata)<-c("Product_Name","Price","Brand")


#Combine into one data frame
gpudata<-rbind(quadrodata,geforcedata,radeondata)
gpudata$Price<-gsub(",","",gpudata$Price) #eliminate the comma in the price
#Add date column
gpudata$Date<-Sys.Date()


#Save data- created so we can't overwrite previous day's data (but can overwrite today's data)
wd<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Amazon 2 GPUs/DATA/Daily Data"
setwd(wd)
filename<-paste("Amazon GPU Data",Sys.Date(),".csv")
write.csv(gpudata,file=filename)

#Combine all Saved Data into one file- it is ok to overwrite previously saved data
#This data will be used for graphing
files<-list.files(path=wd, full.names=TRUE)
alldata<-data.frame(NULL)
len<-c(1:length(files))
for(i in len){
        alldata<-rbind(alldata,read.csv(files[i]))
}
#Eliminate erroneous first column
alldata<-alldata[-1]
#Set column classes
alldata$Price<-as.numeric(as.character(alldata$Price))
alldata$Date<-as.Date(alldata$Date,format="%Y-%m-%d")



#Eliminate certain data
elim<-c("Lenovo 55Y8963 NVIDIA SLI Connector",
        "IBM 33L3543 Nvidia Quadro4 200 NVS 64MB SDRAM AGP 4x Graphics Card",
        "PCIe PCI-e Power Cable for Mac G5 nVidia ATI Video Card - High Quality")
alldata<-alldata[!alldata$Product_Name %in%elim,]



#Save Full Data
wd2<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Amazon 2 GPUs/DATA"
setwd(wd2)
write.csv(alldata,file="ALL DATA.csv")


#Plot Data and Save
unique_product_names<-unique(alldata$Product_Name)
alldata_cumulative<-NULL
for (i in 1:length(unique_product_names)){
        temp<-alldata[alldata$Product_Name==unique_product_names[i],]
        temp$Delt.1.arithmetic<-Delt(temp$Price)
        temp$Delt.1.arithmetic[1]<-0
        temp$Cumulative_Return<-cumprod(1+temp$Delt.1.arithmetic)-1
        alldata_cumulative<-rbind(alldata_cumulative,temp)
}


#Subset Data
quadro_alldata<-alldata_cumulative[alldata_cumulative$Brand=="Nvidia Quadro",]
geforce_alldata<-alldata_cumulative[alldata_cumulative$Brand=="Nvidia GeForce",]
radeon_alldata<-alldata_cumulative[alldata_cumulative$Brand=="AMD Radeon",]
#Quantiles
quant_quadro<-quantile(quadro_alldata$Price,na.rm=TRUE)
quant_geforce<-quantile(geforce_alldata$Price,na.rm=TRUE)
quant_radeon<-quantile(radeon_alldata$Price,na.rm=TRUE)
#quadro subset
quadro_alldata$SubGroup<-1:length(quadro_alldata$Product_Name)
quadro_alldata<-na.omit(quadro_alldata)
for (i in 1:length(quadro_alldata$Product_Name)){
        if(quadro_alldata$Price[i]<quant_quadro[2]){
                quadro_alldata$SubGroup[i]<-paste("Quantile 1: Less than $",quant_quadro[2],sep="")
        }else if(quadro_alldata$Price[i]<quant_quadro[3]){
                quadro_alldata$SubGroup[i]<-paste("Quantile 2: Between $",quant_quadro[2]," and $",quant_quadro[3],sep="")
        }else if(quadro_alldata$Price[i]<quant_quadro[4]){
                quadro_alldata$SubGroup[i]<-paste("Quantile 3: Between $",quant_quadro[3]," and $",quant_quadro[4],sep="")
        }else if(quadro_alldata$Price[i]<1000000){
                quadro_alldata$SubGroup[i]<-paste("Quantile 4: Greater than $",quant_quadro[4],sep="")
        }
}
quadro_alldata$SubGroup<-factor(quadro_alldata$SubGroup,
                                levels=c(paste("Quantile 1: Less than $",quant_quadro[2],sep=""),
                                         paste("Quantile 2: Between $",quant_quadro[2]," and $",quant_quadro[3],sep=""),
                                         paste("Quantile 3: Between $",quant_quadro[3]," and $",quant_quadro[4],sep=""),
                                         paste("Quantile 4: Greater than $",quant_quadro[4],sep="")))

#geforce subset
geforce_alldata$SubGroup<-1:length(geforce_alldata$Product_Name)
geforce_alldata<-na.omit(geforce_alldata)
for (i in 1:length(geforce_alldata$Product_Name)){
        if(geforce_alldata$Price[i]<quant_geforce[2]){
                geforce_alldata$SubGroup[i]<-paste("Quantile 1: Less than $",quant_geforce[2],sep="")
        }else if(geforce_alldata$Price[i]<quant_geforce[3]){
                geforce_alldata$SubGroup[i]<-paste("Quantile 2: Between $",quant_geforce[2]," and $",quant_geforce[3],sep="")
        }else if(geforce_alldata$Price[i]<quant_geforce[4]){
                geforce_alldata$SubGroup[i]<-paste("Quantile 3: Between $",quant_geforce[3]," and $",quant_geforce[4],sep="")
        }else if(geforce_alldata$Price[i]<1000000){
                geforce_alldata$SubGroup[i]<-paste("Quantile 4: Greater than $",quant_geforce[4],sep="")
        }
}
geforce_alldata$SubGroup<-factor(geforce_alldata$SubGroup,
                                 levels=c(paste("Quantile 1: Less than $",quant_geforce[2],sep=""),
                                          paste("Quantile 2: Between $",quant_geforce[2]," and $",quant_geforce[3],sep=""),
                                          paste("Quantile 3: Between $",quant_geforce[3]," and $",quant_geforce[4],sep=""),
                                          paste("Quantile 4: Greater than $",quant_geforce[4],sep="")))


#radeon subset
radeon_alldata$SubGroup<-1:length(radeon_alldata$Product_Name)
radeon_alldata<-na.omit(radeon_alldata)
for (i in 1:length(radeon_alldata$Product_Name)){
        if(radeon_alldata$Price[i]<quant_radeon[2]){
                radeon_alldata$SubGroup[i]<-paste("Quantile 1: Less than $",quant_radeon[2],sep="")
        }else if(radeon_alldata$Price[i]<quant_radeon[3]){
                radeon_alldata$SubGroup[i]<-paste("Quantile 2: Between $",quant_radeon[2]," and $",quant_radeon[3],sep="")
        }else if(radeon_alldata$Price[i]<quant_radeon[4]){
                radeon_alldata$SubGroup[i]<-paste("Quantile 3: Between $",quant_radeon[3]," and $",quant_radeon[4],sep="")
        }else if(radeon_alldata$Price[i]<1000000){
                radeon_alldata$SubGroup[i]<-paste("Quantile 4: Greater than $",quant_radeon[4],sep="")
        }
}
radeon_alldata$SubGroup<-factor(radeon_alldata$SubGroup,
                                levels=c(paste("Quantile 1: Less than $",quant_radeon[2],sep=""),
                                         paste("Quantile 2: Between $",quant_radeon[2]," and $",quant_radeon[3],sep=""),
                                         paste("Quantile 3: Between $",quant_radeon[3]," and $",quant_radeon[4],sep=""),
                                         paste("Quantile 4: Greater than $",quant_radeon[4],sep="")))

#ggplot(quadro_alldata,aes(x=Date,y=Delt.1.arithmetic,col=Product_Name))+
#        geom_line()+facet_wrap(~SubGroup,nrow=2)+theme(legend.position='none')

#SAVE THE DATA TO A PDF
setwd("/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Amazon 2 GPUs/DATA")
pdf("Amazon Cumulative Pricing.pdf")
ggplot(alldata_cumulative,aes(x=Date,y=Cumulative_Return))+
        geom_smooth(method='loess')+facet_wrap(~Brand)+
        xlab(NULL)+ylab("Cumulative Price Return")+
        scale_y_continuous(labels = scales::percent)
ggplot(alldata_cumulative,aes(x=Date,y=Delt.1.arithmetic))+
        geom_smooth(method='loess')+facet_wrap(~Brand)+
        xlab(NULL)+ylab("Daily Price Returns")+
        scale_y_continuous(labels = scales::percent)
ggplot(quadro_alldata,aes(x=Date,y=Delt.1.arithmetic))+
        geom_smooth(method='loess')+facet_wrap(~SubGroup)+
        xlab(NULL)+ylab("Daily Price Returns")+
        ggtitle("Nvidia Quadro's Daily Returns")+
        theme(plot.title=element_text(hjust=0.5))+
        scale_y_continuous(labels = scales::percent)
ggplot(quadro_alldata,aes(x=Date,y=Cumulative_Return))+
        geom_smooth(method='loess')+facet_wrap(~SubGroup)+
        xlab(NULL)+ylab("Cumulative Price Returns")+
        ggtitle("Nvidia Quadro's Cumulative Returns")+
        theme(plot.title=element_text(hjust=0.5))+
        scale_y_continuous(labels = scales::percent)
ggplot(geforce_alldata,aes(x=Date,y=Delt.1.arithmetic))+
        geom_smooth(method='loess')+facet_wrap(~SubGroup)+
        xlab(NULL)+ylab("Daily Price Returns")+
        ggtitle("Nvidia GeForces's Daily Returns")+
        theme(plot.title=element_text(hjust=0.5))+
        scale_y_continuous(labels = scales::percent)
ggplot(geforce_alldata,aes(x=Date,y=Cumulative_Return))+
        geom_smooth(method='loess')+facet_wrap(~SubGroup)+
        xlab(NULL)+ylab("Cumulative Price Returns")+
        ggtitle("Nvidia GeForces's Cumulative Returns")+
        theme(plot.title=element_text(hjust=0.5))+
        scale_y_continuous(labels = scales::percent)
ggplot(radeon_alldata,aes(x=Date,y=Delt.1.arithmetic))+
        geom_smooth(method='loess')+facet_wrap(~SubGroup)+
        xlab(NULL)+ylab("Daily Price Returns")+
        ggtitle("AMD Radeon's Daily Returns")+
        theme(plot.title=element_text(hjust=0.5))+
        scale_y_continuous(labels = scales::percent)
ggplot(radeon_alldata,aes(x=Date,y=Cumulative_Return))+
        geom_smooth(method='loess')+facet_wrap(~SubGroup)+
        xlab(NULL)+ylab("Cumulative Price Returns")+
        ggtitle("AMD Radeon's Cumulative Returns")+
        theme(plot.title=element_text(hjust=0.5))+
        scale_y_continuous(labels = scales::percent)
dev.off()


if(1==2){
        ggplot(quadro_alldata,aes(x=Date,y=Cumulative_Return))+
                stat_summary(fun.y="mean", geom="line")+facet_wrap(~SubGroup)+
                xlab(NULL)+ylab("Cumulative Price Returns")+
                ggtitle("Nvidia Quadro's Cumulative Returns")+
                theme(plot.title=element_text(hjust=0.5))+
                scale_y_continuous(labels = scales::percent)
        
        
        #DECILES instead of QUINTILES
        quant_quadro<-quantile(quadro_alldata$Price,na.rm=TRUE,probs=seq(0,1,0.1))
        #quadro subset
        quadro_alldata$SubGroup<-1:length(quadro_alldata$Product_Name)
        quadro_alldata<-na.omit(quadro_alldata)
        for (i in 1:length(quadro_alldata$Product_Name)){
                if(quadro_alldata$Price[i]<quant_quadro[2]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 1: Less than $",quant_quadro[2],sep="")
                }else if(quadro_alldata$Price[i]<quant_quadro[3]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 2: Between $",quant_quadro[2]," and $",quant_quadro[3],sep="")
                }else if(quadro_alldata$Price[i]<quant_quadro[4]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 3: Between $",quant_quadro[3]," and $",quant_quadro[4],sep="")
                }else if(quadro_alldata$Price[i]<quant_quadro[5]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 4: Between $",quant_quadro[4]," and $",quant_quadro[5],sep="")
                }else if(quadro_alldata$Price[i]<quant_quadro[6]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 5: Between $",quant_quadro[5]," and $",quant_quadro[6],sep="")
                }else if(quadro_alldata$Price[i]<quant_quadro[7]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 6: Between $",quant_quadro[6]," and $",quant_quadro[7],sep="")
                }else if(quadro_alldata$Price[i]<quant_quadro[8]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 7: Between $",quant_quadro[7]," and $",quant_quadro[8],sep="")
                }else if(quadro_alldata$Price[i]<quant_quadro[9]){
                        quadro_alldata$SubGroup[i]<-paste("Decile 8: Between $",quant_quadro[8]," and $",quant_quadro[9],sep="")
                }else if(quadro_alldata$Price[i]<1000000){
                        quadro_alldata$SubGroup[i]<-paste("Decile 9: Greater than $",quant_quadro[9],sep="")
                }
                
        }
        quadro_alldata$SubGroup<-factor(quadro_alldata$SubGroup,
                                        levels=c(paste("Decile 1: Less than $",quant_quadro[2],sep=""),
                                                 paste("Decile 2: Between $",quant_quadro[2]," and $",quant_quadro[3],sep=""),
                                                 paste("Decile 3: Between $",quant_quadro[3]," and $",quant_quadro[4],sep=""),
                                                 paste("Decile 4: Between $",quant_quadro[4]," and $",quant_quadro[5],sep=""),
                                                 paste("Decile 5: Between $",quant_quadro[5]," and $",quant_quadro[6],sep=""),
                                                 paste("Decile 6: Between $",quant_quadro[6]," and $",quant_quadro[7],sep=""),
                                                 paste("Decile 7: Between $",quant_quadro[7]," and $",quant_quadro[8],sep=""),
                                                 paste("Decile 8: Between $",quant_quadro[8]," and $",quant_quadro[9],sep=""),
                                                 paste("Decile 9: Greater than $",quant_quadro[9],sep="")))
        
        ggplot(quadro_alldata,aes(x=Date,y=Delt.1.arithmetic))+
                stat_summary(fun.y="mean", geom="line")+
                facet_wrap(~SubGroup)+
                xlab(NULL)+ylab("Daily Price Returns")+
                ggtitle("Nvidia Quadro's Daily Returns")+
                theme(plot.title=element_text(hjust=0.5))+
                scale_y_continuous(labels = scales::percent)
        ggplot(quadro_alldata,aes(x=Date,y=Cumulative_Return))+
                stat_summary(fun.y="mean", geom="line")+
                facet_wrap(~SubGroup)+
                xlab(NULL)+ylab("Cumulative Price Returns")+
                ggtitle("Nvidia Quadro's Cumulative Returns")+
                theme(plot.title=element_text(hjust=0.5))+
                scale_y_continuous(labels = scales::percent)
        
        
}






