## YOU NEED TO FIGURE OUT HOW TO GRAPH THE CHANGES IN PRICE PER PRODUCT
library(plyr)
library(quantmod)
library(PerformanceAnalytics)
test4<-ddply(alldata, "Product_Name", transform,  DeltaCol = Delt(Price)) #calculates daily returns (not cumulates)
test1<-ddply(test4,"Product_Name",transform,CumulativeCol=Return.cumulative(Delt.1.arithmetic))
#Note: the above formula calculates daily percent change (from the previous day, not the total)
ggplot(test4,aes(x=Date,y=Delt.1.arithmetic,linetype=Product_Name))+geom_line()+geom_point()+
        facet_grid(~Brand)+theme(legend.position='none')
ggplot(test1,aes(x=Date,y=CumulativeCol,linetype=Product_Name))+geom_line()+geom_point()+
        facet_grid(~Brand)+theme(legend.position='none')
ggplot(test4,aes(x=Date,y=Delt.1.arithmetic,color=Brand))+geom_boxplot()


test2<-test1[test1$Product_Name=="ASUS 2GB Graphics Cards R7240-2GD3-L",]
#test2$Delt.1.arithmetic[1]=0
for(i in 1:length(test2$Delt.1.arithmetic)){
        if(is.na(test2$Delt.1.arithmetic[i]==TRUE)){
                test2$Delt.1.arithmetic[i]=0
        }
}

test2.2<-na.omit(test2)

test2<-mutate(test2,cumulative2=Return.cumulative(test2$Delt.1.arithmetic))
test2<-mutate(test2,cumulative3=(cumprod(1+test2$Delt.1.arithmetic)-1))


#Eliminate rows with NAs
test4<-ddply(alldata, "Product_Name", transform,  DeltaCol = Delt(Price))
test3<-na.omit(test4)
test3<-test3[test3$Product_Name=="ASUS 2GB Graphics Cards R7240-2GD3-L"|test3$Product_Name=="Zotac NVIDIA Low Profile PCI-Express Video Card ZT-71113-20L",]
test3<-ddply(test3,"Product_Name",transform,CumulativeCol=(cumprod(1+test3$Delt.1.arithmetic)-1))

ggplot(test3,aes(x=Date,y=test3$Delt.1.arithmetic,col=Product_Name))+
        geom_line()+theme(legend.position='none')


#Example of Difficulty
Product<-c("a","b","a","b","a","b","a")
Date<-c("2017/01/01","2017/01/01","2017/01/02","2017/01/02","2017/01/03",
        "2017/01/03","2017/01/04")
Date<-as.POSIXct(Date,format="%Y/%m/%d")
Price<-c(10,100,11,110,12,120,13)
ex<-data.frame(Product,Date,Price)
ex<-ddply(ex, "Product", transform,  DeltaCol = Delt(Price))
#ex<-ddply(ex,"Product",transform,CumulativeCol=(cumprod(1+ex$Delt.1.arithmetic)-1))



#What to do with data
library(plyr)
library(quantmod)
library(PerformanceAnalytics)
test4<-ddply(alldata, "Product_Name", transform,  DeltaCol = Delt(Price)) #calculates daily returns (not cumulates)
test1<-ddply(test4,"Product_Name",transform,CumulativeCol=Return.cumulative(Delt.1.arithmetic))
test1unique<-subset(test1,!duplicated(test1$Product_Name))
ggplot(test1unique,aes(x=rnorm(1:length(test1unique$Product_Name)),
                       y=test1unique$CumulativeCol,col=test1unique$Brand))+
        geom_point()

test<-alldata[alldata$Brand=="AMD Radeon",]
test<-ddply(test, "Product_Name", transform,  DeltaCol = Delt(Price)) #calculates daily returns (not cumulates)

#Subset Data
quadro_alldata<-alldata[alldata$Brand=="Nvidia Quadro",]
geforce_alldata<-alldata[alldata$Brand=="Nvidia GeForce",]
radeon_alldata<-alldata[alldata$Brand=="AMD Radeon",]

#Quantiles
quantile(quadro_alldata$Price,na.rm=TRUE)
quantile(geforce_alldata$Price,na.rm=TRUE)
quantile(radeon_alldata$Price,na.rm=TRUE)

#Cumulative Returns
quadro_alldata<-ddply(quadro_alldata, "Product_Name", transform,  DeltaCol = Delt(Price)) 
geforce_alldata<-ddply(geforce_alldata, "Product_Name", transform,  DeltaCol = Delt(Price)) 
radeon_alldata<-ddply(radeon_alldata, "Product_Name", transform,  DeltaCol = Delt(Price)) 

quadro_alldata$SubGroup<-1:length(quadro_alldata$Product_Name)
quadro_alldata<-na.omit(quadro_alldata)
for (i in 1:751){
        if(quadro_alldata$Price[i]<100){
                quadro_alldata$SubGroup[i]<-"A"
        }else if(quadro_alldata$Price[i]<169){
                quadro_alldata$SubGroup[i]<-"B"
        }else if(quadro_alldata$Price[i]<696){
                quadro_alldata$SubGroup[i]<-"C"
        }else if(quadro_alldata$Price[i]<1000000){
                quadro_alldata$SubGroup[i]<-"D"
        }
}

ggplot(quadro_alldata,aes(x=Date,y=Delt.1.arithmetic,col=Product_Name))+
        geom_line()+facet_wrap(~SubGroup,nrow=2)+theme(legend.position='none')






#Subset Data
quadro_alldata<-alldata_cumulative[alldata_cumulative$Brand=="Nvidia Quadro",]
geforce_alldata<-alldata_cumulative[alldata_cumulative$Brand=="Nvidia GeForce",]
radeon_alldata<-alldata_cumulative[alldata_cumulative$Brand=="AMD Radeon",]
#Quantiles
quantile(quadro_alldata$Price,na.rm=TRUE)
quantile(geforce_alldata$Price,na.rm=TRUE)
quantile(radeon_alldata$Price,na.rm=TRUE)

quadro_alldata$SubGroup<-1:length(quadro_alldata$Product_Name)
quadro_alldata<-na.omit(quadro_alldata)
for (i in 1:154){
        if(quadro_alldata$Price[i]<100){
                quadro_alldata$SubGroup[i]<-"A"
        }else if(quadro_alldata$Price[i]<169){
                quadro_alldata$SubGroup[i]<-"B"
        }else if(quadro_alldata$Price[i]<696){
                quadro_alldata$SubGroup[i]<-"C"
        }else if(quadro_alldata$Price[i]<1000000){
                quadro_alldata$SubGroup[i]<-"D"
        }
}

ggplot(quadro_alldata,aes(x=Date,y=Delt.1.arithmetic,col=Product_Name))+
        geom_line()+facet_wrap(~SubGroup,nrow=2)+theme(legend.position='none')





#ggplot(alldata_cumulative,aes(x=Date,y=Cumulative_Return,col=Product_Name))+
#        geom_line()+theme(legend.position='none')
