## Amazon prices scraping:
#http://stackoverflow.com/questions/1500365/how-does-one-find-prices-from-amazons-site-programmatically
library(rvest)
#searches:
#https://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=nvidia+grapgics+card&rh=i%3Aaps%2Ck%3Anvidia+grapgics+card

#function to grab price data from amazon
amazonpricegrab<- function(url){
        scraping<-read_html(url) #
        price<-scraping %>%
                html_nodes("#priceblock_ourprice") %>%
                html_text()
        
        name<-scraping %>%
                html_nodes("#productTitle") %>%
                html_text()
        
        #grab the price of the striked out text
        #previousprice<-scraping %>%
        #        html_nodes("#class.a-text-strike") %>%
        #        html_text()
        #remove "\n         "
        #name<-gsub(" ","",name)
        name<-gsub(pattern="\n         ",replacement="",name)
        name<-sub(pattern="                    ","",name)
        name<-sub(pattern="      \n      ","",name)
        return(data.frame(name,price,Sys.time()))
}

#List of urls of PRIME products
producturls<-c("https://www.amazon.com/NVIDIA-GDDR5-ACCELERATOR-PROCESSING-900-52405-0000-000/dp/B01F2P4GBQ/ref=sr_1_1?s=pc&ie=UTF8&qid=1488557342&sr=1-1&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/HP-Tesla-K20X-Graphic-Card/dp/B00AM0FD00/ref=sr_1_2?s=pc&ie=UTF8&qid=1488557342&sr=1-2&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/nVidia-S870-External-Computing-Server/dp/B005GVUTEA/ref=sr_1_3?s=pc&ie=UTF8&qid=1488557342&sr=1-3&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/nVidia-Tesla-M2090-PCI-e-653974-001/dp/B008FQPVZ0/ref=sr_1_4?s=pc&ie=UTF8&qid=1488557342&sr=1-4&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/Nvidia-Tesla-Kepler-Graphics-900-22055-0120-010/dp/B01EM7Q1G8/ref=sr_1_5?s=pc&ie=UTF8&qid=1488557342&sr=1-5&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/Nvidia-Tesla-M2090-Gpu-Card/dp/B005TJKPWU/ref=sr_1_6?s=pc&ie=UTF8&qid=1488557342&sr=1-6&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/NVIDIA-GeForce-Pascal-GDDR5X-900-1G611-2500-000/dp/B01JLKP3IS/ref=sr_1_7?s=pc&ie=UTF8&qid=1488557342&sr=1-7&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/Nvidia-Tesla-M1060-Processing-Processor/dp/B00JMRW0Z0/ref=sr_1_8?s=pc&ie=UTF8&qid=1488557342&sr=1-8&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/NVIDIA-Tesla-Graphic-Card-900-22081-2250-000/dp/B00KDRRTB8/ref=sr_1_9?s=pc&ie=UTF8&qid=1488557342&sr=1-9&keywords=gpus&refinements=p_89%3ANVIDIA",

               "https://www.amazon.com/nVidia-Tesla-Kelper-B3M66A-688982-001/dp/B00BSD4TOG/ref=sr_1_14?s=pc&ie=UTF8&qid=1488557342&sr=1-14&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/616079-001-NVIDIA-graphics-processing-full-height/dp/B008H65MY8/ref=sr_1_15?s=pc&ie=UTF8&qid=1488557342&sr=1-15&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/Nvidia-Gaming-Kepler-Graphics-900-12400-0010-000/dp/B00MH2MFAW/ref=sr_1_17?s=pc&ie=UTF8&qid=1488557342&sr=1-17&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/Nvidia-Gaming-Kepler-Graphics-900-12055-0020-000/dp/B00MGZRYO2/ref=sr_1_18?s=pc&ie=UTF8&qid=1488557342&sr=1-18&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/Nvidia-TESLA-Accelerator-Processing-900-2G600-0000-000/dp/B01MDNO5BK/ref=sr_1_19?s=pc&ie=UTF8&qid=1488557342&sr=1-19&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/Nvidia-Quadro-K3000M-Mobile-N14E-Q1-A2/dp/B015DC6PSK/ref=sr_1_22?s=pc&ie=UTF8&qid=1488557342&sr=1-22&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/NVIDIA-E5Z76AT-Graphics-Card/dp/B00JGOMHXO/ref=sr_1_28?s=pc&ie=UTF8&qid=1488557574&sr=1-28&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/616078-001-NVIDIA-Quadro-6000-graphics/dp/B008FQQ4MY/ref=sr_1_29?s=pc&ie=UTF8&qid=1488557574&sr=1-29&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/TECHNOLOGIES-DisplayPort-Graphics-Adapter-NVIDIA/dp/B00406NS0Y/ref=sr_1_32?s=pc&ie=UTF8&qid=1488557574&sr=1-32&keywords=gpus&refinements=p_89%3ANVIDIA",
               "https://www.amazon.com/671138-001-NVIDIA-Quadro-5000-graphics/dp/B008H65IQ0/ref=sr_1_33?s=pc&ie=UTF8&qid=1488557574&sr=1-33&keywords=gpus&refinements=p_89%3ANVIDIA")
#Stopped Being Sold By Amazon:
#"https://www.amazon.com/NVIDIA-M1060-Passive-Computing-PCI-Express/dp/B017O2DCP6/ref=sr_1_11?s=pc&ie=UTF8&qid=1488557342&sr=1-11&keywords=gpus&refinements=p_89%3ANVIDIA",

#Get the price data for all names
pricedata<-data.frame(NULL)
for (i in 1:length(producturls)){
        test<-amazonpricegrab(producturls[i])
        pricedata<-rbind(pricedata,test)
}

#Save data- created so we can't overwrite previous day's data (but can overwrite today's data)
wd<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Amazon Price Scrapping DATA/Daily Data"
setwd(wd)
filename<-paste("Amazon Price Data",Sys.Date(),".csv")
write.csv(pricedata,file=filename)

#Combine all Saved Data into one file- it is ok to overwrite previously saved data
#This data will be used for graphing
files<-list.files(path=wd, full.names=TRUE)
alldata<-data.frame(NULL)
len<-c(1:length(files))
for(i in len){
        alldata<-rbind(alldata,read.csv(files[i]))
}

#Eliminate the first column that counts the rows
alldata<-alldata[2:4]
#Set column classes (need to remove $ for prices before converting to numeric)
alldata$name<-as.character(alldata$name)
alldata$price<-as.character(alldata$price)
alldata$Sys.time..<-as.POSIXct(alldata$Sys.time..)
#alldata$Sys.time..<-c(as.Date("3/1/2017","%m/%d/%Y"),as.Date("3/1/2017","%m/%d/%Y"),as.Date("3/1/2017","%m/%d/%Y"),as.Date("3/1/2017","%m/%d/%Y"),as.Date("3/2/2017","%m/%d/%Y"),as.Date("3/2/2017","%m/%d/%Y"),as.Date("3/2/2017","%m/%d/%Y"),as.Date("3/2/2017","%m/%d/%Y"))
#Remove $ sign in prices & set as.numeric
alldata$price<-gsub(pattern="[$]","",alldata$price)
alldata$price<-gsub(pattern="[,]","",alldata$price)
alldata$price<-as.numeric(alldata$price)

#Create Codes for the names so that the plot is better
library(ggplot2)
productnames<-data.frame(NULL)
productnames<-data.frame(unique(alldata$name))
productnames$unique.alldata.name.<-as.character(productnames$unique.alldata.name.)
shortnames<-as.character(1:length(productnames$unique.alldata.name.))
productnames<-cbind(productnames,shortnames)
alldatashort<-merge(alldata,productnames,by.x="name",by.y="unique.alldata.name.")

#Save Full Data
wd<-"/Users/JessicaBohning/Documents/Data Science/Projects/Pricing_Data/Amazon Price Scrapping DATA"
setwd(wd)
write.csv(alldatashort,file="ALL DATA.csv")

#Plot Data
#png(filename="timeseriesgraph.png")
pdf(file="timeseriesgraph.pdf")
ggplot(alldata,aes(x=alldatashort$Sys.time..,y=alldatashort$price,color=alldatashort$shortnames))+
        geom_point()+geom_line()+
        xlab(NULL)+
        ylab("Price")
dev.off()
