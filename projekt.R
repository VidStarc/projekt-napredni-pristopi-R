library(XML)

source('uvoz/uvoz_csv.R', encoding='UTF-8')

#urejanje podatkov
colnames(fund)<-c("Epic","Name","Market.cap(m)","Shares.in.issue(m)","P/E ratio", "Dividend.yield(%)")
fund$`Market.cap(m)`<-as.numeric(gsub(",","",fund$`Market.cap(m)`))
fund$`Shares.in.issue(m)`<-as.numeric(gsub(",","",fund$`Shares.in.issue(m)`))

colnames(price)<-c("Epic","Name","Price","TodayChg(%)","7DayChg(%)","30DayChg(%)","6MonthChg(%)")
price$Price<-as.numeric(gsub(",","",price$Price))

colnames(sum)<-c("Epic","Name","Price","Change","Change(%)","30Day(%)","Market.cap(m)")
sum$Price<-as.numeric(gsub(",","",sum$Price))
sum$`Market.cap(m)`<-as.numeric(gsub(",","",sum$`Market.cap(m)`))

colnames(trade)<-c("Epic","Name","Bid","Mid","Offer","Spread","Spread(%)")
trade$Bid<-as.numeric(gsub(",","",trade$Bid))
trade$Mid<-as.numeric(gsub(",","",trade$Mid))
trade$Offer<-as.numeric(gsub(",","",trade$Offer))
