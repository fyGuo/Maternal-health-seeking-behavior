#read and clean data
library(dplyr)
library(foreign)
library(haven)
library(data.table)
hh<-read_dta("D:\\tang_data\\hh.dta")
hh<-dplyr::select(hh,HH1,HH2,HC1A,HC1B,HC2,HH7,helevel)
hh$HH<-paste(hh$HH1,hh$HH2,sep = "_")
sum(duplicated(hh$HH))

wm<-read_dta("D:\\tang_data\\wm.dta")
wm<-dplyr::select(wm,LN,HH1,CM11,HH2,MA1,WM6Y,WB5,WB6A,WB6B,
                  welevel,insurance,wscoreu,wscorer,CM11,WB18,WM14,WM15,WB3Y,WB4,WDOBLC,MN2,MN5
                  ,MN20,PN5,PN17,MN19A,MN19C,MN19D,MN19F,MN19G,MN19H,MN19X,MN19Y,PN9,PN20,PN25A,PN25B,PN25C,CM17,wmweight)
wm$HH<-paste(wm$HH1,wm$HH2,sep = "_")
wm$HHL<-paste(wm$HH,wm$LN,sep = "_")
table(wm$HHL)
sum(duplicated(wm$HHL))

sum(duplicated(wm$HH))
wm_new<-base::merge(wm,hh,by=c("HH","HH1","HH2"),all=F)
rm(hh,wm)
sum(duplicated(wm_new$HHL))

# give birth in last 2 years
wm_new$year=floor(wm_new$WDOBLC/12)+1900
wm_new<-as.data.table(wm_new)
wm_use<-wm_new[year-WM6Y>=-2]

#check the year
table(wm_use$WM6Y,wm_use$year)
rm(wm_new)
#the survey only contains those have alive birth in recent two years
table(wm_use$CM17)
#clean data
wm_clean<-wm_use[!is.na(MN2)&!is.na(MN20)]
wm_clean<-wm_use[MN2!=9&MN20!=9]
table(wm_clean$WM6Y,wm_clean$year)
rm(wm_use)
#Aantenatal care  times
wm_clean$MN5[wm_clean$MN2==2]=0
table(wm_clean$MN5)#you could see there are some outliers
#drop non-response
wm_clean<-wm_clean[MN5!=98]
table(wm_clean$MN5)
boxplot(wm_clean$MN5)
# create new variable to see if the women vistis antenatal care at leats 4 times
wm_clean$ac4<-0
wm_clean$ac4[wm_clean$MN5>=4]<-1
# where give birth to 
table(wm_clean$MN20)
wm_clean<-wm_clean[MN20!=99]
#create where probithpl to indicate the birth place
wm_clean$probithpl<-1
wm_clean$probithpl[wm_clean$MN20<=12|wm_clean$MN20==96]<-0
wm_clean$probithpl[wm_clean$PN7]
table(wm_clean$probithpl)

#postnatal check
wm_clean<-wm_clean[PN25A!=9&PN25A!=8]
wm_clean<-wm_clean[PN25B!=9&PN25B!=8]
wm_clean<-wm_clean[PN25C!=9&PN25C!=8]
summary(wm_clean)
# drop irrational year
table(wm_clean$WM6Y,wm_clean$year)
wm_clean<-wm_clean[!(WM6Y==2017&year==2018),]
table(wm_clean$WM6Y,wm_clean$year)
sum(duplicated(wm_clean$HHL))
write.csv(wm_clean,"D:\\tang_data\\cleandata0503.csv")
