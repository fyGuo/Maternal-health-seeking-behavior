# descripitve statistical results of clustering analysis
########################################
library(data.table)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(car)
library(dplyr)
wm_clean<-read.csv("D:\\tang_data\\cluster0503.csv")
wm_clean<-as.data.table(wm_clean)
wm_clean$cluster<-as.factor(wm_clean$cluster)

#number of people in each cluster
tapply(wm_clean$WB4, wm_clean$cluster, length)

#age distribution
tapply(wm_clean$WB4, wm_clean$cluster, mean)
tapply(wm_clean$WB4, wm_clean$cluster, sd)


shapiro.test(wm_clean$WB4[wm_clean$cluster==1])      
m<-kruskal_test(data=wm_clean,WB4~cluster)
m$p

h<-aov(WB4~cluster,wm_clean)
plot(h,1)
#test for homogenity
leveneTest(WB4 ~ cluster, data = wm_clean)
plot(h,2)
# test for normality
hist(h$residuals)
#so use K_W test
kruskal.test(WB4 ~ cluster, data = wm_clean)
###################################################

#birth order
tapply(wm_clean$CM11, wm_clean$cluster, median)
tapply(wm_clean$CM11, wm_clean$cluster, IQR)
kruskal.test(CM11 ~ cluster, data = wm_clean)
##################################################3
#marital status
wm_clean$MA1[wm_clean$MA1>=2]<-2
h<-tapply(wm_clean$MA1, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))
##############################################
#education level
wm_clean$welevel[wm_clean$welevel>=2]<-2
h<-tapply(wm_clean$welevel, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))
###########################################3

#urban or rural
h<-tapply(wm_clean$HH6, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))
#########################################
#language used transcript or not
h<-tapply(wm_clean$WM15, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(c(78,2157),c(179,2718),c(127,3207)))
#######################################
#media use
#newspaper
h<-tapply(wm_clean$MT1, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(c(2016,137+51+14),c(2786,63+29+6),c(3218,111+60+22)))

#radio
h<-tapply(wm_clean$MT2, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
h
chisq.test(rbind(c(1582,243+178+218),c(2531,162+104+86),c(2612,273+195+245)))

#TV
h<-tapply(wm_clean$MT3, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100

#computer
h<-tapply(wm_clean$MT4, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
h
chisq.test(rbind(c(151,2081),c(42,2852),c(85,3247)))

#create a mass media exposure 

wm_clean$media<-1
wm_clean$media[(wm_clean$MT1==9)&(wm_clean$MT2==9)&(wm_clean$MT3==9)&(wm_clean$MT4==9)]<-9
wm_clean$media[(wm_clean$MT1==0)&(wm_clean$MT2==0)&(wm_clean$MT3==0)&(wm_clean$MT4==2)]<-0
wm_clean<-wm_clean[wm_clean$media!=9]
h<-tapply(wm_clean$media, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))

wm_clean<-read.csv("D:\\tang_data\\cluster0503.csv")
wm_clean<-as.data.table(wm_clean)
wm_clean$cluster<-as.factor(wm_clean$cluster)
########################3
#wanted pregnancy
h<-tapply(wm_clean$DB2, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
h
chisq.test(rbind(c(1328,887),c(1886,1006),c(2135,1193)))
###################
#died child
h<-tapply(wm_clean$CM8, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))
#######################
#insurance
h<-tapply(wm_clean$insurance, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
h
chisq.test(rbind(c(84,2149),c(15,2873),c(65,3257)))
################################
#household wealth group
h<-tapply(wm_clean$windex5, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))

#############3
#household header's gender
h<-tapply(wm_clean$HHSEX, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))

#household header's ethnics
h<-tapply(wm_clean$HC2, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
h$`1`<-c(1952,113,162,0,7,1)
h$`2`
h$`3`<-c(2826,94,393,4,6,15)
chisq.test(rbind(h$`1`,h$`2`,h$`3`),correct = T)
##################################3
#household header's religion
h<-tapply(wm_clean$HC1A, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
#######################3
#household head's francis
h<-tapply(wm_clean$HH17, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`,h$`3`))
###############################
#household head's education level
wm_clean$helevel[wm_clean$helevel==9]<-NA
wm_clean$helevel[wm_clean$helevel>=2&wm_clean$helevel!=9]<-2
h<-tapply(wm_clean$helevel, wm_clean$cluster, table)
prop.table(h$`1`)*100
prop.table(h$`2`)*100
prop.table(h$`3`)*100
chisq.test(rbind(h$`1`,h$`2`[1:3],h$`3`[1:3]))

