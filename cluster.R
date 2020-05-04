library(ggplot2)
library(cluster)
library(clustMixType)
library(dplyr)
library(readr)
library(Rtsne)
library(ggthemes)
library(klaR)
library(factoextra)
wm_clean<-read.csv("D:\\tang_data\\cleandata0503.csv")
hsb<-dplyr::select(wm_clean,MN2,ac4,probithpl,PN25A,
                PN25B,PN25C)

s<-fviz_nbclust(x=hsb,FUNcluster =  kmodes, method = "silhouette",k.max = 5)
s
hsb[] <- lapply(hsb, unclass)
set.seed(2)
r<-kmodes(hsb,modes=3)
r$modes
wm_clean$cluster<-r$cluster
results<-r$modes
results$MN2[results$MN2==2]<-0
results$MN2[results$PN25A==2]<-0
results$MN2[results$PN25B==2]<-0
results$MN2[results$PN25C==2]<-0
write.csv(wm_clean,"D:\\tang_data\\cluster0503.csv")
