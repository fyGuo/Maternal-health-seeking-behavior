library(ggplot2)
library(cluster)
library(clustMixType)
library(dplyr)
library(readr)
library(Rtsne)
library(ggthemes)
library(klaR)
library(factoextra)
library(dplyr)
wm_clean<-read.csv("D:\\tang_data\\cleandata0503.csv")
# try new ways to cluster
#ANC:never used; used but below 4; >=4 times
hsb<-dplyr::select(wm_clean,MN2,ac4,probithpl,PN25A,
                PN25B,PN25C)
hsb$anc_new[hsb$MN2==2]<-0
hsb$anc_new[hsb$MN2==1&hsb$ac4==0]<-1
hsb$anc_new[hsb$MN2==1&hsb$ac4==1]<-2
table(hsb$anc_new)
hsb<-dplyr::select(hsb,-ac4,-MN2)

str(hsb)

table(hsb$anc_new)
s<-fviz_nbclust(x=hsb,FUNcluster =  kmodes, method = "silhouette",k.max = 5)
s
# you could see in this way 3 clusters are not the optimal solution,although the cluster results are same
hsb[] <- lapply(hsb, unclass)
set.seed(2)
r<-kmodes(hsb,modes=3)
r$modes
cluster1<-r$cluster

#So let's turn to orignial methods
hsb<-dplyr::select(wm_clean,MN2,ac4,probithpl,PN25A,
                   PN25B,PN25C)
s<-fviz_nbclust(x=hsb,FUNcluster =  kmodes, method = "silhouette",k.max = 5)
s
set.seed(2)
r<-kmodes(hsb,modes=3)
r$modes
results<-r$modes
cluster2<-r$cluster
results$MN2[results$MN2==2]<-0
results$PN25A[results$PN25A==2]<-0
results$PN25B[results$PN25B==2]<-0
results$PN25C[results$PN25C==2]<-0
results

#let's check two different ways change the cluster results?o or not
sum(!(cluster1==cluster2))/length(cluster1)
#the different proportion is only 0.0059, but the latter way is much easier to 
#justify using silihoutte index


wm_clean$cluster<-r$cluster
results<-r$modes
results$MN2[results$MN2==2]<-0
results$MN2[results$PN25A==2]<-0
results$MN2[results$PN25B==2]<-0
results$MN2[results$PN25C==2]<-0
write.csv(wm_clean,"D:\\tang_data\\cluster0503.csv")
