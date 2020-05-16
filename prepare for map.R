###################################
#THis script was created to prepare the cluster results for following map
#We used weighted number of people in each cluster to draw the map

######################3333333333


# Read the clutster data
#HH7: name of the provinces in French
#create weighted cluster numbers
cluster<-read.csv("D:\\tang_data\\cluster0503.csv")
cluster$HH7
cluster$HH7[cluster$HH7==1]<-"Kinshasa"
cluster$HH7[cluster$HH7==2]<-"Kongo Central"
cluster$HH7[cluster$HH7==3]<-"Kwango"
cluster$HH7[cluster$HH7==4]<-"Kwilu"
cluster$HH7[cluster$HH7==5]<-"Maindombe"
cluster$HH7[cluster$HH7==6]<-"Equateur"
cluster$HH7[cluster$HH7==7]<-"Sud Ubangi"
cluster$HH7[cluster$HH7==8]<-"Nord Ubangi"
cluster$HH7[cluster$HH7==9]<-"Mongala"
cluster$HH7[cluster$HH7==10]<-"Tshuapa"
cluster$HH7[cluster$HH7==11]<-"Tshopo"
cluster$HH7[cluster$HH7==12]<-"Bas Uele"
cluster$HH7[cluster$HH7==13]<-"Haut Uele"
cluster$HH7[cluster$HH7==14]<-"Ituri"
cluster$HH7[cluster$HH7==15]<-"Nord Kivu"
cluster$HH7[cluster$HH7==16]<-"Sud Kivu"
cluster$HH7[cluster$HH7==17]<-"Maniema"
cluster$HH7[cluster$HH7==18]<-"Haut Katanga"
cluster$HH7[cluster$HH7==19]<-"Lualaba"
cluster$HH7[cluster$HH7==20]<-"Haut Lomami"
cluster$HH7[cluster$HH7==21]<-"Tanganyika"
cluster$HH7[cluster$HH7==22]<-"Lomami"
cluster$HH7[cluster$HH7==23]<-"Kasai Oriental"
cluster$HH7[cluster$HH7==24]<-"Sankuru"
cluster$HH7[cluster$HH7==25]<-"Kasai Central"
cluster$HH7[cluster$HH7==26]<-"Kasai"
cluster<-data.frame(cluster=cluster$cluster,HH7=cluster$HH7,wmweight=cluster$wmweight)
cluster$cluster[cluster$cluster==1]<-"cluster_1"
cluster$cluster[cluster$cluster==2]<-"cluster_2"
cluster$cluster[cluster$cluster==3]<-"cluster_3"


#
cluster<-as.data.table(cluster)

#split the data according to their cluster
x<-split(cluster,cluster$cluster)

#cluster 1 processiong
cluster1<-x$cluster_1
cluster1
#caculate the weighted number of people in cluster 1
t<-tapply(cluster1$wmweight, cluster1$HH7, sum)
cluster1<-data.frame(HH7=names(t),people=t)
cluster1$cluster<-"cluster_1"

#cluster 2 procession
cluster2<-x$cluster_2
cluster2
t<-tapply(cluster2$wmweight, cluster2$HH7, sum)
cluster2<-data.frame(HH7=names(t),people=t)
cluster2
cluster2$cluster<-"cluster_2"

#cluster 3 procession
cluster3<-x$cluster_3
cluster3
t<-tapply(cluster3$wmweight, cluster3$HH7, sum)
cluster3<-data.frame(HH7=names(t),people=t)
cluster3
cluster3$cluster<-"cluster_3"

# bind the cluster 1,2,3 to create the whole dataframe
new_cluster<-rbind(cluster1,cluster2,cluster3)
# rank the data according to the province from a to z
new_cluster<-new_cluster[order(new_cluster$HH7),]
table(new_cluster$HH7)

#rename the HH7 to match the name in map script
head(new_cluster)
new_cluster<-plyr::rename(new_cluster,replace=c("HH7"="NAME_1"))

#create id to match the id in map sript 
rownames(new_cluster)<-NULL
new_cluster
new_cluster$id <- rownames(new_cluster)
new_cluster$id<-as.numeric(new_cluster$id)
new_cluster$id <- ceiling(new_cluster$id/3)
new_cluster$id<-as.character(new_cluster$id)


#not use old method
# calculate the proportion of each cluster in each province 
m<-tapply(new_cluster$people, new_cluster$NAME_1, sum)
new_cluster$local_people<-rep(m,each=3)
head(new_cluster)
new_cluster$people_per<-new_cluster$people/new_cluster$local_people

#new method choose the dominant group in each province
new_cluster<-new_cluster[order(new_cluster$NAME_1,new_cluster$people_per),]
new_cluster$seq<-1:length(new_cluster$NAME_1)
new_cluster<-new_cluster[new_cluster$seq%%3==0,]
new_cluster
