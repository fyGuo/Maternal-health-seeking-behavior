#load package
#################################
#read the basic map information and check it
library(ggplot2)
library(maps)
library(raster)
library(plyr)
library(rgdal)
library(maptools)
Congo1<-getData("GADM", country="CD", level=1)
plot(Congo1)


#order the province name from a to z
Congo1_UTM<-spTransform(Congo1, CRS("+init=EPSG:32737"))  
Congo1_UTM@data[order(NAME_1),]

# province name
NAME_1<-Congo1_UTM@data$NAME_1
# create id according to province rank to match the id in last R.script
Congo1_UTM@data$id <- rownames(Congo1_UTM@data)


# merge two files
Congo1_UTM@data <- merge(Congo1_UTM@data, new_cluster, by="id")
Congo1_df <- fortify(Congo1_UTM)
Congo1_df<-join(Congo1_df,Congo1_UTM@data,by="id")

theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_blank()))
Congo1_df$cluster<-factor(Congo1_df$cluster,levels=c("cluster_1","cluster_2","cluster_3"),labels =c("Fully use","Never use","Half use"))


ggplot() + 
        geom_polygon(data = Congo1_df, aes(x = long, y = lat, group = group, fill =
                                                   people_per), color = "black", size = 0.25) +
        theme(aspect.ratio=1)+theme_opts+facet_grid(.~cluster)+
        scale_fill_gradient(name=NULL,low="white",high = "blue")

