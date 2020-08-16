library(maps)
library(ggmap)
mymap <- get_map(location = "New York", maptype = "roadmap")
ggmap(mymap)

if(!requireNamespace("devtools")) 
  install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)


library(ggmap)
register_google(key = "AIzaSyBvoLNroG2Ak5j2IsEuLMiUaN1sFgo1YjA")
statesmap=map_data("state")
ggplot(statesmap,aes(x=long,y=lat,group=group))+geom_polygon(fill="white",colour="black")
nyc_map = get_map(location =c(lon=-73.9549,lat=40.7690),maptype="terrain",zoom=10)

min_lat=40.5774
max_lat=40.9176
min_long=-74.15
max_long=-73.7004

ggplot(statesmap,aes(x=long,y=lat))+geom_point(data=uber,aes(x=Lon, y=Lat,color=month))+scale_x_continuous(limits=c(min_long, max_long)) +scale_y_continuous(limits=c(min_lat, max_lat))
+scale_fill_gradient(high="red",low="black",guide = "legend")+theme_map()


