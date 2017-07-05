library(ggplot2)
library(dplyr)
library(data.table)
library(ggmap)
library(gganimate)
library(animation)
library(jpeg)
library(grid)

img <- readJPEG("img1.jpg")
g <- rasterGrob(img, interpolate=FALSE, height = unit(0.7, "npc"))

bf <- fread("bfro_report_locations.csv", header = T, stringsAsFactors = T)

str(bf)
table(bf$classification)

bf[bf$classification=='Class C']
bf$date <- as.character(bf$timestamp)


list<-strsplit(bf$date,"-")

## Convert the list into dataframe
library(plyr)
bf_Date1<-ldply(list)
colnames(bf_Date1)<-c("Year","Month","Day")

## Column bind with the main dataframe
bf<-cbind(bf,bf_Date1)
bf$Day<-NULL
bf$date<-NULL
bf$Year <- as.numeric(bf$Year)

bf$Magnitude <- 6
bf$Magnitude[bf$classification=='Class A'] <- 2
bf$Magnitude[bf$classification=='Class B'] <- 4

bf <- bf[bf$Year>1900]
bf <- bf[bf$Year<2018]

bf<- bf[bf$longitude > (-190)]


world<-map_data("world")

## Remove Antarctica region from the world map

str(world)

world <- world[world$region == c("USA","Canada"),]
world <- world[world$long < 0,]

map<-ggplot()+geom_map(data=world,map=world,aes(x=long,y=lat,map_id=region),color='#333300',fill='#DDF0E4')

p <- map + annotation_custom(g, xmin=-180, xmax=-130, ymin=20, ymax=55) + geom_point(data = bf, aes(x = longitude, y = latitude, 
                                     frame = Year, 
                                     cumulative = TRUE,size=Magnitude, color = classification), alpha = 0.7)+
  geom_jitter(width = 0.1) +labs(title = "(Abhishek Sinha) Big Foot Sightings -") + ylab("Developed by Abhishek Sinha") +theme_void() + guides(colour=FALSE, size=FALSE)

# Plot .gif file using gganimate function

ani.options(interval=0.5)
gganimate(p, "big_foot_sighting.mp4", ani.width=1000, ani.height=800)
