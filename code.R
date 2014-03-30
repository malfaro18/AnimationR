
rm(list=ls())  # Erase all variables

#####################################################################

#### Install packages:
#install.package(ggplot2)
#install.package(maps)
#install.package(gpclip)
#install.package(maptools)
#install.package(rgdal)

#load packages:
library(ggplot2)
library(maps)
library(gpclib); gpclibPermit() 
library(maptools) 
library(rgdal)

#####################################################################


########################## 
##### Simple example #####

for(i in 1:10){
  # Generate data:
  x<-rnorm(100,0,i) 
  y<-rnorm(100,0,i)
  # Plot 10 different simulations
  plot(x,y, xlim=c(-40,40), ylim=c(-40,40),main=sprintf("Simulation N %s",i))
}
  
#How can we animate this?

#Set directory from your computer:
setwd("SET YOUR DIRECTORY HERE")
#setwd("~/Documents/Cursos NCSU/Summer 2013/Dr.Fuentes/interactive1")

#Construct a for loop with the plots:
for (i in 1:10) {
  #png creates the image file: %d changes the plot name according to the i
  png(filename=sprintf("plot-%d.png",100+i),width=300,height=300,bg="white") 
  #Generate the data:
  x<-rnorm(100,0,i)
  y<-rnorm(100,0,i)
  #Contruct the plots: sprintf("Simulation %d",i) changes title accordingly 
  p<-plot(x,y, xlim=c(-40,40), ylim=c(-40,40),main=sprintf("Simulation %d",i))
  #Print each plot:
  print(p)
  dev.off()
}

#This line will convert the 10 plots that we just created into a .gif file:
system("convert -delay 45 -loop 0 plot*g plot500.gif")

################################### 
##### Spatio-Temporal example #####

### We are using the data from this example:
### http://spatial.ly/2013/12/introduction-spatial-data-ggplot2/

# Read the spatial locations and project:
sport <- readShapePoly("spatialggplot/london_sport.shp") 
sport <- readOGR(dsn = "spatialggplot", "london_sport")
proj4string(sport) <- CRS("+init=epsg:27700")
sport.wgs84 <- spTransform(sport, CRS("+init=epsg:4326"))
sport.f <- fortify(sport, region = "ons_label")
sport.f <- merge(sport.f, sport@data, by.x = "id", by.y = "ons_label")
# This is the data file we are creating:
head(sport.f[, 1:8])

#Read the sports participation data:
london.data.melt <- read.csv(file="spatialggplot/london_data_melt.csv")
plot.data <- merge(sport.f, london.data.melt, by.x = "id", by.y = "Area.Code")
plot.data <- plot.data[order(plot.data$order), ]
# This is the final data file that includes spatial location and variables:
head(plot.data)
# Create a variable with all the possible years:
a<-levels(plot.data$variable)

# This is one of the most common option to present ST data w/o animations:

ggplot(data = plot.data, aes(x = long, y = lat, fill = value/1000, group = group)) + 
  geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() +
  theme(axis.text.x=element_blank(),axis.text.y=element_blank()) +
  scale_fill_gradient(name="People \n (in thousands)",low='ghostwhite',
  high= 'steelblue', limits=c(c,b))+ facet_wrap(~variable)

# Now, we are going to animate it:

# Find the min and max values through all the years to fix the scale
c<-min(plot.data$value/1000)
b<-max(plot.data$value/1000)

# Create a for loop for all the maps:
for (i in 1:21) {
  #png creates the image file: %d changes the plot name according to the i
  png(filename=sprintf("london-%d.png",100+i),width=500,height=250,bg="white") 
  # Reads the data according to the year i:
  df <- plot.data[plot.data$variable==a[i],]
  # Contructs the plot (object named p):
  p<-ggplot(data = df, aes(x = long, y = lat, fill = value/1000, group = group)) + 
    geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() +
    theme(axis.text.x=element_blank(),axis.text.y=element_blank()) +
    scale_fill_gradient(name="People \n (in thousands)",low='ghostwhite',
    high= 'steelblue', limits=c(c,b))
  # Title and labels for each plot:
  p <- p + labs(x="",y="", 
      title=sprintf("London Sports Participation, Year = %s",1801+(i-1)*10))
  #Print the plot:
  print(p)
  dev.off()
}

#This line will convert the 21 plots that we just created into a .gif file:
system("convert -delay 45 -loop 0 london*g london500.gif")

#####################################################################

#install.packages("animation")
library(animation)

getwd()

### Animations in HTML pages ###
ani.options(outdir = getwd(),htmlfile="index.html")
saveHTML({
  ani.options(interval = 0.05, nmax = 10)
  for (i in seq_len(ani.options("nmax"))) {
    dev.hold()
    #png creates the image file: %d changes the plot name according to the i
    #Generate the data:
    x<-rnorm(100,0,i)
    y<-rnorm(100,0,i)
    #Contruct the plots: sprintf("Simulation %d",i) changes title accordingly 
    plot(x,y, xlim=c(-40,40), ylim=c(-40,40),main=sprintf("Simulation %d",i))
    #Print each plot:
    ani.pause()
  }
}, img.name = "bm_plot", title = "Simulations", description =
  c("Simulate normal data for x and y"))


ani.options(outdir = getwd(),htmlfile="index2.html")
saveHTML({
  ani.options(interval = 0.05, nmax = 21)
  for (i in seq_len(ani.options("nmax"))){
    dev.hold()
    #png creates the image file: %d changes the plot name according to the i
    # Reads the data according to the year i:
    df <- plot.data[plot.data$variable==a[i],]
    # Contructs the plot (object named p):
    p<-ggplot(data = df, aes(x = long, y = lat, fill = value/1000, group = group)) + 
      geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() +
      theme(axis.text.x=element_blank(),axis.text.y=element_blank()) +
      scale_fill_gradient(name="People \n (in thousands)",low='ghostwhite',
      high= 'steelblue', limits=c(c,b)) + labs(x="",y="", 
      title=sprintf("London Sports Participation, Year = %s",1801+(i-1)*10))
    print(p)
  ani.pause()}}, img.name = "bm_plotex2", title = "London Sports Participation", description =
  c("London Sports Participation"))

##############################################################





