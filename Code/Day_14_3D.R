#This code plots a 3D football. The inspiration behind this design
#is the giant football located outside Oregon State's Reser Stadium

library(rgl)
library(ggplot2)

#Create the Football
#####
#Set dimensions of ball
mylength = 20
myflat = 8

#Piece1
x <- seq(0,mylength)
y <- (x*(x-20))/myflat
z <- x*0
mydata1 <- as.data.frame(cbind(x,y,z))

#Piece2
x <- seq(0,mylength)
y <- (x*(x-20))/-myflat
z <- x*0
mydata2 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata1,mydata2)

#Piece3
x <- seq(0,mylength)
z <- (x*(x-20))/myflat
y <- x*0
mydata3 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata3)

#Piece4
x <- seq(0,mylength)
z <- (x*(x-20))/-myflat
y <- x*0
mydata4 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata4)

#Piece5 (Ring)
radius = max(mydata$y)
x = rep(10:10,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata5 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata5)

#Piece6 (Ring)
radius = 4.5
x = rep(2:2,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata6 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata6)

#Piece7 (Ring)
radius = 8
x = rep(4:4,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata7 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata7)

#Piece8 (Ring)
radius = 10.5
x = rep(6:6,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata8 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata8)

#Piece9 (Ring)
radius = 12
x = rep(8:8,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata9 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata9)

#Piece10 (Ring)
radius = 4.5
x = rep(18:18,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata10 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata10)

#Piece11 (Ring)
radius = 8
x = rep(16:16,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata11 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata11)

#Piece12 (Ring)
radius = 10.5
x = rep(14:14,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata12 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata12)

#Piece13 (Ring)
radius = 12
x = rep(12:12,20)
points = seq(0,2*pi,length.out = length(x))
y = radius*cos(points)
z = radius*sin(points)
mydata13 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata13)

#Piece 14 (Center Pole)
radius = 12
x = rep(0:20)
points = seq(0,2*pi,length.out = length(x))
y = x*0
z = x*0
mydata14 <- as.data.frame(cbind(x,y,z))
mydata <- rbind(mydata,mydata14)
#####

# Plot
for (i in 1:14){
plot3d(get(paste0('mydata',i)), 
  col = ifelse(i %in% c(5,6,8,10,12),
               'orange',
               'black'), 
  type = 'l', lwd = 7,
  xlim = c(0,20),
  ylim = c(-20,20),
  zlim = c(-20,20),
  boxed = F,
  xlab = '',
  ylab = '',
  zlab = '')
} +
  rgl.bbox(alpha = 0) +
  rgl.bg(color = 'lightgray')

