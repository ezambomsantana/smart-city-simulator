library("ggplot2")

var <- dados

xrange <- var$V1 

options(scipen=5)

ggplot(dados, aes(x = xrange)) + 
 geom_line(aes(y = dados$V2, colour = "total")) + 
 geom_line(aes(y = dados$V3, colour = "route")) + 
 geom_line(aes(y = dados$V4, colour = "building")) + 
 geom_line(aes(y = dados$V5, colour = "sensor")) + 
 geom_line(aes(y = dados$V6, colour = "traffic_light")) + 
 geom_line(aes(y = dados$V7, colour = "car")) + 
 geom_line(aes(y = dados$V8, colour = "terminal")) + 
 geom_line(aes(y = dados$V9, colour = "bus")) + 
 ylab(label="Number of Actors") + 
 xlab("Virtual Time")  + 
 scale_colour_manual(name="Actor Type",
 values=c(total="red", route="blue", building="yellow", 
           sensor="gray", traffic_light="black", 
           car="green", terminal="pink", bus="brown" ))