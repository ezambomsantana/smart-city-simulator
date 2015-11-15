library("ggplot2")

var <- consumo

xrange <- consumo$V1 

options(scipen=5)

ggplot(consumo, aes(x = xrange)) + 
  geom_line(aes(y = consumo$V2, colour = "total_memory")) + 
  geom_line(aes(y = consumo$V3, colour = "erlang_memory")) + 
  geom_line(aes(y = consumo$V4, colour = "swap")) + 
  geom_line(aes(y = consumo$V5, colour = "cpu")) + 
  ylab(label="Resource Consumption") + 
  xlab("Wall-Clock Time (Miliseconds)")  + 
  scale_colour_manual(name="Actor Type",
                      values=c(total_memory="red", 
                               erlang_memory="blue", 
                               swap="yellow", 
                               sensor="gray",
                               cpu="black"))