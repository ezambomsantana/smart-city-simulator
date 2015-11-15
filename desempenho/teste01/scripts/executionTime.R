library("ggplot2")

var <- executionTime
var


xrange <- var$V1 
xrange

ggplot(data=var, aes(y=var$V2, x=50000)) + geom_boxplot() +
ylab(label="Execution Time (Mileseconds)") + 
  xlab("500000 VT") 