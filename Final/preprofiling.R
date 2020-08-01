library(ggplot2)

summary(dtrain.nona)

p <- ggplot(data=dtrain.nona)

p + geom_point(mapping=aes(x=eyb, y=price))

p + geom_point(mapping=aes(x=landarea, y=price))

p + geom_boxplot(mapping= aes(x=heat, y=price))

p + geom_histogram(mapping= aes(x=price))



