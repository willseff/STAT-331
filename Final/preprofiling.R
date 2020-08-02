library(ggplot2)

dim(dtrain)

summary(dtrain)

# class of each col
sapply(dtrain, class)

#year analysis
p <- ggplot(data=dtrain)

#box plot by year
p + geom_boxplot(mapping= aes(x=year,y=price, group=year)) + 
  ggtitle('Price vs Year')

#histogram by year
p+ geom_histogram(mapping = aes(x=year), stat="count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Histogram of values by Year")

# month-year analysis
p + geom_boxplot(mapping= aes(x=month,y=price, group=month)) + 
  ggtitle('Price vs Month')

# month year linear model
m <- lm(price ~ month + year, data= dtrain)
anova(m)

# price 

p+ geom_histogram(mapping = aes(x=price), bins=20) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Histogram of values by Price")

############# Analysis of each predictor #############

#bathrm
m <- lm(price~bathrm, dtrain)
summary(m)
plot(m)

m2 <- lm(price~bathrm + bathrm^2, data=dtrain)
plot(m2)

#hf_bathrm
m <- lm(price~hf_bathrm, dtrain)
summary(m)
plot(m)

m2 <- lm(price~hf_bathrm + hf_bathrm^2, data=dtrain)
plot(m2)

#rooms 
m <- lm(price~rooms, dtrain)
summary(m)
plot(m)

m2 <- lm(price~rooms + I(rooms^2), data=dtrain)
summary(m2)
anova(m2)
plot(m2)

# land area 

p + geom_point(mapping=aes(x=landarea, y=price))

m <- lm(price~landarea, dtrain)
summary(m)
plot(m)

m2 <- lm(price~landarea + poly(landarea,2), data=dtrain)
summary(m2)
anova(m2)
plot(m2)

#gba

p + geom_point(mapping=aes(x=gba, y=price))

m <- lm(price~gba, dtrain)
summary(m)
plot(m)

m2 <- lm(price~landarea + poly(gba,2), data=dtrain)
summary(m2)
anova(m2)
plot(m2)

#gbap 
p + geom_point(mapping=aes(x=gbap, y=price))

#saleyear

m <- lm(price~year, dtrain)
summary(m)
anova(m)
plot(m)

m2 <- lm(price~year + 
           poly(year,2) +
           poly(year,3) + 
           poly(year,4) + poly(year,5) + 
           poly(year,6) + poly(year,7), data=dtrain)
summary(m2)
anova(m2)
plot(m2)

# sale_eyb

p + geom_point(mapping=aes(x=sale_eyb, y=price))

m <- lm(price~gba, dtrain)
summary(m)
plot(m)

m2 <- lm(price~landarea + poly(gba,2), data=dtrain)
summary(m2)
anova(m2)
plot(m2)

p + geom_point(mapping=aes(x=sale_ayb, y=price))







