p <- ggplot(data=dtrain.nona)
names(dtrain.nona)

# bathrm
p + geom_boxplot(mapping= aes(x=bathrm,y=price, group=bathrm)) + 
  ggtitle('Price vs Bathrooms')

m <- lm(price~bathrm, data = dtrain.nona)

summary(m)
plot(m)

# hf_bathrm
p + geom_boxplot(mapping= aes(x=hf_bathrm,y=price, group=hf_bathrm)) + 
  ggtitle('Price vs Half Bathrooms')

m <- lm(price~hf_bathrm, data = dtrain.nona)
summary(m)
plot(m)


# heat
p + geom_boxplot(mapping= aes(x=heat,y=price, group=heat)) + 
  ggtitle('Price vs Heat')

m <- lm(price~heat, data = dtrain.nona)
summary(m)
plot(m)

# ac
p + geom_boxplot(mapping= aes(x=heat,y=price, group=heat)) + 
  ggtitle('Price vs Heat')

m <- lm(price~heat, data = dtrain.nona)
summary(m)
plot(m)

for (name in names(dtrain.nona)){
  p + geom_boxplot(mapping= aes(x=name,y=price, group=name)) + 
    ggtitle(paste('Price vs ', name))
}


for (name in names(dtrain.nona)){
  print(name)
}
