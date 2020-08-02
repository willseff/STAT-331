p <- ggplot(data=dtrain)
names(dtrain.nona)

#year
p + geom_boxplot(mapping= aes(x=year,y=price, group=year)) + 
  ggtitle('Price vs Year')

p+ geom_histogram(mapping = aes(x=year), stat="count")

# bathrm
p + geom_boxplot(mapping= aes(x=bathrm,y=price, group=bathrm)) + 
  ggtitle('Price vs Bathrooms')

m <- lm(price~bathrm, data = dtrain.nona)

summary(m)
plot(m)

# plot all preditors with price
for (name in names(dtrain.nona)){
  
  print(p + geom_boxplot(mapping= aes_string(x=name,y="price", group=name)) + 
    ggtitle(paste('Price vs ', name)))
  
  ggsave(paste("price_vs_",name,".pdf"))
}

for (col1 in dtrain.nona){
  for (col2 in dtrain.nona){
    interaction.plot(col1,col2, dtrain.nona$price)
  }
}


