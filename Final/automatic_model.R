nullmodel <- lm(price~1, data=dtrain.nona)
fullmodel <- lm(price~. , data=dtrain.nona)

s <- step(nullmodel,scope=list(upper=fullmodel),direction="both")
