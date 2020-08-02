nullmodel <- lm(price~1, data=dtrain)
fullmodel <- lm(price~. , data=dtrain)

s <- step(nullmodel,scope=list(upper=fullmodel),direction="both")
