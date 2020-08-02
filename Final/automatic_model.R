# first automatic model with just first order terms
nullmodel <- lm(price~1, data=dtrain)
fullmodel <- lm(price~(.)^2 , data=dtrain)

s <- step(nullmodel,scope=list(upper=fullmodel),direction="both")

names(s)

s$model
plot(s$fitted.values,s$residuals)

m1 <- lm(price ~ saledate + gba + grade + ayb + bathrm_tot + fireplaces + 
          extwall + eyb + gba_p + rooms + remdl + kitchens + landarea, dtrain)

summary(m1)

# residual plot
plot(m1, which=1)

# cooks distance
plot(m1,which=4)

# this model shows a quadratic relationship with the residuals so there must be a missing quadratic term

###### studentized residuals ######
plot(fitted(m1),rstudent(m1),xlab='Fitted', ylab = "Studentized Residuals")


######## Durbin Watson Test #######
library(lmtest)

dwtest(m1)

#### residuals vs missing variable #######

summary(lm(resid(m1) ~ dtrain$stories^2))

########## model 2 ##########
m2 <- lm(price ~ saledate + gba + grade + ayb + bathrm_tot + fireplaces + 
           extwall + eyb + gba_p + rooms + remdl + kitchens + landarea +
           I(stories^2), dtrain)

plot(m2,which=1)

