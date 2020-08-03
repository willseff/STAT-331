library(leaps)

######## best subset #########

best.subset <- regsubsets(price ~ ., data=dtrain, nbest=1, really.big = T, nvmax = 8)
summary(best.subset)

m_bestsub <- lm(price ~ heat + saledate + gba + ayb + eyb + grade + 
                  fireplaces + sale_ayb + bathrm_tot, dtrain)

# use box cox to determine transformation
boxcox(m_bestsub,lambda=seq(-1,1,1/20))

m_bestsub <- lm(price^0.5 ~ heat + saledate + gba + ayb + eyb + grade + 
                  fireplaces + bathrm_tot, dtrain)

plot(m_bestsub)
anova(m_bestsub)

# run best subset with boxcox tranformation
best.subset.box <- regsubsets(price^0.5 ~ ., data=dtrain, nbest=1, really.big = T, nvmax = 8)
summary(best.subset.box)

# best subset model
m.bestsub.box <- lm(price^0.5 ~ bathrm_tot + gba_p + sale_eyb + fireplaces + 
                      grade + ayb + saledate + gba, data = dtrain)
anova(m.bestsub.box)
plot(m.bestsub.box)

####### looking for higher order terms #########

nullmodel <- lm(price^0.5 ~ bathrm_tot + gba_p + sale_eyb + fireplaces + 
                  grade + ayb + saledate + gba, data = dtrain)

fullmodel <- lm(price^0.5 ~ (.)^2 + bathrm_tot + gba_p + sale_eyb + fireplaces + 
                  grade + ayb + saledate + gba + I(bathrm_tot^2) + 
                  I(gba_p^2)+ I(sale_eyb^2) + I(fireplaces^2) +
                  I(ayb^2) + I(gba^2) + I(bathrm_tot^3) + 
                  I(gba_p^3)+ I(sale_eyb^3) + I(fireplaces^3) +
                  I(ayb^3) + I(gba^3) + I(bathrm_tot^4) + 
                  I(gba_p^4)+ I(sale_eyb^4) + I(fireplaces^4) +
                  I(ayb^4) + I(gba^4) , data = dtrain)

####### stepwise ######

s <- step(nullmodel,scope=list(upper=fullmodel),direction="both")

m <- lm(price^0.5 ~ bathrm_tot + gba_p + fireplaces + grade + ayb + saledate + 
          gba + I(sale_eyb^4) + I(ayb^2) + I(sale_eyb^3) + I(gba^4) + 
          I(sale_eyb^2) + saledate:gba, dtrain)
plot(m)
