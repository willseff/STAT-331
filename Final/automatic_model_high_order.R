
########## stepwise ##########
nullmodel <- lm(price~1, data=dtrain)
fullmodel <- lm(price~(.)^2 + I(gba^2) + I(bathrm^2) + I(hf_bathrm^2) + I(rooms^2) + 
                  I(bedrm^2) + I(ayb^2) + I(yr_rmdl^2) + I(eyb^2) + I(stories^2) + 
                  I(gba^2) + I(kitchens^2) + I(fireplaces^2) + I(landarea^2) + 
                  I(sale_year^2) + I(sale_ayb^2) + I(sale_eyb^2) + I(gba_p^2) +
                  I(bathrm_tot^2) + I(gba^3) + I(bathrm^3) + I(hf_bathrm^3) + I(rooms^3) + 
                  I(bedrm^3) + I(ayb^3) + I(yr_rmdl^3) + I(eyb^3) + I(stories^3) + 
                  I(gba^3) + I(kitchens^3) + I(fireplaces^3) + I(landarea^3) + 
                  I(sale_year^3) + I(sale_ayb^3) + I(sale_eyb^3) + I(gba_p^3) +
                  I(bathrm_tot^3) + I(gba^4) + I(bathrm^4) + I(hf_bathrm^4) + I(rooms^4) + 
                  I(bedrm^4) + I(ayb^4) + I(yr_rmdl^4) + I(eyb^4) + I(stories^4) + 
                  I(gba^4) + I(kitchens^4) + I(fireplaces^4) + I(landarea^4) + 
                  I(sale_year^4) + I(sale_ayb^4) + I(sale_eyb^4) + I(gba_p^4) +
                  I(bathrm_tot^4) + I(sale_year^5) +
                  + I(sale_year^6), data=dtrain)
fullmodel <- lm(price~poly(.,2), data=dtrain)

s <- step(nullmodel,scope=list(upper=fullmodel),direction="both")

m1 <- lm(price ~ saledate + gba + grade + I(ayb^4) + I(bathrm_tot^2) + 
           fireplaces + I(sale_eyb^4) + extwall + gba_p + rooms + ayb + 
           I(sale_ayb^2) + I(kitchens^4) + style + stories + sale_ayb + 
           I(gba_p^2) + I(gba_p^4) + I(landarea^3) + I(eyb^4) + I(eyb^3) + 
           saledate:gba + saledate:grade + saledate:extwall + fireplaces:gba_p + 
           gba:gba_p + rooms:ayb + gba:ayb + style:stories + grade:stories + 
           gba:sale_ayb + style:sale_ayb + fireplaces:style + saledate:style + 
           gba_p:stories + rooms:stories + saledate:sale_ayb + saledate:fireplaces, dtrain)

summary(m1)
anova(m1)

# residual plot
plot(m1, which=1)

# cooks distance
plot(m1,which=4)

########## remove all anova insignificant factors ########
m2 <- lm(price ~ saledate + gba + grade + I(ayb^4) + I(bathrm_tot^2) + 
           fireplaces + I(sale_eyb^4) + extwall + gba_p + rooms + 
           I(sale_ayb^2) + I(kitchens^4) + style + stories +
           I(landarea^3) + I(eyb^3) + 
           saledate:gba + saledate:grade + saledate:extwall + fireplaces:gba_p + 
           gba:gba_p + rooms:ayb + gba:ayb + style:stories + grade:stories + 
           gba:sale_ayb + style:sale_ayb + fireplaces:style + saledate:style + 
           saledate:sale_ayb, dtrain)

anova(m2)
plot(m2, which=1)

m3 <- lm(price ~ saledate + gba + grade + I(ayb^4) + I(bathrm_tot^2) + 
           fireplaces + I(sale_eyb^4) + extwall + gba_p + rooms + 
           I(kitchens^4) + style + stories +
           I(landarea^3) + 
           saledate:gba + saledate:grade + saledate:extwall + fireplaces:gba_p + 
           gba:gba_p  + gba:ayb + style:stories + grade:stories + 
           style:sale_ayb + fireplaces:style + saledate:style + 
           saledate:sale_ayb, dtrain)

anova(m3)
plot(m3, which=1)

m4 <- lm(price ~ saledate + gba + grade + I(ayb^4) + I(bathrm_tot^2) + 
           fireplaces + I(sale_eyb^4) + extwall + gba_p + rooms + 
           I(kitchens^4) + style + stories +
           I(landarea^3) + 
           saledate:gba + saledate:grade + saledate:extwall + fireplaces:gba_p + 
           gba:gba_p  + style:stories + grade:stories + 
           style:sale_ayb + saledate:style + 
           saledate:sale_ayb, dtrain)

anova(m4)
plot(m4, which=1)

m5 <- lm(price ~ saledate + gba + grade + I(ayb^4) + I(bathrm_tot^2) + 
           fireplaces + I(sale_eyb^4) + extwall + gba_p + rooms + 
           I(kitchens^4) + style + stories +
           I(landarea^3) + 
           saledate:gba + saledate:grade + saledate:extwall + fireplaces:gba_p + 
           gba:gba_p  + style:stories + grade:stories + 
           saledate:style + 
           saledate:sale_ayb, dtrain)

anova(m5)
plot(m5, which=1, pch=1, cex=0.3)

plot(m5)



########## backwards ############
library(MASS)
nullmodel <- lm(price~1, data=dtrain)
fullmodel <- lm(price~(.)^2 + I(gba^2) + I(bathrm^2) + I(hf_bathrm^2) + I(rooms^2) + 
                  I(bedrm^2) + I(ayb^2) + I(yr_rmdl^2) + I(eyb^2) + I(stories^2) + 
                  I(gba^2) + I(kitchens^2) + I(fireplaces^2) + I(landarea^2) + 
                  I(sale_year^2) + I(sale_ayb^2) + I(sale_eyb^2) + I(gba_p^2) +
                  I(bathrm_tot^2) + I(sale_year^3) + I(sale_year^4)+ I(sale_year^5) +
                  + I(sale_year^6), data=dtrain)

newmodel <- addterm(nullmodel, scope=fullmodel, test='F')
