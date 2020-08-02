source("20519497.R")

preprocessing <- function(df) {
  # treat nas in stories
  # if na then take the value from style
  df$stories <- ifelse(is.na(df$stories),
                       as.numeric(substring(df$style,1,1)),
                       df$stories)
  
  # treat nas in yr remodel
  # create new col binary whether been remodeled
  df$remdl <- ifelse(is.na(df$yr_rmdl),
                     "Y",
                     "N")
  
  # if it is never remodeled then set year as eyb
  df$yr_rmdl <- ifelse(is.na(df$yr_rmdl),
                       df$eyb,
                       df$yr_rmdl)
  
  #sale_year predictor
  df$sale_year <- format(as.Date(df$saledate), "%Y")
  df$sale_year <- as.integer(df$sale_year)
  
  # drop saledate entirely bc its too granular
  df$saledate <- as.Date(df$saledate)
  
  # sale ayb
  df$sale_ayb <- df$sale_year - df$ayb
  
  # sale eyb
  df$sale_eyb <- df$sale_year - df$eyb
  
  # sale year remodel
  df$sale_yr_rmdl <- df$yr_rmdl - df$sale_year
  
  # gba percentage of total land
  df$gba_p <- df$gba/df$landarea
  
  # total bathroom
  df$bathrm_tot <- df$bathrm + 0.5*df$hf_bathrm
  
  # grade predictor change to ordinal
  unique(df$grade)
  df$grade <- factor(df$grade, order = TRUE, 
                     levels = c("Low Quality",
                                "Fair Quality",
                                "Average",
                                "Above Average",
                                "Good Quality",
                                "Very Good",
                                "Superior"))
  
  return(df)
  
}
dtrain <- preprocessing(dtrain)

######## model fitting #########
# A simple linear model fit
fit <- lm(price ~ saledate + gba + grade + I(ayb^4) + I(bathrm_tot^2) + 
            fireplaces + I(sale_eyb^4) + extwall + gba_p, dtrain)
pred <- predict(fit, newdata=preprocessing(dtest))
res <- data.frame(Id=dtest$Id, price=pmax(pred,1))
return(res)

pred[pred<0]<-1
pred
