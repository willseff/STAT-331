# UW ID: 20519497
# Name: Li, William
# Email: w255li@uwaterloo.ca

# dtrain: data.frame for the training set
# dtest: data.frame for the test set
# should return a data.frame for prediction
LinearModel <- function(dtrain, dtest){
  
  ####### preprocessing ############
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
  library(MASS)
  
  # reduced boxcox transform model
  fit <- lm(price^0.5 ~ saledate + gba + grade + ayb + I(ayb^2) + bathrm_tot + 
                    fireplaces+ sale_eyb+ I(sale_eyb^2)+ I(sale_eyb^3) + I(sale_eyb^4) + 
                    extwall + gba_p + landarea + I(landarea^2) + saledate:gba, dtrain)
  
    pred <- predict(fit, newdata=preprocessing(dtest))
    # undo transformation
    pred <- pred^2
    # set negetive values to 1
    pred[pred<0]<-1
  
    res <- data.frame(Id=dtest$Id, price=pred)
    return(res)
    
}

