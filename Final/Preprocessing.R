library(ggplot2)

summary(dtrain)
dim(dtrain)

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
  df$sale_yr_rmdl <- df$yr_rmdl - df$year
  
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

dtrain<- preprocessing(dtrain)


# month predictor
# not needed bc useless
#dtrain$month <- format(as.Date(dtrain$saledate), "%m")
#dtrain$month

# dtrain just categorical varaibles and price

# dtrain just ordinal variabes and price

# dtain just continuous varaiables and price
c("landarea","price")

unique(dtest$grade)






