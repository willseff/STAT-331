library(ggplot2)

summary(dtrain)
dim(dtrain)

# treat nas in stories
# if na then take the value from style
dtrain$stories <- ifelse(is.na(dtrain$stories),
                         as.numeric(substring(dtrain$style,1,1)),
                         dtrain$stories)

# treat nas in yr remodel
# create new col binary whether been remodeled
dtrain$remdl <- ifelse(is.na(dtrain$yr_rmdl),
                       "Y",
                       "N")

# if it is never remodeled then set year as eyb
dtrain$yr_rmdl <- ifelse(is.na(dtrain$yr_rmdl),
                         dtrain$eyb,
                         dtrain$yr_rmdl)

#sale_year predictor
dtrain$sale_year <- format(as.Date(dtrain$saledate), "%Y")
dtrain$sale_year <- as.integer(dtrain$sale_year)

# drop saledate entirely bc its too granular
dtrain$saledate <- as.Date(dtrain$saledate)

# month predictor
# not needed bc useless
#dtrain$month <- format(as.Date(dtrain$saledate), "%m")
#dtrain$month

# sale ayb
dtrain$sale_ayb <- dtrain$sale_year - dtrain$ayb

# sale eyb
dtrain$sale_eyb <- dtrain$sale_year - dtrain$eyb

# sale year remodel
dtrain$sale_yr_rmdl <- dtrain$yr_rmdl - dtrain$year

# gba percentage of total land
dtrain$gba_p <- dtrain$gba/dtrain$landarea

# total bathroom
dtrain$bathrm_tot <- dtrain$bathrm + 0.5*dtrain$hf_bathrm

# grade predictor change to ordinal
unique(dtrain$grade)
dtrain$grade <- factor(dtrain$grade, order = TRUE, 
                                    levels = c("Low Quality",
                                               "Fair Quality",
                                               "Average",
                                               "Above Average",
                                               "Good Quality",
                                               "Very Good",
                                               "Superior"))

# dtrain just categorical varaibles and price

# dtrain just ordinal variabes and price

# dtain just continuous varaiables and price
c("landarea","price")






