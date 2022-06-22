# NOTE : Dont delete/ remove data variables which are created with each R file.
# Some of the scripts use data structures which were formed in the previous R scripts.

library(dplyr)
library(ggplot2)

# reading data

path <- "main.csv"
data<-read.csv(path)
data_temp = data

path <- "update_main.csv"
main<-read.csv(path)
main <- subset (main, select = -X)

# print(data)
# function to calculate mode:

calcMode <- function(dataset){
    unique_vals <- unique(dataset)
    unique_vals <- unique_vals[!is.na(unique_vals)]
    unique_vals[which.max(tabulate(match(dataset, unique_vals)))]
}
# prints mean, median, mode, sd
summarizeDV <- function (x){
  print(paste0("mean: ",  mean(x,na.rm=TRUE)))
  print(paste0("median: ", median(x, na.rm=TRUE)))
  print(paste0("mode: ", calcMode(x)))
  print(paste0("standard deviation: ", sd(x, na.rm=TRUE)))
}
# summarize for all the variables
print("v40: ")
summarizeDV(data$v40)
print("v42: ")
summarizeDV(data$v42)
print("v43: ")
summarizeDV(data$v43)
print("v44: ")
summarizeDV(data$v44)
print("v45: ")
summarizeDV(data$v45)
print("v46: ")
summarizeDV(data$v46)


# b) Plotting histogram 
# NOTE:
# Here abbreviations are used for the 6 variable.
# these are used interchangeably.
# v40:sepsis
# v42:lbw
# v43:measles
# v44:diarrhoea
# v45:fever
# v46:measles

# variable description already given in the assing. pdf.
# these are used interchangeably.
# count = frequency

# here for a particular variable for all the years  and the seasons the histograms are combined.
# so for each variable there are two plots.

v40_year = unique(data.frame(data$state, data$district,data$year, data$v40))
year = as.factor(v40_year$data.year)
ggplot(v40_year, aes(x=v40_year$data.v40, fill = year))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("sepsis")+ggtitle("Histogram for v40 (year)")

v40_season = unique(data.frame(data$state, data$district, data$v40, data$season))
season = as.factor(v40_season$data.season)
ggplot(v40_season, aes(x=data.v40, fill = season))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("sepsis")+ ggtitle("Histogram for v40 (season)")

# v42

v42_year = unique(data.frame(data$state, data$district,data$year, data$v42))
year = as.factor(v42_year$data.year)
ggplot(v42_year, aes(x=v42_year$data.v42, fill = year))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("lbw")+ggtitle("Histogram for v42 (year)")

v42_season = unique(data.frame(data$state, data$district, data$v42, data$season))
season = as.factor(v42_season$data.season)
ggplot(v42_season, aes(x=data.v42, fill = season))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("lbw")+ggtitle("Histogram for v42 (season)")

#v43

v43_year = unique(data.frame(data$state, data$district,data$year, data$v43))
year = as.factor(v43_year$data.year)
ggplot(v43_year, aes(x=v43_year$data.v43, fill = year))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("pneumonia")+ggtitle("Histogram for v43 (year)")

v43_season = unique(data.frame(data$state, data$district, data$v43, data$season))
season = as.factor(v43_season$data.season)
ggplot(v43_season, aes(x=data.v43, fill = season))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("pneumonia")+ggtitle("Histogram for v43 (season)")

# v44

v44_year = unique(data.frame(data$state, data$district,data$year, data$v44))
year = as.factor(v44_year$data.year)
ggplot(v44_year, aes(x=v44_year$data.v44, fill = year))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("diarrhoea")+ggtitle("Histogram for v44 (year)")

v44_season = unique(data.frame(data$state, data$district, data$v44, data$season))
season = as.factor(v44_season$data.season)
ggplot(v44_season, aes(x=data.v44, fill = season))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("diarrhoea")+ggtitle("Histogram for v44 (season)")

# v55

v45_year = unique(data.frame(data$state, data$district,data$year, data$v45))
year = as.factor(v45_year$data.year)
ggplot(v45_year, aes(x=v45_year$data.v45, fill = year))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("fever")+ggtitle("Histogram for v45 (year)")

v45_season = unique(data.frame(data$state, data$district, data$v45, data$season))
season = as.factor(v45_season$data.season)
ggplot(v45_season, aes(x=data.v45, fill = season))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("fever")+ggtitle("Histogram for v45 (season)")

# v46

v46_year = unique(data.frame(data$state, data$district,data$year, data$v46))
year = as.factor(v46_year$data.year)
ggplot(v46_year, aes(x=v46_year$data.v46, fill = year))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("measles")+ggtitle("Histogram for v46 (year)")

v46_season = unique(data.frame(data$state, data$district, data$v46, data$season))
season = as.factor(v46_season$data.season)
ggplot(v46_season, aes(x=data.v46, fill = season))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity') + xlab("measles")+ggtitle("Histogram for v46 (season)")



# PART C) finding outliers in all the variables
# this function finds all the outliers for all the six variables.
# run one by one.
boxplot.stats(data$v40)$out
boxplot.stats(data$v42)$out
boxplot.stats(data$v43)$out
boxplot.stats(data$v44)$out
boxplot.stats(data$v45)$out
boxplot.stats(data$v46)$out

# PART D)
# cor function when supplied with a df makes a correlation matrix with the columns of df provided.


df = data.frame(data$v40, data$v42, data$v43, data$v44, data$v45, data$v46)
# cor matrix with all 6 variables mentioned in the question.
cor(na.omit(df))

# PART D) 1

df_1 = data.frame(data$v40, data$v42, data$v43, data$v44, data$v45, data$v46,main$gdp, main$beds, main$tap)
# cor matrix with all 6 variables, gdp, beds and tap.
cor(na.omit(df_1))

# PART D) 2

# here 6 different cor matrix are made for 6 different types of crop_categories.
# generally here cropcategory_df.index represents the yield index for the given crop category.
# pulse_df.index represents the yield index.
pulse_df = subset(data, data$cropcategory == "Pulse")
pulse_df = data.frame(pulse_df$index, pulse_df$v40, pulse_df$v42, pulse_df$v43, pulse_df$v44, pulse_df$v45, pulse_df$v46)
cor(na.omit(pulse_df))

cash_df = subset(data, data$cropcategory == "Cash")
cash_df = data.frame(cash_df$index, cash_df$v40, cash_df$v42, cash_df$v43, cash_df$v44, cash_df$v45, cash_df$v46)
cor(na.omit(cash_df))

Cereal_df = subset(data, data$cropcategory == "Cereal")
Cereal_df = data.frame(Cereal_df$index, Cereal_df$v40, Cereal_df$v42, Cereal_df$v43, Cereal_df$v44, Cereal_df$v45, Cereal_df$v46)
cor(na.omit(Cereal_df))

Oilseed_df = subset(data, data$cropcategory == "Oilseed")
Oilseed_df = data.frame(Oilseed_df$index, Oilseed_df$v40, Oilseed_df$v42, Oilseed_df$v43, Oilseed_df$v44, Oilseed_df$v45, Oilseed_df$v46)
cor(na.omit(Oilseed_df))


Horticulture_df = subset(data, data$cropcategory == "Horticulture")
Horticulture_df = data.frame(Horticulture_df$index, Horticulture_df$v40, Horticulture_df$v42, Horticulture_df$v43, Horticulture_df$v44, Horticulture_df$v45, Horticulture_df$v46)
cor(na.omit(Horticulture_df))

Coarse_Cereal_df = subset(data, data$cropcategory == "Coarse Cereal")
Horticulture_df = data.frame(Coarse_Cereal_df$index, Coarse_Cereal_df$v40, Coarse_Cereal_df$v42, Coarse_Cereal_df$v43, Coarse_Cereal_df$v44, Coarse_Cereal_df$v45, Coarse_Cereal_df$v46)
cor(na.omit(Horticulture_df))
 

# PART D) 3

# data_2 is a separate df which will be used to add a new column yield_index_growth. It is formed from some columns of data df.
data_2 =  data.frame(data$index, data$year, data$v40, data$v42, data$v43, data$v44, data$v45, data$v46, data$cropcategory)
data_2['yield_index_growth'] = 1

# this double loop is used to calculate yield_index_growth for a particular year for a given crop.

# NOTE: for 2011 there is no data of yield_index for previous years so yield_index_growth cannot be calculated or it will be a NaN value. Instead of this 
# here yield_index_growth for 2011 is taken to be the value of its yield_index.

seq = 1:70572
for(i in seq){
    if(data_2[i,2] == 2011){
      data_2[i,10] = data_2[i,1]
    }
  
  else {
    data_2[i,10] = (data_2[i,1] - data_2[i-1,1])/data_2[i-1,1]
  }
}

# here yield_growth_index has Inf values also, so if we run the model without removing inf 
# values, the model fails. So, to run the model replacing all inf values with mean of yield_growth_index

replaceByMean <- function(x) replace(x, is.infinite(x) | is.nan(x), mean(x[ !is.infinite(x)]))
data_2$yield_index_growth <- replaceByMean(data_2$yield_index_growth)

# taking taking subset of data_2 to find the correlation matrix. the columns taken can be found out below.
# Also, here 6 different cor matrix are made for 6 different types of crop_categories.
# generally here cropcategory_df.yield_index_growth represents the yield growth index for the given crop category.
# pulse_df.yield_index_growth represents the yield growth index.
pulse_df = subset(data_2, data_2$data.cropcategory == "Pulse")
pulse_df = data.frame(pulse_df$yield_index_growth, pulse_df$data.v40, pulse_df$data.v42, pulse_df$data.v43, pulse_df$data.v44, pulse_df$data.v45, pulse_df$data.v46)
cor(na.omit(pulse_df))

cash_df = subset(data_2, data_2$data.cropcategory == "Cash")
cash_df = data.frame(cash_df$yield_index_growth, cash_df$data.v40, cash_df$data.v42, cash_df$data.v43, cash_df$data.v44, cash_df$data.v45, cash_df$data.v46)
cor(na.omit(cash_df))

Cereal_df = subset(data_2, data_2$data.cropcategory == "Cereal")
Cereal_df = data.frame(Cereal_df$yield_index_growth, Cereal_df$data.v40, Cereal_df$data.v42, Cereal_df$data.v43, Cereal_df$data.v44, Cereal_df$data.v45, Cereal_df$data.v46)
cor(na.omit(Cereal_df))

Oilseed_df = subset(data_2, data_2$data.cropcategory == "Oilseed")
Oilseed_df = data.frame(Oilseed_df$yield_index_growth, Oilseed_df$data.v40, Oilseed_df$data.v42, Oilseed_df$data.v43, Oilseed_df$data.v44, Oilseed_df$data.v45, Oilseed_df$data.v46)
cor(na.omit(Oilseed_df))


Horticulture_df = subset(data_2, data_2$data.cropcategory == "Horticulture")
Horticulture_df = data.frame(Horticulture_df$yield_index_growth, Horticulture_df$data.v40, Horticulture_df$data.v42, Horticulture_df$data.v43, Horticulture_df$data.v44, Horticulture_df$data.v45, Horticulture_df$data.v46)
cor(na.omit(Horticulture_df))

Coarse_Cereal_df = subset(data_2, data_2$data.cropcategory == "Coarse Cereal")
Coarse_Cereal_df = data.frame(Coarse_Cereal_df$yield_index_growth, Coarse_Cereal_df$data.v40, Coarse_Cereal_df$data.v42, Coarse_Cereal_df$data.v43, Coarse_Cereal_df$data.v44, Coarse_Cereal_df$data.v45, Coarse_Cereal_df$data.v46)
cor(na.omit(Coarse_Cereal_df))




# v40_2011 <-subset(v40_year, data.year == 2011)
# v40_2012 <-subset(v40_year, data.year == 2012)
# v40_2013 <-subset(v40_year, data.year == 2013)
# v40_2014 <-subset(v40_year, data.year == 2014)
# v40_2015 <-subset(v40_year, data.year == 2015)
# v40_2016 <-subset(v40_year, data.year == 2016)


# hist(v40_2011$data.v40, xlab = "v40", main = "Histogram ")
# hist(v40_2012$data.v40, col='green', add = TRUE)
# hist(v40_2013$data.v40, col='red', add = TRUE)
# hist(v40_2014$data.v40, col='blue', add = TRUE)
# hist(v40_2012$data.v40, col='green', add = TRUE)
# v40_2011$temp = 'a'
# v40_2012$temp = 'a'
# v40_year = rbind(v40_2011, v40_2012)
# ggplot(v40_year, aes(data.v40, fill = data.year)) + 
#   geom_histogram(color = "#e9ecef",alpha = 0.5, position = 'identity')
# # v40_unique_year$data.year = as.numeric(as.character(v40_unique_year$data.year))
# # a = v40_unique_year %>%
# #   group_by(data.year)
# # a_df_plot = as.data.frame(as.table(tapply(v40_unique_year$data.v40, v40_unique_year$data.year, FUN = sum, na.rm=TRUE)))
# # colnames(a_df_plot) <- c('year','freq')
# # a_df_plot$year = as.numeric(as.character(a_df_plot$year))
# # barplot(a_df_plot$freq,names.arg=a_df_plot$year,xlab="Year",ylab="frequency", main="Histogram for v40")
# 
# # season
# 
# v40_season = data.frame(data$state, data$district, data$v40, data$season)
# v40_unique_season = unique(v40_season)
# a_df_season_plot = as.data.frame(as.table(tapply(v40_unique_season$data.v40, v40_unique_season$data.season, FUN = mean, na.rm=TRUE)))
# colnames(a_df_season_plot) <- c('season','freq')
# a_df_season_plot$season = as.character(a_df_season_plot$season)
# barplot(a_df_season_plot$freq,names.arg=a_df_season_plot$season,xlab="Season",ylab="frequency", main="Histogram for v40 (season)")
#  
# table(data$year)
# y_of <-subset(v40_unique_year, data.year == 2011)
# hist(v40_2011$data.v40)


# yield index














