# NOTE : Dont delete/ remove data variables which are created with each R file.
# Some of the scripts use data structures which were formed in the previous R scripts.

path <- "update_main.csv"
main<-read.csv(path)
main_q3 = main
main <- subset (main, select = -X)

# institutional deliveries as a percentage variable: v15 to be taken as a health indicator

# A)

main_3 = data.frame(main$sdyid, main$v15, main$beds, main$gdp, main$tap)
main_3  = unique(main_3)
# this is done to remove duplicate entries.
model_a = lm(main.v15 ~ main.gdp+main.beds+main.tap,data = main_3)
summary(model_a)

# B)
model_b = lm(v15 ~ gdp+beds+tap+index, data = main)
summary(model_b)

# C) for C,E,G parts six separate models had to made.
pulse_df = subset(main, main$cropcategory == "Pulse")
model_c_1 = lm(v15 ~ gdp+beds+tap+index, data = pulse_df)
summary(model_c_1)

cash_df = subset(main, main$cropcategory == "Cash")
model_c_2 = lm(v15 ~ gdp+beds+tap+index, data = cash_df)
summary(model_c_2)

Cereal_df = subset(main, main$cropcategory == "Cereal")
model_c_3 = lm(v15 ~ gdp+beds+tap+index, data = Cereal_df)
summary(model_c_3)

Oilseed_df = subset(main, main$cropcategory == "Oilseed")
model_c_4 = lm(v15 ~ gdp+beds+tap+index, data = Oilseed_df)
summary(model_c_4)

Horticulture_df = subset(main, main$cropcategory == "Horticulture")
model_c_5 = lm(v15 ~ gdp+beds+tap+index, data = Horticulture_df)
summary(model_c_5)

Coarse_Cereal_df = subset(main, main$cropcategory == "Coarse Cereal")
model_c_6 = lm(v15 ~ gdp+beds+tap+index, data = Coarse_Cereal_df)
summary(model_c_6)

# D) 
seq = 1:70572
main['yield_index_growth'] = 0
# this loops fills the values for the column yield_index_growth
for(i in seq){
  if(main[i,7] == 2011){
    main[i,73] = main[i,69]
  }
  
  else {
    main[i,73] = (main[i,69] - main[i-1,69])/main[i-1,69]
  }
}
# here yield_growth_index has Inf values also, so if we run the model without removing inf 
# values, the model fails. So, to run the model replacing all inf values with mean of yield_growth_index
replaceByMean <- function(x) replace(x, is.infinite(x) | is.nan(x), mean(x[ !is.infinite(x)]))
main$yield_index_growth <- replaceByMean(main$yield_index_growth)

model_d = lm(v15 ~ gdp+beds+tap+yield_index_growth, data = main)


# E)
pulse_df = subset(main, main$cropcategory == "Pulse")
model_e_1 = lm(v15 ~ gdp+beds+tap+yield_index_growth, data = pulse_df)
summary(model_e_1)

cash_df = subset(main, main$cropcategory == "Cash")
model_e_2 = lm(v15 ~ gdp+beds+tap+yield_index_growth, data = cash_df)
summary(model_e_2)

Cereal_df = subset(main, main$cropcategory == "Cereal")
model_e_3 = lm(v15 ~ gdp+beds+tap+yield_index_growth, data = Cereal_df)
summary(model_e_3)

Oilseed_df = subset(main, main$cropcategory == "Oilseed")
model_e_4 = lm(v15 ~ gdp+beds+tap+yield_index_growth, data = Oilseed_df)
summary(model_e_4)

Horticulture_df = subset(main, main$cropcategory == "Horticulture")
model_e_5 = lm(v15 ~ gdp+beds+tap+yield_index_growth, data = Horticulture_df)
summary(model_e_5)

Coarse_Cereal_df = subset(main, main$cropcategory == "Coarse Cereal")
model_e_6 = lm(v15 ~ gdp+beds+tap+yield_index_growth, data = Coarse_Cereal_df)
summary(model_e_6)


# F)

main$log.tap = log(main$tap)
main$log.index = log(main$index)
main$log.yield_index_growth = log(main$yield_index_growth)

# NOTE: here when the independent variables that have the value zero. On taking log on them,
# gives undefined values (log(0) undefined) due to this
# the model fails. So, to avoid this, log(0) is replaced by 0.
main$log.tap<-replace(main$log.tap, main$log.tap == -Inf, 0)
main$log.index<-replace(main$log.index, main$log.index == -Inf, 0) 
main$log.yield_index_growth<-replace(main$log.yield_index_growth, main$log.yield_index_growth == -Inf, 0) 

model_f = lm(v15 ~ log(gdp)+log(beds)+log.tap+log.index, data = main)
summary(model_f)

# G)

pulse_df = subset(main, main$cropcategory == "Pulse")
model_g_1 = lm(v15 ~ log(gdp)+log(beds)+log.tap+log.yield_index_growth, data = pulse_df)
summary(model_g_1)

cash_df = subset(main, main$cropcategory == "Cash")
model_g_2 = lm(v15 ~ log(gdp)+log(beds)+log.tap+log.yield_index_growth, data = cash_df)
summary(model_g_2)

Cereal_df = subset(main, main$cropcategory == "Cereal")
model_g_3 = lm(v15 ~ log(gdp)+log(beds)+log.tap+log.yield_index_growth, data = Cereal_df)
summary(model_g_3)

Oilseed_df = subset(main, main$cropcategory == "Oilseed")
model_g_4 = lm(v15 ~ log(gdp)+log(beds)+log.tap+log.yield_index_growth, data = Oilseed_df)
summary(model_g_4)

Horticulture_df = subset(main, main$cropcategory == "Horticulture")
model_g_5 = lm(v15 ~ log(gdp)+log(beds)+log.tap+log.yield_index_growth, data = Horticulture_df)
summary(model_g_5)

Coarse_Cereal_df = subset(main, main$cropcategory == "Coarse Cereal")
model_g_6 = lm(v15 ~ log(gdp)+log(beds)+log.tap+log.yield_index_growth, data = Coarse_Cereal_df)
summary(model_g_6)

