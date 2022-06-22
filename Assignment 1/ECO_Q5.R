# NOTE : Dont delete/ remove data variables which are created with each R file.
# Some of the scripts use data structures which were formed in the previous R scripts.

# making linear models with independent var as yield_index_growth and dependent variable as a health indicator.
pulse_df = subset(main, main$cropcategory == "Pulse")
model_q5_1 = lm(v15 ~ yield_index_growth, data = pulse_df)
summary(model_q5_1)

cash_df = subset(main, main$cropcategory == "Cash")
model_q5_2 = lm(v15 ~ yield_index_growth, data = cash_df)
summary(model_q5_2)

Cereal_df = subset(main, main$cropcategory == "Cereal")
model_q5_3 = lm(v15 ~ yield_index_growth, data = Cereal_df)
summary(model_q5_3)

Oilseed_df = subset(main, main$cropcategory == "Oilseed")
model_q5_4 = lm(v15 ~ yield_index_growth, data = Oilseed_df)
summary(model_q5_4)

Horticulture_df = subset(main, main$cropcategory == "Horticulture")
model_q5_5 = lm(v15 ~ yield_index_growth, data = Horticulture_df)
summary(model_q5_5)

Coarse_Cereal_df = subset(main, main$cropcategory == "Coarse Cereal")
model_q5_6 = lm(v15 ~ yield_index_growth, data = Coarse_Cereal_df)
summary(model_q5_6)



# What could be a potential issue in including yield indices for all six crop categories together?
# 
# The issue is that these crops are different, all the crops have different needs and conditions to grow fully.
# Some crop types might require more land than the others. Some crop types might be more vulnerable
# to pests.
# Some crop types could also be highly dependent on large amount of fertilizers to grow.
# Crops types might have different harvest indices.
# As the crop types are not the same they require different conditions, which results in
# dissimilarity in the yield index. For example: yield index for Pulses would be different than
# that for Horticulture crops, combining the data of yield_index of the two crop types wouldnt make
# sense as the range of yield_index for both the crop types could be very different.
# This creates inconsistency in data, and the regression model wouldnt make much sense.
# It would be better if we model six regressions, one for each crop category.
