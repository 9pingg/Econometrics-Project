# NOTE : Dont delete/ remove data variables which are created with each R file.
# Some of the scripts use data structures which were formed in the previous R scripts.

# finding out the models to compare v15 to yield index growth for all 6 crop types.

pulse_df = subset(main, main$cropcategory == "Pulse")
model_q6_1 = lm(v15 ~ yield_index_growth, data = pulse_df)
summary(model_q6_1)

cash_df = subset(main, main$cropcategory == "Cash")
model_q6_2 = lm(v15 ~ yield_index_growth, data = cash_df)
summary(model_q6_2)

Cereal_df = subset(main, main$cropcategory == "Cereal")
model_q6_3 = lm(v15 ~ yield_index_growth, data = Cereal_df)
summary(model_q6_3)

Oilseed_df = subset(main, main$cropcategory == "Oilseed")
model_q6_4 = lm(v15 ~ yield_index_growth, data = Oilseed_df)
summary(model_q6_4)

Horticulture_df = subset(main, main$cropcategory == "Horticulture")
model_q6_5 = lm(v15 ~ yield_index_growth, data = Horticulture_df)
summary(model_q6_5)

Coarse_Cereal_df = subset(main, main$cropcategory == "Coarse Cereal")
model_q6_6 = lm(v15 ~ yield_index_growth, data = Coarse_Cereal_df)
summary(model_q6_6)

# Is the relation between yield growth and health indicators similar across crop categories?
# No.The relation between yield growth index and health indicator is somewhat  dissimilar
# across crop categories. This  can be confirmed by making models with the dependent variable
# as v15 which is a health indicator and independent variable being yield_index_growth for a particular 
# crop. By analyzing these said models, 5 of them have a postive slope and one of them has a negative slope
# (Horticulture). Also the slope in absolute value varies for all 6 crops. 
# though the intercepts of the regression models are all pretty close and in the range [87, 88].


