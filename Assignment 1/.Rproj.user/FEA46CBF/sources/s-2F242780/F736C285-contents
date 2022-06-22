# NOTE : Dont delete/ remove data variables which are created with each R file.
# Some of the scripts use data structures which were formed in the previous R scripts.

# We know goodness of fit or R^2 is given by 
# and correlation coefficient is root(R^2) or R.
# 
# To verify this relationship, in a multiple regression model we need to find correlation between,
# the fitted values given by the model and the orignal values.
# R^2 value for a model can be found out using summary(model_name).

# proving this for model_a
# model_a R^2 = 0.2047
cor(model_a$fitted.values, model_a$model$main.v15) 
# comes out to be 0.4524225 CHECK: root(0.2047)  = 0.4524378 (almost equal)
# so the theoretical relation between goodness of fit and correlation coefficient can be 
# proven practically by finding the correlation and goodness of fits of the MLRM.

# model_b R^2 = 0.2204
cor(model_b$fitted.values, model_b$model$v15)  
# CHECK: root(0.2204)  = 0.4694678 (almost equal)


# this can be done for all the 21 models and similar results are obtained.

cor(model_c_1$fitted.values, model_c_1$model$v15)  
# Multiple R-squared:  0.216
# cor = .4647398  :  .4647398^2 = 0.2159831


cor(model_d$fitted.values, model_d$model$v15)  
# Multiple R-squared:  0.2993
# cor = 0.5470577  :  0.5470577^2 = 0.2992721


cor(model_c_1$fitted.values, model_c_1$model$v15)  
cor(model_c_2$fitted.values, model_c_2$model$v15)  
cor(model_c_3$fitted.values, model_c_3$model$v15)  
cor(model_c_4$fitted.values, model_c_4$model$v15)  
cor(model_c_5$fitted.values, model_c_5$model$v15)  
cor(model_c_6$fitted.values, model_c_6$model$v15)  



cor(model_e_1$fitted.values, model_e_1$model$v15)  
cor(model_e_2$fitted.values, model_e_2$model$v15)  
cor(model_e_3$fitted.values, model_e_3$model$v15)  
cor(model_e_4$fitted.values, model_e_4$model$v15)  
cor(model_e_5$fitted.values, model_e_5$model$v15)  
cor(model_e_6$fitted.values, model_e_6$model$v15)  

cor(model_f$fitted.values, model_f$model$v15)  

cor(model_g_1$fitted.values, model_g_1$model$v15)  
cor(model_g_2$fitted.values, model_g_2$model$v15)  
cor(model_g_3$fitted.values, model_g_3$model$v15)  
cor(model_g_4$fitted.values, model_g_4$model$v15)  
cor(model_g_5$fitted.values, model_g_5$model$v15)  
cor(model_g_6$fitted.values, model_g_6$model$v15)  


