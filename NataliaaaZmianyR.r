library(AER)
GrowthSW_df <- as.data.frame(GrowthSW)
class(GrowthSW)
?GrowthSW

model_growth <- lm(growth ~ tradeshare, data = GrowthSW_df)

summary(model_growth)