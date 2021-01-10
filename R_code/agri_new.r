#importing packages
library(tseries)
library(lmtest)
library(dynlm)
library(MASS)
library(forecast)
library(faraway)
#reading the csv
d=read.csv("agri_gdp1.csv")
print(head(d))
print(nrow(d))
print(ncol(d))
print(names(d))
d=d[(1:24),]
print(d$Total.Direct.Agriculture.credit)
print(nrow(d))
print(head(d))
print(names(d))
#selecting dataframes
df_new=d[c(1:24),c(1,3,4,7,13,16,18,19,21,24,26,28,10,14,12)]
print(head(df_new))
names(df_new)[1] <- "Year"
names(df_new)[2] <- "Production"
names(df_new)[3] <- "Yield"
names(df_new)[4] <- "Consumption_of_fertilisers"
names(df_new)[5] <- "Import_export_ratio"
names(df_new)[6] <- "Per_capita_foodgrain"
names(df_new)[7]<- "Net_area_sown"
names(df_new)[8] <- "Total_cropped_area"
names(df_new)[9] <- "Net_irrigated_area"
names(df_new)[10] <- "GDP_agriculture_and_Allied_sector"
names(df_new)[11] <- "agriculture_share_to_total_gdp"
names(df_new)[12] <- "Growth_rate_agriculture_gdp"
names(df_new)[13] <- "Total_direct_agricultural_credit"
names(df_new)[14] <- "electricity_consumption_agriculture"
names(df_new)[15] <- "Agricultural_exports"
print(head(df_new))
print(summary(df_new$Production))
print(summary(df_new$Yield))
print(summary(df_new$Consumption_of_fertilisers))
print(summary(df_new$Import_export_ratio))
print(summary(df_new$GDP_agriculture_and_Allied_sector))
print(summary(df_new$agriculture_share_to_total_gdp))
print(summary(df_new$Growth_rate_agriculture_gdp))
print(summary(df_new$Total_cropped_area))
print(summary(df_new$electricity_consumption_agriculture))
print(summary(df_new$Agricultural_exports))
print(summary(df_new$Total_direct_agricultural_credit))
x=df_new$Year
#plots
y=df_new$agriculture_share_to_total_gdp
plot(x,y,ylab = "Agriculture share to total GDP",xlab = "Time (in years)",main="Agriculture share to total GDP",type="l",lwd=2)
#adf.test
adf.test(df_new$Production)
adf.test(df_new$Yield)
adf.test(df_new$Consumption_of_fertilisers)
adf.test(df_new$Import_export_ratio)
adf.test(df_new$GDP_agriculture_and_Allied_sector)
adf.test(df_new$agriculture_share_to_total_gdp)
adf.test(df_new$Growth_rate_agriculture_gdp)
adf.test(df_new$Total_cropped_area)
adf.test(df_new$Total_direct_agricultural_credit)
adf.test(df_new$electricity_consumption_agriculture)
adf.test(df_new$Agricultural_exports)
#print(df_new$Production)
#adf.test 
adf.test(diff(log(df_new$Production),differences=3))
adf.test(diff(log(df_new$Yield),differences=3))
adf.test(diff(log(df_new$Consumption_of_fertilisers),differences = 3))
adf.test(diff(log(df_new$Import_export_ratio),differences=3))
adf.test(diff(log(df_new$GDP_agriculture_and_Allied_sector),differences=3))
adf.test(diff(log(df_new$agriculture_share_to_total_gdp),differences=3))
adf.test(diff(log(df_new$Total_cropped_area),differences = 3))
adf.test(diff(log(df_new$Total_direct_agricultural_credit),differences = 3))
adf.test(diff(log(df_new$electricity_consumption_agriculture),differences = 3))
adf.test(diff(log(df_new$Agricultural_exports),differences=3))
#adf.test(diff(log(df_new$Growth_rate_agriculture_gdp),differences=2))
#making dataframe
#applying log transformation and differenicing
production=diff(log(df_new$Production),differences=3)
yield=diff(log(df_new$Yield),differences=3)
fertiliser_consumption=diff(log(df_new$Import_export_ratio),differences = 3)
import_export_ratio=diff(log(df_new$GDP_agriculture_and_Allied_sector),differences = 3)
agricultural_GDP=diff(log(df_new$agriculture_share_to_total_gdp),differences=3)
agriculture_share_to_gdp=diff(log(df_new$agriculture_share_to_total_gdp),differences=3)
total_cropped_area=diff(log(df_new$Total_cropped_area),differences = 3)
total_agricultural_credit=diff(log(df_new$Total_direct_agricultural_credit),differences = 3)
agriculture_share_to_GDP=diff(log(df_new$agriculture_share_to_total_gdp),differences=3)
agriculture_export=diff(log(df_new$Agricultural_exports),differences = 3)


electricty_consumption_agriculture=diff(log(df_new$electricity_consumption_agriculture),differences = 3)
frame1=cbind(production,yield,fertiliser_consumption,import_export_ratio,agricultural_GDP,agriculture_share_to_gdp,total_cropped_area,total_agricultural_credit,agriculture_share_to_gdp,electricty_consumption_agriculture,agriculture_export)
print(nrow(frame1))
ts_frame=ts(frame1,start=1993,end=2014)
#time series regression
reg1=dynlm(agricultural_GDP~yield+import_export_ratio,data=ts_frame)
print(summary(reg1))
#abline(reg1)
#par(mfrow=c(2,2))
plot(reg1,4)
plot(reg1)
#printing 95% confidence interval
print(confint(reg1,level=0.95))
#printing residuals
print(resid(reg1,level=0.95))
#checking for multicollinearity
faraway::vif(reg1)
cor(yield,import_export_ratio,method="pearson")

#plot(yield~per_capita_foodgrain+total_cropped_area+import_export_ratio)


#

