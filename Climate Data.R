
#Importing necessary packages

  #reading excel files
library(readxl)

#Importing the dataset
Phen <- read_excel("ClimateData2.xlsx")
  #Setting all variables to correct types
Phen$Year <- as.factor(Phen$Year)
Phen$TotGDDJanAug <- as.numeric(Phen$TotGDDJanAug)
Phen$TotPrecipJanAug <- as.numeric(Phen$TotPrecipJanAug)
Phen$MeanPropFlr <- as.numeric(Phen$MeanPropFlr)
Phen$MeanPropDisp <- as.numeric(Phen$MeanPropDisp)

#Evaluating whether Precipitation/GDD50 is significantly different from Years with Early development (2018,2021,2022) and Late (2019,2020).
  #Testing assumptions: normality and equal variance
qqnorm(Phen$TotPrecipJanAug)
qqnorm(Phen$TotGDDJanAug)

tapply(Phen$TotPrecipJanAug, Phen$Group, var) #drastic difference in variance for precip
tapply(Phen$TotGDDJanAug, Phen$Group, var)

t.test(Phen$TotPrecipJanAug~Phen$Group)
t.test(Phen$TotGDDJanAug~Phen$Group)

#Evaluating whether precipitation/GDD50 is correlated with phenology proportions
#Testing assumptions: bivariate normality 
qqnorm(Phen$TotPrecipJanAug)
qqnorm(Phen$TotGDDJanAug)
plot(Phen$TotPrecipJanAug, Phen$MeanPropFlr) #Bivariate normality is very clearly violated
plot(Phen$TotPrecipJanAug, Phen$MeanPropDisp)
plot(Phen$TotGDDJanAug, Phen$MeanPropFlr)
plot(Phen$TotGDDJanAug, Phen$MeanPropDisp)
#Performing a Linear Correlation to evaluate whether precipitation and phenology dividers
cor.test(Phen$TotPrecipJanAug, Phen$MeanPropFlr)
#if nonnormal (all):
cor.test(Phen$TotPrecipJanAug, Phen$MeanPropFlr, method="spearman")
cor.test(Phen$TotPrecipJanAug, Phen$MeanPropDisp, method="spearman")

cor.test(Phen$TotGDDJanAug, Phen$MeanPropFlr, method="spearman")
cor.test(Phen$TotGDDJanAug, Phen$MeanPropDisp, method="spearman")
