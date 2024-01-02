library(readxl)
library(vegan)
Phen <- read_excel("MultiYearPrecip.xlsx")

#Ensure that variables used in statistical models are the types of variables that
#the relevant R commands require
Phen$PlotID <- as.factor(Phen$PlotID)
Phen$Year <- as.factor(Phen$Year)
Phen$EndAugFlwring <- as.numeric(Phen$EndAugFlwring)
Phen$StartOctDisp <- as.numeric(Phen$StartOctDisp)
Phen$ToTHdsFlrdBest <- as.numeric(Phen$ToTHdsFlrdBest)

#Remove observations that include NA and examine the data set 
Phen <- na.omit(Phen)
str(Phen)
head(Phen)
dim(Phen)

#Remove all observations in which the plant produced no flower heads. We cannot
#examine a proportion of heads that flowered by a particular date if the denominator
#is equal to zero
Phen <- Phen[Phen$ToTHdsFlrdBest > 0,]
dim(Phen)

#Pool the three different types of control plots
library(plyr)
Phen1 <- Phen
Phen1$CtlPool <- revalue(Phen$EXCL,c("CTL" = "CTL", "Pseudo" = "CTL", "TrnchCTL" = "CTL"))
Phen1$CtlPool <- as.factor(Phen1$CtlPool)
unique(Phen1$CtlPool)


#Examine the effects of the fertilization and the herbivore exclusion treatments
#and Year on the proportion of flower heads that had flowered by the end of August
library(lme4)
#M1 <- glmer(EndAugFlwring/ToTHdsFlrdBest ~ CtlPool + FERT + Year + (1|Block) + (1|PlotID), weights = ToTHdsFlrdBest, data = Phen1, family = binomial,
#            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 300000)))
#summary(M1)
#drop1(M1, test = "Chi")

M2 <- glmer(EndAugFlwring/ToTHdsFlrdBest ~ CtlPool + FERT + Year + (1|PlotID), weights = ToTHdsFlrdBest, data = Phen1, family = binomial,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 300000)))
summary(M2)
drop1(M2, test = "Chi")

#Consider specifically the effects of insect herbivory.  Pool herbivory
#treatment levels into 'InsPres' and 'InsAbs.'
Phen2 <- Phen1
Phen2$InsPool <- revalue(Phen$EXCL,c("CTL" = "InsPres", "Pseudo" = "InsPres", "TrnchCTL" = "InsPres", "AG" = "InsPres",
                                      "IN" = "InsAbs", "AGIN" = "InsAbs", "AGINBG" = "InsAbs"))

unique(Phen2$InsPool)

Phen2$AGPool <- revalue(Phen$EXCL,c("CTL" = "AGPres", "Pseudo" = "AGPres", "TrnchCTL" = "AGPres", "AG" = "AGAbs",
                                     "IN" = "AGPres", "AGIN" = "AGAbs", "AGINBG" = "AGAbs"))
Phen2$AGPool <- as.factor(Phen2$AGPool)
unique(Phen2$AGPool)
#Use generalized linear mixed models to examine the effects of insect exclusion, soil fertilization
#plant size (height) and year upon the proportion of flower heads that had flowered by late August
M3 <- glmer(EndAugFlwring/ToTHdsFlrdBest ~ InsPool + FERT + standhgt + Year + (1|PlotID), weights = ToTHdsFlrdBest, data = Phen2, family = binomial,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 300000)))
summary(M3)
drop1(M3, test = "Chi")

M4 <- glmer(EndAugFlwring/ToTHdsFlrdBest ~ AGPool+ FERT + standhgt + Year + (1|PlotID), weights = ToTHdsFlrdBest, data = Phen2, family = binomial,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 300000)))
summary(M4)
drop1(M4, test = "Chi")


M5 <- glmer(StartOctDisp/ToTHdsFlrdBest ~ InsPool + FERT + standhgt + Year + (1|PlotID), weights = ToTHdsFlrdBest, data = Phen2, family = binomial,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 300000)))
summary(M5)
drop1(M5, test = "Chi")


M6 <- glmer(StartOctDisp/ToTHdsFlrdBest ~ AGPool + FERT + standhgt + Year + (1|PlotID), weights = ToTHdsFlrdBest, data = Phen2, family = binomial,
            glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 300000)))
summary(M6)
drop1(M6, test = "Chi")

#evaluate any patterns in the residuals and test for overdispersion
EM4 <- resid(M4, type = "pearson")
FM4 <- fitted(M4)
plot(x = FM4, y = EM4, xlab = "Fitted values", ylab = "Residuals")
qqnorm(EM4)

N <- nrow(Phen2)
p <- length(coef(M4))
OverdispM4 <- sum(EM4^2) / (N - p)
OverdispM4

#Compare mean proportion of heads flowered by late August among levels of the
#insecticide treatment and among years.
Phen2$PropFlr <- Phen2$EndAugFlwring/Phen2$ToTHdsFlrdBest
Phen2$PropDisp <- Phen2$StartOctDisp/Phen2$ToTHdsFlrdBest
head(Phen2)
insmean <- tapply(Phen2$PropFlr,Phen2$InsPool,mean)
inssd <- tapply(Phen2$PropFlr,Phen2$InsPool,sd)
insn <- tapply(Phen2$PropFlr,Phen2$InsPool,length)
insse <- inssd / sqrt(insn)
insmean
insse
insn

tapply(Phen2$PropFlr,Phen2$Year,mean)

#There are significant differences among years.  Which pairs of years differ?
library(lsmeans)
lsmM4 <- lsmeans(M4, ~Year)
summary(pairs(lsmM4), type = "response")

#Make a plot to examine the relationship between plant height and
#proportion of flower heads flowered by late August
plot(x = Phen2$PlntHgt, y = Phen2$PropFlr, xlab = "Plant Height (cm)", ylab = "Prop Hds Flrd end August")


#We are looking at the effects of fertilization, insect pooling, and the random effect plot ID on plant height over the years.
ModelPlantHgt<- lmer(PlntHgt~FERT + InsPool + Year +(1|PlotID), data = Phen2)
summary(ModelPlantHgt)
drop1(ModelPlantHgt, test = "Chi")


#evaluating assumptions and seeing if means are different from eachother
tapply(Phen2$PlntHgt, Phen2$InsPool, mean)
tapply(Phen2$PlntHgt, Phen2$InsPool, var)
qqnorm(resid(ModelPlantHgt))

tapply(Phen2$PlntHgt, Phen2$FERT, mean)
tapply(Phen2$PlntHgt, Phen2$FERT, var)

#Testing Precip

plot(x=Phen2$TotPrecipJanJun, y=Phen2$PropFlr, xlab= "Precipitation", ylab= "Prop Hds Flrd end August")
plot(x=Phen2$TotPrecipJanJun, y=Phen2$PropDisp, xlab= "Precipitation", ylab= "Prop Hds Disp start October")


#Testing Flowering
ModelPrecipFlr <- lmer(PropFlr~InsPool+TotPrecipJanJun+(1|PlotID), data=Phen2)
summary(ModelPrecipFlr)
drop1(ModelPrecipFlr, test="Chi")

tapply(Phen2$PropFlr, Phen2$TotPrecipJanJun, mean)
tapply(Phen2$PropFlr, Phen2$TotPrecipJanJun, var)


#Testing Dispersing
ModelPrecipDisp <- lmer(PropDisp~InsPool+TotPrecipJanJun+(1|PlotID), data=Phen2)
summary(ModelPrecipDisp)
drop1(ModelPrecipDisp, test="Chi")

tapply(Phen2$PropDisp, Phen2$TotPrecipJanJun, mean)
tapply(Phen2$PropDisp, Phen2$TotPrecipJanJun, var)




