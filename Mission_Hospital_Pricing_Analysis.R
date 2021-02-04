setwd("~/Desktop/BDA/TERM 2/PPA(Predictive & Prescriptive Analytics)/PPA Working on R")
library(car)
library(corrplot)  
library(caret) 
library(caTools) 
library(psych)   
library(ggplot2)
library(dplyr)

#mission = read.csv("Mission Hospital.csv",na.strings=c(""," ","NA"), stringsAsFactors = TRUE)
mission = read.csv("Mission Hospital.csv", stringsAsFactors = TRUE)
View(mission)

# Removing the NA and missing values:
mission1 <- na.omit(mission)
View(mission1)
summary (mission1)

# Checking Outliers :
# Between PAST.MEDICAL.HISTORY and LENGTH.OF.STAY
ggplot(data = mission1,mapping = aes(x=PAST.MEDICAL.HISTORY.CODE,y=LENGTH.OF.STAY..WARD)) + theme_classic()+ 
  geom_boxplot()+labs(x="Medical History",y= "Length of stay") 

# Between PAST.MEDICAL.HISTORY and TOTAL.AMOUNT.BILLED
ggplot(data = mission1,mapping = aes(x=PAST.MEDICAL.HISTORY.CODE,y=TOTAL.AMOUNT.BILLED.TO.THE.PATIENT)) + theme_classic()+ 
  geom_boxplot()+labs(x="Medical History",y= "Length of stay") 


# Checking the correlation :
names(mission1)
cr <- cor(mission1[c("AGE", "BODY_WEIGHT",  "BODY_HEIGHT", "TOTAL_LENGTH_OF_STAY", "LENGTH_OF_STAY_ICU",  "COST_OF_IMPLANT", "TOTAL_COST_TO_HOSPITAL")])
cr
corrplot(cr, type='full')
corrplot(cr, method ='number') 
corrplot.mixed (cr)

# Making are distribution normally distributed using LOG TRANSFORMATION :
mission1$TOTAL_COST_TO_HOSPITAL <- log(mission1$TOTAL_COST_TO_HOSPITAL)

# Checking the normal distribution and correlation:
pairs.panels(mission1[c("AGE", "BODY_WEIGHT",  "BODY_HEIGHT", "TOTAL_LENGTH_OF_STAY", "LENGTH_OF_STAY_ICU",  "COST_OF_IMPLANT", "TOTAL_COST_TO_HOSPITAL")])
?pairs.panels


# Linear regression model :
model1 <-lm(TOTAL_COST_TO_HOSPITAL ~ BODY_WEIGHT, data = mission1)
summary(model1)

model2 <-lm(TOTAL_COST_TO_HOSPITAL ~ BODY_WEIGHT+ AGE, data = mission1)
summary(model2)

model3 <-lm(TOTAL_COST_TO_HOSPITAL ~ BODY_WEIGHT+AGE+BODY_HEIGHT, data = mission1)
summary(model3)

model4 <-lm(TOTAL_COST_TO_HOSPITAL ~ BODY_WEIGHT+AGE+BODY_HEIGHT+TOTAL_LENGTH_OF_STAY, data = mission1)
summary(model4)

model5 <-lm(TOTAL_COST_TO_HOSPITAL ~ BODY_WEIGHT+AGE+BODY_HEIGHT+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU+COST_OF_IMPLANT, data = mission1)
summary(model5)

#BEST MODEL :o/p- Multiple R-squared:  0.8562,	Adjusted R-squared:  0.8531
model6 <-lm(TOTAL_COST_TO_HOSPITAL ~ BODY_HEIGHT+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU+COST_OF_IMPLANT, data = mission1)
summary(model6)

# Checking Heteroscedasticity of model6:
plot (model6$fitted.values, model6$residuals)


----------------------------------------------------------------------------------------------------------------------------------------------------------------

  
# 2. Data related to only patient having past medical conditions:
#Checking past history of patients for outliers :
Dia1 <- mission %>% filter(PAST.MEDICAL.HISTORY.CODE== c("Diabetes1"))
Dia2 <- mission %>% filter(PAST.MEDICAL.HISTORY.CODE== c("Diabetes2"))
hyp1 <- mission %>% filter(PAST.MEDICAL.HISTORY.CODE== c("hypertension1"))
hyp2 <- mission %>% filter(PAST.MEDICAL.HISTORY.CODE== c("hypertension2"))
hyp3 <- mission %>% filter(PAST.MEDICAL.HISTORY.CODE== c("hypertension3"))
oth <- mission %>% filter(PAST.MEDICAL.HISTORY.CODE== c("other"))
past_history <- combine(Dia1,Dia2,hyp1,hyp2,hyp3,oth)
View(past_history)
summary(past_history)

#Omitting the Na's :
past_history1 <- na.omit(past_history)

# checking outliers
ggplot(data = past_history1,mapping = aes(x=PAST.MEDICAL.HISTORY.CODE,y=LENGTH.OF.STAY..WARD)) + theme_classic()+ 
  geom_boxplot()+labs(x="Medical History",y= "Length of stay")  

#checking correlation :
cr1 <- cor(past_history1[c("AGE", "BODY_WEIGHT",  "BODY_HEIGHT", "TOTAL_LENGTH_OF_STAY", "LENGTH_OF_STAY_ICU",  "COST_OF_IMPLANT", "TOTAL_COST_TO_HOSPITAL")])
cr1
corrplot(cr1, type='full')

# Checking the normal distribution and correlation:
pairs.panels(past_history1[c("AGE", "BODY_WEIGHT",  "BODY_HEIGHT", "TOTAL_LENGTH_OF_STAY", "LENGTH_OF_STAY_ICU",  "COST_OF_IMPLANT", "TOTAL_COST_TO_HOSPITAL")])

#Linear regression model on past medical history :
#model21 : o/p- Multiple R-squared:  0.8425,	Adjusted R-squared:  0.828 
model21 <-lm(TOTAL_COST_TO_HOSPITAL ~ BODY_WEIGHT+AGE+BODY_HEIGHT+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU+COST_OF_IMPLANT, data = past_history)
summary(model21)

# Checking Heteroscedasticity of model21:
plot (model21$fitted.values, model21$residuals)











