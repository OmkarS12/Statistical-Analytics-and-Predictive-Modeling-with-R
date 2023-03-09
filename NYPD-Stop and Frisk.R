library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(stats)
library(ISLR)
library(ggcorrplot)
library(plotly)
library(caret)
library(lattice)
library(pROC)
library(kableExtra)
library(SmartEDA)
library(heatmaply)
library(glmnet)
library(stargazer)
library(MASS)
library(nnet)
library(ggpubr)

Nypd <- read_csv("NYPD_SQF.csv")
str(Nypd)
df <- data.frame(Nypd)
str(df)
dim(df)

#Plot for Missing Data in the Dataset
df  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())

# Creating New variables for analysis
# Creating Area Col
df <- within(df,{
  Area='N/A'
  Area[pct %in% c(1,5,6,7,9,10,13,14,17,18)]='Manhattan South'
  Area[pct %in% c(19,20,22,23,24,25,26,28,30,32,33,34)]='Manhattan North'
  Area[pct %in% c(40,41,42,43,44,45,46,47,48,49,50,52)]='The Bronx'
  Area[pct %in% c(60,61,62,63,66,67,68,69,70,71,72,76,78)]='Brooklyn South'
  Area[pct %in% c(73,75,77,79,81,83,84,88,90,94)]='Brooklyn North'
  Area[pct %in% c(100,101,102,103,105,106,107,113)]='Queens South'
  Area[pct %in% c(104,108,109,110,111,112,114,115)]='Queens North'
  Area[pct %in% c(120,121,122,123)]='Staten Island'
})


#Summary Statistics of all  Variables
stat_table <- as.data.frame(psych::describe(df))
stat_table <- subset(stat_table, select=c("n","mean","sd")) 
stat_table <- stat_table %>%          
  mutate_if(is.numeric,
            round,
            digits = 1)
kbl(stat_table)%>%kable_styling
capture.output(stat_table,file="Sumstats.doc")

#Summary Statistic by Sex
sum_stat <- df %>% group_by(sex) %>% 
  summarise(stopped=round(mean(stopped),2),
            arrested=round(mean(arrested),2),
            frisked=round(mean(frisked),2),
            searched=round(mean(searched),2),
            summoned=round(mean(summoned),2),
            contraband=round(mean(contrabn),2),
            Weapon = round(mean(weapnfnd),2),
            .groups = 'drop')
kbl(sum_stat)%>%kable_styling()
capture.output(sum_stat,file="Sumstatsbysex.doc")

#Summary Statistics by Race
stat_race <- df%>%group_by(race)%>%
  summarise(stopped=round(mean(stopped),2),
            arrested=round(mean(arrested),2),
            frisked=round(mean(frisked),2),
            searched=round(mean(searched),2),
            summoned=round(mean(summoned),2),
            contraband=round(mean(contrabn),2),
            Weapon = round(mean(weapnfnd),2),
            .groups = 'drop')
kbl(stat_race)%>%kable_styling()
capture.output(sum_stat,file="StatsRace.doc")


#Correlation Plot
corr <- select_if(df, is.numeric)
cormat <- round(cor(corr),2)
ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           lab = TRUE)

#EDA
#Total arrests in New York
precint <- df%>%group_by(Area)%>%summarise(arrested=sum(arrested))%>%
  plot_ly(x=~Area, y=~arrested, color=~Area, type = 'bar',text = ~arrested, textposition = 'top', insidetextfont = list(size=10, color = 'black'))%>%
  layout(title= list(text = "Total Arrests in New York",
                     yaxis = list(showgrid = FALSE)))

precint

#People arrested over years
stat_year <- df%>% group_by(year)%>%summarise(arrested=mean(arrested))
t1<- ggplot(data=stat_year, aes(x=year, y=arrested, color = "blue")) +
  geom_line() + geom_point()+ labs(title="Trend for arrested over years")+
  scale_color_brewer(palette="Paired")+
  theme_minimal()
#Plot for arrested over years by Race
race_arrested <- df%>%group_by(race,year)%>%summarise(arrested = round(mean(arrested),2),.groups='drop')
t2<- ggplot(data=race_arrested, aes(x=year, y=arrested, color = race)) + 
  geom_line() + geom_point()+ labs(title="Trend for arrested by Race over years")+
  scale_color_brewer(palette="Paired")+
  theme_minimal()
#Plot for arrested over years by Sex
sex_arrested <- df%>%group_by(sex,year)%>%summarise(arrested = round(mean(arrested),2),.groups='drop')
t3<-ggplot(data=sex_arrested, aes(x=year, y=arrested, color = sex)) + 
  geom_line() + geom_point()+ labs(title="Trend for arrested by sex over years")+
  scale_color_brewer(palette="Paired")+
  theme_minimal()
ggarrange(t1,t2,t3,ncol=1,nrow=3)

#People stopped over years by race
stopped <- df %>% group_by(year,race) %>% summarise(stopped = sum(stopped),.groups = 'drop')
kbl(stopped)%>%kable_styling()
plt <- plot_ly(stopped, x = ~year, y = ~stopped,color = ~race, type='bar',text = ~stopped/1000 , textposition = 'auto', insidetextfont = list(size=10, color = 'black'))%>%
  layout(title= list(text = "People stopped over years"), 
         legend=list(title=list(text='District')), 
         xaxis = list(title = list(text ='Area')),
         yaxis = list(showgrid = FALSE),
         yaxis = list(title = list(text = 'Number of People stopped')), barmode = 'stack')
plt

#People frisked in districts by race
Area_race <- df %>% group_by(Area,race) %>% summarise(frisked = sum(frisked),.groups = 'drop')%>%
  plot_ly( x = ~Area, y = ~frisked,color = ~race, type='bar')%>%
  layout(title= list(text = "People frisked over years"), 
         legend=list(title=list(text='Race')), 
         xaxis = list(title = list(text ='Area')),
         yaxis = list(showgrid = FALSE),
         yaxis = list(title = list(text = 'Number of People frisked')), barmode = 'stack')
Area_race

# Predicting the Number of arrests usin linear regression

set.seed(123)
trainIndex<- sample(x=nrow(df),size=nrow(df)*0.7)
train<-df[trainIndex,]
test<-df[-trainIndex,]

lm.fit1 <- lm(arrested~.,data=train)
summary(lm.fit1)

lm.fit2 <- lm(arrested~searched+contrabn+weapnfnd+pf_weapn+pf_hcuff,data=train)
summary(lm.fit2)

stargazer(lm.fit2, type="html",out="models.htm")

test$predicted.arrested <- predict(lm.fit2 ,test)
test %>% 
  ggplot(aes(arrested,predicted.arrested)) +
  geom_point(alpha=0.75) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of Arrests') +
  ggtitle("Actual Arrests vs Predicted Arrests")+
  ylab('Predicted value of Arrests') +
  theme_classic()+
  theme(legend.position="none")

# At the 0.05 level of significance, is there
# sufficient evidence to conclude that a difference in mean arrests exists among
# races?

racedf<-subset(df,year=="2003")
race<-racedf[, c('race', 'arrested')]  

# Alpha = 0.05%

# H0 = There is a difference in the mean arrests among races.
# Ha = There is no difference in the mean arrests among races.

anova1<-aov(arrested~race,data=race)
summary(anova1)

#Multinomial Logistic Regression 
#Test-Train Split
#df$race <- as.factor(df$race)
#levels(df$race)
#set.seed(3456)
#trainIndex <- createDataPartition(df$race, p = 0.7, list = FALSE, times = 1)
#caret_train <- df[ trainIndex,]
#caret_test <- df[-trainIndex,]

#Training the multinomial model
#multinom_model <- multinom(race ~ ., data = df)
# Checking the model
#summary(multinom_model)

#exp(coef(multinom_model))

#head(round(fitted(multinom_model), 2))

# Predicting the values for train dataset
#caret_train$racePredicted <- predict(multinom_model, newdata = caret_train, "class")
# Building classification table
#tab <- table(caret_train$race, caret_train$racePredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
#round((sum(diag(tab))/sum(tab))*100,2)

# Predicting the class for test dataset
#caret_test$racePredicted <- predict(multinom_model, newdata = caret_test, "class")
# Building classification table
#tab1 <- table(caret_test$race, caret_test$racePredicted)
#tab1
# Calculating accuracy - sum of diagonal elements divided by total obs
#round((sum(diag(tab1))/sum(tab))*100,2)

#Model
trainIndex <- createDataPartition(df$race, p = 0.7, list = FALSE, times = 1)
caret_train <- df[ trainIndex,]
caret_test <- df[-trainIndex,]
full.model <-  glm(as.factor(sex)~., data = caret_train, family = binomial(link = 'logit'))
coef(full.model)
summary(full.model)
caret_train$sex <- as.factor(caret_train$sex)
#Stepwise Selection
step.model <- full.model %>% stepAIC(trace = FALSE)
coef(step.model)
summary(step.model)

#Comparing the models
anova(full.model, step.model)
# Compare models with AIC
AIC(full.model, step.model)
# Compare models with BIC
BIC(full.model, step.model)
#Regression coefficients
exp(coef(step.model))

#Making predictions on train data uasing lambda.min
probabilities.train <- predict(step.model, newdata = caret_train, type = 'response')
predicted.classes.min <- as.factor(ifelse(probabilities.train >= 0.5, "Yes", "No"))
#Model Accuracy
confusionMatrix(predicted.classes.min,data = caret_train$sex, positive = 'Yes')


# Best subsets with regsubsets
library(leaps)
m3 <- regsubsets(arrested ~ ., data = caret_train, nvmax = 18)
reg.summary <- summary(m3)
reg.summary

reg.summary$cp 
reg.summary$adjr2 
reg.summary$bic

m4<-lm(arrested ~ year + race + stopped + arrested + 
         frisked + searched + summoned + contrabn + weapnfnd + pf_weapn + 
         pf_hcuff + cs_objcs + cs_descr + cs_casng + cs_lkout + cs_cloth + 
         cs_drgtr + cs_furtv + cs_vcrim + rf_vcrim + rf_othsw + rf_attir + 
         rf_vcact + rf_rfcmp + rf_verbl + rf_knowl + rf_furtv + rf_bulge + 
         Area, data = caret_train)

summary(m4)
capture.output(m4,file = "m4f.doc")

pred_test<- predict(m4,new=test)
pred_test
summary(pred_test)

rmse(test$grad_rate,pred_test)
R2(test$grad_rate,pred_test)


# plot actual vs predicted
plot(x=pred_test, y=test$grad_rate,col = "red",
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')

#add diagonal line for estimated regression line
abline(a=0, b=1)


















