install.packages(patternplot)
installed.packages(patternplot)
installed.packages("ggpubr")
install.packages("ggridges")
install.packages("psych")
install.packages("car") # vif values
install.packages("lmtest") #dw test

# required packages 
library(ggplot2) #data visualization
library(patternplot)#plots with patterns
library(dplyr)# calculating frequency
library(ggpubr)
library(ggridges)
library(data.table)
library(psych)
library(tidyverse)
library(caret)
library(lmtest)
library(car)

#importing csv file to R
data=read.csv("insuranceData.csv",header=TRUE)
data

#to see variable names
variable.names(data)

psych::describe(data)

# number of observations and number of varibles in the data set
dimension=dim(data)


#checking missing values

dim(data[complete.cases(data),])
any(is.na(data))

#data exploration
str(data)
attributes(data)

# looking the first and last few rowsof data set
head(data)
tail(data)


#summry of each variables 
summary(data)

#explore the individual variables

#1.age

hist(data$age,col="peachpuff",border="black",main ="Distribution of Age",xlab="Age" )
hist(data$age,col="peachpuff",border="black",main ="Density of Age",xlab="Age",prob =TRUE  )
lines(density(data$age),lwd=2,
      col = "chocolate3")


#box plot for age
ggplot(data, aes(y=age)) + 
  geom_boxplot(outlier.colour="red",outlier.size=1)+ theme_bw() +
  scale_fill_manual(values=c("lightgray", "#peachpuff"))+
  ggtitle("Boxplot of age")

boxplot.stats(data$age)

summary(data$age)
sd(data$age)


#2.exploring gender variable
summary(data$gender)

#pie chart for gender 
genderTable=data.frame(table(data$gender)) #creating a data frame
genderTable$pct <- round(genderTable$Freq/sum(genderTable$Freq)*100) #calculating %
lbls <- paste(genderTable$Var1, genderTable$pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(genderTable$Freq,labels=lbls,col=c("lightgray","lightyellow"),main="Pie chart of Gender")


#using patternplot
#pattern.type<-c('grid', 'dots')
#patternpie(group=genderTable$Var1,pct=genderTable$pct,label=lbls,pixel=0.3,background.color=rep("white",length(genderTable$Var1)),pattern.type=pattern.type) 


# coding gender varible female =1& male =0

# create the new column for gender -> gender_code
data$gender_code = NA
data$gender_code[data$gender =="male"] = 0
data$gender_code[data$gender == "female"]  = 1


class(data$gender_code)


#3.Explorin BMI
summary(data$bmi)

hist(data$bmi,col="peachpuff",border="black",main ="Distribution of bmi",xlab="bmi" ,prob=TRUE)
lines(density(data$bmi),lwd=2,
      col = "chocolate3")

#box plot for bmi
ggplot(data, aes(y=bmi)) + 
  geom_boxplot(outlier.colour="red",outlier.size=1)+ theme_bw() +
  scale_fill_manual(values=c("lightgray", "#peachpuff"))+
  ggtitle("Boxplot of bmi")

#identfying outliers and removing and assighn to new dataset called data_bmiclean
outliers_bmi=boxplot.stats(data$bmi)
outliers_bmi <- boxplot(data$bmi, plot=FALSE)$out
data[which(data$bmi %in% outliers_bmi),]
data_bmiclean <- data[-which(data$bmi %in% outliers_bmi),]
dim(data_bmiclean)
#box plot for bmiwithiut outliers 
ggplot(data_bmiclean, aes(y=bmi)) + 
  geom_boxplot(outlier.colour="red",outlier.size=1)+ theme_bw() +
  scale_fill_manual(values=c("lightgray", "#peachpuff"))+
  ggtitle("Boxplot of bmi-withot outliers")




#coding BMI

under_weight=(data$bmi<18.5)
under_weight=sum(under_weight[under_weight==TRUE])

normal_weight=(data$bmi>=18.5 &  data$bmi<25)
normal_weight=sum(normal_weight[normal_weight==TRUE])

over_weight=(data$bmi>=25 &  data$bmi<30)
over_weight=sum(over_weight[over_weight==TRUE])

obese=(data$bmi>=30)
obese=sum(obese[obese==TRUE])


# create the new column bmi_range
data$bmi_ranges <- NA
data$bmi_ranges[data$bmi<18.5] <- 1 #under weight
data$bmi_ranges[data$bmi>=18.5 &  data$bmi<25] <- 2 #"normal"
data$bmi_ranges[data$bmi>=25 &  data$bmi<30] <- 3 #"over"
data$bmi_ranges[data$bmi>=30] <- 4 #"obese"

head(data$bmi_ranges)



#pie chart for bmi_range 
bmi_range=data.frame(weight=c("under","normal","over","obese"),freq=c(under_weight,normal_weight,over_weight,obese))
bmi_range$pct <- round(bmi_range$freq/sum(bmi_range$freq)*100) #calculating %
lbls <- paste(bmi_range$weight, bmi_range$pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(bmi_range$freq,labels=lbls,col=c("lightgray","lightyellow","lightpink","peachpuff"),main="Pie chart of bmi_weight")


summary(data$bmi_ranges)


#4.Exploring number of kids
summary(data$num_kids)

ggplot(data, aes(factor(num_kids))) +
  geom_bar(fill = "peachpuff")

#calcualting frequency 
library(dplyr)
df_numKids <- data %>%
  group_by(num_kids) %>%
  summarise(counts = n())
df_numKids

#bar chart for number of kids 

ggplot(df_numKids, aes(x=factor(num_kids), y=counts/sum(counts)*100)) +geom_bar(fill="peachpuff",stat ="identity")+
  theme_bw() +
  ggtitle("Bar chart of number of kids")+
  xlab("number of kids coverd by insuarance")+
  ylab("Precentage(%)")+
  geom_text(aes(label = paste(round(counts/sum(counts)*100,digits = 0),"%")),vjust = -0.3)



#coding Num_of_kids

# create the new column kids_cover  yes=1  no=0
data$kids_cover <- NA
data$kids_cover[data$num_kids > 0] <- 1 #yes covering
data$kids_cover[data$num_kids == 0] <- 0 #no covering

head(data$kids_cover)
levels(data$kids_cover)
class(data$kids_cover)

#bar chart for numberkids after recoded 

#calcualting frequency 
library(dplyr)
df_numKids1 <- data %>%
  group_by(kids_cover) %>%
  summarise(counts = n())
df_numKids1

ggplot(df_numKids1, aes(x=factor(kids_cover), y=counts/sum(counts)*100)) +geom_bar(fill="peachpuff",stat ="identity")+
  theme_bw() +
  ggtitle("Bar chart of kids_covering")+
  xlab("status of kids coverd by insuarance")+
  ylab("Precentage(%)")+
  geom_text(aes(label = paste(round(counts/sum(counts)*100,digits = 0),"%")),vjust = -0.3)




#4.Exploring smoking status
levels(data$smoking_status)
summary(data$smoking_status)


# create the new column smoking_code  yes=1  no=0
data$smoking_code <- NA
data$smoking_code[data$smoking_status== "yes"] <- 1 #yes smoking
data$smoking_code[data$smoking_status== "no"] <- 0 #no smoking

head(data$smoking_code)

#pie chart for smoking status 
smokingStatusTable=data.frame(table(data$smoking_status)) #creating a data frame
smokingStatusTable$pct <- round(smokingStatusTable$Freq/sum(smokingStatusTable$Freq)*100) #calculating %
lbls <- paste(smokingStatusTable$Var1,smokingStatusTable$pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(smokingStatusTable$Freq,labels=lbls,col=c("lightgray","lightyellow"),main="Pie chart of smoking status")


#5.Exploring district
levels(data$district)
summary(data$district)


# create the new column district_code  y
data$district_code <- NA
data$district_code[data$district== "colombo"] <- 1 #colombo
data$district_code[data$district== "galle"] <- 2 #galle
data$district_code[data$district== "badulla"] <- 3 #badulla
data$district_code[data$district== "trinco"] <- 4 #trinco

head(data$district_code)




#pie chart of district
districtTable=data.frame(table(data$district)) #creating a data frame
districtTable
districtTable$pct <- round(districtTable$Freq/sum(districtTable$Freq)*100) #calculating %
lbls <- paste(districtTable$Var1, districtTable$pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(districtTable$Freq,labels=lbls,col=c("lightgray","lightyellow","peachpuff","lightpink"),main="Pie chart of district")





#exploring premium
summary(data$premium)

#histogram of premium
hist(data$premium,col="peachpuff",border="black",main ="Distribution of premium",xlab="premium" ,prob=FALSE)

hist(log(data$premium),col="peachpuff",border="black",main ="Distribution of log(premium)",xlab="log(premium)" ,prob=FALSE)



#density of premium
hist(data$premium,col="peachpuff",border="black",main ="Density of premium",xlab="premium" ,prob=TRUE)
lines(density(data$premium),lwd=2,
col = "chocolate3")

hist(log(data$premium),col="peachpuff",border="black",main ="Density of log(premium)",xlab="log(premium)" ,prob=TRUE)
lines(density(log(data$premium)),lwd=2,
      col = "chocolate3")


#box plot for premium
ggplot(data, aes(y=premium)) + 
  geom_boxplot(outlier.colour="red",outlier.size=1)+ theme_bw() +
  scale_fill_manual(values=c("lightgray", "#peachpuff"))+
  ggtitle("Boxplot of premium")


#outliers=boxplot.stats(data$premium)

#removing outliers from dataset after removing bmi outliers
outliers_premium=boxplot.stats(data$premium)
outliers <- boxplot(data1$premium, plot=FALSE)$out
data[which(data1$premium %in% outliers),]
data2 <- data[-which(data1$premium %in% outliers),]


#box plot for premium
ggplot(data2, aes(y=premium)) + 
  geom_boxplot(outlier.colour="red",outlier.size=1)+ theme_bw() +
  scale_fill_manual(values=c("lightgray", "#peachpuff"))+
  ggtitle("Boxplot of premium")

outliers <- boxplot(data2$premium, plot=FALSE)$out



#exploring multiple variables

#age Vs gender 

aggregate(age ~ gender, summary, data=data)

#densityplot for age by gender
ggplot(data, aes(x = age, y = gender)) +
  geom_density_ridges(aes(fill = gender)) +
  scale_fill_manual(values = c("lightgray", "peachpuff"))+
  ggtitle("Age ditribution with gender")


#sphereman rank correlation test gender vs age 
res_gendervage <-cor.test(data$gender_code, data$age,  method = "spearman")
res_gendervage



#age vs bmi


#group by bmi_ranges
ggplot(data, aes(x = age, y = bmi))+geom_jitter()+ggtitle("Scatter plot for Age vs bmi")

#Person correlation test bmi vs age 
res_agevbmi <-cor.test((data$age), data$bmi)
res_agevbmi



#age vs bmi_ranges

aggregate(age ~ bmi_ranges, summary, data=data)

#density plot for age by bmi ranges
ggplot(data, aes(x = age, y = bmi_ranges)) +
  geom_density_ridges(aes(fill = bmi_ranges)) +
  scale_fill_manual(values = c("lightgray", "peachpuff","lightpink","lightyellow"))+
  ggtitle("Age distribution with bmi ranges")




#sphereman rank correlation test gender vs age 
res_agevbmi_ranges <-cor.test(data$age, data$bmi_ranges,  method = "spearman")
res_agevbmi_ranges 




#age vs Smoking status 
aggregate(age ~ smoking_status, summary, data=data)


#density plot for age by smoking status
ggplot(data, aes(x = age, y = smoking_status)) +
  geom_density_ridges(aes(fill = smoking_status)) +
  scale_fill_manual(values = c("lightgray", "peachpuff","lightpink","lightyellow"))+
  ggtitle("Age distribution with smoking_status")



#sphereman rank correlation test smoking status vs age 
res_smokingvage <-cor.test(data$gender_code, data$smoking_code,  method = "spearman")
res_smokingvage



#age vs premium


#group by bmi_ranges
ggplot(data, aes(x = age, y = premium,color=bmi_ranges))+geom_jitter()+ggtitle("Scatter plot for Age vs Premium group by bmi ranges")
res_premiumvage1 <-cor.test((data$age), (data$premium))
res_premiumvage1

#group by smoking status
ggplot(data, aes(x = age, y = premium,color=smoking_status))+geom_jitter()+ggtitle("Scatter plot for Age vs Premium group by smoking status")

#transforming age and premium to log transformation
ggplot(data, aes(x =sqrt(age), y = log(premium)))+geom_jitter()+ggtitle("Scatter plot for age vs Premium")

res_premiumvage2 <-cor.test(sqrt(data$age), log(data$premium))
res_premiumvage2




#Gender vs BMi
aggregate(bmi ~ gender, summary, data=data)


#sphereman rank correlation test gender vs bmi 

res_genderVbmi <-cor.test(data$gender_code, (data$bmi),  method = "spearman")
res_genderVbmi 


#gender vs smoking status
genderVSsmoking=data.frame(table(data$gender,data$smoking_status),names("gender","smoking_status"))
genderVSsmoking

#renaming varible names using dplyr package 
genderVSsmoking=genderVSsmoking %>% 
  rename(
    gender = Var1,
    smoking_status=Var2
  )

# Grouped bar chart for gender vs smoking
ggplot(genderVSsmoking, aes(fill=smoking_status, y=Freq/sum(Freq)*100, x=gender)) + 
  geom_bar(position="dodge", stat="identity")+
theme_bw() +
  ggtitle("Gender vs Smoking status")+
  xlab("Gender")+
  ylab("Precentage(%)")+
  geom_text(aes(label = paste(round(Freq/sum(Freq)*100,digits = 0),"%")),vjust = -0.005,hjust=0.1)

#sphereman rank correlation test gender vs smokin status 

res_genderVsmoking <-chisq.test(data$gender_code, data$smoking_code)
res_genderVsmoking 




#Gender vs premium
aggregate(premium ~ gender, summary, data=data)


#boxplot for premium by gender
ggplot(data, aes(x=gender, y=premium)) + 
  geom_boxplot(outlier.colour="red",outlier.size=1)+ theme_bw() +
scale_fill_manual(values=c("lightgray", "#peachpuff"))

#sphereman rank correlation test gender vs premium 
res_gendervsprm <-cor.test(data$gender_code, data$premium,  method = "spearman")

res_gendervsprm



#bmi vs smoking status
aggregate(bmi ~ smoking_status, summary, data=data)
table(data$smoking_status,data$bmi_ranges)


ggplot(data, aes(x = bmi, y = smoking_status)) +
  geom_density_ridges(aes(fill = smoking_status)) +
  scale_fill_manual(values = c("lightgray", "peachpuff"))+
  ggtitle("bmi ditribution with smoking status")


#sphereman rank correlation test bmi vs smokin status 

res_bmiVsmoking <-cor.test(data$bmi, data$smoking_code,method = "spearman")
res_bmiVsmoking 



#bmi vs premium

ggplot(data, aes(x =(bmi), y = (premium)))+geom_jitter()+ggtitle("Scatter plot for bmi vs Premium")
ggplot(data, aes(x =log(bmi), y = log(premium),color=smoking_status))+geom_jitter()+ggtitle("Scatter plot for bmi vs Premium group by smoking status")


res_bmiVspremium1 <-cor.test((data$bmi), (data$premium))
 

#exploring relationships between multiple variables using covarience and correlation

#covariance 
cov(data[,c(2,4,5,8)])

#correlation
cor(data2[,c(2,4,5,8)])

#scatterplot matrix
cols_1 <- c("#00AFBB", "#E7B800")  
pairs(data2[,c(2,4,5,8)],pch=19, lower.panel = NULL, col = cols_1[data$gender],main="scatterplot matrix for insurance data group by gender")
 

cols_2 <- c("#E7B800","#FC4E07") 
pairs(data[,c(2,4,5,8)],pch=19, lower.panel = NULL, col = cols_2[data$smoking_status],main="scatterplots for insurance data group by smoking status")



# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  #cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, r)
}

# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}

# Create the plots
pairs(data[,c(2,4,5,8)], 
      lower.panel = panel.cor,
      upper.panel = upper.panel,main="scatter plot matrix with correlation")

#creating scatterplot using psych library
library(psych)
pairs.panels(data[,c(2,4,5,8)],  
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE ,# show correlation ellipses
             main="scatter plot matrix with correlation and histogram"
)








#fitting regression model


#fitting full model with all the variable in raw format

full.raw.model1= lm(premium ~ age+gender+bmi+num_kids+smoking_status+district,data=data)
summary(full.raw.model1)

str(summary(full.raw.model1))
summary(full.raw.model1)$r.squared

hist(full.raw.model1$residuals) #normality of the resuduals

#full.modelbbb <- lm(premium ~ .-premium, data=data)
#reduced.model <- step(full.modelbbb, direction="backward")

#resiudal plot
par(mfrow=c(1,1))

#for age
plot(data$age, full.raw.model1$residuals, ylab="Residuals",
     xlab= "age", main="Residual Plot age")
abline(0,0)

#for gender
plot(data$gender, full.raw.model1$residuals, ylab="Residuals",
     xlab= "gender", main="Residual Plot gender")
abline(0,0)

#for bmi
plot(data$bmi, full.raw.model1$residuals, ylab="Residuals",
     xlab= "bmi", main="Residual Plot bmi")
abline(0,0)



#The mean of residuals is zero
mean(full.raw.model1$residuals)


#checking equal varience 
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(full.raw.model1) # linear model


premium_resid =  data.frame(data$premium, 
                         fitted.value = fitted.values(full.raw.model1),
                         residuls = full.raw.model1$residuals)


head(premium_resid)
qqnorm(rstandard(full.raw.model1),ylab="Standarized residuals",xlab="Normal Scores", main="Q-Q plot of residuals")
qqline(rstandard(full.raw.model1))

shapiro.test(full.raw.model1$residuals)


#Cheking auto correlton 
# : Durbin-Watson test
dwtest(full.raw.model1)




#cheking multicollinearity

vif(full.raw.model1)


plot(full.raw.model1)

#to validate the out come use RMSE
RMSE1=sqrt(mean((data$premium-full.raw.model1$fitted.values)**2))

outlierTest(full.raw.model1)# Bonferonni p-value for most extreme obs

#added varible plot for check influential observation
avPlots(full.raw.model1)

# non-constant error variance test
ncvTest(full.raw.model1)


#using step function to find best model


dim(data_bmiclean)
head(data_bmiclean)
installed.packages("tab")
library(tab)
min.raw.model1= lm((premium)~ 1,data=data)#intercept only model
forward.raw.model1=step(min.raw.model1,direction = "forward",scope = (~(age)+gender+(bmi)+num_kids+smoking_status+district))
summary(forward.raw.model1)
model=forward.raw.model1

full.model <- lm(premium ~ .-premium -X, data=testdata)
reduced.model <- step(full.model, direction="backward")
summary(reduced.model)


str(summary(model))
summary(model)$r.squared

hist(model$residuals) #normality of the resuduals


#resiudal plot
par(mfrow=c(1,1))

#for age
plot(data$age, model$residuals, ylab="Residuals",
     xlab= "age", main="Residual Plot age")
abline(0,0)

#for gender
plot(data$gender, model$residuals, ylab="Residuals",
     xlab= "gender", main="Residual Plot gender")
abline(0,0)

#for bmi
plot(data$bmi, model$residuals, ylab="Residuals",
     xlab= "bmi", main="Residual Plot bmi")
abline(0,0)



#The mean of residuals is zero
mean(md$residuals)


#checking equal varience 
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(model) # linear model


premium_resid =  data.frame(data$premium, 
                            fitted.value = fitted.values(model),
                            residuls = model$residuals)


head(premium_resid)
qqnorm(rstandard(model),ylab="Standarized residuals",xlab="Normal Scores", main="Q-Q plot of residuals")
qqline(rstandard(model))

shapiro.test(model$residuals)


#Cheking auto correlton 
# : Durbin-Watson test
dwtest(model)




#cheking multicollinearity

vif(model)


plot(md)

#to validate the out come use RMSE
RMSE1=sqrt(mean((data$premium-model$fitted.values)**2))

outlierTest(model)# Bonferonni p-value for most extreme obs

#added varible plot for check influential observation
avPlots(model)

# non-constant error variance test
ncvTest(model)


#improvin model by recopding and transforming

#log(premium)

min.raw.model1= lm(log(premium)~ 1,data=data)#intercept only model
forward.raw.model1=step(min.raw.model1,direction = "forward",scope = (~(age)+(bmi)+num_kids+smoking_status+district))
summary(forward.raw.model1)
model=forward.raw.model1

hist(model$residuals) #normality of the resuduals


#resiudal plot
par(mfrow=c(1,1))

#for age
plot(data$age, model$residuals, ylab="Residuals",
     xlab= "age", main="Residual Plot age")
abline(0,0)

#for gender
plot(data$gender, model$residuals, ylab="Residuals",
     xlab= "gender", main="Residual Plot gender")
abline(0,0)

#for bmi
plot(data$bmi, model$residuals, ylab="Residuals",
     xlab= "bmi", main="Residual Plot bmi")
abline(0,0)



#The mean of residuals is zero
mean(md$residuals)


#checking equal varience 
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(model) # linear model


premium_resid =  data.frame(data$premium, 
                            fitted.value = fitted.values(model),
                            residuls = model$residuals)


head(premium_resid)
qqnorm(rstandard(model),ylab="Standarized residuals",xlab="Normal Scores", main="Q-Q plot of residuals")
qqline(rstandard(model))

shapiro.test(model$residuals)


#Cheking auto correlton 
# : Durbin-Watson test
dwtest(model)



#adding more recoded varibles

min.raw.model1= lm(log(premium)~ 1,data=data)#intercept only model
forward.raw.model1=step(min.raw.model1,direction = "forward",scope = (~sqrt(age)+as.factor(bmi_ranges)+as.factor(kids_cover)+smoking_status+district))
summary(forward.raw.model1)
model=forward.raw.model1

hist(model$residuals) #normality of the resuduals


#resiudal plot
par(mfrow=c(1,1))

#for age
plot(data$age, model$residuals, ylab="Residuals",
     xlab= "age", main="Residual Plot age")
abline(0,0)

#for gender
plot(data$gender, model$residuals, ylab="Residuals",
     xlab= "gender", main="Residual Plot gender")
abline(0,0)

#for bmi
plot(data$bmi, model$residuals, ylab="Residuals",
     xlab= "bmi", main="Residual Plot bmi")
abline(0,0)



#The mean of residuals is zero
mean(md$residuals)


#checking equal varience 
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(model) # linear model


premium_resid =  data.frame(data$premium, 
                            fitted.value = fitted.values(model),
                            residuls = model$residuals)


head(premium_resid)
qqnorm(rstandard(model),ylab="Standarized residuals",xlab="Normal Scores", main="Q-Q plot of residuals")
qqline(rstandard(model))

shapiro.test(model$residuals)



# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))  # row indices for training data
trainingData <- data[trainingRowIndex, ]  # model training data
testData  <- data[-trainingRowIndex, ]   # test data
dim(testData)
min.model= lm(log(premium)~ 1,data=trainingData)#intercept only model
rm(testdata)
forwardmodel=step(min.model,direction = "forward",scope = (~sqrt(age)+as.factor(bmi_ranges)+as.factor(kids_cover)+smoking_status+district))

summary(forwardmodel)

plot(forwardmodel)

#outlierTest(forwardmodel)

predict_premium <- predict(forwardmodel, testData)  # predict premium

length(predict_premium)

premium_resid =  data.frame((testData$premium), 
                            exp(predict_premium))
                    


head(premium_resid)

RMSE1=sqrt(mean((testData$premium-exp(predict_premium))**2))
RMSE1





#coding bmi 

min.model1= lm(trainingData$premium~ 1,data=trainingData)#intercept only model

forwardmodel1=step(min.model,direction = "forward",scope = (~age+gender_code+kids_cover+smoking_code+district_code))

summary(forwardmodel1)

plot(forwardmodel)










#k- Fold Cross validation
#library(DAAG)
#cvResults <- suppressWarnings(CVlm(df=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
#attr(cvResults, 'ms')  # => 251.2783 mean squared error



#summary (lmMod)  # model summary
#> 
#> Call:
#> lm(formula = dist ~ speed, data = trainingData)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -23.350 -10.771  -2.137   9.255  42.231 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  -22.657      7.999  -2.833  0.00735 ** 
#> speed          4.316      0.487   8.863 8.73e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 15.84 on 38 degrees of freedom
#> Multiple R-squared:  0.674,  Adjusted R-squared:  0.6654 
#> F-statistic: 78.56 on 1 and 38 DF,  p-value: 8.734e-11
AIC (lmMod)  # Calculate akaike information criterion
#> [1] 338.4489


#Assumption 1
#The regression model is linear in parameters
#Assumption 2
#The mean of residuals is zero
mod <- lm(dist ~ speed, data=cars)
mean(mod$residuals)
#=> 2.442491e-17

#Assumption 3
Homoscedasticity of residuals or equal variance

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
mod_1 <- lm(mpg ~ disp, data=mtcars)  # linear model
plot(mod_1)


par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
mod_1 <- lm(mpg ~ disp, data=mtcars)  # linear model
plot(mod_1)

#Assumption 4
No autocorrelation of residuals

# Method 1: Visualise with acf plot
library(ggplot2)
data(economics)
lmMod <- lm(pce ~ pop, data=economics)
acf(lmMod$residuals)  # highly autocorrelated from the picture.



# Method 2: Runs test to test for randomness
lawstat::runs.test(lmMod$residuals)
#=>   Runs Test - Two sided

#=> data:  lmMod$residuals
#=> Standardized Runs Statistic = -23.812, p-value < 2.2e-16


# Method 3: Durbin-Watson test
lmtest::dwtest(lmMod)
#=>   Durbin-Watson test

#=> data:  lmMod
#=> DW = 0.0021559, p-value < 2.2e-16
#=> alternative hypothesis: true autocorrelation is greater than 0



#How to rectify?
# Add lag1 of residual as an X variable to the original model. This can be conveniently done using the slide function in DataCombine package.


library(DataCombine)
econ_data <- data.frame(economics, resid_mod1=lmMod$residuals)
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
econ_data_2 <- na.omit(econ_data_1)
lmMod2 <- lm(pce ~ pop + lag1, data=econ_data_2)
acf(lmMod2$residuals)

#Assumption 5
The X variables and residuals are uncorrelated
mod.lm <- lm(dist ~ speed, data=cars)
cor.test(cars$speed, mod.lm$residuals)  # do correlation test 
#=>   Pearson's product-moment correlation
#=> 
#=> data:  cars$speed and mod.lm$residuals
#=> t = -8.1225e-17, df = 48, p-value = 1
#=> alternative hypothesis: true correlation is not equal to 0
#=> 95 percent confidence interval:
#=> -0.2783477  0.2783477
#=> sample estimates:
#=>           cor 
#=> -1.172376e-17


Assumption 6
The number of observations must be greater than number of Xs

Assumption 7

The variability in X values is positive
var(cars$speed)  
#=> [1] 27.95918

Assumption 8

The regression model is correctly specified

Assumption 9

No perfect multicollinearity

library(car)
mod2 <- lm(mpg ~ ., data=mtcars)
vif(mod2)
#   cyl      disp        hp      drat        wt      qsec        vs        am      gear      carb 
#   15.373833 21.620241  9.832037  3.374620 15.164887  7.527958  4.965873  4.648487  5.357452  7.908747 


Assumption 10

Normality of residuals








#ggdensity(data, x = "age",
 #         add = "mean", rug = TRUE,
  #        color = "district", fill = "district",
   #       palette = c("#0073C2FF", "#FC4E07","blue","red"))
