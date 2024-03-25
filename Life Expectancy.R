#Final Project
#setup
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("leaps")
install.packages("car")
install.packages("Metrics")
install.packages("reshape2")
install.packages("ggpubr")
install.packages("moments")

library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(leaps)
library(car)
library(Metrics)
library(reshape2)
library(ggpubr)
library(moments)

set_plot_dimensions <- function(width_choice , height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}

#load data
data <- read.csv("C:/Users/xy/Desktop/2023Fall/R/Final/Life Expectancy Data.csv")
head(data)
sprintf("Dataset size: [%s]", toString(dim(data)))

#clean and filter data
##remove unnecessary variables
data <- subset(data,select=-c(Country,Year))

#Original Analysis
#original correlation matrix
set_plot_dimensions(16,10)
corr <- round(cor(subset(data,select=-c(Status)),use = "complete.obs"),3)
ggcorrplot(corr,type='upper',lab=T,outline.color='black',lab_size=4,legend.title='Correlation')+
  ggtitle('Correlation Matrix')
#original model
set_plot_dimensions(16,10)
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.70,0.30))
train <- data[sample, ]
x.test <-data[!sample, ]
y.test <- data[!sample, ]$Life.expectancy
model.original<- lm(Life.expectancy~., data = train)
summary(model.original)


##missing data
missing.rows=dim(data)[1]-dim(na.omit(data))[1]
sprintf("Dataset size: [%s]",toString(dim(data)))
round((missing.rows*100)/dim(data)[1],2)
missing_df <- data.frame(type=c("missing","non-missing"),count=c(missing.rows,dim(na.omit(data))[1]))

set_plot_dimensions(16,4)
ggplot(missing_df,aes(fill=type,y="",x=count))+geom_bar(position="stack",stat="identity")+ggtitle("Missing vs Non-missing row counts")+xlab("Missing count")+ylab("")+theme(text=element_text(size=18))+scale_fill_brewer(palette="Set1")
missing_counts <- data.frame(feature = factor(names(data)),counts=sapply(data, function(x) sum(is.na(x))))
set_plot_dimensions(16,8)
ggplot(missing_counts,aes(x=reorder(feature, -counts), y=counts, fill=counts)) +
  geom_bar(stat="identity") +
  ggtitle("Missing counts in each feature") +
  xlab("Feature") + ylab("Missing count") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_continuous(trans = 'reverse')

###check outliers
set_plot_dimensions(20,10)
par(mfrow=c(2,7))
boxplot(data$Life.expectancy,
        ylab = "Life Expectancy",
        main = "Boxplot of Life Expectancy",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Adult.Mortality,
        ylab = "Adult Mortality",
        main = "Boxplot of Adult Mortality",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Alcohol,
        ylab = "Alcohol",
        main = "Boxplot of Alcohol",
        col= "#008080",
        outcol="#008080")
boxplot(data$Hepatitis.B,
        ylab = "Hepatitis B",
        main = "Boxplot of Hepatitis B",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$BMI,
        ylab = "BMI",
        main = "Boxplot of BMI",
        col= "#008080",
        outcol="#008080")
boxplot(data$Polio,
        ylab = "Polio",
        main = "Boxplot of Polio",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Total.expenditure,
        ylab = "Total Expenditure",
        main = "Boxplot of Total Expenditure",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Diphtheria,
        ylab = "Diphteria",
        main = "Boxplot of Diphteria",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$GDP,
        ylab = "GDP",
        main = "Boxplot of GDP",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Population,
        ylab = "Population",
        main = "Boxplot of Population",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$thinness..1.19.years,
        ylab = "Thinness 1-19 years",
        main = "Boxplot of Thinness for 1-19 years old",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$thinness.5.9.years,
        ylab = "Thinness 5-9 years",
        main = "Boxplot of Thinness for 5-9 years old",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(data$Income.composition.of.resources,
        ylab = "Income Composition",
        main = "Boxplot of Income Composition",
        col= "#008080",
        outcol="#008080")
boxplot(data$Schooling,
        ylab = "Schooling",
        main = "Boxplot of Schooling",
        col= "#FF6666",
        outcol="#FF6666")

###Apply Imputation
####calculate median for high outliers variables
Life.expectancy_median <- median(data$Life.expectancy,na.rm=T)
Adult.Mortality_median <- median(data$Adult.Mortality,na.rm=T)
Hepatitis.B_median <- median(data$Hepatitis.B,na.rm=T)
Polio_median <- median(data$Polio,na.rm=T)
Diphtheria_median <- median(data$Diphtheria,na.rm=T)
Total.expenditure_median <- median(data$Total.expenditure,na.rm=T)
GDP_median <- median(data$GDP,na.rm=T)
Population_median <- median(data$Population,na.rm=T)
thinness..1.19.years_median <- median(data$thinness..1.19.years,na.rm=T)
thiness.5.9.years_median <- median(data$thinness.5.9.years,na.rm=T)
Schooling_median <- median(data$Schooling,na.rm=T)
####calculate mean for the low or none outliers variables
Alcohol_mean <- mean(data$Measles,na.rm=T)
BMI_mean <- mean(data$BMI,na.rm=T)
Income.composition.of.resources_mean <- mean(data$Income.composition.of.resources,na.rm=T)
####replace the NAs with mean and median values respectively
####medians
data$Life.expectancy[is.na(data$Life.expectancy)] <- Life.expectancy_median
data$Adult.Mortality[is.na(data$Adult.Mortality)] <- Adult.Mortality_median
data$Hepatitis.B[is.na(data$Hepatitis.B)] <- Hepatitis.B_median
data$Polio[is.na(data$Polio)] <- Polio_median
data$Diphtheria[is.na(data$Diphtheria)] <- Diphtheria_median
data$Total.expenditure[is.na(data$Total.expenditure)] <- Total.expenditure_median
data$GDP[is.na(data$GDP)] <- GDP_median
data$Population[is.na(data$Population)] <- Population_median
data$thinness..1.19.years[is.na(data$thinness..1.19.years)] <- thinness..1.19.years_median
data$thinness.5.9.years[is.na(data$thinness.5.9.years)] <- thiness.5.9.years_median
data$Schooling[is.na(data$Schooling)] <- Schooling_median
####means
data$Alcohol[is.na(data$Alcohol)] <- Alcohol_mean
data$BMI[is.na(data$BMI)] <- BMI_mean
data$Income.composition.of.resources[is.na(data$Income.composition.of.resources)] <- Income.composition.of.resources_mean

##factorize categorical variables
data$Status <- as.factor(data$Status)

##data transformation
copy_data <- data
####density of response
set_plot_dimensions(10,8)
ggplot(data,aes(x=Life.expectancy)) +
  geom_density(alpha=.3,fill='red',color='red',size=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)),size=1)+
  ggtitle('Original Distribution density of Life.expectancy')+
  theme(text=element_text(size=18))
sprintf('Skewness: [%s]',toString(skewness(data$Life.expectancy,na.rm=T)))
####transform response
data$Life.expectancy <- sqrt(max(data$Life.expectancy+1)-data$Life.expectancy)
data$Life.expectancy <- scale(data$Life.expectancy,scale=T,center=T)
ggplot(data,aes(x=Life.expectancy)) +
  geom_density(alpha=.3,fill='red',color='red',size=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)))+
  ggtitle('Transformed Distribution density of Life.expectancy')+
  theme(text=element_text(size=18))
sprintf('Skewness:[%s]',toString(skewness(data$Life.expectancy,na.rm=T)))

###scale the continuous variables
data <- as.data.frame(scale(subset(data,select=-c(Status)),scale=T,center=T))
data$Status <- copy_data$Status
data_FULL <- data

##EDA
###Categorical variables
set_plot_dimensions(10,8)
ggplot(copy_data,aes(x=Status,y=Life.expectancy,fill=Status))+
  geom_boxplot()+
  ggtitle('Life expectancy per country Status')+
  theme(text=element_text(size=18))+
  scale_fill_brewer(palette='Set1')

###Continuous variables
####correlation and collinearity
set_plot_dimensions(16,10)
corr <- round(cor(subset(data,select=-c(Status))),3)
ggcorrplot(corr,type='upper',lab=T,outline.color='black',lab_size=4,legend.title='Correlation')+
  ggtitle('Correlation Matrix')
####VIF
mod.linear <- lm(Life.expectancy~ .,data=subset(data,select=-c(Status)))
vifs <- data.frame(vif(mod.linear))
set_plot_dimensions(16,8)
ggplot(vifs,aes(y=vif.mod.linear.,x=row.names(vifs)))+
  geom_bar(aes(fill=vif.mod.linear.>5),stat='identity')+
  scale_y_continuous(trans='sqrt',breaks=c(5,10,50,100))+
  geom_hline(yintercept=5,colour='red')+
  ggtitle('VIF per feature')+
  theme(axis.text.x=element_text(angle=20,hjust=1))+
  theme(text=element_text(size=18))+
  scale_fill_brewer(palette='Dark2')
####delete high correlated variables
data_EDA <- subset(data,select=-c(infant.deaths))
data_EDA <- subset(data_EDA,select=-c(GDP))
data_EDA <- subset(data_EDA,select=-c(thinness..1.19.years))
####check the new correlation matrix and VIF after eliminations
set_plot_dimensions(16,10)
corr <- round(cor(subset(data_EDA,select=-c(Status))),3)
ggcorrplot(corr,type='upper',lab=T,outline.color='black',lab_size=4,legend.title='Correlation')+
  ggtitle('New Correlation Matrix')

r <- lm(Life.expectancy~ .,data=subset(data_EDA,select=-c(Status)))
vifs <- data.frame(vif(mod.linear))
set_plot_dimensions(16,8)
ggplot(vifs,aes(y=vif.mod.linear.,x=row.names(vifs)))+
  geom_bar(aes(fill=vif.mod.linear.>5),stat='identity')+
  scale_y_continuous(trans='sqrt',breaks=c(5,10,50,100))+
  geom_hline(yintercept=5,colour='red')+
  ggtitle('VIF per feature')+
  theme(axis.text.x=element_text(angle=20,hjust=1))+
  theme(text=element_text(size=18))+
  scale_fill_brewer(palette='Dark2')

##Feature Selection methods
###Best subset
regfit.best <- regsubsets(Life.expectancy~., data= data_EDA, nvmax = 16)
reg.summary <- summary(regfit.best)
par(mfrow=c(2,2))
####residual sum of squares:
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
which.min(reg.summary$rss)
points(16,reg.summary$rss[16], col="red",cex=2,pch=20)
####adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
which.max(reg.summary$adjr2)
points(15,reg.summary$adjr2[15], col="red",cex=2,pch=20)
####BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(12,reg.summary$bic[12],col="red",cex=2,pch=20)

###forward inclusion
par(mfrow=c(1,1))
regfit.fwd <- regsubsets(Life.expectancy~.,data=data_EDA,nvmax=16,method='forward')
fwd.summary <- summary(regfit.fwd)
set_plot_dimensions(8,6)
plot(fwd.summary$bic,xlab='Number of Variables',ylab='BIC',type='l')
which.min(fwd.summary$bic)
points(12,fwd.summary$bic[12],col='red',cex=2,pch=20)

###backward elimination
regfit.bwd <- regsubsets(Life.expectancy~.,data=data_EDA,nvmax=16,method='backward')
bwd.summary <- summary(regfit.bwd)
set_plot_dimensions(8,6)
plot(bwd.summary$bic,xlab='Number of Variables',ylab='BIC',type='l')
which.min(bwd.summary$bic)
points(12,bwd.summary$bic[12],col='red',cex=2,pch=20)

v_names <- rownames(as.data.frame(coef(regfit.best,12)))
coefs<- data.frame(v_names)
coefs$best_coef_value <- coef(regfit.best,12)
coefs$fwd_coef_value <-  coef(regfit.fwd,12)
coefs$bwd_coef_value <-  coef(regfit.bwd,12)
set_plot_dimensions(18,4)
ggplot(coefs,
       aes(x=v_names, y=best_coef_value, fill=best_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Features & coeffecients: [method Best]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))
ggplot(coefs,
       aes(x=v_names, y=fwd_coef_value, fill=fwd_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Features & coeffecients: [method Forward inclusion]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))
ggplot(coefs,
       aes(x=v_names, y=bwd_coef_value, fill=bwd_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Feature & coeffecients: [method Backward elimination]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))

data_FS <- subset(data_EDA, select=c(Life.expectancy,Status, Adult.Mortality, percentage.expenditure,Hepatitis.B, Polio, BMI, thinness.5.9.years, Measles,Diphtheria,HIV.AIDS,Income.composition.of.resources,Schooling))

#Models
##Full Model
set_plot_dimensions(16,10)
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data_FULL), replace=TRUE, prob=c(0.70,0.30))
train <- data_FULL[sample, ]
x.test <-data_FULL[!sample, ]
y.test <- data_FULL[!sample, ]$Life.expectancy
model.full<- lm(Life.expectancy~., data = train)
summary(model.full)
##evaluation
pred <- predict(model.full, newdata=x.test)
rmse(pred,y.test)
summary(model.full)$adj.r.squared
par(mfrow=c(2,2))
plot(model.full)

##EDA Model
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data_EDA), replace=TRUE, prob=c(0.70,0.30))
train <- data_EDA[sample, ]
x.test <-data_EDA[!sample, ]
y.test <- data_EDA[!sample, ]$Life.expectancy
model.EDA <- lm(Life.expectancy~., data = train)
summary(model.EDA)
###evaluation
pred <- predict(model.EDA, newdata=x.test)
rmse(pred,y.test)
summary(model.EDA)$adj.r.squared
par(mfrow=c(2,2))
plot(model.EDA)

##FS Model
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data_FS), replace=TRUE, prob=c(0.70,0.30))
train <- data_FS[sample, ]
x.test <-data_FS[!sample, ]
y.test <- data_FS[!sample, ]$Life.expectancy
model.FS <- lm(Life.expectancy~., data = train)
summary(model.FS)
###evaluation
pred <- predict(model.FS, newdata=x.test)
rmse(pred,y.test)
summary(model.FS)$adj.r.squared
par(mfrow=c(2,2))
plot(model.FS)