# Install packages
if(!require(dplyr)) install.packages("dplyr",
                                     repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", 
                                      repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                       repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", 
                                       repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", 
                                     repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", 
                                          repos = "http://cran.us.r-project.org")

# Load the libraries
library(dplyr)
library(tidyverse)
library(caret)
library(dslabs)
library(lubridate)
library(ggplot2)
library(data.table)
library(stringr)
library(readr)
library(rpart)
library(rpart.plot)

# Extract data
covid<-read_csv("https://raw.githubusercontent.com/davidsp-github/Covid19/master/WHO-COVID-19-global-data.csv",
                skip = 0,col_names = TRUE)
life_expectancy<-read_csv("https://raw.githubusercontent.com/davidsp-github/Covid19/master/Life_expectancy.csv", 
                          skip = 0,col_names = TRUE)
population<-read_csv("https://raw.githubusercontent.com/davidsp-github/Covid19/master/density.csv", 
                     skip = 0,col_names = TRUE)

# Combine and mutate data
population<- population %>% mutate(Population=Population*1000, Area_km2=Area_km2*10)
combine <- covid %>% inner_join(life_expectancy)
covid_dataset <- combine %>% inner_join(population)
rm(combine)
covid_dataset <- covid_dataset %>% mutate(ratio=Cumulative_deaths/Cumulative_cases)
covid_dataset$Country_code[is.na(covid_dataset$Country_code)]<-"NAM"


# Create test and training sets
# Test set will be 20% of Covid data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = covid_dataset$Country,
                                  times = 1, p = 0.2, list = FALSE)
train_set <- covid_dataset[-test_index,]
temp <- covid_dataset[test_index,]

# Make sure userId and movieId in validation set are also in edx set
test_set <- temp %>% 
  semi_join(train_set, by = "Country")
rm(temp)


#Group by region, mean of deaths
Africa_region<-covid_dataset %>% filter(WHO_region=="AFRO") %>% 
  group_by(Country) %>% summarise(Deaths=max(Cumulative_deaths), Ratio=max(ratio), LE=mean(Life_Expectancy), Density=mean(Density_km), Region="AFRO")
Africa_region$Country<-make.names(Africa_region$Country)
op <- par(mar=c(10,4,4,2))
barplot(Africa_region$Deaths,
        ylab="Africa Deaths",
        axisnames = TRUE,
        names.arg=Africa_region$Country,
        cex.names=0.7,
        las=2)
rm(op)
region_deaths <- data_frame(Region = "Africa", 
                           Deaths = sum(Africa_region$Deaths))


America_region<-covid_dataset %>% filter(WHO_region=="AMRO") %>% 
  group_by(Country) %>% summarise(Deaths=max(Cumulative_deaths), Ratio=max(ratio), LE=mean(Life_Expectancy), Density=mean(Density_km), Region="AMRO")
America_region$Country<-make.names(America_region$Country)
op <- par(mar=c(10,4,4,2))
barplot(America_region$Deaths,
        ylab="America Deaths",
        axisnames = TRUE,
        names.arg=America_region$Country,
        cex.names=0.7,
        las=2)
rm(op)
region_deaths <- bind_rows(region_deaths,
                          data_frame(Region = "America", 
                                     Deaths = sum(America_region$Deaths)))

Eastern_Mediterranean_region<-covid_dataset %>% filter(WHO_region=="EMRO") %>% 
  group_by(Country) %>% summarise(Deaths=max(Cumulative_deaths), Ratio=max(ratio), LE=mean(Life_Expectancy), Density=mean(Density_km), Region="EMRO")
Eastern_Mediterranean_region$Country<-make.names(Eastern_Mediterranean_region$Country)
op <- par(mar=c(11,4,4,2))
barplot(Eastern_Mediterranean_region$Deaths,
        log="y",
        ylab="Eastern Mediterranean Deaths",
        axisnames = TRUE,
        names.arg=Eastern_Mediterranean_region$Country,
        las=2)
rm(op)
region_deaths <- bind_rows(region_deaths,
                           data_frame(Region = "East Mediterranean", 
                                      Deaths = sum(Eastern_Mediterranean_region$Deaths)))

Europe_region<-covid_dataset %>% filter(WHO_region=="EURO") %>% 
  group_by(Country) %>% summarise(Deaths=max(Cumulative_deaths), Ratio=max(ratio), LE=mean(Life_Expectancy), Density=mean(Density_km), Region="EURO")
Europe_region$Country<-make.names(Europe_region$Country)
op <- par(mar=c(11,4,4,2))
barplot(Europe_region$Deaths,
        log="y",
        ylab="Europe Deaths",
        axisnames = TRUE,
        names.arg=Europe_region$Country,
        cex.names=0.7,
        las=2)
rm(op)
region_deaths <- bind_rows(region_deaths,
                           data_frame(Region = "Europe", 
                                      Deaths = sum(Europe_region$Deaths)))

South_East_Asia_region<-covid_dataset %>% filter(WHO_region=="SEARO") %>% 
  group_by(Country) %>% summarise(Deaths=max(Cumulative_deaths), Ratio=max(ratio), LE=mean(Life_Expectancy), Density=mean(Density_km), Region="SEARO")
op <- par(mar=c(11,4,4,2))
barplot(South_East_Asia_region$Deaths,
        ylab="South East Asia Deaths",
        axisnames = TRUE,
        names.arg=South_East_Asia_region$Country,
        las=2)
rm(op)
region_deaths <- bind_rows(region_deaths,
                           data_frame(Region = "South East Asia", 
                                      Deaths = sum(South_East_Asia_region$Deaths)))

Western_Pacific_region<-covid_dataset %>% filter(WHO_region=="WPRO") %>% 
  group_by(Country) %>% summarise(Deaths=max(Cumulative_deaths), Ratio=max(ratio), LE=mean(Life_Expectancy), Density=mean(Density_km), Region="WPRO")
op <- par(mar=c(11,4,4,2))
barplot(Western_Pacific_region$Deaths,
        ylab="Western Pacific Deaths",
        axisnames = TRUE,
        names.arg=Western_Pacific_region$Country,
        las=2)
rm(op)
region_deaths <- bind_rows(region_deaths,
                           data_frame(Region = "West Pacific", 
                                      Deaths = sum(Western_Pacific_region$Deaths)))

region_deaths <- bind_rows(region_deaths,
                           data_frame(Region = "TOTAL", 
                                      Deaths = sum(region_deaths$Deaths)))
region_deaths %>% knitr::kable()


# Top 10 highest/lowest Life_Expectancy/Density_km
top_n(life_expectancy,10,Life_Expectancy) %>% arrange(-Life_Expectancy)
top_n(life_expectancy,-10,Life_Expectancy) %>% arrange(Life_Expectancy)
top_n(population,10,Density_km) %>% arrange(-Density_km)
top_n(population,-10,Density_km) %>% arrange(Density_km)


# Plot new deaths & cumulative deaths
plot(covid_dataset$Date_reported,covid_dataset$Cumulative_deaths,main="World cumulative deaths",
     xlab="Date (2020)", ylab="Cumulative deaths")
plot(covid_dataset$Date_reported,covid_dataset$New_deaths,main="World new deaths",
     xlab="Date (2020)", ylab="New deaths")

plot(covid_dataset$Date_reported,covid_dataset$Cumulative_cases,main="World cumulative cases",
     xlab="Date (2020)", ylab="Cumulative cases")
plot(covid_dataset$Date_reported,covid_dataset$New_cases,main="World new cases",
     xlab="Date (2020)", ylab="New cases")

# Countries
unique(covid_dataset$Country)

# Plot new deaths & cumulative deaths for different countries
Spain<-filter(covid_dataset, Country=="Spain")
Spain$Colour="black"
Spain$Colour[Spain$New_deaths>=1000]="red"
Spain$Colour[Spain$New_deaths<0]="purple"
plot(Spain$Date_reported,Spain$Cumulative_deaths,main="Spain cumulative deaths",
     xlab="Date (2020)", ylab="Cumulative deaths",pch = 16)
plot(Spain$Date_reported,Spain$New_deaths,main="Spain new deaths",
     xlab="Date (2020)", ylab="New deaths",col=Spain$Colour,pch = 16)
plot(Spain$Date_reported,Spain$Cumulative_cases,main="Spain cumulative cases",
     xlab="Date (2020)", ylab="Cumulative cases",pch = 16)
plot(Spain$Date_reported,Spain$New_cases,main="Spain new cases",
     xlab="Date (2020)", ylab="New cases",pch = 16)


China<-filter(covid_dataset, Country=="China")
China$Colour="black"
China$Colour2="black"
China$Colour[China$New_deaths>=1000]="red"
China$Colour2[China$New_cases>=10000]="red"
plot(China$Date_reported,China$Cumulative_deaths,main="China cumulative deaths",
     xlab="Date (2020)", ylab="Cumulative deaths",pch = 16)
plot(China$Date_reported,China$New_deaths,main="China new deaths",
     xlab="Date (2020)", ylab="New deaths",col=China$Colour,pch = 16)
plot(China$Date_reported,China$Cumulative_cases,main="China cumulative cases",
     xlab="Date (2020)", ylab="Cumulative cases",pch = 16)
plot(China$Date_reported,China$New_cases,main="China new cases",
     xlab="Date (2020)", ylab="New cases",col=China$Colour2,pch = 16)


USA<-filter(covid_dataset, Country=="United States of America")
USA$Colour="black"
USA$Colour[USA$New_deaths<0]="purple"
plot(USA$Date_reported,USA$Cumulative_deaths,main="USA cumulative deaths",
     xlab="Date (2020)", ylab="Cumulative deaths",pch = 16)
plot(USA$Date_reported,USA$New_deaths,main="USA new deaths",
     xlab="Date (2020)", ylab="New deaths",col=USA$Colour,pch = 16)
plot(USA$Date_reported,USA$Cumulative_cases,main="USA cumulative cases",
     xlab="Date (2020)", ylab="Cumulative cases",pch = 16)
plot(USA$Date_reported,USA$New_cases,main="USA new cases",
     xlab="Date (2020)", ylab="New cases",pch = 16)


# Regions study
covid_summary<-Africa_region %>%
  full_join(America_region) %>%
  full_join(Eastern_Mediterranean_region) %>%
  full_join(Europe_region) %>%
  full_join(South_East_Asia_region) %>%
  full_join(Western_Pacific_region)

# All variables boxplot
cov <- subset(covid_summary, select = -Country )
cov %>% gather(Variables,percentage, -Region) %>%
  ggplot(aes(Region, percentage, fill = Region)) +
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free") +
  theme(axis.text.x = element_blank())

# All variables boxplot without outliers
cov %>% gather(Variables,percentage, -Region) %>%
  ggplot(aes(Region, percentage, fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~Variables, scales = "free") +
  theme(axis.text.x = element_blank())

# Deaths boxplot without outliers
cov %>% 
  ggplot(aes(Region, Deaths, fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 200)) +
  theme(axis.text.x = element_blank())

# Density boxplot without outliers
cov %>% 
  ggplot(aes(Region, Density, fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 200)) +
  theme(axis.text.x = element_blank())

# Ratio boxplot without outliers
cov %>% 
  ggplot(aes(Region, Ratio, fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 0.2)) +
  theme(axis.text.x = element_blank())

# Life expectancy boxplot without outliers
cov %>% 
  ggplot(aes(Region, LE, fill = Region)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(60, 90)) +
  theme(axis.text.x = element_blank())

# Deaths-LE plot
cov %>% 
  ggplot(aes(Deaths,LE, color = Region)) + 
  geom_point()+ 
  geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)+
  scale_x_continuous(limits = c(0, 50000))

# Deaths-LE plot (zoom)
cov %>% 
  ggplot(aes(Deaths,LE, color = Region)) + 
  geom_point()+ 
  geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)+
  scale_x_continuous(limits = c(0, 1000))

# Modified train and test sets
train_set<-train_set%>%mutate(cases_new=1*(New_cases>0))
train_set$cases_new<-as.factor(train_set$cases_new)
train_set_m<-train_set%>%select(-New_cases)%>%select(-Female_LE)%>%
  select(-Male_LE)%>%select(-Density_Mi)%>%select(-Population)%>%select(-Area_km2)%>%
  select(-ratio)
test_set<-test_set%>%mutate(cases_new=1*(New_cases>0))
test_set$cases_new<-as.factor(test_set$cases_new)
test_set_m<-test_set%>%select(-New_cases)%>%select(-Female_LE)%>%
  select(-Male_LE)%>%select(-Density_Mi)%>%select(-Population)%>%select(-Area_km2)%>%
  select(-ratio)

# LDA method
set.seed(1, sample.kind = "Rounding")
train_lda <- train(cases_new ~ ., method = "lda", data = train_set_m)
lda_preds <- predict(train_lda, test_set_m)
mean(lda_preds == test_set_m$cases_new)
results <- data_frame(Method = "LDA", 
                      Accuracy = lda_ac)

# QDA method
set.seed(1, sample.kind = "Rounding")
train_qda <- train(cases_new ~ Life_Expectancy, method = "qda", data = train_set_m)
qda_preds <- predict(train_qda, test_set_m)
mean(qda_preds == test_set_m$cases_new)
results <- bind_rows(results,
                     data_frame(Method = "QDA", 
                                Accuracy = qda_ac))

# GLM method
set.seed(1, sample.kind = "Rounding")
train_glm <- train(cases_new ~ Life_Expectancy, method = "glm", data = train_set_m)
glm_preds <- predict(train_glm, test_set_m)
mean(glm_preds == test_set_m$cases_new)
results <- bind_rows(results,
                     data_frame(Method = "GLM", 
                                Accuracy = glm_ac))

# KNN method  
set.seed(1, sample.kind = "Rounding")
train_knn <- train(cases_new ~ Density_km,
                   method = "knn",
                   data = train_set_m,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune
max(train_knn$results$Accuracy)
knn_preds <- predict(train_knn, test_set_m)
mean(knn_preds == test_set_m$cases_new)
results <- bind_rows(results,
                     data_frame(Method = "KNN", 
                                Accuracy = knn_ac))

# Decision trees method
set.seed(1, sample.kind = "Rounding")
train_rpart <- train(cases_new ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set_m)
train_rpart$bestTune
rpart_preds <- predict(train_rpart, test_set_m)
rpart_ac<-mean(rpart_preds == test_set_m$cases_new)
results <- bind_rows(results,
                     data_frame(Method = "Decision trees", 
                                Accuracy = rpart_ac))
rpart.plot(train_rpart$finalModel, type=0,fallen.leaves=FALSE,
           clip.facs=TRUE, shadow.col="gray", nn=TRUE, cex=0.5, extra=0,tweak=1.2)

# RMSE function  
RMSE<-function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
  }

maxi<-max(covid_dataset$ratio)
train_set_reg<-train_set %>% mutate(ratio2= (round((ratio/(max(covid_dataset$ratio))*100))/0.5)*0.5)
test_set_reg<-test_set %>% mutate(ratio2= (round((ratio/(max(covid_dataset$ratio))*100))/0.5)*0.5)
  
# Regularization method
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$ratio)
  b_c <- train_set %>%
    group_by(Country) %>%
    summarize(b_c = sum(ratio - mu)/(n()+l))
  b_d <- train_set %>% 
    left_join(b_c, by="Country") %>%
    group_by(Date_reported) %>%
    summarize(b_d = sum(ratio - b_c - mu)/(n()+l))
  predicted_data <- 
    test_set %>% 
    left_join(b_c, by = "Country") %>%
    left_join(b_d, by = "Date_reported") %>%
    mutate(pred = mu + b_c + b_d)
  return(RMSE(predicted_data$pred, test_set$ratio))
})
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda

model_rmse<-min(rmses)

mu <- mean(train_set$ratio)
b_c <- train_set %>%
  group_by(Country) %>%
  summarize(b_c = sum(ratio - mu)/(n()+lambda))
b_d <- train_set %>% 
  left_join(b_c, by="Country") %>%
  group_by(Date_reported) %>%
  summarize(b_d = sum(ratio - b_c - mu)/(n()+lambda))
predicted_data <- 
  test_set %>% 
  left_join(b_c, by = "Country") %>%
  left_join(b_d, by = "Date_reported") %>%
  mutate(pred = mu + b_c + b_d)
RMSE(predicted_data$pred, test_set$ratio)
results <- bind_rows(results,
                     data_frame(Method = "Regularization", 
                                RMSE = model_rmse))

# Results summary
results %>% knitr::kable()