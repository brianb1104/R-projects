library(tidyverse)
library(reshape2)
housing = read.csv(housing.csv)
head(housing)
summary(housing)
par(mfrow=c(2,5))
colnames(housing)
ggplot(data = melt(housing), mapping = aes(x = value)) +
    geom_histogram(bins = 30) + facet_wrap(~variable, scales ='free_x')
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms
, na.rm = TRUE)
housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households
drops =c('total_bedrooms', 'total_rooms')
housing = housing[ , !(names(housing) %in% drops)]
head(housing)
categories = unique(housing$ocean_proximity)
#split the categories off
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)
for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}
head(cat_housing) #see the new columns on the right
for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}
head(cat_housing)
cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))

tail(cat_housing)
colnames(housing)
drops = c('ocean_proximity', 'median_house_value')
housing_num = housing[ , !(names(housing) %in% drops)]
head(housing_num)
scaled_housing_num = scale(housing_num)
head(scaled_housing_num)
cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$
median_house_value)
head(cleaned_housing)
set.seed(1738) # Set a random seed so that same sample can be reproduced in future runs

sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test  = cleaned_housing[-sample, ] #everything but the samples
head(train)
nrow(train) + nrow(test) == nrow(cleaned_housing)
library('boot')
?cv.glm # note the K option for K fold cross validation
glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)
k_fold_cv_error$delta
glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse #off by about $83,000... it is a start
names(glm_house) #what parts of the model are callable?
glm_house$coefficients
library('randomForest')
