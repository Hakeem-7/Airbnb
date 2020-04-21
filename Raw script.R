library(dplyr)
library(tidyverse)
library(DT)
library("d3heatmap")
?heatmap

listing <- as_tibble(read.csv("files/listings.csv"))
head(listing)

dim(listing)

min(listing$price) #zero listing price should be eliminated - possible missing data

# convert 0's to NA
listing[, "price"][listing[, "price"] == 0] <- NA

is.na(listing) %>% sum() # 21 percent of the price entry is "zero"
dim(listing)

# Rental price range of each neighbourhood

price_var <- listing %>%
  group_by(neighbourhood) %>%
  summarise_each(funs(min(.,na.rm = T), max(., na.rm = T)),
                 matches("pric")) %>% rename("price_min" = min, "price_max" = max) %>%
  arrange(desc(price_max))%>%
  DT::datatable()

price_var

price_var1 <- as.matrix(price_var)
heatmap(price_var1)

# average rental price in each neighbourhood
options(digits = 4)
price_avg <- listing %>%
  group_by(neighbourhood) %>%
  summarise_each(funs(mean),matches("pric")) %>% 
  rename("Average_price" = price) %>%
  arrange(desc(Average_price)) %>%
  mutate_if(is.numeric, format, 2) %>%
  DT::datatable()

price_avg



#------------------------------------------------#
          #Detailed Listing#
#------------------------------------------------#

#pre-processing

# Replaced rentals with no cleaning fee with $0.
# Rentals with no review score rating were assigned a score of 99


det_listing <- read.csv("files/Detailed_listings.csv", header = TRUE)

glimpse(det_listing)

# Features selection using knowledge gained from literature reviews

keeps <- c("host_id", "host_name", "neighbourhood_cleansed", "accommodates", "bathrooms", "bedrooms", "beds", "price","cleaning_fee",
           "minimum_nights","maximum_nights", "review_scores_rating")

listing_v <- det_listing[keeps] %>%
  rename("zip_code" = neighbourhood_cleansed)%>%
  select(-price, price) # Push price variable to the back

glimpse(listing_v)
dim(listing_v)

class_variable <- function(data){
  sapply(data, function(x) class(x))
}

listing_v$zip_code <- as.factor(listing_v$zip_code)
class_variable(listing_v)

# 0.8% of the data is missing data
sum(is.na(listing_v))

# remove missing data
listing_v <- na.omit(listing_v)

# Data Visualization
library(ggcorrplot)
library(corrplot)
ggcorrplot(cor(listing_v[,-c(1,2,3)]))
corrplot(cor(listing_v[,-c(1,2,3)]))


# Feature selection using lasso regression

keeps1 <- c("accommodates", "bathrooms", "bedrooms", "beds", "price","cleaning_fee",
           "minimum_nights","maximum_nights", "review_scores_rating")

listing_v1 <- det_listing[keeps1] %>%
  select(price, everything()) #Move the variable "price" to the first column

sum(is.na(listing_v1))

listing_v1 <- na.omit(listing_v1)

head(listing_v1)
dim(listing_v1)

# Lasso Regression

library(glmnet)

x <- model.matrix(price~., listing_v1)[,-1]
y <- listing_v1$price

# Sample splitting
set.seed(7)

train.r<-sample(1:nrow(x), 0.7*nrow(x))
test.r<-(-train.r)
y.test<-y[test.r]

# Generate the model

grid<- 10^seq(6,-2,length = 100)

lasso <- glmnet(x[train.r,], y[train.r], alpha = 1, lambda = grid) #alpha = 1 is lasso
plot(lasso)

set.seed(7)
cv.out<-cv.glmnet(x[train.r, ],y[train.r],alpha = 1)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam
lasso.pred<-predict(lasso, s=0, newx = x[test.r,])
#test MSE is 2,164,444. when s = 0 (i.e., least regression), the MSE is 2,165,929, which is
#significantly higher than the MSE of the Lasso regression
mean((lasso.pred - y.test)^2) 

lasso.out<-glmnet(x,y,alpha = 1,lambda = grid)
lasso.coef<-predict(lasso.out, type = "coefficients", s=bestlam)[2:9,]
lasso.coef
# None of the features was eliminated. Thus, all features are relevant for modelling.
lasso.coef[lasso.coef!=0]














library(neuralnet)

?neuralnet










