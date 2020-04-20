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
attach(det_listing)

# Features selection using knowledge gained from literature reviews

keeps <- c("host_id", "host_name", "neighbourhood_cleansed", "accommodates", "bathrooms", "bedrooms", "beds", "price","cleaning_fee",
           "minimum_nights","maximum_nights", "review_scores_rating")

listing_v <- det_listing[keeps] %>%
  rename("zip_code" = neighbourhood_cleansed) 

glimpse(listing_v)






