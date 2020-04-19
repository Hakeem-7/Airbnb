# let's gooo
library(dplyr)
library(tidyverse)
library(DT)

?heatmap

listing <- as_tibble(read.csv("files/listings.csv"))
head(listing)
listing$latitude <- as.integer(listing$latitude)

head(listing)

max(listing$price)

# Price range of each neighbourhood
library("d3heatmap")
price_var <- listing %>%
  group_by(neighbourhood) %>%
  summarise_each(funs(min(.,na.rm = T), max(., na.rm = T)),
                 matches("pric")) %>% rename("price_min" = min, "price_max" = max) %>%
  arrange(desc(price_max))%>%
  DT::datatable()

price_var

price_var1 <- as.matrix(price_var)
heatmap(price_var1)
