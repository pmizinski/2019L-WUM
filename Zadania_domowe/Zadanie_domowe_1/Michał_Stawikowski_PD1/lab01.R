library(readr)
library(dplyr)
library(vioplot)
library(ggplot2)
library(scales)
library(devtools)
library(autoEDA)
library(visdat)
library(dataMaid)
library(rmarkdown)

options(stringsAsFactors = FALSE)
api <- read_csv("allegro-api-transactions.csv")
txt <- read.delim("allegro-categories-eng.txt",sep=";")
colnames(txt)[1] <- "main_category"
final <- full_join(api, txt)
colnames(final)

summary(final$price)
summary(final$it_is_allegro_standard)
summary(final$it_is_brand_zone)
summary(final$it_seller_rating)
summary(final$date)
summary(final$pay_option_on_delivery)
summary(final$pay_option_transfer)

final$date <- as.POSIXct(final$date, format = "%H:%M:%S")
#Prize
boxplot(final$price)
ograniczonaCena <- final$price[final$price<1000]
boxplot(ograniczonaCena)
vioplot(ograniczonaCena)

#Seller rating
boxplot(final$it_seller_rating)

#Allegro Standard
Yes <- sum(na.omit(final$it_is_allegro_standard))
Noo <- length(na.omit(final$it_is_allegro_standard)) - Yes
Liczba <- c(Yes,Noo)
BrandZone <- c("Yes", "No")
Standard <- data.frame(Liczba,BrandZone)

ggplot(data=Standard, aes(x=BrandZone, y=Liczba)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Number of orders") + ggtitle("Number of orders by Allegro Standard")

#BrandZone

#Allegro Standard
Yes <- sum(na.omit(final$it_is_brand_zone))
Noo <- length(na.omit(final$it_is_brand_zone)) - Yes
Liczba <- c(Yes,Noo)
BrandZone <- c("Yes", "No")
Standard <- data.frame(Liczba,BrandZone)

ggplot(data=Standard, aes(x=BrandZone, y=Liczba)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Number of orders") + ggtitle("Number of orders by Brand Zone")

#Seller Rating

ggplot(final,aes(x=final$it_seller_rating)) + geom_histogram(bins = 100,fill="steelblue",alpha=0.9) +
  scale_x_continuous(labels = comma) +
  xlab("User rating") + ylab("Number of orders") + ggtitle("Number of orders by user rating")

#Date

ggplot(final, aes(x=as.POSIXct(final$date, format = "%H:%M:%S"))) +
  geom_histogram(bins = 100,fill="steelblue",alpha=0.9) +
  xlab("Time") + ylab("Number of orders") + ggtitle("Number of orders by time")

#Pay option on delivery

Yes <- sum(na.omit(final$pay_option_on_delivery))
Noo <- length(na.omit(final$pay_option_on_delivery)) - Yes
Liczba <- c(Yes,Noo)
DeliveryOption <- c("On delivery", "Not on delivery")
Standard <- data.frame(Liczba,DeliveryOption)

ggplot(data=Standard, aes(x=DeliveryOption, y=Liczba)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Number of orders") + ggtitle("Number of orders by Pay Option")+ scale_y_continuous(labels = comma)


#Pay option on transfer

Yes <- sum(na.omit(final$pay_option_transfer))
Noo <- length(na.omit(final$pay_option_transfer)) - Yes
Liczba <- c(Yes,Noo)
DeliveryOption <- c("Transfer", "Not on transfer")
Standard <- data.frame(Liczba,DeliveryOption)

ggplot(data=Standard, aes(x=DeliveryOption, y=Liczba)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Number of orders") + ggtitle("Number of orders by Pay Option")+ scale_y_continuous(labels = comma)

#Categories
number <- count(final, main_category)
number <- arrange(number, -n)
number <- head(number,7)

ggplot(data=number, aes(x=reorder(number$main_category, -number$n), y=number$n)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Number of orders") + ggtitle("Number of orders by Categories")+ scale_y_continuous(labels = comma) + xlab("Category")

#Location

number <- count(final, it_location)
number <- arrange(number, -n)
number <- head(number,7)

ggplot(data=number, aes(x=reorder(number$it_location, -number$n), y=number$n)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Number of orders") + ggtitle("Number of orders by City")+ scale_y_continuous(labels = comma) + xlab("City")

#Seller

number <- count(final, seller)
number <- arrange(number, -n)
number <- head(number,7)

ggplot(data=number, aes(x=reorder(number$seller, -number$n), y=number$n)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Number of orders") + ggtitle("Number of orders by Seller")+ scale_y_continuous(labels = comma) + xlab("Seller")


#Category ~ Price
# final1 <- final[final$main_category == "Motoryzacja",]
# final1 <- final1[final1$price<5000,]
# ggplot(final1,aes(x=final1$price)) + geom_histogram(bins = 100,fill="steelblue",alpha=0.9) +
#   scale_x_continuous(labels = comma) +
#   xlab("User rating") + ylab("Number of orders") + ggtitle("Number of orders by user rating")
# 
# unique(final$main_category)

# Category prize
final2 <- na.omit(final)
agg <- aggregate(final2$price, list(final2$main_category), mean)
agg <- arrange(agg, -x)

#most expensive
agg1 <- head(agg,9)

ggplot(data=agg1, aes(x=reorder(agg1$Group.1, -agg1$x), y=agg1$x)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Mean price") + ggtitle("Price by most expensive Category")+ scale_y_continuous(labels = comma) + xlab("Category")

agg2 <- tail(agg,9)

ggplot(data=agg2, aes(x=reorder(agg2$Group.1, -agg2$x), y=agg2$x)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("Mean price") + ggtitle("Price by cheapest Category")+ scale_y_continuous(labels = comma) + xlab("Category")

# Date ~Price 
# delfi <- final[final$price<2000,]
# delfi$date <- as.POSIXct(delfi$date, format = "%H:%M:%S")
# ggplot(data=delfi, aes(x=delfi$date, y=delfi$price)) + geom_point(alpha=0.5) +  geom_smooth(method=lm)

# Number of orders by seller to seller rating

myFreqs <- final %>% 
  group_by(seller, it_seller_rating) %>%
  summarise(Freq = n())
myFreqs <- arrange(myFreqs, -Freq)
myFreqs

ggplot(data=myFreqs, aes(x=myFreqs$Freq, y=myFreqs$it_seller_rating)) + geom_point(alpha=0.5) +
  geom_smooth(method=lm) + scale_y_continuous(labels = comma) + xlab("Number of orders") + ylab("Seller rating") +
  ggtitle("Seller rating to number of sold products")


#Gross Merchandise Value
final2 <- na.omit(final)
agg <- aggregate(final2$price, list(final2$main_category), sum)
agg <- arrange(agg, -x)

#most expensive
agg1 <- head(agg,7)


ggplot(data=agg1, aes(x=reorder(agg1$Group.1, -agg1$x), y=agg1$x)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("GMV for day in PLN") + ggtitle("Top biggest GMV by Category")+ scale_y_continuous(labels = comma) + xlab("Category")

agg2 <- tail(agg,7)

ggplot(data=agg2, aes(x=reorder(agg2$Group.1, -agg2$x), y=agg2$x)) + geom_bar(stat = "identity",fill="steelblue") +
  ylab("GMV for day in PLN") + ggtitle("Top lowest GMV by Category")+ scale_y_continuous(labels = comma) + xlab("Category")


# Visdat

vis_dat(final, warn_large_data = FALSE) + scale_y_continuous(labels = comma)

vis_miss(final,warn_large_data = FALSE) + scale_y_continuous(labels = comma)

num <- final %>% select(price, it_is_allegro_standard, it_seller_rating)

vis_cor(num,warn_large_data = FALSE)

# DataMaid
final3 <- final
makeDataReport(final3, replace = T)

visualize(final)


