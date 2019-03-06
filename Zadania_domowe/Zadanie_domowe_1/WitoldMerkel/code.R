# library
library(readr)
library(ggplot2)
library(dplyr)
library(autoEDA)
library(funModeling)
library(visdat)
library(dataMaid)
library(DataExplorer)

# data
options(stringsAsFactors = FALSE)
api <- read_csv("allegro-api-transactions.csv")
txt <- read.delim("allegro-categories-eng.txt", sep = ";")
colnames(txt)[1] <- "main_category"
data <- full_join(api, txt)
summary(data$price)
data$date <- as.POSIXct(data$date, format = "%H:%M:%S")
max_price <- max(data$price)
step <- 100
buys <- rep(0, length(seq(from = 0, to = max_price, by = step)))
for (k in seq(from = 0, to = length(seq(from = 0, to = max_price, by = step)), by = 1)){
  data1 <- data[data$price <= 0 + step * k, ]
  buys[k] <- length(data1$price)
}

# Eksploracja i poprawność ręcznie jednej zmiennej

## Brakujące dane
na_in_columns <- unique(is.na(data))

## Kategorie(Kategoryczna)
popular_categories <- as.data.frame(sort(table(data$main_category_eng), decreasing=FALSE))

plot1 <- ggplot(data = popular_categories, aes(x = Var1, y = Freq)) + geom_col(fill = "gold3") + theme_dark() +
  xlab("Categories") + ylab("Transactions") + ggtitle("Which categories are more popular?") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 110000)) + coord_flip() +
  geom_text(label = popular_categories$Freq, y = popular_categories$Freq + 5000) +
  scale_x_discrete(labels = c("Real Estate", "Tickets", "Technics, studio & DJ", "Instruments",
                              "Films", "Consoles", "Antiques & Arts", "Music", "Photography",
                              "Office & Advertising", "Jewelry & Watches", "Industry",
                              "Collections", "Crafts", "Games", "Delicatessen", 
                              "Books and Comics", "Helath", "Computers", "Phones & Accessories",
                              "RTV & AGD", "Sport & Tourisn", "Beauty", "Babies", "Automotive",
                              "Clothing, Shoes, Accessories", "Home & Garden"))

## Standard(Kategoryczna)
allegro_standard <- as.data.frame(sort(table(data$it_is_allegro_standard), decreasing=TRUE))

plot2 <- ggplot(data = allegro_standard, aes(x = Var1, y = Freq)) + geom_col(fill = "gold3") + theme_dark() +
  xlab("Is it allegro standard?") + ylab("Number of transactions") + ggtitle("How is allegro standard distributed?") +
  scale_y_continuous(limits = c(0, 300000), labels = c("0", "100000", "200000", "300000"), expand = c(0 ,0)) + 
  scale_x_discrete(labels = c("Yes", "No"), expand = c(0, 0)) +
  geom_text(label = allegro_standard$Freq, y = allegro_standard$Freq + 5000)

## Brand Zone(Kategoryczna)
brand_zone <- as.data.frame(sort(table(data$it_is_brand_zone), decreasing=TRUE))

plot3 <- ggplot(data = brand_zone, aes(x = Var1, y = Freq)) + geom_col(fill = "gold3") + theme_dark() +
  xlab("Is it in the brand zone?") + ylab("Number of appearances") + ggtitle("What we can tell about the zones?") +
  scale_x_discrete(labels = c("No", "Yes"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 500000), labels = c("0", "100000", "200000", "300000", "400000", "500000"),
                     expand = c(0 ,0)) + 
  geom_text(label = brand_zone$Freq, y = brand_zone$Freq + 8000)
  
## Pay options(Kategoryczne)
transfer_data <- as.data.frame(data%>%group_by()%>%count(pay_option_transfer))
delivery_data <- as.data.frame(data%>%group_by()%>%count(pay_option_on_delivery))

plot4 <- ggplot(data = transfer_data, aes(x = pay_option_transfer, y = n)) + geom_col(fill = "gold3") + theme_dark() +
  xlab("Was it pay with a bank transfer?") + ylab("Number of appearances") + ggtitle("How was it paid?") +
  scale_x_discrete(labels = c("No", "Yes"), limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 400000),labels = c("0", "100000", "200000", "300000", "400000"), 
                     expand = c(0 ,0)) + 
  geom_text(label = transfer_data$n, y = transfer_data$n + 8000)

plot5 <- ggplot(data = delivery_data, aes(x = pay_option_on_delivery, y = n)) + geom_col(fill = "gold3") + theme_dark() +
  xlab("Was it pay on delivery?") + ylab("Number of appearances") + ggtitle("How was it paid?") +
  scale_x_discrete(labels = c("No", "Yes"), limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 400000),labels = c("0", "100000", "200000", "300000", "400000"), 
                     expand = c(0 ,0)) + 
  geom_text(label = delivery_data$n, y = delivery_data$n + 8000)

## Cena(Ciągła)
price_summary <- summary(data$price)

plot6 <- ggplot(data = NULL, aes(x = seq(from = 0, to = max_price, by = step), y = buys)) + geom_point() +
  geom_hline(yintercept = buys[2], col = "orange") +
  xlab("Amount of money spent [zł]") + ylab("Number of transactions with at most x zlotys") +
  ggtitle("How does the number of transactions chage with the amount of money spent?") +
  annotate("text", x = 29000, y = buys[1] - 8000, label = "82.96% of transactions are below 100 zł", col = "navy") +
  geom_hline(yintercept = buys[1], col = "yellow") + geom_hline(yintercept = buys[5], col = "red") + theme_dark() +
  annotate("text", x = 29000, y = buys[2] - 8000, label = "92.59% of transactions are below 200 zł", col = "navy") +
  annotate("text", x = 29000, y = buys[5] - 8000, label = "97.90% of transactions are below 500 zł", col = "navy") +
  scale_y_continuous(breaks = seq(0, 420000, 100000),labels = c("0", "100000", "200000", "300000", "400000"),
                     limits = c(0, buys[1191])) +
  geom_vline(xintercept = median(data$price) + 500, color = "royalblue4") +
  geom_vline(xintercept = mean(data$price) + 900, color = "darkorchid4") +
  annotate("text", x = 27000, y = 250000, label = "Mean of the price is equal to 76.81 zł", col = "darkorchid4") +
  annotate("text", x = 28000, y = 210000, label = "Median of the price is equal to 24.99 zł", col = "royalblue4") +
  scale_x_continuous(expand = c(0, 100))

## Rating(Ciągła)
rating_summary <- summary(data$it_seller_rating)

plot7 <- ggplot(data, aes(x = data$it_seller_rating)) + geom_density(aes(y=..density..), alpha=0.7, fill="gold3") +
  theme_dark() + xlab("Rating of sellers") + ylab("Density") + ggtitle("What is the density of user ratings?") + 
  scale_y_continuous(limits = c(0, 0.0001), labels = c("0", "0.000025", "0.00005", "0.000075", "0.0001"), 
                     expand = c(0 ,0)) + 
  scale_x_continuous(limits = c(0, 300000), labels = c("0", "100000", "200000", "300000"))

boxplot(data$it_seller_rating, horizontal = TRUE)
title(main = "Boxplot of user ratings", xlab = "User rating")

## Data(Ciągła)
date_summary <- summary(data$date)

plot9 <- ggplot(data, aes(x = as.POSIXct(data$date, format = "%H:%M:%S"))) +
  geom_density(aes(y=..density..), alpha=0.7, fill="gold3") + theme_dark() +
  xlab("Time of the day: 2016-04-03") + ylab("Density") +
  ggtitle("What is the density of number of transactions at a certain point of the day?") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.00004),
                     labels = c("0", "0.00001", "0.00002", "0.00003", "0.00004"))

# Eksploracja i poprawność ręcznie dwóch zmiennych

## Jak price zależy od brand_zone
price__brand_zone <- by(data$price, data$it_is_brand_zone, summary)

## Jak price zmiania się od standardu
price__standard <- by(data$price, data$it_is_allegro_standard, summary)

# DataExplorer i visdat

## DataExplorer
data_info_DE <- as.data.frame(introduce(data))
data_plot_DE <- plot_intro(data)
qq_data_DE <- data[, c("price", "it_seller_rating")]
qqplot_DE <- plot_qq(qq_data_DE, sampled_rows = 1000L) #kropki = dane, linia = rozkład normalny
corelation_DE <- plot_correlation(na.omit(data), maxcat = 20L)

## visdat
data_plot_VD <- vis_dat(data, warn_large_data = FALSE)
miss_data_VD <- vis_miss(data, warn_large_data = FALSE)
pay_option_comapre_VD <- vis_compare(as.data.frame(data$pay_option_transfer),
                                     as.data.frame(data$pay_option_on_delivery))