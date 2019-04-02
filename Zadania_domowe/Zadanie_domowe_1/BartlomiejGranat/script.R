library(readr)
library(dplyr)
library(DataExplorer)
library(funModeling)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(multipanelfigure)
library(stringi)
library(RColorBrewer) 
library(corrplot)
library(minerva)

df <- read_csv("allegro-api-transactions.csv")
head(df)
colnames(df)
introduce(df)
plot_intro(df)
summary(df)

p5 <- ggplot(data=as.data.frame(sort(table(df$main_category))), aes(x=Var1,y=Freq)) +
  geom_bar(stat="identity", fill = "#104E8B") + coord_flip() + theme_minimal() + ylab("Category") +
  xlab("Number of orders")


p1 <- ggplot(data=as.data.frame(table(df$it_is_allegro_standard)), aes(x=Var1,y=Freq)) +
  geom_bar(stat="identity", fill = "#104E8B") + theme_minimal()  + 
  scale_x_discrete(labels = c("No", "Yes"), expand = c(0, 0)) + xlab("Does it fulfill allegro standard?") +
  ylab("Number of orders")

p2 <- ggplot(data=as.data.frame(table(df$it_is_brand_zone)), aes(x=Var1,y=Freq)) +
  geom_bar(stat="identity", fill = "#104E8B") + theme_minimal() + 
  scale_x_discrete(labels = c("No", "Yes"), expand = c(0, 0)) + xlab("Is it from the brand zone?") +
  ylab("Number of orders")

p3 <- ggplot(data=as.data.frame(table(df$pay_option_transfer)), aes(x=Var1,y=Freq)) +
  geom_bar(stat="identity", fill = "#104E8B") + theme_minimal() + 
  scale_x_discrete(labels = c("No", "Yes"), expand = c(0, 0)) + xlab("Paid with bank transfer") +
  ylab("Number of orders")

p4 <- ggplot(data=as.data.frame(table(df$pay_option_on_delivery)), aes(x=Var1,y=Freq)) +
  geom_bar(stat="identity", fill = "#104E8B") + theme_minimal() + 
  scale_x_discrete(labels = c("No", "Yes"), expand = c(0, 0)) + xlab("Paid on delivery") +
  ylab("Number of orders")


figure1 <- multi_panel_figure(columns = 2, rows = 2, panel_label_type = "none")
figure1 %<>%
  fill_panel(p1, column = 1, row = 1) %<>%
  fill_panel(p2, column = 2, row = 1) %<>%
  fill_panel(p3, column = 1, row = 2) %<>%
  fill_panel(p4, column = 2, row = 2)
figure1

df$price
df_small <- df[seq(1,nrow(df), 100), ]

p6 <- ggplot(data = df_small) + geom_boxplot(aes(y=df_small$price)) + theme_minimal() + ylab("Price (PLN)")
price_summ <- summary(df$price)

p7 <- ggplot(data = df_small) + geom_boxplot(aes(y=df_small$it_seller_rating)) + theme_minimal() + ylab("Rating")
rating_summ <- summary(df$it_seller_rating)

p8 <- ggplot(data = df_small) + geom_boxplot(aes(y=df_small$date)) + theme_minimal() + ylab("Date")
date_summ <- summary(df$date)

df2 <- df_small[c("price", "it_is_allegro_standard", "it_quantity", "it_seller_rating","it_is_brand_zone","pay_option_on_delivery","pay_option_transfer")]
df2
mine_res_hd=mine(df2)


