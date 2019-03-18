library(data.table)
library(DataExplorer)
library(funModeling)
library(ggplot2)
library(lubridate)
library(visdat)

# dane
allegro <- data.table(read.csv("allegro-api-transactions.csv"))
head(allegro)
tekst <- data.table(read.csv2("allegro-categories-eng.csv"))
head(tekst)

# merge na dwoch plikach
head(allegro$main_category)
head(tekst$main_category)
merged <- merge(allegro,tekst,by ="main_category")
head(merged)

# filtrowanie interesujących nas kolumn
filtered <- merged[, .(price,it_is_allegro_standard,it_is_brand_zone,it_seller_rating,date)]
head(filtered)

# dataExplorer
# plot_histogram(filtered)
# plot_bar(filtered)

# funModeling
# df_status(filtered)
# plot_num(filtered)

# correlation_table(filtered, "it_is_allegro_standard")
# correlation_table(filtered, "it_is_brand_zone")
# correlation_table(filtered, "price")
# correlation_table(filtered, "it_seller_rating")
