library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(plotly)
library(heatmaply)
library(qgraph)
library(highcharter)
library(dygraphs)

setwd('/home/davi/Desktop/dashboard_precipitacao/')

raw_data <- read_excel(path = 'tabela_precipitacao_tome_acu.xlsx')
raw_data <- as.data.frame(raw_data)
head(raw_data)

firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

raw_data_gather <- raw_data %>%
    gather(key= month, value= precipitation_mm, -day, -year) %>%
    mutate(month_numeric = match(firstup(month), month.abb),
           date = ymd(paste(year, month_numeric, day, sep = '-'))) 

str(raw_data_gather)

# Cleaning miss values
raw_data_gather$precipitation_mm[ which(raw_data_gather$precipitation_mm  == '0') ] <- NA
raw_data_gather$precipitation_mm[ which(raw_data_gather$precipitation_mm  == '-') ] <- NA
raw_data_gather$precipitation_mm <- as.numeric(raw_data_gather$precipitation_mm)

# Formating data
raw_data_gather$day <- as.factor(raw_data_gather$day)
raw_data_gather$month <- as.factor(raw_data_gather$month) 
raw_data_gather$year <- as.factor(raw_data_gather$year)

head(raw_data_gather)

order_month <- unique(as.character(raw_data_gather$month))

# Remover linhas com NA na data (nao existem por conta do mes)
raw_data_gather <- raw_data_gather[ -which(is.na(raw_data_gather$date)), ]

# Mean precipitation per months per rain days
preciptation_per_month_per_rain_days <- raw_data_gather %>%
    group_by(year, month) %>%
    summarise(days_rain_month = sum(!is.na(precipitation_mm)),
              mean_precip_in_rain_days = sum(precipitation_mm, na.rm = TRUE)/ sum(!is.na(precipitation_mm)))

# Dias de chuva por mes
preciptation_per_month_per_rain_days %>%
    ggplot(aes(x=factor(month, levels = order_month), y=days_rain_month, col=year, group=year )) +
    geom_line() +
    geom_point()
    

preciptation_per_month_per_rain_days %>%
    ggplot(aes(x=factor(month, levels = order_month), y=days_rain_month, col=year, group=year )) +
    geom_line() +
    geom_point() +
    facet_wrap(~year)

# Precipitacao media por mes
raw_data_gather %>%
    group_by(year, month) %>%
    summarise(mean_precip = mean(precipitation_mm, na.rm = TRUE)) %>%
    ggplot(aes(x=factor(month, levels = order_month), y=mean_precip, col=year, group=year )) +
    geom_line() +
    geom_point()

# Precipitacao toal por mes
raw_data_gather %>%
    group_by(year, month) %>%
    summarise(total_precip = sum(precipitation_mm, na.rm = TRUE)) %>%
    ggplot(aes(x=factor(month, levels = order_month), y=total_precip, col=year, group=year )) +
    geom_line() +
    geom_point()

# Precipitacao toal por mes
raw_data_gather %>%
    group_by(year, month) %>%
    summarise(total_precip = sum(precipitation_mm, na.rm = TRUE)) %>%
    ggplot(aes(x=factor(month, levels = order_month), y=total_precip, col=year, group=year )) +
    geom_line() +
    geom_point() +
    facet_wrap(~year)

# boxplot precipitacao  mensao ao logo de todos os anos
raw_data_gather %>%
    ggplot(aes(x=factor(month, levels = order_month), y=precipitation_mm, fill=year)) +
    geom_boxplot()

# boxplot precipitacao  mensal geral 
raw_data_gather %>%
    ggplot(aes(x=factor(month, levels = order_month), y=precipitation_mm, fill=month)) +
    geom_boxplot()


hclust_df <- preciptation_per_month_per_rain_days %>%
    mutate(var = paste(year, month, sep = '_')) %>%
    ungroup() %>%
    select(days_rain_month, mean_precip_in_rain_days, var) %>%
    as.data.frame()

rownames(hclust_df) <- hclust_df$var

hclust_df <- hclust_df[ ,c(1,2)]

boxplot(hclust_df[ ,1])
boxplot(hclust_df[ ,2]) # Remover o outlier, esta causando problema!

hclust_df <- subset(hclust_df, hclust_df[,2] < max(hclust_df[,2]))

# scale data to mean=0, sd=1 and convert to matrix
hclust_df_scaled <- as.matrix(scale(hclust_df))

heatmaply(t(hclust_df_scaled), k_col = 10 )

# hclust
hclust_obj <- hclust(dist(hclust_df_scaled))
hclust_obj

highchart() %>% 
hc_xAxis(categories = preciptation_per_month_per_rain_days$year) 

    hc_add_serie(name = "Tokyo", data = citytemp$tokyo) %>% 
    hc_add_serie(name = "London", data = citytemp$london)

    
library(dygraphs)
    
    preciptation_per_month_per_rain_days %>%
        ungroup() %>%
        select(year, month, mean_precip_in_rain_days) %>%
        spread(year, mean_precip_in_rain_days)

