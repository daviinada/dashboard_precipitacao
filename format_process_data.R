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
library(d3heatmap)

setwd('/home/toshi/Desktop/work_butanta/dashboard_precipitacao/')

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

d3heatmap(hclust_df, scale="column", colors="Blues")

    
    
library(dygraphs)
library(xts)    

df_dygraph <-  preciptation_per_month_per_rain_days %>%
        ungroup() %>%
        select(year, month, mean_precip_in_rain_days) %>%
        spread(year, mean_precip_in_rain_days)

# df_dygraph <- as.data.frame(df_dygraph)

# df_dygraph <- df_dygraph[ ,-1]

library("viridisLite")

cols <- viridis(3)
cols <- substr(cols, 0, 7)

m_order <- c(unique(as.character(raw_data_gather$month)))
# sorting month in correct order
df_dygraph <- df_dygraph %>%
    mutate(month = factor(month, levels = m_order)) %>%
    arrange(factor(month, levels = m_order) )


hc <- highchart() %>% 
    hc_xAxis(categories = df_dygraph$month) %>% 
    hc_add_series(name = "2001", data = df_dygraph$`2001`) %>% 
    hc_add_series(name = "2002", data = df_dygraph$`2002`) %>% 
    hc_add_series(name = "2003", data = df_dygraph$`2003`) %>%
    hc_add_series(name = "2004", data = df_dygraph$`2004`) %>% 
    hc_add_series(name = "2005", data = df_dygraph$`2005`) %>% 
    hc_add_series(name = "2006", data = df_dygraph$`2006`) %>% 
    hc_add_series(name = "2007", data = df_dygraph$`2007`) %>% 
    hc_add_series(name = "2008", data = df_dygraph$`2008`) %>% 
    hc_add_series(name = "2009", data = df_dygraph$`2009`) %>% 
    hc_add_series(name = "2010", data = df_dygraph$`2010`) %>% 
    hc_add_series(name = "2011", data = df_dygraph$`2011`) %>% 
    hc_add_series(name = "2012", data = df_dygraph$`2012`) %>% 
    hc_add_series(name = "2013", data = df_dygraph$`2013`) %>% 
    hc_add_series(name = "2014", data = df_dygraph$`2014`) %>% 
    hc_add_series(name = "2015", data = df_dygraph$`2015`) %>% 
    hc_add_series(name = "2016", data = df_dygraph$`2016`) 

hc

str(citytemp)
