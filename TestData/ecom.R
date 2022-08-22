library(dplyr)
library(tidyverse)
library(sf)
library(jsonlite)
library(leaflet)
library(tibbletime)
library(ggthemes)
library(scales)
library(ggplot2)
library(tidytuesdayR)
library(lubridate)
library(httpgd)
library(tidytext)
library(rvest)
library(jsonlite)
library(janitor)
library(openxlsx)
library(zoo)


session <- read_csv("TestData/DataAnalyst_Ecom_data_sessionCounts.csv")
addsToCart <- read_csv("TestData/DataAnalyst_Ecom_data_addsToCart.csv")

#Check if any browsers should be excluded
unique(session$dim_browser)

#Looks like error and (not set) should be eliminated.
session <- session %>% filter(
    dim_browser != "error", dim_browser != "(not set)")

#dim_date chr to date
session$dim_date <- as.Date(session$dim_date, "%m/%d/%y")

#group by device and month/year, summarise, create ECR
sheet1 <- session %>% mutate(
    month = month(dim_date), year = year(dim_date)) %>%
    group_by(dim_deviceCategory, month, year) %>%
    summarise(Sessions = sum(sessions), Transactions = sum(transactions),
    QTY = sum(QTY)) %>%
    mutate(ECR = Transactions / Sessions) %>%
    arrange(year, month)

#percent of Transactions by device per month - Transpermonth
sheet1 <- sheet1 %>% group_by(month) %>%
    mutate(Trans_permonth = Transactions / sum(Transactions),
    qty_pertrans = QTY / Transactions)

sheet1 <- sheet1 %>% unite("date", month:year, sep = "/")

sheet1$date <- paste("01/", sheet1$date, sep = "")

sheet1$date <- as.Date(sheet1$date, format = "%d/%m/%Y")

#ECR for Desktop #1 by far.
sheet1 %>% group_by(dim_deviceCategory) %>%
    ggplot(aes(x = dim_deviceCategory, y = ECR)) +
    geom_boxplot()

#Time series of Sessions by month
sheet1 %>% group_by(date) %>%
    summarise(Sessions = mean(Sessions)) %>%
    ggplot(aes(x = date, y = Sessions)) +
    geom_line()

sheet2 <- session %>% mutate(
    month = month(dim_date), year = year(dim_date)) %>%
    inner_join(addsToCart, by = c("month" = "dim_month"))

sheet2 <- sheet2 %>% filter(month %in% c(5, 6)) %>% 
    group_by(month, year) %>%
    summarise(Sessions = sum(sessions), Transactions = sum(transactions),
    QTY = sum(QTY), addsToCart = max(addsToCart)) %>%
    mutate(ECR = Transactions / Sessions,
    transperadd = Transactions / addsToCart) %>%
    arrange(year, month)

sheet2 <- sheet2 %>% unite("date", month:year, sep = "/")

sheet2$date <- paste("01/", sheet2$date, sep = "")

sheet2$date <- as.Date(sheet2$date, format = "%d/%m/%Y")

sheet2 <- sheet2 %>% mutate(sess_mom = Sessions - lag(Sessions),
    sess_momper = (Sessions - lag(Sessions)) / lag(Sessions),
    trans_mom = Transactions - lag(Transactions),
    trans_momper = (Transactions - lag(Transactions)) / lag(Transactions),
    QTY_mom = QTY - lag(QTY),
    QTY_momper = (QTY - lag(QTY)) / lag(QTY),
    adds_mom = addsToCart - lag(addsToCart),
    adds_momper = (addsToCart - lag(addsToCart)) / lag(addsToCart),
    ECR_mom = ECR - lag(ECR),
    ECR_momper = (ECR - lag(ECR)) / lag(ECR),
    transperadd_mom = transperadd - lag(transperadd),
    transperadd_momper = (transperadd - lag(transperadd)) / lag(transperadd))

ecom_list <- list("Mon * Dev Metrics" = sheet1, "MoM" = sheet2)

write.xlsx(ecom_list, "TestData/ecom_Lee20220821.xlsx")

test <- sheet2 %>% filter(date == "2013-06-01") %>%
    select(trans_momper, QTY_momper, adds_momper, ECR_momper,
    transperadd_momper)

test
