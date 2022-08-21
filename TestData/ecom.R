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

sheet1

sheet2 <- session %>% mutate(
    month = month(dim_date), year = year(dim_date)) %>%
    inner_join(addsToCart, by = c("month" = "dim_month"))


sheet2 <- sheet2 %>% filter(month %in% c(5, 6)) %>%
    group_by(month, year) %>%
    summarise(Sessions = sum(sessions), Transactions = sum(transactions),
    QTY = sum(QTY), addsToCart = max(addsToCart)) %>%
    arrange(year, month)

sheet2 %>% group_by(month, year) %>%
    mutate(sess_mom = Sessions - lag(Sessions),
    sess_momper = (Sessions - lag(Sessions)) / lag(Sessions))

sheet2$date <- as.yearmon(paste(sheet2$year, sheet2$month, "%Y %m"))

sheet2

test <- sheet2 %>% unite("date", month:year, sep = "/")

test$date <- paste("01/", test$date, sep = "")

test$date <- as.Date(test$date, format = "%d/%m/%Y")

test %>% mutate(sess_mom = Sessions - lag(Sessions),
    sess_momper = (Sessions - lag(Sessions)) / lag(Sessions),
    trans_mom = Transactions - lag(Transactions),
    trans_momper = (Transactions - lag(Transactions)) / lag(Transactions),
    QTY_mom = QTY - lag(QTY),
    QTY_momper = (QTY - lag(QTY)) / lag(QTY),
    adds_mom = addsToCart - lag(addsToCart),
    adds_momper = (addsToCart - lag(addsToCart)) / lag(addsToCart))
