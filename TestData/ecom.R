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
library(formattable)

httpgd::hgd()
httpgd::hgd_browse()

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

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

#Date format for sheet1
sheet1 <- sheet1 %>% unite("date", month:year, sep = "/")

sheet1$date <- paste("01/", sheet1$date, sep = "")

sheet1$date <- as.Date(sheet1$date, format = "%d/%m/%Y")

#ECR by device, desktop #1
sheet1 %>% group_by(dim_deviceCategory) %>%
    ggplot(aes(x = dim_deviceCategory, y = ECR)) +
    geom_boxplot() +
    scale_y_continuous() +
    xlab("Device") +
    ylab("ECR") +
    labs(title = "Boxplots of ECR by Device") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

#Time series of Sessions by month
sheet1 %>% group_by(date) %>%
    summarise(Sessions = mean(Sessions)) %>%
    ggplot(aes(x = date, y = Sessions)) +
    geom_line() +
    scale_y_continuous(labels = comma) +
    xlab("Date") +
    ylab("# of Sessions") +
    labs(title = "Time Series of Total Sessions") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

#Session %
sheet1 %>% group_by(dim_deviceCategory) %>%
    summarise(Sessions = sum(Sessions)) %>%
    mutate(Sessionsper = (Sessions / sum(Sessions)) * 100) %>%
    ggplot(aes(x = dim_deviceCategory, y = Sessionsper,
    fill = dim_deviceCategory)) +
    geom_col() +
    xlab("Device") +
    ylab("% of Total") +
    labs(title = "% of Sessions by Device", fill = "Device") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

#Join session and addsToCart for sheet2
sheet2 <- session %>% mutate(
    month = month(dim_date), year = year(dim_date)) %>%
    inner_join(addsToCart, by = c("month" = "dim_month"))

#Filter months, summarise, arrange for sheet2
sheet2 <- sheet2 %>% filter(month %in% c(5, 6)) %>% 
    group_by(month, year) %>%
    summarise(Sessions = sum(sessions), Transactions = sum(transactions),
    QTY = sum(QTY), addsToCart = max(addsToCart)) %>%
    mutate(ECR = Transactions / Sessions,
    transperadd = Transactions / addsToCart) %>%
    arrange(year, month)

#Date foramt for sheet2
sheet2 <- sheet2 %>% unite("date", month:year, sep = "/")

sheet2$date <- paste("01/", sheet2$date, sep = "")

sheet2$date <- as.Date(sheet2$date, format = "%d/%m/%Y")

#Create desired mom and mom_percent for variables
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

#Create and export xlsx
ecom_list <- list("Mon * Dev Metrics" = sheet1, "MoM" = sheet2)

write.xlsx(ecom_list, "TestData/ecom_Lee20220821.xlsx")

#Make pretty mom_percent table for Deck
test <- sheet2 %>% filter(date == "2013-06-01") %>%
    select(trans_momper, QTY_momper, adds_momper, ECR_momper,
    transperadd_momper)

#Values to percent
test$trans_momper <- percent(test$trans_momper, accuracy = 0.1)
test$QTY_momper <- percent(test$QTY_momper, accuracy = 0.1)
test$adds_momper <- percent(test$adds_momper, accuracy = 0.1)
test$ECR_momper <- percent(test$ECR_momper, accuracy = 0.1)
test$transperadd_momper <- percent(test$transperadd_momper, accuracy = 0.1)

#Change to df for formatting
testdf <- as.data.frame(test)
rownames(testdf) <- paste0("% Change")

#Rename to make pretty :)
testdf <- testdf %>% rename(Transactions = trans_momper, QTY = QTY_momper,
    addsToCart = adds_momper, ECR = ECR_momper,
    Trans_per_addsToCart = transperadd_momper)

#Formattable to make pretty
mom_percent <- formattable(testdf, align = c("c", "c", "c", "c", "c"),
    list(
    "Transactions" = color_tile(customGreen, customGreen0),
    "QTY" = color_tile(customGreen, customGreen0),
    "addsToCart" = color_bar(customRed),
    "ECR" = color_tile(customGreen, customGreen0),
    "Trans_per_addsToCart" = color_tile(customGreen, customGreen0)))