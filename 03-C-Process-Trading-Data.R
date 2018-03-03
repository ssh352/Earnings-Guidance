# Copyright (c) 2018 by Einzbern

#####----Guidance----#####

load("02-D-Trading.RData")
options(scipen = 99, digits = 3)
library(tidyverse)
library(lubridate)
library(moments)
library(xlsx)
library(stargazer)


#####----CAR----#####

estimation_window <- c(-240, -60)  # 日历日，未注明同
announcement_window <- c(-30, 30)
least_valid_days <- 63  # 交易日
event <- "业绩报告第一时间"

Mandatory_And_Forecast <- Mandatory_And_Forecast %>% 
  unite(`业绩预告情况`, `发布情况`, `业绩预告准确性离散型`, remove = FALSE)
table(Mandatory_And_Forecast$业绩预告情况)

group.by <- "业绩预告情况"

calculate_ERiRf <- function(Trading) {
  lm(RiRf ~ RmRf + SMB + HML, data = Trading, subset = Used_for_Estimation) %>% 
    predict(newdata = filter(Trading, !Used_for_Estimation)) %>% 
    mutate(Trading %>% filter(!Used_for_Estimation) %>% select(`交易日期`, RiRf), ERiRf = .)
}

calculate_t <- function(event_date, date_vector) {
  if (event_date %in% date_vector) {
    return(seq_along(date_vector) - match(event_date, date_vector))
  } else {
    date_vector_aug <- sort(union(date_vector, event_date))
    date_vector_aug <- seq_along(date_vector_aug) - match(event_date, date_vector_aug)
    return(date_vector_aug[date_vector_aug != 0])
  }
}

AR <- Mandatory_And_Forecast %>% 
  select(`证券代码`, `业绩报告期`, `事件日期` = !!event) %>% 
  na.omit() %>% 
  group_by(`证券代码`, `业绩报告期`, `事件日期`) %>% 
  do(tibble(`交易日期` = c(seq(from = .$`事件日期` + days(!!estimation_window[1]), to = .$`事件日期` + days(!!estimation_window[2]), by = "day"), 
                       seq(from = .$`事件日期` + days(!!announcement_window[1]), to = .$`事件日期` + days(!!announcement_window[2]), by = "day")), 
            Used_for_Estimation = c(rep(TRUE, diff(!!estimation_window) + 1), rep(FALSE, diff(!!announcement_window) + 1)))) %>% 
  inner_join(select(Trading, -`成交额`, -Ri, -Rm, -Rf, -`涨跌停状态`)) %>% 
  filter(sum(Used_for_Estimation) >= !!least_valid_days) %>% 
  do(calculate_ERiRf(.)) %>% 
  arrange(`证券代码`, `业绩报告期`, `事件日期`, `交易日期`) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(t = calculate_t(`事件日期`[1], `交易日期`), AR = RiRf - ERiRf)
  # select(-(`事件日期`:ERiRf)) %>% 

AR_grouped <- AR %>% 
  left_join(select(Mandatory_And_Forecast, `证券代码`, `业绩报告期`, `分组` = !!group.by)) %>% 
  group_by(`分组`, t) %>% 
  summarise(AR = mean(AR, na.rm = TRUE))

## 事件日附近AR走势图（应该再画个CAR，写不动了……）
ggplot(AR_grouped, aes(x = t, y = AR, group = `分组`, color = `分组`)) + 
  geom_line() + 
  theme_grey(base_family = "STKaiti")


#####----Risk----#####

announcement_window <- c(-30, -1)
event <- "业绩报告第一时间"

tmp <- Mandatory_And_Forecast %>% 
  select(`证券代码`, `业绩报告期`, `事件日期` = !!event) %>% 
  na.omit() %>% 
  group_by(`证券代码`, `业绩报告期`, `事件日期`) %>% 
  do(tibble(`交易日期` = seq(from = .$`事件日期` + days(!!announcement_window[1]), to = .$`事件日期` + days(!!announcement_window[2]), by = "day"))) %>% 
  inner_join(select(Trading, `证券代码`, `交易日期`, Ri, `涨跌停状态`))
tmp2 <- tmp %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  summarise(Volatility = sd(Ri), 
            Skewness = skewness(Ri), 
            Kurtosis = kurtosis(Ri), 
            `涨停次数` = sum(`涨跌停状态` == 1L, na.rm = TRUE), 
            `跌停次数` = sum(`涨跌停状态` == -1L, na.rm = TRUE), 
            `涨跌停次数` = `涨停次数` + `跌停次数`) %>% 
  left_join(select(Mandatory_And_Forecast, `证券代码`, `业绩报告期`, `分组` = !!group.by))
tmp2 %>% 
  group_by(`分组`) %>% 
  summarise(Volatility = mean(Volatility, na.rm = TRUE), 
            Skewness = mean(Skewness, na.rm = TRUE), 
            Kurtosis = mean(Kurtosis, na.rm = TRUE), 
            `涨停次数` = mean(`涨停次数`, na.rm = TRUE), 
            `跌停次数` = mean(`跌停次数`, na.rm = TRUE), 
            `涨跌停次数` = mean(`涨跌停次数`, na.rm = TRUE)) %>% 
  write.xlsx("02-T-业绩预告情况与风险指标.xlsx")



#####----Others----#####

Mandatory_And_Forecast %>% 
  count(`应该发布业绩预告`, `业绩预告与分析师预测`) %>% 
  na.omit() %>% 
  spread(`业绩预告与分析师预测`, n, fill = 0) %>% 
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "03-T-应该发布业绩预告与分析师预测.htm")

Mandatory_And_Forecast %>% 
  count(`应该发布业绩预告`, `业绩预告准确性离散型`) %>% 
  na.omit() %>% 
  spread(`业绩预告准确性离散型`, n, fill = 0) %>% 
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "03-T-应该发布业绩预告与业绩预告准确性.htm")



























Estimation_Windows <- Estimation_Windows %>% 
  as_tibble() %>% 
  mutate(AR = Ri - Fitted) %>% 
  group_by(`证券代码`, `业绩报告期`)

# 计算CAR和PJR
CAR <- Estimation_Windows %>% 
  summarise(CAR_long = sum(AR))

CAR <- Estimation_Windows %>% 
  filter(between(`交易日期`, `事件日期`[1] + days(-1), `事件日期`[1] + days(announcement_window[2]))) %>% 
  summarise(CAR_short = sum(AR)) %>% 
  inner_join(CAR, .) %>% 
  ungroup() %>% 
  mutate(PJR = CAR_short / CAR_long)

Mandatory_Forecast_And_Risk <- Mandatory_And_Forecast %>% 
  left_join(CAR)

## 描述性统计
Mandatory_Forecast_And_Risk %>% 
  group_by(`应该发布`, `是否发布`) %>% 
  summarise(PJR = mean(PJR, na.rm = TRUE)) %>% 
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "03-T-Price-Jump-Ratio.htm")

