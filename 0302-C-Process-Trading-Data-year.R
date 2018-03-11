# Copyright (c) 2018 by Einzbern

#####----Guidance----#####

load("../Output/02-D-Trading-Data.RData")
options(scipen = 99, digits = 3)
library(tidyverse)
library(lubridate)
library(moments)
library(xlsx)
library(stargazer)

#####----筛选业绩报告期为年报的样本----#####
Report_Data <- filter(Report_Data, month(`业绩报告期`) == 12)

#####----CAR----#####

estimation_window <- c(-240, -60)  # 日历日，未注明同
announcement_window <- c(-30, 30)
least_valid_days <- 63  # 交易日
event <- "业绩报告_第一时间"

Report_Data <- Report_Data %>%
  mutate(`业绩预告发布情况_4类` = if_else(`业绩预告发布情况_4类` == "不需发没发", "A不需发没发", `业绩预告发布情况_4类`)) %>% 
  unite(`业绩预告情况_10类`, `业绩预告发布情况_4类`, `业绩预告与业绩报告比较_离散型`, remove = FALSE)
table(Report_Data$`业绩预告情况_10类`)

group.by <- "业绩预告与业绩报告比较_离散型" 
## 把03-C-Process-Trading-Data.R中 "业绩预告情况_10类" 改成 "业绩预告与业绩报告比较_离散型"

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

AR <- Report_Data %>% 
  select(`证券代码`, `业绩报告期`, `事件日期` = !!event) %>% 
  na.omit() %>% 
  group_by(`证券代码`, `业绩报告期`, `事件日期`) %>% 
  do(tibble(`交易日期` = c(seq(from = .$`事件日期` + days(!!estimation_window[1]), to = .$`事件日期` + days(!!estimation_window[2]), by = "day"), 
                       seq(from = .$`事件日期` + days(!!announcement_window[1]), to = .$`事件日期` + days(!!announcement_window[2]), by = "day")), 
            Used_for_Estimation = c(rep(TRUE, diff(!!estimation_window) + 1), rep(FALSE, diff(!!announcement_window) + 1)))) %>% 
  inner_join(select(Trading_Data, -`成交额`, -Ri, -Rm, -Rf, -`涨跌停状态`)) %>% 
  filter(sum(Used_for_Estimation) >= !!least_valid_days) %>% 
  do(calculate_ERiRf(.)) %>% 
  arrange(`证券代码`, `业绩报告期`, `事件日期`, `交易日期`) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(t = calculate_t(`事件日期`[1], `交易日期`), AR = RiRf - ERiRf)
# select(-(`事件日期`:ERiRf)) %>% 

AR_grouped <- AR %>% 
  left_join(select(Report_Data, `证券代码`, `业绩报告期`, `分组` = !!group.by)) %>% 
  group_by(`分组`, t) %>% 
  summarise(AR = mean(AR, na.rm = TRUE))


## 事件日附近AR走势图（应该再画个CAR，写不动了……）
ggplot(AR_grouped, aes(x = t, y = AR, group = `分组`, color = `分组`)) + 
  geom_line() + 
  theme_grey(base_family = "STKaiti")

#####----CAR, Volatility, Skewness, Kurtosis, 涨停次数, 跌停次数, 涨跌停次数
## 的t检验，时间窗口选择c(-3, -1)

window.t.list <- list(c(-28, -1), c(-14, -1), c(-7, -1), 
                      c(0, 1), c(2, 8), c(2, 15), c(2, 29)) # 日历日


lapply(window.t.list, function(window.t){
  ## 截取AR中t在时间窗口中的数据， 计算此时间窗口CAR
  CAR <- AR %>% 
    left_join(select(Report_Data, `证券代码`, `业绩报告期`, `分组` = !!group.by)) %>% 
    group_by(`分组`, `证券代码`, `事件日期`) %>% 
    filter(t >= window.t[1] & t <= window.t[2]) %>% 
    summarise(CAR = sum(AR, na.rm = T))
  
  CAR.summarise <- CAR %>% 
    group_by(`分组`) %>% 
    summarise(CAR = mean(CAR, na.rm = T))
  
  CAR.p <- pairwise.t.test(CAR$CAR, CAR$分组, paired = F, p.adjust.method = "none")$p.value
  
  #####----Risk----#####
  
  announcement_window <- window.t
  event <- "业绩报告_第一时间"
  
  tmp <- Report_Data %>% 
    select(`证券代码`, `业绩报告期`, `事件日期` = !!event) %>% 
    na.omit() %>% 
    group_by(`证券代码`, `业绩报告期`, `事件日期`) %>% 
    do(tibble(`交易日期` = seq(from = .$`事件日期` + days(!!announcement_window[1]), to = .$`事件日期` + days(!!announcement_window[2]), by = "day"))) %>% 
    inner_join(select(Trading_Data, `证券代码`, `交易日期`, Ri, `涨跌停状态`))
  tmp2 <- tmp %>% 
    group_by(`证券代码`, `业绩报告期`) %>% 
    summarise(Volatility = sd(Ri), 
              Skewness = skewness(Ri), 
              Kurtosis = kurtosis(Ri), 
              `涨停次数` = sum(`涨跌停状态` == 1L, na.rm = TRUE), 
              `跌停次数` = sum(`涨跌停状态` == -1L, na.rm = TRUE), 
              `涨跌停次数` = `涨停次数` + `跌停次数`) %>% 
    left_join(select(Report_Data, `证券代码`, `业绩报告期`, `分组` = !!group.by)) %>% 
    na.omit() %>% 
    filter(!is.nan(Volatility), !is.nan(Skewness), !is.nan(Kurtosis)) %>% 
    group_by(`分组`)
  tmp3 <- tmp2 %>% 
    group_by(`分组`) %>% 
    summarise(Volatility = mean(Volatility, na.rm = TRUE), 
              Skewness = mean(Skewness, na.rm = TRUE), 
              Kurtosis = mean(Kurtosis, na.rm = TRUE), 
              `涨停次数` = mean(`涨停次数`, na.rm = TRUE), 
              `跌停次数` = mean(`跌停次数`, na.rm = TRUE), 
              `涨跌停次数` = mean(`涨跌停次数`, na.rm = TRUE))
  
  ## Volatility, Skewness, Kurtosis, 涨停次数, 跌停次数, 张跌停次数的t检验
  Volatility.p <- pairwise.t.test(tmp2$Volatility, tmp2$分组, paired = F, p.adjust.method = "none")$p.value
  Skewness.p <- pairwise.t.test(tmp2$Skewness, tmp2$分组, paired = F, p.adjust.method = "none")$p.value
  Kurtosis.p <- pairwise.t.test(tmp2$Kurtosis, tmp2$分组, paired = F, p.adjust.method = "none")$p.value
  Up_Limit.p <- pairwise.t.test(tmp2$涨停次数, tmp2$分组, paired = F, p.adjust.method = "none")$p.value
  Down_Limit.p <- pairwise.t.test(tmp2$涨停次数, tmp2$分组, paired = F, p.adjust.method = "none")$p.value
  Up_Down.p <- pairwise.t.test(tmp2$涨跌停次数, tmp2$分组, paired = F, p.adjust.method = "none")$p.value
  
  ## 输出数据
  re <- left_join(tmp3, CAR.summarise) %>% 
    bind_cols(tibble(Volatility.p = c(1, Volatility.p[, 1]),
                     Skewness.p = c(1, Skewness.p[, 1]), 
                     Kurtosis.p = c(1, Kurtosis.p[, 1]),
                     Up_Limit.p = c(1, Up_Limit.p[, 1]),
                     Down_Limit.p = c(1, Down_Limit.p[, 1]),
                     Up_Down.p = c(1, Up_Down.p[, 1]),
                     CAR.p = c(1, CAR.p[, 1]))) %>% 
    select(1, 2, 9, 3, 10, 4, 11, 5, 12, 6, 13, 7, 14, 8, 15)
  
  write.xlsx(re, paste0("../Output/Year/", group.by, "[", window.t[1], ", ", window.t[2], "].xlsx"))
  
  return(re)
})



