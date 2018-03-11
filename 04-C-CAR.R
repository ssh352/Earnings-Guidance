# Copyright (c) 2018 by Einzbern
# 目的1：计算 "不需要发发了” 与 "需要发发了" 两类从发业绩预告开始之后一年的市场表现
# 目的2：计算以定期报告为t=0，从t=-100天开始算到t=50天的CAR

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

Report_Data <- Report_Data %>%
  mutate(`业绩预告发布情况_4类` = if_else(`业绩预告发布情况_4类` == "不需发没发", "A不需发没发", `业绩预告发布情况_4类`)) %>% 
  unite(`业绩预告情况_10类`, `业绩预告发布情况_4类`, `业绩预告与业绩报告比较_离散型`, remove = FALSE)
table(Report_Data$`业绩预告情况_10类`)
table(Report_Data$`业绩预告发布情况_4类`)

#####----目的1的CAR----#####

estimation_window <- c(-240, -60)  # 日历日，未注明同
announcement_window <- c(0, 500)
least_valid_days <- 63  # 交易日
event <- "业绩预告日期"

group.by <- "业绩预告发布情况_4类" 

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

## 下面两种对各组平均CAR的计算方法按照道理讲应该得到一样的结果，但是不一样，
## 我还没查出原因在哪里，所以选取最保险的那种算法。因为我总觉得第一种算法是
## 因为t的排序导致的。

# AR_grouped <- AR %>% 
#   left_join(select(Report_Data, `证券代码`, `业绩报告期`, `分组` = !!group.by)) %>% 
#   group_by(`证券代码`, `业绩报告期`) %>% 
#   mutate(group.num = n()) %>% 
#   filter(group.num > 260) %>% 
#   mutate(CAR = cumsum(AR)) %>% 
#   group_by(`分组`, t) %>% 
#   summarise(CAR.mean = mean(CAR), CAR.mean.trim = mean(CAR, trim = 0.01))

##
AR_grouped <- AR %>% 
  left_join(select(Report_Data, `证券代码`, `业绩报告期`, `分组` = !!group.by))

n1.1 <- nrow(AR_grouped)

AR_grouped <- AR_grouped %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(group.num = n()) %>% 
  filter(group.num > 260) 

n1.2 <- nrow(AR_grouped)

AR_grouped <- AR_grouped %>% 
  group_by(`分组`, t) %>% 
  summarise(AR.mean = mean(AR), AR.mean.trim = mean(AR, trim = 0.01)) %>% 
  mutate(CAR.mean = cumsum(AR.mean), CAR.mean.trim = cumsum(AR.mean.trim))

## 画CAR的图
## png(filename = "../Output/事件点是业绩预告时间.Mean.png")

ggplot(AR_grouped, aes(x = t, y = CAR.mean, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩预告时间.Mean") +
  xlim(0, 260)

## dev.off()

##
## png(filename = "../Output/事件点是业绩预告时间.Mean.Trim.png")

ggplot(AR_grouped, aes(x = t, y = CAR.mean.trim, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩预告时间.Mean.Trim")+
  xlim(0, 260)

## dev.off()

#####----目的2：是不是 "不需发发了" 降低的天数对应定期发的日子----#####

## 先画出定期报告时间与业绩预告时间的时间差的分布
Reason <- Report_Data %>% 
  filter(`业绩预告发布情况_4类` == "不需发发了" | `业绩预告发布情况_4类` == "应该发发了") %>% 
  mutate(`时间差` = `业绩报告_第一时间` - `业绩预告日期`, 
         `时间差1` = `业绩报告_第一时间` - `业绩预告日期_最后一次`)

## png(filename = "../Output/定期报告与业绩预告时间差.png")

ggplot(Reason, aes(x = `时间差`, color = `业绩预告发布情况_4类`)) + 
  geom_freqpoly(size = 1)

## dev.off()

#####----计算以定期报告为t=0，从t=-300天开始算到t=100天的CAR----#####

estimation_window.regular <- c(-440, -260)  # 日历日，未注明同
announcement_window.regular <- c(-200, 100)
least_valid_days.regular <- 63  # 交易日
event.regular <- "业绩报告_第一时间"

AR.regular <- Report_Data %>% 
  select(`证券代码`, `业绩报告期`, `事件日期` = !!event.regular) %>% 
  na.omit() %>% 
  group_by(`证券代码`, `业绩报告期`, `事件日期`) %>% 
  do(tibble(`交易日期` = c(seq(from = .$`事件日期` + days(!!estimation_window.regular[1]), to = .$`事件日期` + days(!!estimation_window.regular[2]), by = "day"), 
                       seq(from = .$`事件日期` + days(!!announcement_window.regular[1]), to = .$`事件日期` + days(!!announcement_window.regular[2]), by = "day")), 
            Used_for_Estimation = c(rep(TRUE, diff(!!estimation_window.regular) + 1), rep(FALSE, diff(!!announcement_window.regular) + 1)))) %>% 
  inner_join(select(Trading_Data, -`成交额`, -Ri, -Rm, -Rf, -`涨跌停状态`)) %>% 
  filter(sum(Used_for_Estimation) >= !!least_valid_days.regular) %>% 
  do(calculate_ERiRf(.)) %>% 
  arrange(`证券代码`, `业绩报告期`, `事件日期`, `交易日期`) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(t = calculate_t(`事件日期`[1], `交易日期`), AR = RiRf - ERiRf)

##
AR_grouped.regular <- AR.regular %>% 
  left_join(select(Report_Data, `证券代码`, `业绩报告期`, `分组` = !!group.by))

n2.1 <- nrow(AR_grouped.regular)

AR_grouped.regular <- AR_grouped.regular %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(group.num = n(), group.num1 = sum(t < 0), group.num2 = sum(t > 0)) %>% 
  filter((group.num1 > 99) & (group.num2 > 50))

n2.2 <- nrow(AR_grouped.regular)

AR_grouped.regular <- AR_grouped.regular %>% 
  group_by(`分组`, t) %>% 
  summarise(AR.mean = mean(AR), AR.mean.trim = mean(AR, trim = 0.01)) %>% 
  mutate(CAR.mean = cumsum(AR.mean), CAR.mean.trim = cumsum(AR.mean.trim))

## 画CAR的图
## png(filename = "../Output/事件点是业绩报告_第一时间.Mean.png")

ggplot(AR_grouped.regular, aes(x = t, y = CAR.mean, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩报告_第一时间.Mean") +
  xlim(-100, 50)

## dev.off()

##
## png(filename = "../Output/事件点是业绩报告_第一时间.Mean.Trim.png")

ggplot(AR_grouped.regular, aes(x = t, y = CAR.mean.trim, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩报告_第一时间.Mean.Trim")+
  xlim(-100, 50)

## dev.off()

#####----目的3：看 "不需发发了"，"应该发发了" 里面细分的子类的表现----#####

group.by.sub <- "业绩预告情况_10类" 

##
AR_grouped.sub <- AR %>% 
  left_join(select(Report_Data, `证券代码`, `业绩报告期`, `分组` = !!group.by.sub))

n3.1 <- nrow(AR_grouped.sub)

AR_grouped.sub <- AR_grouped.sub %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(group.num = n()) %>% 
  filter(group.num > 260) 

n3.2 <- nrow(AR_grouped.sub)

AR_grouped.sub <- AR_grouped.sub %>% 
  group_by(`分组`, t) %>% 
  summarise(AR.mean = mean(AR), AR.mean.trim = mean(AR, trim = 0.01)) %>% 
  mutate(CAR.mean = cumsum(AR.mean), CAR.mean.trim = cumsum(AR.mean.trim))

AR_grouped.sub <- rbind(AR_grouped.sub, AR_grouped)

## 画CAR的图
## png(filename = "../Output/事件点是业绩预告时间.Mean.png")

AR_grouped.sub1 <- AR_grouped.sub %>% 
  filter(`分组`  %in% c("不需发发了", 
                      "应该发发了", 
                      "不需发发了_NA",
                      "不需发发了_业绩预告低于业绩报告",
                      "不需发发了_业绩预告符合业绩报告",
                      "不需发发了_业绩预告高于业绩报告"))

ggplot(AR_grouped.sub1, aes(x = t, y = CAR.mean, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩预告时间.Mean") +
  xlim(0, 260)

## dev.off()

##
## png(filename = "../Output/事件点是业绩预告时间.Mean.Trim.png")

AR_grouped.sub2 <- AR_grouped.sub %>% 
  filter(`分组`  %in% c("不需发发了", 
                      "应该发发了", 
                      "应该发发了_NA",
                      "应该发发了_业绩预告低于业绩报告",
                      "应该发发了_业绩预告符合业绩报告",
                      "应该发发了_业绩预告高于业绩报告"))

ggplot(AR_grouped.sub2, aes(x = t, y = CAR.mean, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩预告时间.Mean") +
  xlim(0, 260)

## dev.off()

AR_grouped.sub3 <- AR_grouped.sub %>% 
  filter(`分组`  %in% c("不需发发了_NA",
                      "不需发发了_业绩预告低于业绩报告",
                      "不需发发了_业绩预告符合业绩报告",
                      "不需发发了_业绩预告高于业绩报告"))
  
ggplot(AR_grouped.sub3, aes(x = t, y = CAR.mean, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩预告时间.Mean") +
  xlim(0, 260)

##

AR_grouped.sub4 <- AR_grouped.sub %>% 
  filter(`分组`  %in% c("应该发发了_NA",
                      "应该发发了_业绩预告低于业绩报告",
                      "应该发发了_业绩预告符合业绩报告",
                      "应该发发了_业绩预告高于业绩报告"))
  
ggplot(AR_grouped.sub4, aes(x = t, y = CAR.mean, group = `分组`, color = `分组`)) + 
  geom_line(size = 1) + 
  theme_grey(base_family = "STKaiti") +
  labs(title = "事件点是业绩预告时间.Mean") +
  xlim(0, 260)
  

#####----Ending----#####

save.image("../Output/04-D-CAR-Data.RData")

