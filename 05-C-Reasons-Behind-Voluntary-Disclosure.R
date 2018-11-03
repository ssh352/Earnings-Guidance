# Copyright Reserved

# This R source file analyze when and why to release earnings forecast

#####----Guidance----#####

load("../Output/02-D-Trading-Data.RData")
load("../Output/01-D-Report-Data.RData")
library(tidyverse)
library(lubridate)
library(msm)
options(digits = 3)


# 只要年报
Report_Data <- Report_Data %>% 
  filter(month(`业绩报告期`) == 12)

#####----各类报告发布时间分布----#####

## 发布业绩快报比例
Report_Data %>% 
  count(`是否发布业绩快报`) %>% 
  mutate(`%` = n / sum(n) * 100)
## 16.8%发了业绩快报

## 定期报告公布日期的分布-1
Report_Data %>% 
  mutate(`定期报告公布日期_月日` = `year<-`(`定期报告公布日期`, 0000)) %>% 
  # count(month(`定期报告公布日期_月日`))
  # ## 有4个5月份之后发年报的样本点，以及26个缺失值
  filter(month(`定期报告公布日期_月日`) <= 4) %>% 
  ggplot(aes(x = `定期报告公布日期_月日`)) + 
  geom_histogram(binwidth = 1) + 
  theme_grey(base_family = "STKaiti")
## 定期报告（年报）集中在3、4月份分布，且每月下旬较上旬呈递增趋势

## 3、4月和行业的关系-2
Report_Data %>% 
  filter(month(`定期报告公布日期`) %in% c(3, 4)) %>% 
  group_by(year(`业绩报告期`), `行业代码`) %>% 
  filter(n() > 10) %>% 
  ggplot(aes(x = `行业代码`, fill = factor(month(`定期报告公布日期`)))) + 
  geom_bar(position = "fill") + 
  facet_wrap( ~ year(`业绩报告期`)) + 
  theme_grey(base_family = "STKaiti") + 
  theme(axis.text.x = element_text(angle = -90))
## 无关系

## 行业集中度（箱线图）-3
Report_Data %>% 
  mutate(`定期报告公布日期_月日` = `year<-`(`定期报告公布日期`, 0000)) %>% 
  group_by(year(`业绩报告期`), `行业代码`) %>% 
  filter(n() > 10) %>% 
  ggplot(aes(x = `行业代码`, y = `定期报告公布日期_月日`)) + 
  geom_boxplot() + 
  facet_wrap( ~ year(`业绩报告期`)) +
  theme_grey(base_family = "STKaiti") + 
  theme(axis.text.x = element_text(angle = -90))
## 并无发现行业集聚现象

## 公司是否倾向于在固定的月份发布定期报告-4
Report_Data %>% 
  statetable.msm(month(`定期报告公布日期`), `证券代码`, data = .)
## 是有这个倾向


## 业绩报告_第一时间的分布-1
Report_Data %>% 
  mutate(`业绩报告_第一时间_月日` = `year<-`(`业绩报告_第一时间`, 0000)) %>% 
  # count(month(`业绩报告_第一时间_月日`))
  # ## 还是有3个5月份之后的
  filter(month(`业绩报告_第一时间_月日`) <= 4) %>% 
  ggplot(aes(x = `业绩报告_第一时间_月日`)) + 
  geom_histogram(binwidth = 1) + 
  theme_grey(base_family = "STKaiti")
## 部分3、4月份发布定期报告的公司选择在1、2月份先发布业绩快报
## 整体而言，业绩报告依然集中在3、4月份


## 业绩预告日期的分布-1
Report_Data %>% 
  mutate(`业绩预告日期_月日` = `year<-`(`业绩预告日期`, 0000)) %>% 
  # count(month(`业绩预告日期_月日`))
  ggplot(aes(x = `业绩预告日期_月日`)) + 
  geom_histogram(binwidth = 10) + 
  theme_grey(base_family = "STKaiti")
## 业绩预告集中在1月和上年10月

## 更具体的月内分布-2
Report_Data %>% 
  filter(month(`业绩预告日期`) %in% c(1, 10)) %>% 
  ggplot(aes(x = day(`业绩预告日期`), fill = `业绩预告发布情况_4类`)) + 
  geom_bar(position = "fill") + 
  facet_wrap( ~ month(`业绩预告日期`)) + 
  theme_grey(base_family = "STKaiti")
## 月内也是呈递增的趋势

## 那是否1月和上年10月依行业而不同-3
Report_Data %>% 
  filter(month(`业绩预告日期`) %in% c(1, 10)) %>% 
  group_by(year(`业绩报告期`), `行业代码`) %>% 
  filter(n() > 10) %>% 
  ggplot(aes(x = `行业代码`, fill = factor(month(`业绩预告日期`)))) + 
  geom_bar(position = "fill") + 
  facet_wrap( ~ year(`业绩报告期`)) + 
  theme_grey(base_family = "STKaiti") + 
  theme(axis.text.x = element_text(angle = -90))
## 未发现

## 公司是否倾向于在固定的月份发布定期报告-4
Report_Data %>% 
  statetable.msm(month(`业绩预告日期`), `证券代码`, data = .)
## 有一定倾向

## 几月发布定期报告和几月发布业绩预告有无关系-5
Report_Data %>% 
  filter(month(`业绩预告日期`) %in% c(1, 10), 
         month(`定期报告公布日期`) %in% c(3, 4)) %>% 
  with(table(month(`业绩预告日期`), month(`定期报告公布日期`)))
## 无

## 1月和上年10月和是否自愿发布有无关系
Report_Data %>% 
  filter(month(`业绩预告日期`) %in% c(1, 10)) %>% 
  ggplot(aes(x = `业绩预告发布情况_4类`, fill = factor(month(`业绩预告日期`)))) + 
  geom_bar(position = "fill") + 
  facet_wrap( ~ year(`业绩报告期`)) + 
  theme_grey(base_family = "STKaiti") + 
  theme(axis.text.x = element_text(angle = -90))

Report_Data %>% 
  filter(month(`业绩预告日期`) %in% c(1, 10)) %>% 
  with(table(`业绩预告发布情况_4类`, month(`业绩预告日期`))) %>% 
  chisq.test()
## 自愿发布的有10月发布的特征

## 同月内是否自愿发布跟随应该发布
Report_Data %>% 
  group_by(`证券代码`) %>% 
  mutate(`上期业绩预告发布情况_4类` = `业绩预告发布情况_4类`[match(`业绩报告期` - years(1), `业绩报告期`)]) %>% 
  mutate(Index = if_else(`上期业绩预告发布情况_4类` == "不需发发了" & `业绩预告发布情况_4类` == "不需发发了", 1, 0)) %>% 
  filter(month(`业绩预告日期`) %in% c(1, 10), 
         Index == 0) %>% 
  group_by(year(`业绩报告期`), month(`业绩预告日期`), `行业代码`, `业绩预告发布情况_4类`) %>% 
  summarise(`业绩预告日` = median(day(`业绩预告日期`))) %>% 
  spread(`业绩预告发布情况_4类`, `业绩预告日`) %>% 
  ggplot(aes(x = `不需发发了`, y = `应该发发了`)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_grid(`month(业绩预告日期)` ~ `year(业绩报告期)`) + 
  theme_grey(base_family = "STKaiti")
## 否

## 公司是否倾向于一直发布业绩报告
Report_Data %>% 
  statetable.msm(`业绩预告发布情况_4类`, `证券代码`, data = .)
## 有一定倾向

## 同一个公司同样不需要发布什么时候发什么时候不发
Report_Data %>% 
  count(`证券代码`, `业绩预告发布情况_4类`) %>% 
  spread(`业绩预告发布情况_4类`, n, fill = 0L) %>% 
  filter((`不需发发了` + `不需发没发`) > 2) %>% 
  filter(`不需发没发` == 0L)









# 业绩预告日期距离业绩报告日期时间的分布
Report_Data %>% 
  mutate(`业绩预告距离业绩报告的月数` = (`业绩报告_第一时间` - `业绩预告日期`) / 30) %>% 
  filter(`业绩预告距离业绩报告的月数` <= 7) %>%
  # glimpse() %>% 
  ggplot(aes(x = `业绩预告距离业绩报告的月数`)) + 
  # geom_histogram(binwidth = 1 / 30) + 
  geom_histogram(aes(fill = `业绩预告发布情况_4类`), binwidth = 1 / 30, position = "fill") + 
  theme_grey(base_family = "STKaiti")


#####----End----#####

save.image("../Output/05-D-Results.RData")
