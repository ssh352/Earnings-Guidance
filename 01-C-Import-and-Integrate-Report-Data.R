# Copyright (c) 2017-2018 by Einzbern

#####----Guidance----#####

options(scipen = 99, digits = 3)
library(tidyverse)
library(readxl)
library(DBI)
library(lubridate)
library(stargazer)

#####----Functions define----#####

read_csmar_data <- function(path, skip = 1, col_types = NULL) {
  ### import the first file of data, return the whole combined dataset, in the case of a seqence of ".xls" data
  ### otherwise, directly import the single data file (for ".csv" and ".txt")
  
  stopifnot(file.exists(path))
  
  col_names <- path %>% 
    dirname() %>% 
    dir(pattern = "\\[DES\\]") %>% 
    `[`(length(.)) %>% 
    file.path(dirname(path), .) %>% 
    readLines(encoding = "UTF-8") %>%
    sub("^.+\\[(.+)\\].+$", "\\1", .)
  
  if (grepl(".xls$", path)) {
    if (is.character(col_types)) col_names <- col_names[col_types != "skip"]
    return(path %>% 
             dirname() %>% 
             dir(pattern = paste0("^", unlist(strsplit(basename(path), "\\."))[1], "[0-9]*\\.xls$")) %>% 
             file.path(dirname(path), .) %>% 
             lapply(read_excel, skip = skip, col_names = col_names, col_types = col_types) %>% 
             bind_rows())
  } else if (grepl(".csv$", path)) {
    if (is.character(col_types)) col_names <- col_names[unlist(strsplit(col_types, "")) != "_"]
    return(read_csv(path, skip = skip, col_names = col_names, col_types = col_types))
  } else if (grepl(".txt$", path)) {
    if (is.character(col_types)) col_names <- col_names[unlist(strsplit(col_types, "")) != "_"]
    return(read_tsv(path, skip = skip, col_names = col_names, col_types = col_types))
  } else {
    stop("unknown file type")
  }
}


#####----读取并合并与实际盈利状况相关的3个数据集（用以判断是否应该发布业绩预告）----#####

# 样本期间2010-12-31 -- 2015-03-31
start_time <- ymd("2010-12-31")
end_time <- ymd("2015-03-31")

# 读取合并报表中的归属母公司净利润的数据
Earnings <- read_csmar_data("../Data/Shanghai_NetEarnings/FS_Comins.xls") %>% 
  rename(`业绩报告期` = `会计期间`) %>% 
  filter(`报表类型` == "A", month(`业绩报告期`) != 1) %>% 
  mutate(`报表类型` = NULL)
  
# 读取合并报表中的归属母公司净利润的增长率数据
Earnings_Increase_Rate <- read_csmar_data("../Data/Shanghai_NetEarnings_Rate/FI_T8.xls", skip = 1) %>% 
  rename(`证券代码` = `股票代码`, `业绩报告期` = `会计年度`, `报表类型` = `报表类型编码`) %>% 
  filter(`报表类型` == "A") %>%
  mutate(`报表类型` = NULL)
  
# 读取合并报表中的每股归属母公司净利润2的数据
Eps <- read_csmar_data("../Data/Shanghai_EPS/FI_T9.xls", skip = 1) %>% 
  rename(`证券代码` = `股票代码`, `业绩报告期` = `截止日期`, `报表类型` = `报表类型编码`) %>% 
  filter(`报表类型` == "A") %>% 
  mutate(`报表类型` = NULL)

# 合并与利润相关的3个数据集
Performance <- list(Earnings, Earnings_Increase_Rate, Eps) %>% 
  reduce(left_join) %>% 
  mutate_at(vars(`业绩报告期`), ymd) %>% 
  filter(between(`业绩报告期`, start_time - years(1), end_time)) %>% 
  arrange(`证券代码`, `业绩报告期`)


#####----根据实际盈利状况判断是否应该发布业绩预告*----#####

# 主要的三条：（1）净利润为负；（2）净利润与上年同期相比上升或下降50%；（3）实现扭亏为盈
# 豁免要求：（1）上一年年度每股收益绝对值低于或等于0.05人民币；
#           （2）上一年半年度每股收益率绝对值低于或等于0.03元人民币；
#           （3）上一年前三季度每股收益率绝对值低于或等于0.04元人民币

## 是否每只股票每一季的报告是连续的，即中间有无缺失
Performance %>% 
  group_by(`证券代码`) %>% 
  transmute(`间隔天数` = c(NA, diff(`业绩报告期`))) %>% 
  with(summary(as.factor(`间隔天数`)))
## 有非连续的季度，所以不能用lag匹配上一期，需要匹配上年同期的数据

# 识别应该发布业绩预告的公司季度
Performance <- Performance %>% 
  group_by(`证券代码`) %>% 
  mutate(`上期归属于母公司所有者的净利润` = `归属于母公司所有者的净利润`[match(`业绩报告期` - years(1), `业绩报告期`)], 
         `满足应该发布的三个条件` = case_when(`归属于母公司所有者的净利润` < 0 ~ 1L, 
                                      `上期归属于母公司所有者的净利润` < 0 ~ 1L, 
                                      abs(`归属于母公司净利润增长率`) >= 0.5 ~ 1L, 
                                      TRUE ~ 0L), 
         `上期归属于母公司每股收益2` = `归属于母公司每股收益2`[match(`业绩报告期` - years(1), `业绩报告期`)], 
         `满足豁免条件` = case_when(month(`业绩报告期`) == 6 & `上期归属于母公司每股收益2` <= 0.03 ~ 1L, 
                              month(`业绩报告期`) == 9 & `上期归属于母公司每股收益2` <= 0.04 ~ 1L, 
                              month(`业绩报告期`) == 12 & `上期归属于母公司每股收益2` <= 0.05 ~ 1L, 
                              TRUE ~ 0L), 
         `应该发布业绩预告` = as.integer(`满足应该发布的三个条件` & !`满足豁免条件`), 
         `应该发布业绩预告` = as.integer(`应该发布业绩预告` & month(`业绩报告期`) == 12)) %>% 
  ungroup() %>% 
  filter(between(`业绩报告期`, start_time, end_time))

## 应该发布业绩预告的统计结果（年度）
Performance %>% 
  filter(month(`业绩报告期`) == 12) %>% 
  summarise(`满足应该发布的三个条件数` = sum(`满足应该发布的三个条件`), 
            `其中满足豁免条件` = sum(`满足应该发布的三个条件` & `满足豁免条件`), 
            `应该发布业绩预告数` = sum(`应该发布业绩预告`))


#####----读取业绩预告的数据，并合并----#####

# 读取并处理业绩预告（季）的数据
Earnings_Forecast <- read_csmar_data("../Data/Shanghai_Forecast_Report/FIN_F_ForecFin.xls", 
                                     col_types = c(rep("guess", 2), "skip", rep("guess", 6), "skip", rep("guess", 2), rep("skip", 13))) %>% 
  mutate_at(vars(`业绩报告期`, `预告日期`), ymd) %>% 
  filter(between(`业绩报告期`, start_time, end_time)) %>% 
  # count(`证券代码`, `业绩报告期`, `预告日期`) %>% 
  # count(n)
  ## 有两个NC公司同一天发了2次业绩预告（600010，2015-03-11；600381，2014-01-30）。一个是预告，一个是更正，但参数一致。
  distinct(`证券代码`, `业绩报告期`, `预告日期`, .keep_all = TRUE) %>% 
  mutate(`业绩来源` = if_else(`业绩来源` == 0, "业绩预告", "定期公告")) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(`业绩预告发布次数` = n(), 
         `最后一次预告日期` = max(`预告日期`)) %>% 
  filter(row_number(`预告日期`) == 1L) %>% 
  arrange(`证券代码`, `业绩报告期`) %>% 
  ungroup()

## 统计业绩预告修正次数
Earnings_Forecast %>% 
  count(`业绩预告发布次数`) %>% 
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "01-T-重复发布业绩预告情况（业绩预告更正）.htm")

# 并将按照实际财务数据判断的必须，与实际发布业绩预告的数据合并
Mandatory_And_Forecast <- Performance %>% 
  left_join(Earnings_Forecast) %>% 
  mutate(`是否发布业绩预告` = if_else(!is.na(`预告日期`), 1L, 0L)) %>% 
  mutate(`发布情况` = case_when(`应该发布业绩预告` & !`是否发布业绩预告` ~ "应该发没发", 
                            `应该发布业绩预告` & `是否发布业绩预告` ~ "应该发发了", 
                            !`应该发布业绩预告` & !`是否发布业绩预告` ~ "不需发没发", 
                            !`应该发布业绩预告` & `是否发布业绩预告` ~ "不需发发了"))

## 应该发布业绩预告与实际发布情况统计
Mandatory_And_Forecast %>% 
  count(`业绩报告期`, `发布情况`) %>% 
  spread(`发布情况`, n, fill = 0) %>% 
  mutate(`业绩报告期` = as.character(`业绩报告期`)) %>%
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "01-T-应该发布业绩预告与实际发布情况统计.htm")


#####----读取业绩快报的数据，并合并----#####

# 读取并处理业绩快报（半年）的数据
Early_Report <- read_csmar_data("../Data/业绩快报（季）上海/FIN_F_QuiTraFin.xls", 
                                col_types = c("guess", "skip", rep("guess", 3), rep("skip", 9), rep("guess", 3), rep("skip", 25))) %>% 
  rename_at(vars(-(`证券代码`:`业绩报告期`)), funs(paste0("快报", .))) %>% 
  mutate_at(vars(`业绩报告期`, `业绩快报披露日`), ymd) %>% 
  filter(between(`业绩报告期`, start_time, end_time)) %>% 
  # count(`证券代码`, `业绩报告期`, `业绩快报披露日`) %>% 
  # count(n)
  ## 无公司同一天发布2次业绩快报
  # distinct(`证券代码`, `业绩报告期`, `业绩快报披露日`, .keep_all = TRUE) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  mutate(`业绩快报发布次数` = n(), 
         `最后一次快报更正披露日` = max(`业绩快报披露日`)) %>% 
  filter(row_number(`业绩快报披露日`) == 1L) %>% 
  arrange(`证券代码`, `业绩报告期`) %>% 
  ungroup()

## 统计业绩快报修正次数
Early_Report %>% 
  count(`业绩快报发布次数`) %>% 
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "01-T-重复发布业绩快报情况（业绩快报更正）.htm")

# 合并数据
Mandatory_And_Forecast <- Mandatory_And_Forecast %>% 
  left_join(Early_Report) %>% 
  mutate(`是否发布业绩快报` = if_else(!is.na(`业绩快报披露日`), 1L, 0L))

## 业绩快报发布情况统计
Mandatory_And_Forecast %>% 
  count(`业绩报告期`, `是否发布业绩快报`) %>% 
  spread(`是否发布业绩快报`, n, fill = 0) %>% 
  mutate(`业绩报告期` = as.character(`业绩报告期`)) %>%
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "01-T-业绩快报发布情况统计.htm")

## 业绩快报和定期报告实际值的差异
Mandatory_And_Forecast %>% 
  ggplot(aes(x = log(`快报归属于母公司所有者的净利润`), y = log(`归属于母公司所有者的净利润`))) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_grey(base_family = "STKaiti")

Mandatory_And_Forecast %>% 
  mutate(`业绩快报与实际值差异(%)的分布` = (`快报归属于母公司所有者的净利润` - `归属于母公司所有者的净利润`) / `归属于母公司所有者的净利润` * 100) %>% 
  filter(abs(`业绩快报与实际值差异(%)的分布`) < 100) %>%   # 有两个极端值
  ggplot(aes(x = `业绩快报与实际值差异(%)的分布`)) + 
  geom_histogram() + 
  theme_grey(base_family = "STKaiti")


#####----读取并合并定期报告的发布时间数据----#####

Mandatory_And_Forecast <- read_csmar_data("../Data/年、中、季报公布日期(上海)/IAR_Rept.xls", skip = 1) %>% 
  select(`证券代码`, `业绩报告期` = `会计截止日期`, `定期报告公布日期` = `报告公布日期`) %>% 
  mutate_at(vars(`业绩报告期`, `定期报告公布日期`), ymd) %>% 
  left_join(Mandatory_And_Forecast, .) %>% 
  mutate(`业绩报告第一时间` = if_else(as.logical(`是否发布业绩快报`), `业绩快报披露日`, `定期报告公布日期`))


#####----读取行业数据（是否金融行业；SQL），并合并----#####

conn <- dbConnect(RSQLServer::SQLServer(), 
                  server = "124.65.136.170", 
                  database = "Wind_FS", 
                  properties = list(user = "Reuters", password = "ucas2017"))
Industries_Class <- dbGetQuery(conn, "SELECT S_INFO_WINDCODE, SEC_IND_CODE FROM ASHARESECNINDUSTRIESCLASS WHERE CUR_SIGN = 1")
dbDisconnect(conn)

# 金融行业以“1210”开头
Mandatory_And_Forecast <- Industries_Class %>% 
  as_tibble() %>% 
  transmute(`证券代码` = substr(S_INFO_WINDCODE, 1, 6), 
            `是否金融行业` = if_else(substr(SEC_IND_CODE, 1, 4) == "1210", 1L, 0L)) %>% 
  left_join(Mandatory_And_Forecast, .)


#####----读取分析师预测数据（SQL），并合并----#####

conn <- dbConnect(RSQLServer::SQLServer(), 
                  server = "124.65.136.170", 
                  database = "Wind_FS", 
                  properties = list(user = "Reuters", password = "ucas2017"))

# 只取分析师报告中对第一年的预测
Analyst_Forecast <- dbGetQuery(conn, "
SELECT S_INFO_WINDCODE, ANALYST_NAME, EST_DT, REPORTING_PERIOD, EST_NET_PROFIT 
                               FROM 
                               (
                               SELECT S_INFO_WINDCODE, ANALYST_NAME, EST_DT, REPORTING_PERIOD, EST_NET_PROFIT, 
                               ROW_NUMBER() OVER (PARTITION BY S_INFO_WINDCODE, ANALYST_NAME, EST_DT ORDER BY REPORTING_PERIOD ASC) NUM
                               FROM AShareEarningEst
                               ) T
                               WHERE NUM = 1
                               ")

dbDisconnect(conn)

Mandatory_And_Forecast <- Analyst_Forecast %>% 
  as_tibble() %>% 
  na.omit() %>% 
  rename(`证券代码` = S_INFO_WINDCODE, `分析师名称` = ANALYST_NAME, `预测日期` = EST_DT, 
         `业绩报告期` = REPORTING_PERIOD, `预测净利润` = EST_NET_PROFIT) %>% 
  mutate_at(vars(`预测日期`, `业绩报告期`), ymd) %>% 
  filter(between(`业绩报告期`, start_time, end_time)) %>% 
  mutate(`证券代码` = substr(`证券代码`, 1, 6), `预测净利润` = `预测净利润` * 10000) %>%   # 预测净利润原单位为万元
  arrange(`证券代码`, `业绩报告期`, `分析师名称`, `预测日期`) %>% 
  inner_join(select(Mandatory_And_Forecast, `证券代码`, `业绩报告期`, `预告日期`)) %>% 
  filter(`预测日期` >= `预告日期` - days(90), `预测日期` <= `预告日期`) %>% 
  group_by(`证券代码`, `业绩报告期`, `分析师名称`) %>% 
  filter(row_number(`预测日期`) == n()) %>%   # 只取每个分析师最后一次预测
  group_by(`证券代码`, `业绩报告期`) %>% 
  summarise(`业绩预告前分析师预测净利润` = mean(`预测净利润`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(Mandatory_And_Forecast, .) %>% 
  mutate(`预告归属于母公司所有者的净利润（下限）` = if_else(is.na(`预告归属于母公司所有者的净利润（下限）`), `上期归属于母公司所有者的净利润` * (1 + `预告归属于母公司所有者的净利润变动幅度（下限）%` / 100), `预告归属于母公司所有者的净利润（下限）`), 
         `预告归属于母公司所有者的净利润（上限）` = if_else(is.na(`预告归属于母公司所有者的净利润（上限）`), `上期归属于母公司所有者的净利润` * (1 + `预告归属于母公司所有者的净利润变动幅度（上限）%` / 100), `预告归属于母公司所有者的净利润（上限）`), 
         `预告归属于母公司所有者的净利润（下限）` = if_else(is.na(`预告归属于母公司所有者的净利润（下限）`) & !is.na(`预告归属于母公司所有者的净利润（上限）`), -Inf, `预告归属于母公司所有者的净利润（下限）`), 
         `预告归属于母公司所有者的净利润（上限）` = if_else(is.na(`预告归属于母公司所有者的净利润（上限）`) & !is.na(`预告归属于母公司所有者的净利润（下限）`), Inf, `预告归属于母公司所有者的净利润（上限）`), 
         `业绩预告与分析师预测` = case_when((`业绩预告前分析师预测净利润` >= `预告归属于母公司所有者的净利润（下限）`) & (`业绩预告前分析师预测净利润` < `预告归属于母公司所有者的净利润（上限）`) ~ "符合分析师预测", 
                                  `业绩预告前分析师预测净利润` < `预告归属于母公司所有者的净利润（下限）` ~ "高于分析师预测", 
                                  `业绩预告前分析师预测净利润` > `预告归属于母公司所有者的净利润（上限）` ~ "低于分析师预测"))


#####----比较业绩预告与定期报告----#####

Mandatory_And_Forecast <- Mandatory_And_Forecast %>% 
  mutate(`归属于母公司所有者的净利润.第一时间` = if_else(!is.na(`快报归属于母公司所有者的净利润`), `快报归属于母公司所有者的净利润`, `归属于母公司所有者的净利润`), 
         `业绩预告准确性离散型` = case_when((`归属于母公司所有者的净利润.第一时间` >= `预告归属于母公司所有者的净利润（下限）`) & (`归属于母公司所有者的净利润.第一时间` < `预告归属于母公司所有者的净利润（上限）`) ~ "符合定期报告", 
                                  `归属于母公司所有者的净利润.第一时间` < `预告归属于母公司所有者的净利润（下限）` ~ "高于定期报告", 
                                  `归属于母公司所有者的净利润.第一时间` > `预告归属于母公司所有者的净利润（上限）` ~ "低于定期报告"), 
         `业绩预告准确性连续型` = case_when((`归属于母公司所有者的净利润.第一时间` >= `预告归属于母公司所有者的净利润（下限）`) & (`归属于母公司所有者的净利润.第一时间` < `预告归属于母公司所有者的净利润（上限）`) ~ 0, 
                                  `归属于母公司所有者的净利润.第一时间` < `预告归属于母公司所有者的净利润（下限）` ~ (`预告归属于母公司所有者的净利润（下限）` - `归属于母公司所有者的净利润.第一时间`) / `归属于母公司所有者的净利润.第一时间`, 
                                  `归属于母公司所有者的净利润.第一时间` > `预告归属于母公司所有者的净利润（上限）` ~ (`预告归属于母公司所有者的净利润（上限）` - `归属于母公司所有者的净利润.第一时间`) / `归属于母公司所有者的净利润.第一时间`))

# ## dfdfdfwefsvrgsje
# Mandatory_And_Forecast %>% 
#   filter(as.logical(`是否发布业绩预告`)) %>% 
#   filter(abs(`业绩预告准确性连续型`) < 10) %>% 
#   ggplot(aes(x = `业绩预告准确性连续型`, fill = as.factor(`应该发布业绩预告`))) + 
#   geom_histogram(position = "dodge")
# hist(Mandatory_And_Forecast$业绩预告准确性连续型)
# boxplot(Mandatory_And_Forecast$业绩预告准确性连续型)
# quantile(Mandatory_And_Forecast$业绩预告准确性连续型, probs = seq(0, 1, by = 0.025), na.rm = TRUE)


#####----End----#####

save(read_csmar_data, start_time, end_time, Mandatory_And_Forecast, file = "01-D-Mandatory-and-Forecast.RData")
