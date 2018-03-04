# Copyright Reserved

# The 1st R source file imports and integrates all necessary REPORT relevant data

#####----Guidance----#####

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
# 开始时间2010年年报，是因为之前数据库（CSMAR）中没有业绩预告详细数据，只有一个简报
# 结束时间2015年一季，是因为之后更改了业绩预告披露规则
start_time <- ymd("2010-12-31")
end_time <- ymd("2015-03-31")

# 读取合并报表中的归属母公司净利润的数据
Earnings <- read_csmar_data("../Data/01-净利润-上海/FS_Comins.xls") %>% 
  bind_rows(read_csmar_data("../Data/01-净利润-深圳-主板/FS_Comins.xls")) %>% 
  rename(`业绩报告期` = `会计期间`) %>% 
  filter(`报表类型` == "A", month(`业绩报告期`) != 1) %>% 
  mutate(`报表类型` = NULL)
  
# 读取合并报表中的归属母公司净利润的增长率数据
Earnings_Increase_Rate <- read_csmar_data("../Data/02-净利润增长率-上海/FI_T8.xls") %>% 
  bind_rows(read_csmar_data("../Data/02-净利润增长率-深圳-主板/FI_T8.xls")) %>% 
  rename(`证券代码` = `股票代码`, `业绩报告期` = `会计年度`, `报表类型` = `报表类型编码`) %>% 
  filter(`报表类型` == "A") %>%
  mutate(`报表类型` = NULL)
  
# 读取合并报表中的归属于母公司每股收益2的数据
EPS <- read_csmar_data("../Data/03-EPS-上海/FI_T9.xls") %>% 
  bind_rows(read_csmar_data("../Data/03-EPS-深圳-主板/FI_T9.xls")) %>% 
  rename(`证券代码` = `股票代码`, `业绩报告期` = `截止日期`, `报表类型` = `报表类型编码`) %>% 
  filter(`报表类型` == "A") %>% 
  mutate(`报表类型` = NULL)

# 合并与利润相关的3个数据集
Report_Data <- list(Earnings, Earnings_Increase_Rate, EPS) %>% 
  reduce(left_join) %>% 
  mutate_at(vars(`业绩报告期`), ymd) %>% 
  filter(between(`业绩报告期`, start_time - years(1), end_time)) %>% 
  arrange(`证券代码`, `业绩报告期`)


#####----根据实际盈利状况判断是否应该发布业绩预告*----#####

# 主要的三条：（1）净利润为负；
#             （2）净利润与上年同期相比上升或下降50%；
#             （3）实现扭亏为盈
# 豁免要求：（1）上一年年度每股收益绝对值低于或等于0.05人民币；
#           （2）上一年半年度每股收益率绝对值低于或等于0.03元人民币；
#           （3）上一年前三季度每股收益率绝对值低于或等于0.04元人民币

## 是否每只股票每一季的报告是连续的，即中间有无缺失
Report_Data %>% 
  group_by(`证券代码`) %>% 
  transmute(`间隔天数` = c(NA, diff(`业绩报告期`))) %>% 
  with(summary(as.factor(`间隔天数`)))
## 有非连续的季度，所以不能用lag匹配上一期，需要匹配上年同期的数据

# 识别应该发布业绩预告的公司季度
Report_Data <- Report_Data %>% 
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
         `应该发布业绩预告` = as.integer(`满足应该发布的三个条件` & month(`业绩报告期`) != 3 & !`满足豁免条件`), 
         # 区分上交所和深交所
         `应该发布业绩预告` = if_else(`应该发布业绩预告` & substr(`证券代码`, 1, 1) == "6" & month(`业绩报告期`) != 12, 0L, `应该发布业绩预告`)) %>% 
  ungroup() %>% 
  filter(between(`业绩报告期`, start_time, end_time))

## 应该发布业绩预告的统计结果（年度）
Report_Data %>% 
  filter(month(`业绩报告期`) == 12) %>% 
  summarise(`满足应该发布的三个条件数` = sum(`满足应该发布的三个条件`), 
            `其中满足豁免条件` = sum(`满足应该发布的三个条件` & `满足豁免条件`), 
            `应该发布业绩预告数` = sum(`应该发布业绩预告`))


#####----读取业绩预告的数据，并合并----#####

# 读取并处理业绩预告的数据
Earnings_Forecast <- read_csmar_data("../Data/04-业绩预告-上海/FIN_F_ForecFin.xls", col_types = c(rep("guess", 2), "skip", rep("guess", 6), "skip", rep("guess", 2), rep("skip", 13))) %>% 
  bind_rows(read_csmar_data("../Data/04-业绩预告-深圳-主板/FIN_F_ForecFin.xls", col_types = c(rep("guess", 2), "skip", rep("guess", 6), "skip", rep("guess", 2), rep("skip", 13)))) %>% 
  rename(`业绩预告来源` = `业绩来源`) %>% 
  rename_at(vars(starts_with("预告")), funs(paste0("业绩", .))) %>% 
  mutate_at(vars(`业绩报告期`, `业绩预告日期`), ymd) %>% 
  filter(between(`业绩报告期`, start_time, end_time)) %>% 
  # count(`证券代码`, `业绩报告期`, `业绩预告日期`) %>% 
  # count(n)
  ## 有两个NC公司同一天发了2次业绩预告（600010，2015-03-11；600381，2014-01-30）。一个是预告，一个是更正，但参数一致。
  distinct(`证券代码`, `业绩报告期`, `业绩预告日期`, .keep_all = TRUE) %>% 
  mutate(`业绩预告来源` = if_else(`业绩预告来源` == 0, "业绩预告", "定期公告")) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  # 若同一报告期发布过多次业绩预告（更正），添加业绩预告发布次数和最后一次预告日期，其余内容保留第一次的
  mutate(`业绩预告发布次数` = n(), 
         `业绩预告日期_最后一次` = max(`业绩预告日期`)) %>% 
  filter(row_number(`业绩预告日期`) == 1L) %>% 
  arrange(`证券代码`, `业绩报告期`) %>% 
  ungroup()

## 统计业绩预告修正次数
Earnings_Forecast %>% 
  count(`业绩预告发布次数`) %>% 
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "../Output/01-T-重复发布业绩预告情况（业绩预告更正）.htm")

# 并将按照实际财务数据判断的必须，与实际发布业绩预告的数据合并
Report_Data <- Report_Data %>% 
  left_join(Earnings_Forecast) %>% 
  mutate(`是否发布业绩预告` = if_else(!is.na(`业绩预告日期`), 1L, 0L)) %>% 
  mutate(`业绩预告发布情况_4类` = case_when(`应该发布业绩预告` & !`是否发布业绩预告` ~ "应该发没发", 
                            `应该发布业绩预告` & `是否发布业绩预告` ~ "应该发发了", 
                            !`应该发布业绩预告` & !`是否发布业绩预告` ~ "不需发没发", 
                            !`应该发布业绩预告` & `是否发布业绩预告` ~ "不需发发了"))

## 业绩预告发布情况_4类 统计
Report_Data %>% 
  count(`业绩报告期`, `业绩预告发布情况_4类`) %>% 
  spread(`业绩预告发布情况_4类`, n, fill = 0) %>% 
  mutate_at(vars(`业绩报告期`), as.character) %>%
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "../Output/01-T-应该发布业绩预告与实际发布情况统计.htm")


#####----读取业绩快报的数据，并合并----#####

# 读取并处理业绩快报的数据
Early_Announcement <- read_csmar_data("../Data/05-业绩快报-上海/FIN_F_QuiTraFin.xls", col_types = c("guess", "skip", rep("guess", 3), rep("skip", 9), rep("guess", 3), rep("skip", 25))) %>% 
  bind_rows(read_csmar_data("../Data/05-业绩快报-深圳-主板/FIN_F_QuiTraFin.xls", col_types = c("guess", "skip", rep("guess", 3), rep("skip", 9), rep("guess", 3), rep("skip", 25)))) %>% 
  rename_at(vars(-(`证券代码`:`业绩报告期`)), funs(paste0("业绩快报", .))) %>% 
  mutate_at(vars(`业绩报告期`, `业绩快报披露日`), ymd) %>% 
  filter(between(`业绩报告期`, start_time, end_time)) %>% 
  # count(`证券代码`, `业绩报告期`, `业绩快报披露日`) %>% 
  # count(n)
  ## 无公司同一天发布2次业绩快报
  # distinct(`证券代码`, `业绩报告期`, `业绩快报披露日`, .keep_all = TRUE) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  # 若同一报告期发布过多次业绩快报（更正），添加业绩快报发布次数和最后一次快报日期，其余内容保留第一次的
  mutate(`业绩快报发布次数` = n(), 
         `业绩快报披露日_最后一次` = max(`业绩快报披露日`)) %>% 
  filter(row_number(`业绩快报披露日`) == 1L) %>% 
  arrange(`证券代码`, `业绩报告期`) %>% 
  ungroup()

## 统计业绩快报修正次数
Early_Announcement %>% 
  count(`业绩快报发布次数`) %>% 
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "../Output/01-T-重复发布业绩快报情况（业绩快报更正）.htm")

# 合并数据
Report_Data <- Report_Data %>% 
  left_join(Early_Announcement) %>% 
  mutate(`是否发布业绩快报` = if_else(!is.na(`业绩快报披露日`), 1L, 0L))

## 业绩快报发布情况统计
Report_Data %>% 
  count(`业绩报告期`, `是否发布业绩快报`) %>% 
  spread(`是否发布业绩快报`, n, fill = 0) %>% 
  mutate_at(vars(`业绩报告期`), as.character) %>%
  stargazer(summary = FALSE, rownames = FALSE, type = "html", out = "../Output/01-T-业绩快报发布情况统计.htm")

## 业绩快报和定期报告实际值的差异
Report_Data %>% 
  ggplot(aes(x = log(`业绩快报归属于母公司所有者的净利润`), y = log(`归属于母公司所有者的净利润`))) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_grey(base_family = "STKaiti")

Report_Data %>% 
  mutate(`业绩快报与实际值差异(%)的分布` = (`业绩快报归属于母公司所有者的净利润` - `归属于母公司所有者的净利润`) / `归属于母公司所有者的净利润` * 100) %>% 
  filter(abs(`业绩快报与实际值差异(%)的分布`) < 100) %>%   # 有两个极端值
  ggplot(aes(x = `业绩快报与实际值差异(%)的分布`)) + 
  geom_histogram() + 
  theme_grey(base_family = "STKaiti")


#####----读取并合并定期报告的发布时间数据----#####

Report_Data <- read_csmar_data("../Data/06-定期报告公布日期-上海/IAR_Rept.xls") %>% 
  bind_rows(read_csmar_data("../Data/06-定期报告公布日期-深圳-主板/IAR_Rept.xls")) %>% 
  select(`证券代码`, `业绩报告期` = `会计截止日期`, `定期报告公布日期` = `报告公布日期`) %>% 
  mutate_at(vars(`业绩报告期`, `定期报告公布日期`), ymd) %>% 
  left_join(Report_Data, .) %>% 
  mutate(`业绩报告_第一时间` = if_else(as.logical(`是否发布业绩快报`), `业绩快报披露日`, `定期报告公布日期`))


#####----读取行业数据（是否金融行业；SQL），并合并----#####

conn <- dbConnect(RSQLServer::SQLServer(), 
                  server = "124.65.136.170", 
                  database = "Wind_FS", 
                  properties = list(user = "Reuters", password = "ucas2017"))
Industry_Category <- dbGetQuery(conn, "SELECT S_INFO_WINDCODE, SEC_IND_CODE FROM ASHARESECNINDUSTRIESCLASS WHERE CUR_SIGN = 1")
dbDisconnect(conn)

# 金融行业以“1210”开头
Report_Data <- Industry_Category %>% 
  as_tibble() %>% 
  transmute(`证券代码` = substr(S_INFO_WINDCODE, 1, 6), 
            `是否金融行业` = if_else(substr(SEC_IND_CODE, 1, 4) == "1210", 1L, 0L)) %>% 
  left_join(Report_Data, .)


#####----读取分析师预测数据（SQL），并合并----#####

conn <- dbConnect(RSQLServer::SQLServer(), 
                  server = "124.65.136.170", 
                  database = "Wind_FS", 
                  properties = list(user = "Reuters", password = "ucas2017"))

# 只取分析师报告中对第一年的预测（分析师报告一般会预测将来2-3年的数据）
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

Report_Data <- Analyst_Forecast %>% 
  as_tibble() %>% 
  na.omit() %>% 
  rename(`证券代码` = S_INFO_WINDCODE, `分析师名称` = ANALYST_NAME, `预测日期` = EST_DT, 
         `业绩报告期` = REPORTING_PERIOD, `预测净利润` = EST_NET_PROFIT) %>% 
  mutate_at(vars(`预测日期`, `业绩报告期`), ymd) %>% 
  filter(between(`业绩报告期`, start_time, end_time)) %>% 
  mutate(`证券代码` = substr(`证券代码`, 1, 6), `预测净利润` = `预测净利润` * 10000) %>%   # 预测净利润原单位为万元
  arrange(`证券代码`, `业绩报告期`, `分析师名称`, `预测日期`) %>% 
  inner_join(select(Report_Data, `证券代码`, `业绩报告期`, `业绩预告日期`)) %>% 
  filter(`预测日期` >= `业绩预告日期` - days(90), `预测日期` <= `业绩预告日期`) %>% 
  group_by(`证券代码`, `业绩报告期`, `分析师名称`) %>% 
  filter(row_number(`预测日期`) == n()) %>%   # 只取每个分析师最后一次预测
  group_by(`证券代码`, `业绩报告期`) %>% 
  summarise(`分析师预测净利润_业绩预告之前` = mean(`预测净利润`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(Report_Data, .) %>% 
  # 若业绩预告中净利润的绝对值缺失，则利用业绩预告中净利润的变动幅度与上期值计算
  mutate(`业绩预告归属于母公司所有者的净利润（下限）` = if_else(is.na(`业绩预告归属于母公司所有者的净利润（下限）`), `上期归属于母公司所有者的净利润` * (1 + `业绩预告归属于母公司所有者的净利润变动幅度（下限）%` / 100), `业绩预告归属于母公司所有者的净利润（下限）`), 
         `业绩预告归属于母公司所有者的净利润（上限）` = if_else(is.na(`业绩预告归属于母公司所有者的净利润（上限）`), `上期归属于母公司所有者的净利润` * (1 + `业绩预告归属于母公司所有者的净利润变动幅度（上限）%` / 100), `业绩预告归属于母公司所有者的净利润（上限）`), 
         `业绩预告归属于母公司所有者的净利润（下限）` = if_else(is.na(`业绩预告归属于母公司所有者的净利润（下限）`) & !is.na(`业绩预告归属于母公司所有者的净利润（上限）`), -Inf, `业绩预告归属于母公司所有者的净利润（下限）`), 
         `业绩预告归属于母公司所有者的净利润（上限）` = if_else(is.na(`业绩预告归属于母公司所有者的净利润（上限）`) & !is.na(`业绩预告归属于母公司所有者的净利润（下限）`), Inf, `业绩预告归属于母公司所有者的净利润（上限）`), 
         `业绩预告与之前分析师预测比较` = case_when((`分析师预测净利润_业绩预告之前` >= `业绩预告归属于母公司所有者的净利润（下限）`) & (`分析师预测净利润_业绩预告之前` < `业绩预告归属于母公司所有者的净利润（上限）`) ~ "业绩预告符合之前分析师预测", 
                                  `分析师预测净利润_业绩预告之前` < `业绩预告归属于母公司所有者的净利润（下限）` ~ "业绩预告高于之前分析师预测", 
                                  `分析师预测净利润_业绩预告之前` > `业绩预告归属于母公司所有者的净利润（上限）` ~ "业绩预告低于之前分析师预测"))


#####----比较业绩预告与定期报告----#####

Report_Data <- Report_Data %>% 
  mutate(`归属于母公司所有者的净利润_第一时间` = if_else(!is.na(`业绩快报归属于母公司所有者的净利润`), `业绩快报归属于母公司所有者的净利润`, `归属于母公司所有者的净利润`), 
         `业绩预告与业绩报告比较_离散型` = case_when((`归属于母公司所有者的净利润_第一时间` >= `业绩预告归属于母公司所有者的净利润（下限）`) & (`归属于母公司所有者的净利润_第一时间` < `业绩预告归属于母公司所有者的净利润（上限）`) ~ "业绩预告符合业绩报告", 
                                  `归属于母公司所有者的净利润_第一时间` < `业绩预告归属于母公司所有者的净利润（下限）` ~ "业绩预告高于业绩报告", 
                                  `归属于母公司所有者的净利润_第一时间` > `业绩预告归属于母公司所有者的净利润（上限）` ~ "业绩预告低于业绩报告"), 
         `业绩预告与业绩报告比较_连续型` = case_when((`归属于母公司所有者的净利润_第一时间` >= `业绩预告归属于母公司所有者的净利润（下限）`) & (`归属于母公司所有者的净利润_第一时间` < `业绩预告归属于母公司所有者的净利润（上限）`) ~ 0, 
                                  `归属于母公司所有者的净利润_第一时间` < `业绩预告归属于母公司所有者的净利润（下限）` ~ (`业绩预告归属于母公司所有者的净利润（下限）` - `归属于母公司所有者的净利润_第一时间`) / `归属于母公司所有者的净利润_第一时间`, 
                                  `归属于母公司所有者的净利润_第一时间` > `业绩预告归属于母公司所有者的净利润（上限）` ~ (`业绩预告归属于母公司所有者的净利润（上限）` - `归属于母公司所有者的净利润_第一时间`) / `归属于母公司所有者的净利润_第一时间`))

# ## 几个统计图
# Report_Data %>%
#   filter(as.logical(`是否发布业绩预告`)) %>%
#   filter(abs(`业绩预告与业绩报告比较_连续型`) < 10) %>%
#   ggplot(aes(x = `业绩预告与业绩报告比较_连续型`, fill = as.factor(`应该发布业绩预告`))) +
#   geom_histogram(position = "dodge") + 
#   theme_grey(base_family = "STKaiti")
# hist(Report_Data$业绩预告与业绩报告比较_连续型)
# boxplot(Report_Data$业绩预告与业绩报告比较_连续型)
# quantile(Report_Data$业绩预告与业绩报告比较_连续型, probs = seq(0, 1, by = 0.025), na.rm = TRUE)


#####----End----#####

save(read_csmar_data, start_time, end_time, Report_Data, file = "../Output/01-D-Report-Data.RData")
