# Copyright Reserved
# Imports and integrates all necessary REPORT relevant data

#####----Guidance----#####

library(tidyverse)
library(readxl)
library(lubridate)
library(cnquant)

theme_set(
  theme_grey(base_size = 15, base_family = "STSong")
)


#####----读取并合并与实际盈利状况相关的3个数据集（用以判断是否应该发布业绩预告）----#####

# 样本期间2010-12-31 -- 2015-03-31
# 开始时间2010年年报，是因为之前数据库（CSMAR）中没有业绩预告详细数据，只有一个简报
# 结束时间2015年一季，是因为之后更改了业绩预告披露规则
start_time <- ymd("2010-12-31")
end_time <- ymd("2015-03-31")

# 读取合并报表中的归属母公司净利润的数据
Earnings <- read_csmar_data("../Data/01-净利润-上海/FS_Comins.xls", col_names = "Chinese") %>% 
  bind_rows(read_csmar_data("../Data/01-净利润-深圳-主板/FS_Comins.xls", col_names = "Chinese")) %>% 
  rename(`业绩报告期` = `会计期间`) %>% 
  filter(`报表类型` == "A", month(`业绩报告期`) != 1) %>% 
  select(-`报表类型`)

# 读取合并报表中的归属母公司净利润的增长率数据
Earnings_Increase_Rate <- read_csmar_data("../Data/02-净利润增长率-上海/FI_T8.xls", col_names = "Chinese") %>% 
  bind_rows(read_csmar_data("../Data/02-净利润增长率-深圳-主板/FI_T8.xls", col_names = "Chinese")) %>% 
  rename(`证券代码` = `股票代码`, `业绩报告期` = `会计年度`, `报表类型` = `报表类型编码`) %>% 
  filter(`报表类型` == "A") %>%
  select(-`报表类型`)

# 读取合并报表中的归属于母公司每股收益2的数据
EPS <- read_csmar_data("../Data/03-EPS-上海/FI_T9.xls", col_names = "Chinese") %>% 
  bind_rows(read_csmar_data("../Data/03-EPS-深圳-主板/FI_T9.xls", col_names = "Chinese")) %>% 
  rename(`证券代码` = `股票代码`, `业绩报告期` = `截止日期`, `报表类型` = `报表类型编码`) %>% 
  filter(`报表类型` == "A") %>% 
  select(-`报表类型`)

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
  mutate(`归属于母公司所有者的净利润_上期` = lag_certain(`归属于母公司所有者的净利润`, `业绩报告期`, "1 year"), 
         `满足应该发布的三个条件` = case_when(`归属于母公司所有者的净利润` < 0 ~ 1L, 
                                   `归属于母公司所有者的净利润_上期` < 0 ~ 1L, 
                                   abs(`归属于母公司净利润增长率`) >= 0.5 ~ 1L, 
                                   TRUE ~ 0L), 
         `归属于母公司每股收益2_上期` = lag_certain(`归属于母公司每股收益2`, `业绩报告期`, "1 year"), 
         `满足豁免条件` = case_when(month(`业绩报告期`) == 6 & `归属于母公司每股收益2_上期` <= 0.03 ~ 1L, 
                              month(`业绩报告期`) == 9 & `归属于母公司每股收益2_上期` <= 0.04 ~ 1L, 
                              month(`业绩报告期`) == 12 & `归属于母公司每股收益2_上期` <= 0.05 ~ 1L, 
                              TRUE ~ 0L), 
         `应该发布业绩预告` = as.integer(`满足应该发布的三个条件` & month(`业绩报告期`) != 3 & !`满足豁免条件`), 
         # 区分上交所和深交所
         `应该发布业绩预告` = if_else(`应该发布业绩预告` & substr(`证券代码`, 1, 1) == "6" & month(`业绩报告期`) != 12, 0L, `应该发布业绩预告`)) %>% 
  ungroup() %>% 
  filter(between(`业绩报告期`, start_time, end_time))

## 应该发布业绩预告的统计结果（分季度市场）
Category_Count <- Report_Data %>% 
  mutate(`业绩报告季度` = month(`业绩报告期`) %>% factor(labels = c("一季报", "半年报", "三季报", "年报")), 
         `证券交易所` = if_else(substr(`证券代码`, 1, 1) == "6", "上海", "深圳")) %>% 
  group_by(`业绩报告季度`, `证券交易所`) %>% 
  summarise(`总样本` = n(), 
            `满足应该发布的三个条件数` = sum(`满足应该发布的三个条件`), 
            `其中满足豁免条件` = sum(`满足应该发布的三个条件` & `满足豁免条件`), 
            `总共满足豁免条件` = sum(`满足豁免条件`), 
            `应该发布业绩预告数` = sum(`应该发布业绩预告`)) %>% 
  gather(`类别`, `数量`, -`业绩报告季度`, -`证券交易所`) %>% 
  group_by(`业绩报告季度`, `证券交易所`) %>% 
  mutate(`比例` = `数量` / `数量`[1]) %>% 
  gather(`指标`, value, `数量`, `比例`) %>% 
  spread(`类别`, value) %>% 
  arrange(desc(`业绩报告季度`), `证券交易所`, desc(`指标`))

Category_Count


#####----读取业绩预告的数据，并合并----#####

# 读取并处理业绩预告的数据
col_type_tmp <- c(rep("guess", 2), "skip", rep("guess", 6), "skip", rep("guess", 2), rep("skip", 13))
Earnings_Forecast <- read_csmar_data("../Data/04-业绩预告-上海/FIN_F_ForecFin.xls", 
                                     col_names = "Chinese", 
                                     col_types = col_type_tmp) %>% 
  bind_rows(read_csmar_data("../Data/04-业绩预告-深圳-主板/FIN_F_ForecFin.xls", 
                            col_names = "Chinese", 
                            col_types = col_type_tmp)) %>% 
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
  count(`业绩预告发布次数`)

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
  spread(`业绩预告发布情况_4类`, n, fill = 0)


#####----读取业绩快报的数据，并合并----#####

# 读取并处理业绩快报的数据
col_type_tmp <- c("guess", "skip", rep("guess", 3), rep("skip", 9), rep("guess", 3), rep("skip", 25))
Early_Announcement <- read_csmar_data("../Data/05-业绩快报-上海/FIN_F_QuiTraFin.xls", 
                                      col_names = "Chinese", 
                                      col_types = col_type_tmp) %>% 
  bind_rows(read_csmar_data("../Data/05-业绩快报-深圳-主板/FIN_F_QuiTraFin.xls", 
                            col_names = "Chinese", 
                            col_types = col_type_tmp)) %>% 
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
  count(`业绩快报发布次数`)

# 合并数据
Report_Data <- Report_Data %>% 
  left_join(Early_Announcement) %>% 
  mutate(`是否发布业绩快报` = if_else(!is.na(`业绩快报披露日`), 1L, 0L))

## 业绩快报发布情况统计
Report_Data %>% 
  count(`业绩报告期`, `是否发布业绩快报`) %>% 
  spread(`是否发布业绩快报`, n, fill = 0)

## 业绩快报和定期报告实际值的差异
Report_Data %>% 
  mutate_at(vars(`业绩快报归属于母公司所有者的净利润`, `归属于母公司所有者的净利润`), asinh) %>% 
  ggplot(aes(x = `业绩快报归属于母公司所有者的净利润`, y = `归属于母公司所有者的净利润`)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1)

Report_Data %>% 
  mutate(`业绩快报与实际值差异(%)的分布` = (`业绩快报归属于母公司所有者的净利润` - `归属于母公司所有者的净利润`) / `归属于母公司所有者的净利润` * 100) %>% 
  filter(abs(`业绩快报与实际值差异(%)的分布`) < 100) %>%   # 有两个极端值
  ggplot(aes(x = `业绩快报与实际值差异(%)的分布`)) + 
  geom_histogram()


#####----读取并合并定期报告的发布时间数据----#####

Report_Data <- read_csmar_data("../Data/06-定期报告公布日期-上海/IAR_Rept.xls", 
                               col_names = "Chinese") %>% 
  bind_rows(read_csmar_data("../Data/06-定期报告公布日期-深圳-主板/IAR_Rept.xls", 
                            col_names = "Chinese")) %>% 
  select(`证券代码`, `业绩报告期` = `会计截止日期`, `定期报告公布日期` = `报告公布日期`) %>% 
  mutate_at(vars(`业绩报告期`, `定期报告公布日期`), ymd) %>% 
  left_join(Report_Data, .) %>% 
  mutate(`业绩报告_第一时间` = if_else(as.logical(`是否发布业绩快报`), `业绩快报披露日`, `定期报告公布日期`))


#####----读取行业数据（SQL），并合并----#####

conn <- wind_sql_connect()

Industry_Data <- conn %>% 
  tbl("AShareIndustriesClassCITICS") %>% 
  select(S_INFO_WINDCODE, CITICS_IND_CODE, ENTRY_DT, REMOVE_DT) %>% 
  collect()

# # 这是原先按证监会的行业标准
# # 将制造业大类拆分（1203应该是制造业）
# Report_Data <- Industry_Data %>% 
#   as_tibble() %>% 
#   transmute(`证券代码` = substr(S_INFO_WINDCODE, 1, 6), 
#             `行业代码` = if_else(substr(SEC_IND_CODE, 1, 4) == "1203", substr(SEC_IND_CODE, 1, 6), substr(SEC_IND_CODE, 1, 4))) %>% 
#   left_join(Report_Data, .)

Industry_Data <- Industry_Data %>% 
  arrange(S_INFO_WINDCODE, ENTRY_DT) %>% 
  mutate(S_INFO_WINDCODE = substr(S_INFO_WINDCODE, 1L, 6L), 
         CITICS_IND_CODE = substr(CITICS_IND_CODE, 1L, 4L), 
         ENTRY_DT = ymd(ENTRY_DT), 
         REMOVE_DT = ymd(REMOVE_DT)) %>% 
  rename(`证券代码` = S_INFO_WINDCODE)

# Report_Data %>% 
#   select(`证券代码`, `业绩报告期`) %>% 
#   left_join(Industry_Data) %>% 
#   replace_na(list(REMOVE_DT = ymd("99991231"))) %>% 
#   mutate(FLAG = `业绩报告期` >= ENTRY_DT & `业绩报告期` <= REMOVE_DT) %>% 
#   filter(FLAG) %>% 
#   select(`证券代码`, `业绩报告期`, CITICS_IND_CODE) %>% 
#   left_join(Report_Data, .)

Report_Data <- match_industry(Report_Data, Industry_Data, `证券代码`, `业绩报告期`, 
                              ENTRY_DT, REMOVE_DT, CITICS_IND_CODE)


#####----读取分析师预测数据（SQL），并合并----#####

# # 之前用SQL语句读的数据
# # 只取分析师报告中对第一年的预测（分析师报告一般会预测将来2-3年的数据）
# Analyst_Forecast <- dbGetQuery(conn, "
# SELECT S_INFO_WINDCODE, ANALYST_NAME, EST_DT, REPORTING_PERIOD, EST_NET_PROFIT 
#                                FROM 
#                                (
#                                SELECT S_INFO_WINDCODE, ANALYST_NAME, EST_DT, REPORTING_PERIOD, EST_NET_PROFIT, 
#                                ROW_NUMBER() OVER (PARTITION BY S_INFO_WINDCODE, ANALYST_NAME, EST_DT ORDER BY REPORTING_PERIOD ASC) NUM
#                                FROM AShareEarningEst
#                                ) T
#                                WHERE NUM = 1
#                                ")

Analyst_Forecast <- conn %>% 
  tbl("AShareEarningEst") %>% 
  select(S_INFO_WINDCODE, ANALYST_NAME, EST_DT, REPORTING_PERIOD, EST_NET_PROFIT) %>% 
  collect()

DBI::dbDisconnect(conn)

Analyst_Forecast %>% 
  arrange(S_INFO_WINDCODE, ANALYST_NAME, EST_DT, REPORTING_PERIOD) %>% 
  
  # 只取分析师报告中对第一年的预测（分析师报告一般会预测将来2-3年的数据）
  group_by(S_INFO_WINDCODE, ANALYST_NAME, EST_DT) %>% 
  filter(row_number(REPORTING_PERIOD) == 1L) %>% 
  na.omit() %>% 
  
  # 数据处理
  rename(`证券代码` = S_INFO_WINDCODE, `分析师名称` = ANALYST_NAME, `分析师预测日期` = EST_DT, 
         `业绩报告期` = REPORTING_PERIOD, `分析师预测净利润` = EST_NET_PROFIT) %>% 
  mutate_at(vars(`分析师预测日期`, `业绩报告期`), ymd) %>% 
  filter(between(`业绩报告期`, start_time, end_time)) %>% 
  mutate(`证券代码` = substr(`证券代码`, 1, 6), `分析师预测净利润` = `分析师预测净利润` * 10000) %>%   # 分析师预测净利润原单位为万元
  arrange(`证券代码`, `业绩报告期`, `分析师名称`, `分析师预测日期`) %>% 
  
  inner_join(select(Report_Data, `证券代码`, `业绩报告期`, `业绩预告日期`)) %>% 
  filter(`分析师预测日期` >= `业绩预告日期` - days(90), `分析师预测日期` <= `业绩预告日期`) %>% 
  group_by(`证券代码`, `业绩报告期`, `分析师名称`) %>% 
  filter(row_number(`分析师预测日期`) == n()) %>%   # 只取每个分析师最后一次预测
  group_by(`证券代码`, `业绩报告期`) %>% 
  summarise(`分析师预测净利润_业绩预告之前` = mean(`分析师预测净利润`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(Report_Data, .) %>% 
  # 若业绩预告中净利润的绝对值缺失，则利用业绩预告中净利润的变动幅度与上期值计算
  mutate(`业绩预告归属于母公司所有者的净利润（下限）` = if_else(is.na(`业绩预告归属于母公司所有者的净利润（下限）`), `归属于母公司所有者的净利润_上期` * (1 + `业绩预告归属于母公司所有者的净利润变动幅度（下限）%` / 100), `业绩预告归属于母公司所有者的净利润（下限）`), 
         `业绩预告归属于母公司所有者的净利润（上限）` = if_else(is.na(`业绩预告归属于母公司所有者的净利润（上限）`), `归属于母公司所有者的净利润_上期` * (1 + `业绩预告归属于母公司所有者的净利润变动幅度（上限）%` / 100), `业绩预告归属于母公司所有者的净利润（上限）`), 
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
#   geom_histogram(position = "dodge")
# hist(Report_Data$业绩预告与业绩报告比较_连续型)
# boxplot(Report_Data$业绩预告与业绩报告比较_连续型)
# quantile(Report_Data$业绩预告与业绩报告比较_连续型, probs = seq(0, 1, by = 0.025), na.rm = TRUE)


#####----End----#####

save.image("../Output/01-D-Report-Data.RData")
