# Copyright Reserved
# Imports and integrates all necessary TRADING relevant data

#####----Guidance----#####

library(tidyverse)
library(readxl)
library(lubridate)
library(cnquant)
load("../Output/01-D-Report-Data.RData")

theme_set(
  theme_grey(base_size = 15, base_family = "STSong")
)


#####----读取个股回报率数据----#####

# # 个股日涨跌幅
# Return_Rate <- "../Data/" %>% 
#   dir() %>% 
#   grep("个股日涨跌幅", ., value = T) %>% 
#   file.path("../Data", ., "STK_MKT_Dalyr.xls") %>% 
#   lapply(read_csmar_data) %>% 
#   bind_rows() %>% 
#   unique() %>% 
#   select(`证券代码`, `交易日期`, `涨跌幅`) %>% 
#   arrange(`证券代码`, `交易日期`) %>% 
#   mutate(`交易日期` = ymd(`交易日期`)) %>% 

# 日个股回报率
col_type_tmp <- c("text", "text", rep("skip", 5), "numeric", rep("skip", 2), "numeric", rep("skip", 6))
Trading_Data <- "../Data/" %>% 
  dir() %>% 
  grep("日个股回报率文件", ., value = T) %>% 
  file.path("../Data", ., "TRD_Dalyr.xls") %>% 
  lapply(read_csmar_data, col_names = "Chinese", col_types = col_type_tmp) %>% 
  bind_rows() %>% 
  unique() %>% 
  rename(`成交额` = `日个股交易金额`, Ri = `考虑现金红利再投资的日个股回报率`) %>% 
  mutate_at(vars(`交易日期`), ymd) %>% 
  filter(between(`交易日期`, start_time - years(1), end_time %m+% months(6))) %>% 
  arrange(`证券代码`, `交易日期`)


#####----读取并合并FF三因子数据（CSMAR，考虑现金红利再投资）----#####

Trading_Data <- read_csmar_data("../Data/08-三因子模型指标-日/STK_MKT_ThrfacDay.xls", 
                                col_names = "Chinese") %>% 
  mutate_at(vars(`交易日期`), ymd) %>% 
  filter(`股票市场类型编码` == "P9706") %>% 
  select(`交易日期`, Rm = `市场风险溢价因子(流通市值加权)`, SMB = `市值因子(流通市值加权)`, HML = `账面市值比因子(流通市值加权)`) %>% 
  left_join(Trading_Data, .)

# # FF三因子模型指标中的市场回报率和市场回报率文件有多少差异？
# read_csmar_data("../Data/日市场回报率文件/TRD_Dalym.xls") %>% 
#   mutate_at(vars(`交易日期`), ymd) %>% 
#   filter(`市场类型` == 1) %>% 
#   select(`交易日期`, Rm2 = `考虑现金红利再投资的日市场回报率(流通市值加权平均法)`) %>% 
#   arrange(`交易日期`) %>% 
#   left_join(FF_Three_Factor, .) %>% 
#   mutate(diff = abs(Rm - Rm2)) %>% 
#   {quantile(.$diff, probs = seq(0, 1, by = 0.05), na.rm = TRUE)}
# ## 这里的市场回报率文件中取的上海的，如果是综合市场应该差异更小


#####----读取并合并无风险利率数据----#####

col_type_tmp <- c(rep("guess", 2), "skip", "guess", rep("skip", 2))
Trading_Data <- read_csmar_data("../Data/09-无风险利率文件/TRD_Nrrate.xls", 
                                col_names = "Chinese", 
                                col_types = col_type_tmp) %>% 
  filter(`无风险利率基准` == "NRI01") %>% 
  select(`交易日期` = `统计日期`, Rf = `日度化无风险利率(%)`) %>% 
  mutate_at(vars(`交易日期`), ymd) %>% 
  left_join(Trading_Data, .) %>% 
  mutate(RiRf = Ri - Rf, RmRf = Rm - Rf)


#####----读取个股涨跌停数据（SQL），并合并----#####

# 从SQL现下数据
conn <- wind_sql_connect()

Up_Down_Limit_Status <- DBI::dbGetQuery(conn,
                                   sprintf("
                                           SELECT S_INFO_WINDCODE, TRADE_DT, UP_DOWN_LIMIT_STATUS
                                           FROM AShareEODDerivativeIndicator
                                           WHERE TRADE_DT BETWEEN '%s' AND '%s'
                                           ",
                                           start_time - years(1) - days(1), end_time %m+% months(6)))  # SQLServer中between好像不包括左端点

DBI::dbDisconnect(conn)

Trading_Data <- Up_Down_Limit_Status %>% 
  as_tibble() %>% 
  na.omit() %>% 
  rename(`证券代码` = S_INFO_WINDCODE, `交易日期` = TRADE_DT, `涨跌停状态` = UP_DOWN_LIMIT_STATUS) %>% 
  mutate(`证券代码` = substr(`证券代码`, 1, 6), `交易日期` = ymd(`交易日期`), `涨跌停状态` = as.integer(`涨跌停状态`)) %>% 
  left_join(Trading_Data, .)

# # 读取并合并涨跌停数据（Wind）
# Trading_Data <- "../Data/涨跌停数据(沪深)" %>% 
#   dir(pattern = "\\.xls$", full.names = TRUE) %>% 
#   `[`(-(140:141)) %>% # 去掉2个空表
#   lapply(read_excel, 
#          col_types = c("skip", "text", "text", rep("skip", 32), "numeric", rep("skip", 3)), 
#          col_names = c("证券代码", "交易日期", "涨跌停状态"), 
#          skip = 1) %>% 
#   bind_rows() %>% 
#   mutate(`证券代码` = substr(`证券代码`, 1, 6), 
#          `交易日期` = ymd(`交易日期`), 
#          `涨跌停状态` = as.integer(`涨跌停状态`)) %>% 
#   arrange(`证券代码`, `交易日期`) %>% 
#   left_join(Trading_Data, .)


#####----End----#####

rm(Up_Down_Limit_Status)
save.image("../Output/02-D-Trading-Data.RData")
