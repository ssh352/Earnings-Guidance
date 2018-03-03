#####----4类情况与波动率及暴涨暴跌风险统计----#####

# 计算风险变量并匹配回原表
Mandatory_Forecast_And_Risk <- Return_Rate_Windows %>% 
  arrange(`证券代码`, `业绩报告期`, `交易日期`) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  summarise(Volatility = sd(`涨跌幅`), Skewness = skewness(`涨跌幅`), Kurtosis = kurtosis(`涨跌幅`)) %>% 
  left_join(Mandatory_And_Forecast, .)

## 描述性统计
Mandatory_Forecast_And_Risk %>% 
  group_by(`应该发布`, `是否发布`) %>% 
  summarise(Volatility = mean(Volatility, na.rm = TRUE), 
            Skewness = mean(Skewness, na.rm = TRUE), 
            Kurtosis = mean(Kurtosis, na.rm = TRUE)) %>% 
  write_excel_csv("02-T-发布情况与股票收益波动率.csv")


# 筛选事件窗口内的涨跌停状态数据、计算事件窗口内涨跌停次数并匹配回原表
Mandatory_Forecast_And_Risk <- Up_Down_Limit_Status %>% 
  inner_join(Report_Date_Windows, by = c("证券代码", "交易日期" = "报告公布日期事件窗")) %>% 
  arrange(`证券代码`, `业绩报告期`, `交易日期`) %>% 
  group_by(`证券代码`, `业绩报告期`) %>% 
  summarise(`涨停次数` = sum(`涨跌停状态` == 1, na.rm = TRUE), 
            `跌停次数` = sum(`涨跌停状态` == -1, na.rm = TRUE)) %>% 
  left_join(Mandatory_Forecast_And_Risk, .)

## 描述性统计
Mandatory_Forecast_And_Risk %>% 
  filter(`非金融行业` == 1) %>% 
  # filter(lubridate::month(`业绩报告期`) == 12) %>% 
  group_by(`应该发布`, `是否发布`) %>% 
  summarise(Volatility = mean(Volatility, na.rm = TRUE), 
            Skewness = mean(Skewness, na.rm = TRUE), 
            Kurtosis = mean(Kurtosis, na.rm = TRUE), 
            `涨停次数` = mean(`涨停次数`, na.rm = TRUE), 
            `跌停次数` = mean(`跌停次数`, na.rm = TRUE)) %>% 
  write_excel_csv("02-T-发布情况与涨跌停.csv")
