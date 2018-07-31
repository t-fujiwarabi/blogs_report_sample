library(dplyr)
library(lubridate)

recept_def <- 
  tibble(patient_id = c(018729,018729,018729,018729,018729,018729,018729),
         procedure_code = c("A100","A100","A100","A100","A100","A100","A100"),
         date = ymd(c(20151101,20151102,20151103,20151104,20150108,20150109,20150110)))

# lagを用いてdateが連続しているかどうか同定する
# 日にちが連続している期間をvar2作成、Group_byで行う
recept_def2 <- recept_def %>%
  arrange(date) %>%
  mutate(dd = date - lag(date)) %>%
  mutate(var1 = if_else(
    {dd != 1} | is.na(dd), 1, 0
  )) %>%
  mutate(var2 = cumsum(var1))

# Group_byで入退院日を同定
recept_def2 %>% group_by(patient_id, var2) %>% 
  summarise(admit_date = min(date),
            ent_date = max(date))

